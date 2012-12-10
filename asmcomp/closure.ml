(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Introduction of closures, uncurrying, recognition of direct calls *)

open Misc
open Asttypes
open Primitive
open Lambda
open Switch
open Clambda

(* Auxiliaries for compiling functions *)

let rec split_list n l =
  if n <= 0 then ([], l) else begin
    match l with
      [] -> fatal_error "Closure.split_list"
    | a::l -> let (l1, l2) = split_list (n-1) l in (a::l1, l2)
  end

let rec build_closure_env env_param pos = function
    [] -> Tbl.empty
  | id :: rem ->
      Tbl.add id (Uprim(Pfield pos, [Uvar env_param], Debuginfo.none))
              (build_closure_env env_param (pos+1) rem)

(* Auxiliary for accessing globals.  We change the name of the global
   to the name of the corresponding asm symbol.  This is done here
   and no longer in Cmmgen so that approximations stored in .cmx files
   contain the right names if the -for-pack option is active. *)

let getglobal id =
  Uprim(Pgetglobal (Ident.create_persistent (Compilenv.symbol_for_global id)),
        [], Debuginfo.none)

(* Check if a variable occurs in a [clambda] term. *)

let occurs_var var u =
  let rec occurs = function
      Uvar v -> v = var
    | Uconst (cst,_) -> false
    | Udirect_apply(lbl, args, _) -> List.exists occurs args
    | Ugeneric_apply(funct, args, _) -> occurs funct || List.exists occurs args
    | Uclosure(fundecls, clos) -> List.exists occurs clos
    | Uoffset(u, ofs) -> occurs u
    | Ulet(id, def, body) -> occurs def || occurs body
    | Uletrec(decls, body) ->
        List.exists (fun (id, u) -> occurs u) decls || occurs body
    | Uprim(p, args, _) -> List.exists occurs args
    | Uswitch(arg, s) ->
        occurs arg ||
        occurs_array s.us_actions_consts || occurs_array s.us_actions_blocks
    | Ustaticfail (_, args) -> List.exists occurs args
    | Ucatch(_, _, body, hdlr) -> occurs body || occurs hdlr
    | Utrywith(body, exn, hdlr) -> occurs body || occurs hdlr
    | Uifthenelse(cond, ifso, ifnot) ->
        occurs cond || occurs ifso || occurs ifnot
    | Usequence(u1, u2) -> occurs u1 || occurs u2
    | Uwhile(cond, body) -> occurs cond || occurs body
    | Ufor(id, lo, hi, dir, body) -> occurs lo || occurs hi || occurs body
    | Uassign(id, u) -> id = var || occurs u
    | Usend(_, met, obj, args, _) ->
        occurs met || occurs obj || List.exists occurs args
  and occurs_array a =
    try
      for i = 0 to Array.length a - 1 do
        if occurs a.(i) then raise Exit
      done;
      false
    with Exit ->
      true
  in occurs u

(* Determine whether the estimated size of a clambda term is below
   some threshold *)

let prim_size prim args =
  match prim with
    Pidentity -> 0
  | Pgetglobal id -> 1
  | Psetglobal id -> 1
  | Pmakeblock(tag, mut) -> 5 + List.length args
  | Pfield f -> 1
  | Psetfield(f, isptr) -> if isptr then 4 else 1
  | Pfloatfield f -> 1
  | Psetfloatfield f -> 1
  | Pduprecord _ -> 10 + List.length args
  | Pccall p -> (if p.prim_alloc then 10 else 4) + List.length args
  | Praise -> 4
  | Pstringlength -> 5
  | Pstringrefs | Pstringsets -> 6
  | Pmakearray kind -> 5 + List.length args
  | Parraylength kind -> if kind = Pgenarray then 6 else 2
  | Parrayrefu kind -> if kind = Pgenarray then 12 else 2
  | Parraysetu kind -> if kind = Pgenarray then 16 else 4
  | Parrayrefs kind -> if kind = Pgenarray then 18 else 8
  | Parraysets kind -> if kind = Pgenarray then 22 else 10
  | Pbittest -> 3
  | Pbigarrayref(_, ndims, _, _) -> 4 + ndims * 6
  | Pbigarrayset(_, ndims, _, _) -> 4 + ndims * 6
  | _ -> 2 (* arithmetic and comparisons *)

(* Very raw approximation of switch cost *)

let lambda_smaller lam threshold =
  let size = ref 0 in
  let rec lambda_size lam =
    if !size > threshold then raise Exit;
    match lam with
      Uvar v -> ()
    | Uconst(
        (Const_base(Const_int _ | Const_char _ | Const_float _ |
                        Const_int32 _ | Const_int64 _ | Const_nativeint _) |
             Const_pointer _), _) -> incr size
(* Structured Constants are now emitted during closure conversion. *)
    | Uconst (_, Some _) -> incr size
    | Uconst _ ->
        raise Exit (* avoid duplication of structured constants *)
    | Udirect_apply(fn, args, _) ->
        size := !size + 4; lambda_list_size args
    | Ugeneric_apply(fn, args, _) ->
        size := !size + 6; lambda_size fn; lambda_list_size args
    | Uclosure(defs, vars) ->
        raise Exit (* inlining would duplicate function definitions *)
    | Uoffset(lam, ofs) ->
        incr size; lambda_size lam
    | Ulet(id, lam, body) ->
        lambda_size lam; lambda_size body
    | Uletrec(bindings, body) ->
        raise Exit (* usually too large *)
    | Uprim(prim, args, _) ->
        size := !size + prim_size prim args;
        lambda_list_size args
    | Uswitch(lam, cases) ->
        if Array.length cases.us_actions_consts > 1 then size := !size + 5 ;
        if Array.length cases.us_actions_blocks > 1 then size := !size + 5 ;
        lambda_size lam;
        lambda_array_size cases.us_actions_consts ;
        lambda_array_size cases.us_actions_blocks
    | Ustaticfail (_,args) -> lambda_list_size args
    | Ucatch(_, _, body, handler) ->
        incr size; lambda_size body; lambda_size handler
    | Utrywith(body, id, handler) ->
        size := !size + 8; lambda_size body; lambda_size handler
    | Uifthenelse(cond, ifso, ifnot) ->
        size := !size + 2;
        lambda_size cond; lambda_size ifso; lambda_size ifnot
    | Usequence(lam1, lam2) ->
        lambda_size lam1; lambda_size lam2
    | Uwhile(cond, body) ->
        size := !size + 2; lambda_size cond; lambda_size body
    | Ufor(id, low, high, dir, body) ->
        size := !size + 4; lambda_size low; lambda_size high; lambda_size body
    | Uassign(id, lam) ->
        incr size;  lambda_size lam
    | Usend(_, met, obj, args, _) ->
        size := !size + 8;
        lambda_size met; lambda_size obj; lambda_list_size args
  and lambda_list_size l = List.iter lambda_size l
  and lambda_array_size a = Array.iter lambda_size a in
  try
    lambda_size lam; !size <= threshold
  with Exit ->
    false

(* Check if a clambda term is ``pure'',
   that is without side-effects *and* not containing function definitions *)

let rec is_pure_clambda = function
    Uvar v -> true
  | Uclosure(_,((_::_) as args)) ->
     (* If a closure with an environment is not used, its function is
        never called. It is not the case for function without environment.
        TODO: detect if a function is effectively called *)
     List.for_all is_pure_clambda args
  | Uconst _ -> true
  | Uprim((Psetglobal _ | Psetfield _ | Psetfloatfield _ | Pduprecord _ |
           Pccall _ | Praise | Poffsetref _ | Pstringsetu | Pstringsets |
           Parraysetu _ | Parraysets _ | Pbigarrayset _), _, _) -> false
  | Uprim(p, args, _) -> List.for_all is_pure_clambda args
  | Uifthenelse(u1, u2, u3) ->
     is_pure_clambda u1 && is_pure_clambda u2 && is_pure_clambda u3
  | Uswitch(uarg, uswitch) ->
     is_pure_clambda uarg &&
     List.for_all is_pure_clambda (Array.to_list uswitch.us_actions_consts) &&
     List.for_all is_pure_clambda (Array.to_list uswitch.us_actions_blocks)
  | _ -> false

let sequence_constant_uexp ulam1 ulam2 =
  if is_pure_clambda ulam1
  then
    ulam2
  else Usequence(ulam1, ulam2)

(* Simplify primitive operations on integers *)

let make_const_int n = (Uconst(Const_base(Const_int n), None), value_integer n)
let make_const_ptr n = (Uconst(Const_pointer n, None), value_constptr n)
let make_const_bool b = make_const_ptr(if b then 1 else 0)

let simplif_prim_pure fenv sb p (args, approxs) dbg =
  match approxs with
    [{ approx_desc = Value_integer x }] ->
      begin match p with
        Pidentity -> make_const_int x
      | Pnegint -> make_const_int (-x)
      | Pbswap16 ->
         make_const_int (((x land 0xff) lsl 8) lor
                         ((x land 0xff00) lsr 8))
      | Poffsetint y -> make_const_int (x + y)
      | _ -> (Uprim(p, args, dbg), value_unknown)
      end
  | [{ approx_desc = Value_integer x }; { approx_desc = Value_integer y }] ->
      begin match p with
        Paddint -> make_const_int(x + y)
      | Psubint -> make_const_int(x - y)
      | Pmulint -> make_const_int(x * y)
      | Pdivint when y <> 0 -> make_const_int(x / y)
      | Pmodint when y <> 0 -> make_const_int(x mod y)
      | Pandint -> make_const_int(x land y)
      | Porint -> make_const_int(x lor y)
      | Pxorint -> make_const_int(x lxor y)
      | Plslint -> make_const_int(x lsl y)
      | Plsrint -> make_const_int(x lsr y)
      | Pasrint -> make_const_int(x asr y)
      | Pintcomp cmp ->
          let result = match cmp with
              Ceq -> x = y
            | Cneq -> x <> y
            | Clt -> x < y
            | Cgt -> x > y
            | Cle -> x <= y
            | Cge -> x >= y in
          make_const_bool result
      | _ -> (Uprim(p, args, dbg), value_unknown)
      end
  | [{ approx_desc = Value_constptr x }] ->
      begin match p with
        Pidentity -> make_const_ptr x
      | Pnot -> make_const_bool(x = 0)
      | Pisint -> make_const_bool true
      | Pctconst c ->
          begin
            match c with
            | Big_endian -> make_const_bool Arch.big_endian
            | Word_size -> make_const_int (8*Arch.size_int)
            | Ostype_unix -> make_const_bool (Sys.os_type = "Unix")
            | Ostype_win32 -> make_const_bool (Sys.os_type = "Win32")
            | Ostype_cygwin -> make_const_bool (Sys.os_type = "Cygwin")
          end
      | _ -> (Uprim(p, args, dbg), value_unknown)
      end
  | [{ approx_desc = Value_constptr x }; { approx_desc = Value_constptr y }] ->
      begin match p with
        Psequand -> make_const_bool(x <> 0 && y <> 0)
      | Psequor  -> make_const_bool(x <> 0 || y <> 0)
      | _ -> (Uprim(p, args, dbg), value_unknown)
      end
  | [{ approx_desc = Value_block(_,a) }]
  | [{ approx_desc = Value_closure{ clos_approx_env = a } }] ->
      begin match p with
        Pfield n ->
        if n < Array.length a
        then
          let approx = a.(n) in
          match approx.approx_desc with
            Value_constptr x -> make_const_ptr x
          | Value_integer x -> make_const_int x
          | _ ->
             match approx.approx_var with
               Var_local v when Tbl.mem v fenv ->
                Uvar v, approx
             | Var_global(id,n) ->
                Uprim(Pfield n, [getglobal id], Debuginfo.none), approx
             | _ -> (Uprim(p, args, dbg), value_unknown)
        else (Uprim(p, args, dbg), value_unknown)
      | _ -> (Uprim(p, args, dbg), value_unknown)
      end
  | [] ->
     begin match p with
       Pgetglobal id ->
        let approx =
          if try
              Scanf.sscanf (Ident.name id) "caml_exn_" true
            with
            | _ -> false
          then value_unknown
          else Compilenv.global_approx
                 (* TODO: A bit too hackish *)
                 (Scanf.sscanf (Ident.name id) "caml%s" Ident.create_persistent)
        in
        (Uprim(p, args, dbg), approx)
     | _ -> (Uprim(p, args, dbg), value_unknown)
     end
  | _ ->
      (Uprim(p, args, dbg), value_unknown)

let simplif_prim fenv sb p (args, approxs as args_approxs) dbg =
  if List.for_all is_pure_clambda args
  then simplif_prim_pure fenv sb p args_approxs dbg
  else (Uprim(p, args, dbg), value_unknown)

(* Substitute variables in a [ulambda] term (a body of an inlined function)
   and perform some more simplifications on integer primitives.
   Also perform alpha-conversion on let-bound identifiers to avoid
   clashes with locally-generated identifiers.
   The variables must not be assigned in the term.
   This is used to substitute "trivial" arguments for parameters
   during inline expansion, and also for the translation of let rec
   over functions. *)

let filter_match_cases approx
                       (const_index, const_actions)
                       (block_index, block_actions) =
  let keep vals a =
    Array.mapi (fun i v -> if vals.(i) then v else None) a in
  let clean_and_compress index actions =
    let index = Array.to_list index in
    let add_used (pos,l) (i,action) =
      if List.mem (Some i) index
      then ( pos+1 , (i,(pos,action))::l )
      else ( pos, l )
    in
    let actions = Array.to_list (Array.mapi (fun i v -> i,v) actions) in
    let (_,actions) = List.fold_left add_used (0,[]) actions in
    let index = List.map (function
      | None -> None
      | Some i ->
         let (pos,_) = List.assoc i actions in
         Some pos) index in
    let index = Array.of_list index in
    let actions = Array.of_list (List.map (fun (_,(_,a)) -> a) (List.rev actions)) in
    index, actions
  in
  match approx.approx_desc with
  | Value_tag tags ->
     ([| |], [| |]),
     clean_and_compress (keep tags block_index) block_actions
  | _ ->
     (const_index, const_actions),
     (block_index, block_actions)

let rec simpl_const_approx_desc = function
  | Const_base(Const_int n) -> Value_integer n
  | Const_base(Const_char c) -> Value_integer(Char.code c)
  | Const_pointer n -> Value_constptr n
  | Const_block(tag,a) -> Value_block (tag, Array.of_list (List.map simpl_const_approx a))
  | _ -> Value_unknown

and simpl_const_approx ulam =
  mkapprox (simpl_const_approx_desc ulam)

let clean_no_occur_let id let_val body =
  if occurs_var id body
  then Ulet(id, let_val, body)
  else
     sequence_constant_uexp let_val body

let close_approx_var fenv cenv id =
  let approx = try Tbl.find id fenv with Not_found -> value_unknown in
  match approx.approx_desc with
    Value_integer n ->
      make_const_int n
  | Value_constptr n ->
      make_const_ptr n
  | _ ->
      let subst = try Tbl.find id cenv with Not_found -> Uvar id in
      (subst, approx)

(* extract common informations from approximations of two different values.
   Used to get an approximation of an it_then_else *)

let rec merge_approx a1 a2 =
  let approx_desc = match a1.approx_desc, a2.approx_desc with
  | Value_bottom, _ -> a2.approx_desc
  | _, Value_bottom -> a1.approx_desc
  | Value_unknown, _
  | _, Value_unknown -> Value_unknown
  | Value_tag a, Value_tag b ->
     Value_tag (Array.mapi (fun i b -> a.(i) || b) b)
  | Value_integer i, Value_integer j ->
     if i = j
     then a1.approx_desc
     else Value_unknown
     (* TODO: remember that this is an interger *)
  | Value_constptr i, Value_constptr j ->
     if i = j
     then a1.approx_desc
     else Value_unknown
  | Value_closure c1, Value_closure c2 ->
     if (c1.clos_desc = c2.clos_desc) &&
        (Array.length c1.clos_approx_env = Array.length c2.clos_approx_env)
     then
       Value_closure
         { clos_desc = c1.clos_desc;
           clos_approx_res = merge_approx c1.clos_approx_res c2.clos_approx_res;
           clos_approx_env = merge_approx_array c1.clos_approx_env c2.clos_approx_env }
     else Value_unknown
  | Value_tag tags, Value_block (tag1,_)
  | Value_block (tag1,_), Value_tag tags ->
     let tags = List.map fst
       (List.filter (fun (i,b) -> b)
          (Array.to_list (Array.mapi (fun i v -> i,v) tags))) in
     (possible_tag ~tag:(tag1::tags) ()).approx_desc
  | Value_block (tag1,ar1), Value_block (tag2,ar2) ->
     if (tag1 = tag2)
     then Value_block(tag1, merge_approx_array ar1 ar2)
     else (possible_tag ~tag:[tag1;tag2] ()).approx_desc
  | (Value_closure _ | Value_block _ | Value_integer _
     | Value_constptr _ | Value_tag _ ), _ ->
     Value_unknown
  in
  let approx_var = match a1.approx_var, a2.approx_var with
    | Var_global (id1,p1), Var_global (id2,p2)
         when Ident.same id1 id2 && p1 = p2  ->
       a1.approx_var
    | Var_local id1, Var_local id2
         when Ident.same id1 id2 ->
       a1.approx_var
    | (Var_global _ | Var_local _ | Var_unknown), _ ->
       Var_unknown
  in
  { approx_desc; approx_var }

and merge_approx_array ar1 ar2 =
  Array.of_list (List.map2 merge_approx (Array.to_list ar1) (Array.to_list ar2))

(* merge informations from two approximations of a same value *)

let rec fuse_approx a1 a2 =
  let approx_desc = match a1.approx_desc, a2.approx_desc with
  | Value_unknown, _ -> a2.approx_desc
  | _, Value_unknown -> a1.approx_desc
  | Value_bottom, Value_bottom -> a1.approx_desc
  | Value_tag a, Value_tag b ->
     Value_tag (Array.mapi (fun i b -> a.(i) && b) b)
  | (Value_block (tag1,ar1) as block), Value_tag tags
  | Value_tag tags, (Value_block (tag1,ar1) as block) ->
     assert(tags.(tag1));
     block
  | Value_integer i, Value_integer j ->
     assert(i = j);
     a1.approx_desc
  | Value_constptr i, Value_constptr j ->
     assert(i = j);
     a1.approx_desc
  | Value_closure c1, Value_closure c2 ->
     assert(c1.clos_desc = c2.clos_desc);
     assert(Array.length c1.clos_approx_env = Array.length c2.clos_approx_env);
     Value_closure
       { clos_desc = c1.clos_desc;
         clos_approx_res = fuse_approx c1.clos_approx_res c2.clos_approx_res;
         clos_approx_env =
           fuse_approx_array c1.clos_approx_env c2.clos_approx_env }
  | Value_block (tag1,ar1), Value_block (tag2,ar2) ->
     assert(tag1 = tag2);
     assert(Array.length ar1 = Array.length ar2);
     Value_block (tag1,fuse_approx_array ar1 ar2)
  | (Value_closure _ | Value_block _ | Value_integer _
     | Value_constptr _ | Value_bottom   | Value_tag _ ), _ ->
     assert false
  in
  let approx_var = match a1.approx_var, a2.approx_var with
    | Var_global _, _ ->
       a1.approx_var
    | _, Var_global (id,p) ->
       a2.approx_var
    | Var_local _, _ ->
       (* when there are two var_local just choose one... we could do
       better by looking at the environment to know if one is not
       bounded *)
       a1.approx_var
    | _, Var_local _ ->
       a2.approx_var
    | Var_unknown, Var_unknown ->
       Var_unknown
  in
  { approx_desc; approx_var }

and fuse_approx_array ar1 ar2 =
  Array.of_list (List.map2 fuse_approx (Array.to_list ar1) (Array.to_list ar2))


(* add the approximation of switch argument to the environment *)
let switch_branch_approx (uarg,arg_approx) block index i fenv =
  let actions_index =
    let l = ref [] in
    Array.iteri (fun p v -> if v = Some i then l := p:: !l) index;
    !l in
  let branch_approx =
    let approx =
      if block
      then
        possible_tag ~tag:actions_index ()
      else
        match actions_index with
        | [i] -> value_constptr i
        | _ -> value_unknown in
    fuse_approx approx arg_approx
  in
  let fenv =
    match uarg with
    | Uvar id ->
       Tbl.add id branch_approx fenv
    | _ -> fenv
  in
  let fenv =
    match arg_approx.approx_var with
    | Var_local id ->
       Tbl.add id branch_approx fenv
    | _ -> fenv
  in
  fenv

let sequence_bottom ulam approx normal =
  match approx.approx_desc with
  | Value_bottom ->
     ulam, approx
  | _ -> normal

let switch_approx consts blocks =
  let approxs =
    Array.to_list (Array.map snd consts) @
    Array.to_list (Array.map snd blocks) in
  match approxs with
  | [] -> value_unknown
  | t::q -> List.fold_left merge_approx t q

let approx_ulam fenv sb = function
    Uvar id ->
    (try Tbl.find id fenv with Not_found -> value_unknown)
  | _ -> value_unknown

let rec substitute_approx fenv sb ulam =
  match ulam with
    Uvar id ->
     let ulam =
       try
         Tbl.find id sb
       with Not_found -> ulam in
     let approx = approx_ulam fenv sb ulam in
     begin match approx.approx_desc with
       Value_integer n ->
        make_const_int n
     | Value_constptr n ->
        make_const_ptr n
     | _ ->
        ulam, approx
     end
  | Uconst (c,_) ->
     ulam, simpl_const_approx c
  | Udirect_apply(lbl, args, dbg) ->
      Udirect_apply(lbl, List.map (substitute fenv sb) args, dbg),
      (* TODO: better *)
      value_unknown
  | Ugeneric_apply(fn, args, dbg) ->
      Ugeneric_apply(substitute fenv sb fn, List.map (substitute fenv sb) args, dbg),
      (* TODO: better *)
      value_unknown
  | Uclosure(defs, env) ->
      (* Question: should we rename function labels as well?  Otherwise,
         there is a risk that function labels are not globally unique.
         This should not happen in the current system because:
         - Inlined function bodies contain no Uclosure nodes
           (cf. function [lambda_smaller])
         - When we substitute offsets for idents bound by let rec
           in [close], case [Lletrec], we discard the original
           let rec body and use only the substituted term. *)
      Uclosure(defs, List.map (substitute fenv sb) env),
      (* TODO: better *)
      value_unknown
  | Uoffset(u, ofs) ->
     Uoffset(substitute fenv sb u, ofs),
      (* TODO: better *)
      value_unknown
  | Ulet(id, u1, u2) ->
      let id' = Ident.rename id in
      let let_val,approx = substitute_approx fenv sb u1 in
      let approx = { approx with approx_var = Var_local id' } in
      let fenv' = Tbl.add id' approx fenv in
      let ubody,approx = substitute_approx fenv' (Tbl.add id (Uvar id') sb) u2 in
      clean_no_occur_let id' let_val ubody, approx
  | Uletrec(bindings, body) ->
      let bindings1 =
        List.map (fun (id, rhs) -> (id, Ident.rename id, rhs)) bindings in
      let sb' =
        List.fold_right
          (fun (id, id', _) s -> Tbl.add id (Uvar id') s)
          bindings1 sb in
      Uletrec(
        List.map (fun (id, id', rhs) -> (id', substitute fenv sb' rhs)) bindings1,
        substitute fenv sb' body),
      (* TODO: better *)
      value_unknown
  | Uprim(Pmakeblock(tag, mut) as prim, ulams, dinfo) ->
      let (ulams, approxs) = List.split
        (List.map (substitute_approx fenv sb) ulams) in
      (Uprim(prim, ulams, dinfo),
       begin match mut with
           Immutable -> mkapprox (Value_block(tag, Array.of_list approxs))
         | Mutable -> value_unknown
       end)
  | Uprim(Praise, args, dinfo) ->
     let uargs = List.map (substitute fenv sb) args in
     Uprim(Praise, uargs, dinfo), value_bottom
  | Uprim(p, args, dbg) ->
      let sargs = List.map (substitute_approx fenv sb) args in
      let uargs = List.map fst sargs in
      let approxs = List.map snd sargs in
      simplif_prim fenv sb p (uargs, approxs) dbg
  | Uswitch(arg, sw) ->
     substitute_uswitch fenv sb arg sw
  | Ustaticfail (nfail, args) ->
      Ustaticfail (nfail, List.map (substitute fenv sb) args),
      value_bottom
  | Ucatch(nfail, ids, u1, u2) ->
      Ucatch(nfail, ids, substitute fenv sb u1, substitute fenv sb u2),
      value_unknown
  | Utrywith(u1, id, u2) ->
      let id' = Ident.rename id in
      Utrywith(substitute fenv sb u1, id', substitute fenv (Tbl.add id (Uvar id') sb) u2),
      value_unknown
  | Uifthenelse(u1, u2, u3) ->
      begin match substitute_approx fenv sb u1 with
        (su1, { approx_desc = Value_constptr n }) ->
        let ulam, approx =
          if n <> 0 then substitute_approx fenv sb u2
          else substitute_approx fenv sb u3 in
        sequence_constant_uexp su1 ulam, approx
      | (su1, { approx_desc = Value_block _ }) ->
        let ulam, approx = substitute_approx fenv sb u2 in
        sequence_constant_uexp su1 ulam, approx
      | (su1, { approx_desc = Value_tag _ }) ->
        let ulam, approx = substitute_approx fenv sb u2 in
        sequence_constant_uexp su1 ulam, approx
      | (su1, ({ approx_desc = Value_bottom } as approx)) ->
          su1, approx
      | (su1, _) ->
          let (uifso, approxso) = substitute_approx fenv sb u2 in
          let (uifnot, approxnot) = substitute_approx fenv sb u3 in
          let approx = merge_approx approxso approxnot in
          Uifthenelse(su1, uifso, uifnot), approx
      end
  | Usequence(u1, u2) ->
     let s1,approx1 = substitute_approx fenv sb u1 in
     let s2,approx2 = substitute_approx fenv sb u2 in
     sequence_bottom s1 approx1
       ((sequence_constant_uexp s1 s2),approx2)
  | Uwhile(u1, u2) ->
      let (ucond, cond_approx) = substitute_approx fenv sb u1 in
      let ubody = substitute fenv sb u2 in
      begin match cond_approx.approx_desc with
      | Value_constptr 0 ->
         make_const_ptr 0
      | Value_constptr _ ->
         (Uwhile(ucond, ubody), value_bottom)
      | _ ->
         (Uwhile(ucond, ubody), value_unknown)
      end
  | Ufor(id, u1, u2, dir, u3) ->
      let id' = Ident.rename id in
      Ufor(id', substitute fenv sb u1, substitute fenv sb u2, dir,
           substitute fenv (Tbl.add id (Uvar id') sb) u3),
      value_unknown
  | Uassign(id, u) ->
      let id' =
        try
          match Tbl.find id sb with Uvar i -> i | _ -> assert false
        with Not_found ->
          id in
      Uassign(id', substitute fenv sb u), value_unknown
  | Usend(k, u1, u2, ul, dbg) ->
      Usend(k, substitute fenv sb u1, substitute fenv sb u2, List.map (substitute fenv sb) ul, dbg),
      value_unknown

and substitute fenv sb ulam =
  fst (substitute_approx fenv sb ulam)

and substitute_uswitch fenv sb arg sw =
  let uarg, approx = substitute_approx fenv sb arg in
  match approx.approx_desc with
  | Value_bottom ->
     uarg, approx
  | Value_constptr i ->
     (match sw.us_index_consts.(i) with
      | None -> assert false
      (* TODO: Verify that this can't happen
         normaly this branch shouldn't have been compiled
         if there is no possible value *)
      | Some n -> substitute_approx fenv sb (sw.us_actions_consts.(n)))
  | Value_block (tag,_) ->
     (match sw.us_index_blocks.(tag) with
      | None -> assert false
      (* TODO: Verify that this can't happen
         normaly this branch shouldn't have been compiled
         if there is no possible value *)
      | Some n -> substitute_approx fenv sb (sw.us_actions_blocks.(n)))
  | _ ->
     let ((us_index_consts, actions_consts),
          (us_index_blocks, actions_blocks)) =
       filter_match_cases approx
                          (sw.us_index_consts, sw.us_actions_consts)
                          (sw.us_index_blocks, sw.us_actions_blocks) in
     let substitute_branch block index i ulam =
       let fenv = switch_branch_approx (uarg,approx) block index i fenv in
       substitute_approx fenv sb ulam
     in
     let consts = Array.mapi (substitute_branch false us_index_consts) actions_consts in
     let blocks = Array.mapi (substitute_branch true us_index_blocks) actions_blocks in
     Uswitch(uarg,
       { us_index_consts;
         us_actions_consts = Array.map fst consts;
         us_index_blocks;
         us_actions_blocks = Array.map fst blocks;
        }), switch_approx consts blocks

(* Perform an inline expansion *)

let is_simple_argument = function
    Uvar _ -> true
  | Uconst(Const_base(Const_int _ | Const_char _ | Const_float _ |
                      Const_int32 _ | Const_int64 _ | Const_nativeint _),_) ->
      true
  | Uconst(Const_pointer _, _) -> true
  | _ -> false

let rec no_effects = function
    Uclosure _ -> true
  | Uconst(Const_base(Const_string _),_) -> true
  | Uprim((Psetglobal _ | Psetfield _ | Psetfloatfield _ | Pduprecord _ |
           Pccall _ | Praise | Poffsetref _ | Pstringsetu | Pstringsets |
           Parraysetu _ | Parraysets _ | Pbigarrayset _), _, _) -> false
  | Uconst(_, _) -> true
  | Uprim(p, args, _) -> List.for_all no_effects args
  | u -> is_simple_argument u

let rec bind_params_rec subst fenv params args body =
  match (params, args) with
    ([], []) ->
    substitute_approx fenv subst body
  | (p1 :: pl, (a1,approx) :: al) ->
      if is_simple_argument a1 then
        let fenv = match a1 with
            Uvar id -> Tbl.add id approx fenv
          | _ -> fenv in
        bind_params_rec (Tbl.add p1 a1 subst) (Tbl.add p1 approx fenv) pl al body
      else begin
        let p1' = Ident.rename p1 in
        let fenv = Tbl.add p1' approx (Tbl.add p1 approx fenv) in
        let body',approx =
          bind_params_rec (Tbl.add p1 (Uvar p1') subst) fenv pl al body in
        (if occurs_var p1' body' then Ulet(p1', a1, body')
         else
           if no_effects a1 then body'
           else Usequence(a1, body')),
        approx
      end
  | (_, _) -> assert false

let bind_params fenv subst params args body =
  (* Reverse parameters and arguments to preserve right-to-left
     evaluation order (PR#2910). *)
  bind_params_rec subst fenv (List.rev params) (List.rev args) body

(* Check if a lambda term is ``pure'',
   that is without side-effects *and* not containing function definitions *)

let rec is_pure = function
    Lvar v -> true
  | Lconst cst -> true
  | Lprim((Psetglobal _ | Psetfield _ | Psetfloatfield _ | Pduprecord _ |
           Pccall _ | Praise | Poffsetref _ | Pstringsetu | Pstringsets |
           Parraysetu _ | Parraysets _ | Pbigarrayset _), _) -> false
  | Lprim(p, args) -> List.for_all is_pure args
  | Levent(lam, ev) -> is_pure lam
  | _ -> false

(* Generate a direct application *)

let direct_apply fenv subst fundesc funct ufunct uargs fun_approx =
  let app_args =
    if fundesc.fun_closed then uargs else uargs @ [ufunct,fun_approx] in
  let app, approx =
    match fundesc.fun_inline with
      None -> Udirect_apply(fundesc.fun_label, List.map fst app_args, Debuginfo.none), value_unknown
    | Some(params, body) -> bind_params fenv subst params app_args body in
  (* If ufunct can contain side-effects or function definitions,
     we must make sure that it is evaluated exactly once.
     If the function is not closed, we evaluate ufunct as part of the
     arguments.
     If the function is closed, we force the evaluation of ufunct first. *)
  if not fundesc.fun_closed || is_pure funct
  then app, approx
  else Usequence(ufunct, app), approx

(* If a term has approximation Value_integer or Value_constptr and is pure,
   replace it by an integer constant *)

let check_constant_result fenv lam ulam approx =
  match approx.approx_desc with
    Value_integer n when is_pure lam -> make_const_int n
  | Value_constptr n when is_pure lam -> make_const_ptr n
  | _ ->
     match approx.approx_var with
       Var_local v when Tbl.mem v fenv && is_pure lam ->
        Uvar v, approx
     | Var_global(id,n) when is_pure lam ->
        Uprim(Pfield n, [getglobal id], Debuginfo.none), approx
     | _ -> (ulam, approx)

(* Evaluate an expression with known value for its side effects only,
   or discard it if it's pure *)

let sequence_constant_expr lam ulam1 (ulam2, approx2 as res2) =
  if is_pure lam then res2 else (Usequence(ulam1, ulam2), approx2)

(* Maintain the approximation of the global structure being defined *)

let global_approx = ref([||] : value_approximation array)

(* Maintain the nesting depth for functions *)

let function_nesting_depth = ref 0
let excessive_function_nesting_depth = 5

(* Decorate clambda term with debug information *)

let rec add_debug_info ev u =
  match ev.lev_kind with
  | Lev_after _ ->
      begin match u with
      | Udirect_apply(lbl, args, dinfo) ->
          Udirect_apply(lbl, args, Debuginfo.from_call ev)
      | Ugeneric_apply(Udirect_apply(lbl, args1, dinfo1),
                       args2, dinfo2) ->
          Ugeneric_apply(Udirect_apply(lbl, args1, Debuginfo.from_call ev),
                         args2, Debuginfo.from_call ev)
      | Ugeneric_apply(fn, args, dinfo) ->
          Ugeneric_apply(fn, args, Debuginfo.from_call ev)
      | Uprim(Praise, args, dinfo) ->
          Uprim(Praise, args, Debuginfo.from_call ev)
      | Uprim(p, args, dinfo) ->
          Uprim(p, args, Debuginfo.from_call ev)
      | Usend(kind, u1, u2, args, dinfo) ->
          Usend(kind, u1, u2, args, Debuginfo.from_call ev)
      | Usequence(u1, u2) ->
          Usequence(u1, add_debug_info ev u2)
      | _ -> u
      end
  | _ -> u

(* removing all local binding informations from an approximation:
   used for building function environment: It is not possible to
   have access to a local variable bound outside of a function from
   inside of the function. *)
let clean_local_approx fenv =
  Tbl.map (fun _ approx -> remove_approx approx) fenv

(* Uncurry an expression and explicitate closures.
   Also return the approximation of the expression.
   The approximation environment [fenv] maps idents to approximations.
   Idents not bound in [fenv] approximate to [Value_unknown].
   The closure environment [cenv] maps idents to [ulambda] terms.
   It is used to substitute environment accesses for free identifiers. *)

let close_var fenv cenv id =
  let (ulam, app) = close_approx_var fenv cenv id in ulam

let rec close fenv cenv = function
    Lvar id ->
      close_approx_var fenv cenv id
  | Lconst cst ->
      let ulam = match cst with
          Const_base(Const_int _)
        | Const_base(Const_char _)
        | Const_pointer _ -> Uconst (cst, None)
        | _ -> Uconst (cst, Some (Compilenv.new_structured_constant cst true))
      in
      ulam, simpl_const_approx cst
  | Lfunction(kind, params, body) as funct ->
      close_one_function fenv cenv (Ident.create "fun") funct

    (* We convert [f a] to [let a' = a in fun b c -> f a' b c]
       when fun_arity > nargs *)
  | Lapply(funct, args, loc) ->
      let nargs = List.length args in
      begin match (close fenv cenv funct, close_list_approx fenv cenv args) with
        ((ufunct, ({ approx_desc = Value_closure{ clos_desc = fundesc;
                                                 clos_approx_res = approx_res } } as fun_approx)),
         ([Uprim(Pmakeblock(_, _), uargs, _)],[approx]))
        when List.length uargs = - fundesc.fun_arity ->
          let uargs = match approx.approx_desc with
            | Value_block(tag,a) -> List.combine uargs (Array.to_list a)
            | _ -> assert false
          in
          let app, approx_subst = direct_apply fenv cenv fundesc funct ufunct uargs fun_approx in
          let approx = fuse_approx approx_subst approx_res in
          (app, approx)
      | ((ufunct, ({ approx_desc = Value_closure{ clos_desc = fundesc;
                                                 clos_approx_res = approx_res }}as fun_approx)),
         (uargs,approx))
        when nargs = fundesc.fun_arity ->
          let uargs = List.combine uargs approx in
          let app, approx_subst = direct_apply fenv cenv fundesc funct ufunct uargs fun_approx in
          let approx = fuse_approx approx_subst approx_res in
          (app, approx)

      | ((ufunct, { approx_desc = Value_closure{ clos_desc = fundesc;
                                                 clos_approx_res = approx_res }}),
         (uargs,_))
          when nargs < fundesc.fun_arity ->
        let first_args = List.map (fun arg ->
          (Ident.create "arg", arg) ) uargs in
        let final_args = Array.to_list (Array.init (fundesc.fun_arity - nargs) (fun _ ->
          Ident.create "arg")) in
        let rec iter args body =
          match args with
              [] -> body
            | (arg1, arg2) :: args ->
              iter args
                (Ulet ( arg1, arg2, body))
        in
        let internal_args =
          (List.map (fun (arg1, arg2) -> Lvar arg1) first_args)
          @ (List.map (fun arg -> Lvar arg ) final_args)
        in
        let (new_fun, approx) = close fenv cenv
          (Lfunction(
            Curried, final_args, Lapply(funct, internal_args, loc)))
        in
        let new_fun = iter first_args new_fun in
        (new_fun, approx)

      | ((ufunct, ({ approx_desc = Value_closure{ clos_desc = fundesc;
                                                 clos_approx_res = approx_res } } as fun_approx)),
         (uargs,approx))
        when fundesc.fun_arity > 0 && nargs > fundesc.fun_arity ->
          let (first_approx, rem_approx) = split_list fundesc.fun_arity approx in
          let (first_args, rem_args) = split_list fundesc.fun_arity uargs in
          let first_args = List.combine first_args first_approx in
          let app, _ = direct_apply fenv cenv fundesc funct ufunct first_args fun_approx in
          (Ugeneric_apply(app, rem_args, Debuginfo.none),
           value_unknown)
      | ((ufunct, _), (uargs,_)) ->
          (Ugeneric_apply(ufunct, uargs, Debuginfo.none), value_unknown)
      end
  | Lsend(kind, met, obj, args, _) ->
      let (umet, _) = close fenv cenv met in
      let (uobj, _) = close fenv cenv obj in
      (Usend(kind, umet, uobj, close_list fenv cenv args, Debuginfo.none),
       value_unknown)
  | Llet(str, id, lam, body) ->
      let (ulam, alam) = close_named fenv cenv id lam in
      let alam = { alam with approx_var = Var_local id } in
      begin match (str, alam) with
        (Variable, _) ->
          let (ubody, abody) = close fenv cenv body in
          (clean_no_occur_let id ulam ubody, abody)
      | (_, {approx_desc = (Value_integer _ | Value_constptr _)})
        when str = Alias || is_pure lam ->
          close (Tbl.add id alam fenv) cenv body
      | (_, _) ->
          let (ubody, abody) = close (Tbl.add id alam fenv) cenv body in
          (clean_no_occur_let id ulam ubody, abody)
      end
  | Lletrec(defs, body) ->
      if List.for_all
           (function (id, Lfunction(_, _, _)) -> true | _ -> false)
           defs
      then begin
        (* Simple case: only function definitions *)
        let (clos, infos) = close_functions fenv cenv defs in
        let clos_ident = Ident.create "clos" in
        let fenv_body =
          List.fold_right
            (fun (id, pos, approx) fenv -> Tbl.add id approx fenv)
            infos fenv in
        let (ubody, approx) = close fenv_body cenv body in
        let sb =
          List.fold_right
            (fun (id, pos, approx) sb ->
              Tbl.add id (Uoffset(Uvar clos_ident, pos)) sb)
            infos Tbl.empty in
        (Ulet(clos_ident, clos, substitute fenv sb ubody),
         approx)
      end else begin
        (* General case: recursive definition of values *)
        let rec clos_defs = function
          [] -> ([], fenv)
        | (id, lam) :: rem ->
            let (udefs, fenv_body) = clos_defs rem in
            let (ulam, approx) = close fenv cenv lam in
            ((id, ulam) :: udefs, Tbl.add id approx fenv_body) in
        let (udefs, fenv_body) = clos_defs defs in
        let (ubody, approx) = close fenv_body cenv body in
        (Uletrec(udefs, ubody), approx)
      end
  | Lprim(Pdirapply loc,[funct;arg])
  | Lprim(Prevapply loc,[arg;funct]) ->
      close fenv cenv (Lapply(funct, [arg], loc))
  | Lprim(Pgetglobal id, []) as lam ->
      check_constant_result fenv lam
                            (getglobal id)
                            (Compilenv.global_approx id)
  | Lprim(Pmakeblock(tag, mut) as prim, lams) ->
      let (ulams, approxs) = List.split (List.map (close fenv cenv) lams) in
      (Uprim(prim, ulams, Debuginfo.none),
       begin match mut with
           Immutable -> mkapprox (Value_block(tag, Array.of_list approxs))
         | Mutable -> value_unknown
       end)
  | Lprim(Pfield n, [lam]) ->
      let (ulam, approx) = close fenv cenv lam in
      let fieldapprox =
        match approx.approx_desc with
          Value_block(_,a)
        | Value_closure{ clos_approx_env = a } when n < Array.length a -> a.(n)
        | _ -> value_unknown in
      check_constant_result fenv lam (Uprim(Pfield n, [ulam], Debuginfo.none)) fieldapprox
  | Lprim(Psetfield(n, _), [Lprim(Pgetglobal id, []); lam]) ->
      let (ulam, approx) = close fenv cenv lam in
      let approx = { approx with approx_var = Var_global(id,n) } in
      (!global_approx).(n) <- approx;
      (Uprim(Psetfield(n, false), [getglobal id; ulam], Debuginfo.none),
       value_unknown)
  | Lprim(Praise, [Levent(arg, ev)]) ->
      let (ulam, approx) = close fenv cenv arg in
      (Uprim(Praise, [ulam], Debuginfo.from_raise ev),
       value_bottom)
  | Lprim(Praise, [arg]) ->
      let (ulam, approx) = close fenv cenv arg in
      (Uprim(Praise, [ulam], Debuginfo.none),
       value_bottom)
  | Lprim(p, args) ->
      simplif_prim fenv cenv p (close_list_approx fenv cenv args) Debuginfo.none
  | Lswitch(arg, sw) ->
(* NB: failaction might get copied, thus it should be some Lstaticraise *)
      let ((uarg, approx) as arg) = close fenv cenv arg in
      let use_action i l =
        let case =
          try List.assoc i l with
          | Not_found ->
             match sw.sw_failaction with
             | None -> assert false
             | Some a -> a
        in
        close fenv cenv case
      in
      begin
        match approx.approx_desc with
        | Value_bottom ->
           uarg, approx
        | Value_constptr i ->
           use_action i sw.sw_consts
        | Value_block (tag,_) ->
           use_action tag sw.sw_blocks
        | _ ->
           let const_index, const_actions =
             close_switch fenv cenv sw.sw_consts sw.sw_numconsts sw.sw_failaction arg false
           and block_index, block_actions =
             close_switch fenv cenv sw.sw_blocks sw.sw_numblocks sw.sw_failaction arg true in
           let (const_index, const_actions),
               (block_index, block_actions)
             = filter_match_cases approx (const_index, const_actions)
                                         (block_index, block_actions) in
           (Uswitch(uarg,
                    {us_index_consts = const_index;
                     us_actions_consts = Array.map fst const_actions;
                     us_index_blocks = block_index;
                     us_actions_blocks = Array.map fst block_actions}),
            switch_approx const_actions block_actions)
      end
  | Lstaticraise (i, args) ->
      (Ustaticfail (i, close_list fenv cenv args), value_bottom)
  | Lstaticcatch(body, (i, vars), handler) ->
      let (ubody, _) = close fenv cenv body in
      let (uhandler, _) = close fenv cenv handler in
      (Ucatch(i, vars, ubody, uhandler), value_unknown)
  | Ltrywith(body, id, handler) ->
      let (ubody, _) = close fenv cenv body in
      let (uhandler, _) = close fenv cenv handler in
      (Utrywith(ubody, id, uhandler), value_unknown)
  | Lifthenelse(arg, ifso, ifnot) ->
      begin match close fenv cenv arg with
        (uarg, { approx_desc = Value_constptr n }) ->
          sequence_constant_expr arg uarg
            (close fenv cenv (if n = 0 then ifnot else ifso))
      | (uarg, { approx_desc = Value_block _ }) ->
         sequence_constant_expr arg uarg
            (close fenv cenv ifso)
      | (uarg, { approx_desc = Value_tag _ }) ->
         sequence_constant_expr arg uarg
            (close fenv cenv ifso)
      | (uarg, ({ approx_desc = Value_bottom } as approx)) ->
          uarg, approx
      | (uarg, _ ) ->
          let (uifso, approxso) = close fenv cenv ifso in
          let (uifnot, approxnot) = close fenv cenv ifnot in
          let approx = merge_approx approxso approxnot in
          (Uifthenelse(uarg, uifso, uifnot), approx)
      end
  | Lsequence(lam1, lam2) ->
      let (ulam1, approx1) = close fenv cenv lam1 in
      let (ulam2, approx2) = close fenv cenv lam2 in
      sequence_bottom ulam1 approx1
        ((sequence_constant_uexp ulam1 ulam2),approx2)
  | Lwhile(cond, body) ->
      let (ucond, cond_approx) = close fenv cenv cond in
      let (ubody, _) = close fenv cenv body in
      begin match cond_approx.approx_desc with
      | Value_constptr 0 ->
         make_const_ptr 0
      | Value_constptr _ ->
         (Uwhile(ucond, ubody), value_bottom)
      | _ ->
         (Uwhile(ucond, ubody), value_unknown)
      end
  | Lfor(id, lo, hi, dir, body) ->
      let (ulo, _) = close fenv cenv lo in
      let (uhi, _) = close fenv cenv hi in
      let (ubody, _) = close fenv cenv body in
      (Ufor(id, ulo, uhi, dir, ubody), value_unknown)
  | Lassign(id, lam) ->
      let (ulam, _) = close fenv cenv lam in
      (Uassign(id, ulam), value_unknown)
  | Levent(lam, ev) ->
      let (ulam, approx) = close fenv cenv lam in
      (add_debug_info ev ulam, approx)
  | Lifused _ ->
      assert false

and close_list fenv cenv = function
    [] -> []
  | lam :: rem ->
      let (ulam, _) = close fenv cenv lam in
      ulam :: close_list fenv cenv rem

and close_list_approx fenv cenv = function
    [] -> ([], [])
  | lam :: rem ->
      let (ulam, approx) = close fenv cenv lam in
      let (ulams, approxs) = close_list_approx fenv cenv rem in
      (ulam :: ulams, approx :: approxs)

and close_named fenv cenv id = function
    Lfunction(kind, params, body) as funct ->
      close_one_function fenv cenv id funct
  | lam ->
      close fenv cenv lam

(* Build a shared closure for a set of mutually recursive functions *)

and close_functions fenv cenv fun_defs =
  (* Update and check nesting depth *)
  incr function_nesting_depth;
  let initially_closed =
    !function_nesting_depth < excessive_function_nesting_depth in
  (* Determine the free variables of the functions *)
  let fv_set = free_variables (Lletrec(fun_defs, lambda_unit)) in
  let fv = IdentSet.elements fv_set in
  (* Build the function descriptors for the functions.
     Initially all functions are assumed not to need their environment
     parameter. *)
  let uncurried_defs =
    List.map
      (function
          (id, Lfunction(kind, params, body)) ->
            let label = Compilenv.make_symbol (Some (Ident.unique_name id)) in
            let arity = List.length params in
            let fundesc =
              {fun_label = label;
               fun_arity = (if kind = Tupled then -arity else arity);
               fun_closed = initially_closed;
               fun_inline = None } in
            (id, params, body, fundesc)
        | (_, _) -> fatal_error "Closure.close_functions")
      fun_defs in
  (* Build an approximate fenv for compiling the functions *)
  let original_env = fenv in
  let fenv =
    Tbl.fold (fun var approx acc ->
              if IdentSet.mem var fv_set
              then Tbl.add var approx acc
              else acc) fenv Tbl.empty in
  let fenv = clean_local_approx fenv in
  (* TODO: trouver une manière plus propre / plus efficace de faire ça.
     Une manière de le faire serait de mettre un niveau de binding aux
     variable et d'augmenter à chaque rentrée dans une cloture.
     Si le niveau est different du niveau courant, alors c'est interdit
     de réécrire comme une variable *)
  (* ou ne le faire que sur les variables libre *)
  let add_params params fenv =
    List.fold_left (fun fenv id -> Tbl.add id (mkapprox ~id Value_unknown) fenv)
                    fenv params in
  let fenv_rec =
    List.fold_right
      (fun (id, params, body, fundesc) fenv ->
       let fenv =
         Tbl.add id (value_closure fundesc value_unknown [||]) fenv in
      add_params params fenv)
      uncurried_defs fenv in
  (* Determine the offsets of each function's closure in the shared block *)
  let env_pos = ref (-1) in
  let clos_offsets =
    List.map
      (fun (id, params, body, fundesc) ->
        let pos = !env_pos + 1 in
        env_pos := !env_pos + 1 + (if fundesc.fun_arity <> 1 then 3 else 2);
        pos)
      uncurried_defs in
  let fv_pos = !env_pos in
  let fva = Array.of_list fv in
  let env_approx = Array.init (List.length fv + fv_pos)
    (fun i -> if i < fv_pos
           then value_unknown
           else
             try Tbl.find fva.(i - fv_pos) original_env with Not_found -> value_unknown) in
  (* This reference will be set to false if the hypothesis that a function
     does not use its environment parameter is invalidated. *)
  let useless_env = ref initially_closed in
  (* Translate each function definition *)
  let clos_fundef (id, params, body, fundesc) env_pos =
    let dbg = match body with
      | Levent (_,({lev_kind=Lev_function} as ev)) -> Debuginfo.from_call ev
      | _ -> Debuginfo.none in
    let env_param = Ident.create "env" in
    let cenv_fv =
      build_closure_env env_param (fv_pos - env_pos) fv in
    let cenv_body =
      List.fold_right2
        (fun (id, params, arity, body) pos env ->
          Tbl.add id (Uoffset(Uvar env_param, pos - env_pos)) env)
        uncurried_defs clos_offsets cenv_fv in
    let (ubody, approx) = close fenv_rec cenv_body body in
    if !useless_env && occurs_var env_param ubody then useless_env := false;
    let fun_params = if !useless_env then params else params @ [env_param] in
    ({ label  = fundesc.fun_label;
       arity  = fundesc.fun_arity;
       params = fun_params;
       body   = ubody;
       dbg },
     let env_approx = if !useless_env then [||] else env_approx in
     (id, env_pos, value_closure fundesc approx env_approx)) in
  (* Translate all function definitions. *)
  let clos_info_list =
    if initially_closed then begin
      let cl = List.map2 clos_fundef uncurried_defs clos_offsets in
      (* If the hypothesis that the environment parameters are useless has been
         invalidated, then set [fun_closed] to false in all descriptions and
         recompile *)
      if !useless_env then cl else begin
        List.iter
          (fun (id, params, body, fundesc) -> fundesc.fun_closed <- false)
          uncurried_defs;
        List.map2 clos_fundef uncurried_defs clos_offsets
      end
    end else
      (* Excessive closure nesting: assume environment parameter is used *)
        List.map2 clos_fundef uncurried_defs clos_offsets
    in
  (* Update nesting depth *)
  decr function_nesting_depth;
  (* Return the Uclosure node and the list of all identifiers defined,
     with offsets and approximations. *)
  let (clos, infos) = List.split clos_info_list in
  let env = if !useless_env then [] else List.map (close_var fenv cenv) fv in
  (Uclosure(clos, env), infos)

(* Same, for one non-recursive function *)

and close_one_function fenv cenv id funct =
  match close_functions fenv cenv [id, funct] with
      ((Uclosure([f], _) as clos),
       [_, _, ({approx_desc = Value_closure{ clos_desc = fundesc }} as approx)]) ->
        (* See if the function can be inlined *)
        if lambda_smaller f.body
          (!Clflags.inline_threshold + List.length f.params)
        then fundesc.fun_inline <- Some(f.params, f.body);
        (clos, approx)
    | _ -> fatal_error "Closure.close_one_function"

(* Close a switch *)

and close_switch fenv cenv cases num_keys default arg block =
  let index = Array.create num_keys 0
  and store = mk_store Lambda.same in

  (* First default case *)
  begin match default with
  | Some def when List.length cases < num_keys ->
      ignore (store.act_store def)
  | _ -> ()
  end ;
  (* Then all other cases *)
  List.iter
    (fun (key,lam) ->
     index.(key) <- store.act_store lam)
    cases ;
  (* Compile action *)
  let index = Array.map (fun i -> Some i) index in
  let close_branch i lam =
    let fenv = switch_branch_approx arg block index i fenv in
    close fenv cenv lam
  in
  let actions = Array.mapi close_branch (store.act_get ()) in
  match actions with
  | [| |] -> [| |], [| |] (* May happen when default is None *)
  | _     -> index, actions


(* The entry point *)

let rec deconstruct_makeblock = function
    Lvar v as lam -> lam
  | Lconst cst as lam -> lam
  | Lapply(e1, el, loc) ->
      Lapply(deconstruct_makeblock e1, List.map (deconstruct_makeblock) el, loc)
  | Lfunction(kind, params, body) ->
      Lfunction(kind, params, deconstruct_makeblock body)
  | Llet(str, v, e1, e2) ->
      Llet(str, v, deconstruct_makeblock e1, deconstruct_makeblock e2)
  | Lletrec(idel, e2) ->
      Lletrec(List.map (fun (v, e) -> (v, deconstruct_makeblock e)) idel,
              deconstruct_makeblock e2)
  | Lprim(Pmakeblock(tag, mut) as prim, lams) as lam ->
      begin match mut with
           Mutable -> lam
         | Immutable ->
            let vars = List.mapi
              (fun i _ -> Ident.create (Printf.sprintf "block_%i" i)) lams in
            let block_lam = Lprim(prim, List.map (fun id -> Lvar id) vars) in
            List.fold_left2 (fun acc_lam var lam -> Llet(Strict,var,lam,acc_lam))
              block_lam vars lams
      end
  | Lprim(p, el) ->
      Lprim(p, List.map (deconstruct_makeblock) el)
  | Lswitch(e, sw) ->
      Lswitch(deconstruct_makeblock e,
        {sw_numconsts = sw.sw_numconsts;
         sw_consts =
            List.map (fun (n, e) -> (n, deconstruct_makeblock e)) sw.sw_consts;
         sw_numblocks = sw.sw_numblocks;
         sw_blocks =
            List.map (fun (n, e) -> (n, deconstruct_makeblock e)) sw.sw_blocks;
         sw_failaction = match sw.sw_failaction with
         | None -> None
         | Some l -> Some (deconstruct_makeblock l)})
  | Lstaticraise (i,args) ->
      Lstaticraise (i,List.map (deconstruct_makeblock) args)
  | Lstaticcatch(e1, i, e2) ->
      Lstaticcatch(deconstruct_makeblock e1, i, deconstruct_makeblock e2)
  | Ltrywith(e1, v, e2) ->
      Ltrywith(deconstruct_makeblock e1, v, deconstruct_makeblock e2)
  | Lifthenelse(e1, e2, e3) ->
      Lifthenelse(deconstruct_makeblock e1,
                  deconstruct_makeblock e2,
                  deconstruct_makeblock e3)
  | Lsequence(e1, e2) ->
      Lsequence(deconstruct_makeblock e1, deconstruct_makeblock e2)
  | Lwhile(e1, e2) ->
      Lwhile(deconstruct_makeblock e1, deconstruct_makeblock e2)
  | Lfor(v, e1, e2, dir, e3) ->
      Lfor(v, deconstruct_makeblock e1, deconstruct_makeblock e2,
           dir, deconstruct_makeblock e3)
  | Lassign(v, e) ->
      Lassign(v, deconstruct_makeblock e)
  | Lsend(k, m, o, el, loc) ->
      Lsend(k, deconstruct_makeblock m, deconstruct_makeblock o,
            List.map (deconstruct_makeblock) el, loc)
  | Levent(l, ev) ->
      Levent(deconstruct_makeblock l, ev)
  | Lifused(v, e) ->
      Lifused(v, deconstruct_makeblock e)

let rec let_lifting = function
    Lvar v as lam -> lam
  | Lconst cst as lam -> lam
  | Lapply(e1, el, loc) ->
      Lapply(let_lifting e1, List.map (let_lifting) el, loc)
  | Lfunction(kind, params, body) ->
      Lfunction(kind, params, let_lifting body)
  | Llet(str_out, v_out, Llet(str_in, v_in, e1, body_in), body_out) ->
      let_lifting
        (Llet(str_in, v_in, e1, Llet(str_out, v_out, body_in, body_out)))
  | Llet(str, v, e1, e2) ->
      Llet(str, v, let_lifting e1, let_lifting e2)
  | Lletrec(idel, e2) ->
      Lletrec(List.map (fun (v, e) -> (v, let_lifting e)) idel,
              let_lifting e2)
  | Lprim(p, el) ->
      Lprim(p, List.map (let_lifting) el)
  | Lswitch(e, sw) ->
      Lswitch(let_lifting e,
        {sw_numconsts = sw.sw_numconsts;
         sw_consts =
            List.map (fun (n, e) -> (n, let_lifting e)) sw.sw_consts;
         sw_numblocks = sw.sw_numblocks;
         sw_blocks =
            List.map (fun (n, e) -> (n, let_lifting e)) sw.sw_blocks;
         sw_failaction = match sw.sw_failaction with
         | None -> None
         | Some l -> Some (let_lifting l)})
  | Lstaticraise (i,args) ->
      Lstaticraise (i,List.map (let_lifting) args)
  | Lstaticcatch(e1, i, e2) ->
      Lstaticcatch(let_lifting e1, i, let_lifting e2)
  | Ltrywith(e1, v, e2) ->
      Ltrywith(let_lifting e1, v, let_lifting e2)
  | Lifthenelse(e1, e2, e3) ->
      Lifthenelse(let_lifting e1,
                  let_lifting e2,
                  let_lifting e3)
  | Lsequence(e1, e2) ->
      Lsequence(let_lifting e1, let_lifting e2)
  | Lwhile(e1, e2) ->
      Lwhile(let_lifting e1, let_lifting e2)
  | Lfor(v, e1, e2, dir, e3) ->
      Lfor(v, let_lifting e1, let_lifting e2,
           dir, let_lifting e3)
  | Lassign(v, e) ->
      Lassign(v, let_lifting e)
  | Lsend(k, m, o, el, loc) ->
      Lsend(k, let_lifting m, let_lifting o,
            List.map (let_lifting) el, loc)
  | Levent(l, ev) ->
      Levent(let_lifting l, ev)
  | Lifused(v, e) ->
      Lifused(v, let_lifting e)

let intro size lam =
  function_nesting_depth := 0;
  global_approx := Array.create size value_unknown;
  Compilenv.set_global_approx(mkapprox (Value_block(0,!global_approx)));
  let lam = deconstruct_makeblock lam in
  let lam = let_lifting lam in
  let (ulam, approx) = close Tbl.empty Tbl.empty lam in
  global_approx := [||];
  ulam
