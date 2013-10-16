(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                     Pierre Chambart, OCamlPro                       *)
(*                                                                     *)
(*  Copyright 2013 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Lambda
open Asttypes
open Flambda

type tag = int

type descr =
  | Value_block of tag * descr array
  | Value_int of int
  | Value_constptr of int
  | Value_unoffseted_closure of value_closure
  | Value_closure of value_offset
  | Value_unknown
  | Value_bottom
  | Value_extern of Flambdaexport.ExportId.t
  | Value_symbol of Symbol.t

and value_offset =
  { fun_id : offset;
    closure : value_closure }

and value_closure =
  { ffunctions : ExprId.t ffunctions;
    bound_var : descr OffsetMap.t }

let rec print_descr ppf = function
  | Value_int i -> Format.pp_print_int ppf i
  | Value_constptr i -> Format.fprintf ppf "%ia" i
  | Value_block (tag,fields) ->
    let p ppf fields =
      Array.iter (fun v -> Format.fprintf ppf "%a@ " print_descr v) fields in
    Format.fprintf ppf "[%i:@ @[<1>%a@]]" tag p fields
  | Value_unknown -> Format.fprintf ppf "?"
  | Value_bottom -> Format.fprintf ppf "bottom"
  | Value_extern id -> Format.fprintf ppf "_%a_" Flambdaexport.ExportId.print id
  | Value_symbol sym -> Format.fprintf ppf "%a" Symbol.print sym
  | Value_closure { fun_id } ->
    Format.fprintf ppf "(fun:@ %a)" Offset.print fun_id
  | _ -> Format.fprintf ppf "TODO"

module Import = struct
  type t = descr
  open Flambdaexport
  let rec import_ex ex : t =
    let ex_info = Compilenv.approx_env () in
    try match EidMap.find ex ex_info.ex_values with
      | Value_int i -> Value_int i
      | Value_block (tag, fields) ->
          Value_block (tag, Array.map import_approx fields)
      | Value_closure { fun_id; closure = { closure_id; bound_var } } ->
        let bound_var = OffsetMap.map import_approx bound_var in
        Value_closure
          { fun_id;
            closure =
              { ffunctions = Compilenv.imported_closure closure_id;
                bound_var } }
      | _ -> Value_unknown
    with Not_found -> Value_unknown

  and import_approx (ap:Flambdaexport.approx) : t =
    match ap with
    | Value_unknown -> Value_unknown
    | Value_id ex -> Value_extern ex
    | Value_symbol sym -> Value_symbol sym

  let import_symbol ((unit,_) as sym) =
    let symbol_id_map =
      (Compilenv.approx_for_global unit).ex_symbol_id in
    try import_ex (SymbolMap.find sym symbol_id_map) with
    | Not_found ->
      Value_unknown

  let import_symbol sym =
    let r = import_symbol sym in
    (* Format.printf "import %a:@[<1>@ %a@]@." Symbol.print sym print_descr r; *)
    r

  let rec really_import = function
    | Value_extern ex -> really_import_ex ex
    | Value_symbol sym -> really_import_symbol sym
    | r -> r

  and really_import_ex ex =
    really_import (import_ex ex)

  and really_import_symbol sym =
    really_import (import_symbol sym)

  let import_global id =
    import_approx
      (IdentMap.find id (Compilenv.approx_for_global id).ex_globals)

end

open Import

let make_const_int n eid = Fconst(Fconst_base(Const_int n),eid), Value_int n
let make_const_ptr n eid = Fconst(Fconst_pointer n,eid), Value_constptr n
let make_const_bool b eid = make_const_ptr (if b then 1 else 0) eid

type env =
  { env_approx : descr IdentMap.t;
    global : (int, descr) Hashtbl.t }

let empty_env () =
  { env_approx = IdentMap.empty;
    global = Hashtbl.create 10 }

let local_env env =
  { env with env_approx = IdentMap.empty }

let find id env = IdentMap.find id env.env_approx
let find_unknwon id env =
  try find id env
  with Not_found -> Value_unknown
let add_approx id approx env =
  { env with env_approx = IdentMap.add id approx env.env_approx }

let add_global i approx env =
  Hashtbl.add env.global i approx
let find_global i env =
  try Hashtbl.find env.global i with
  | Not_found -> Value_unknown

let const_approx = function
  | Fconst_base const ->
      begin match const with
      | Const_int i -> Value_int i
      | Const_char c -> Value_int (Char.code c)
      | Const_string _ -> Value_unknown
      | Const_float  _ -> Value_unknown
      | Const_int32  _ -> Value_unknown
      | Const_int64  _ -> Value_unknown
      | Const_nativeint  _ -> Value_unknown
      end
  | Fconst_pointer i -> Value_constptr i
  | Fconst_float_array _ -> Value_unknown
  | Fconst_immstring _ -> Value_unknown

(* Determine whether the estimated size of a flambda term is below
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
  | Pccall p -> (if p.Primitive.prim_alloc then 10 else 4) + List.length args
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
  | Pgetglobalfield _ -> 2
  | Psetglobalfield _ -> 2
  | _ -> 2 (* arithmetic and comparisons *)

(* Very raw approximation of switch cost *)

let lambda_smaller lam threshold =
  let size = ref 0 in
  let rec lambda_size lam =
    if !size > threshold then raise Exit;
    match lam with
      Fvar _ -> ()
    | Fsymbol _ -> ()
    | Fconst(
        (Fconst_base(Const_int _ | Const_char _ | Const_float _ |
                     Const_int32 _ | Const_int64 _ | Const_nativeint _) |
         Fconst_pointer _), _) -> incr size
    | Fconst(
        (Fconst_base( Const_string _ )
        | Fconst_float_array _ | Fconst_immstring _ ) , _) ->
        raise Exit (* Do not duplicate: should be moved out by a previous pass *)
    | Fapply(fn, args, direct, _, _) ->
        let call_cost = match direct with None -> 6 | Some _ -> 4 in
        size := !size + call_cost; lambda_size fn; lambda_list_size args
    | Fclosure(ffuns, fv, _) ->
        IdentMap.iter (fun _ -> lambda_size) fv;
        IdentMap.iter (fun _ ffun -> lambda_size ffun.body) ffuns.funs
    | Foffset(lam, _, _, _) ->
        incr size; lambda_size lam
    | Fenv_field({ env }, _) ->
        incr size; lambda_size env
    | Flet(id, _, lam, body, _) ->
        lambda_size lam; lambda_size body
    | Fletrec(bindings, body, _) ->
        List.iter (fun (_,lam) -> lambda_size lam) bindings;
        lambda_size body
    | Fprim(prim, args, _, _) ->
        size := !size + prim_size prim args;
        lambda_list_size args
    | Fswitch(lam, sw, _) ->
        let aux = function _::_::_ -> size := !size + 5 | _ -> () in
        aux sw.fs_consts; aux sw.fs_blocks;
        lambda_size lam;
        List.iter (fun (_,lam) -> lambda_size lam) sw.fs_consts;
        List.iter (fun (_,lam) -> lambda_size lam) sw.fs_blocks
    | Fstaticfail (_,args,_) -> lambda_list_size args
    | Fcatch(_, _, body, handler, _) ->
        incr size; lambda_size body; lambda_size handler
    | Ftrywith(body, id, handler, _) ->
        size := !size + 8; lambda_size body; lambda_size handler
    | Fifthenelse(cond, ifso, ifnot, _) ->
        size := !size + 2;
        lambda_size cond; lambda_size ifso; lambda_size ifnot
    | Fsequence(lam1, lam2, _) ->
        lambda_size lam1; lambda_size lam2
    | Fwhile(cond, body, _) ->
        size := !size + 2; lambda_size cond; lambda_size body
    | Ffor(id, low, high, dir, body, _) ->
        size := !size + 4; lambda_size low; lambda_size high; lambda_size body
    | Fassign(id, lam, _) ->
        incr size;  lambda_size lam
    | Fsend(_, met, obj, args, _, _) ->
        size := !size + 8;
        lambda_size met; lambda_size obj; lambda_list_size args
    | Funreachable _ -> ()
  and lambda_list_size l = List.iter lambda_size l in
  try
    lambda_size lam; !size <= threshold
  with Exit ->
    false


(* Simple effectfull test, should be replace by call to Purity module *)

let no_effects_prim = function
    Psetglobal _ | Psetfield _ | Psetfloatfield _ | Pduprecord _ |
    Pccall _ | Praise | Poffsetref _ | Pstringsetu | Pstringsets |
    Parraysetu _ | Parraysets _ | Pbigarrayset _

  | Psetglobalfield _

  | Pstringrefs | Parrayrefs _ | Pbigarrayref (false,_,_,_)

  | Pstring_load_16 false | Pstring_load_32 false | Pstring_load_64 false

  | Pbigstring_load_16 false | Pbigstring_load_32 false
  | Pbigstring_load_64 false

    -> false
  | _ -> true

let rec no_effects = function
  | Fvar _ (* notice: var acces is not pure, but has no effect *)
  | Fsymbol _
  | Fconst _
    -> true
  | Flet (_,_,def,body,_) ->
      no_effects def && no_effects body
  | Fletrec (defs,body,_) ->
      no_effects body &&
      List.for_all (fun (_,def) -> no_effects def) defs
  | Fprim(p, args, _, _) ->
      no_effects_prim p &&
      List.for_all no_effects args
  | Fclosure (_, fv, _) ->
      IdentMap.for_all (fun id def -> no_effects def) fv
  | Foffset (lam,_ , _, _) ->
      no_effects lam
  | Fenv_field ({ env }, _) -> no_effects env

  | Fifthenelse _

  | Fswitch _ ->
      (* TODO *)
      false

  | Fcatch (_,_,body,_,_)
  | Ftrywith (body, _, _, _) ->
      (* the raise is effectful, no need to test the handler *)
      no_effects body

  | Fsequence (l1,l2,_) ->
      no_effects l1 && no_effects l2

  | Fwhile _
  | Ffor _
  | Fapply _
  | Fsend _
  | Fassign _
  | Fstaticfail _
    -> false

  | Funreachable _ -> true

let check_constant_result lam approx =
  match approx with
    Value_int n when no_effects lam -> make_const_int n (data lam)
  | Value_constptr n when no_effects lam -> make_const_ptr n (data lam)
  | Value_symbol sym when no_effects lam -> Fsymbol(sym, data lam), approx
  | _ -> (lam, approx)

let get_field i = function
  | [Value_block (tag, fields)] ->
    if i >= 0 && i < Array.length fields
    then fields.(i)
    else Value_unknown
  | _ -> Value_unknown

let simplif_prim_pure p (args, approxs) expr dbg =
  match p with
  | Pmakeblock(tag, Immutable) ->
      expr, Value_block(tag, Array.of_list approxs)
  | Pfield i ->
      check_constant_result expr (get_field i approxs)
  | _ ->
      let eid = data expr in
      match approxs with
        [Value_int x] ->
          begin match p with
            Pidentity -> make_const_int x eid
          | Pnegint -> make_const_int (-x) eid
          | Pbswap16 ->
              make_const_int (((x land 0xff) lsl 8) lor
                              ((x land 0xff00) lsr 8)) eid
          | Poffsetint y -> make_const_int (x + y) eid
          | _ ->
              expr, Value_unknown
          end
      | [Value_int x; Value_int y] ->
          begin match p with
            Paddint -> make_const_int(x + y) eid
          | Psubint -> make_const_int(x - y) eid
          | Pmulint -> make_const_int(x * y) eid
          | Pdivint when y <> 0 -> make_const_int(x / y) eid
          | Pmodint when y <> 0 -> make_const_int(x mod y) eid
          | Pandint -> make_const_int(x land y) eid
          | Porint -> make_const_int(x lor y) eid
          | Pxorint -> make_const_int(x lxor y) eid
          | Plslint -> make_const_int(x lsl y) eid
          | Plsrint -> make_const_int(x lsr y) eid
          | Pasrint -> make_const_int(x asr y) eid
          | Pintcomp cmp ->
              let result = match cmp with
                  Ceq -> x = y
                | Cneq -> x <> y
                | Clt -> x < y
                | Cgt -> x > y
                | Cle -> x <= y
                | Cge -> x >= y in
              make_const_bool result eid
          | _ ->
              expr, Value_unknown
          end
      | [Value_constptr x] ->
          begin match p with
            Pidentity -> make_const_ptr x eid
          | Pnot -> make_const_bool(x = 0) eid
          | Pisint -> make_const_bool true eid
          | Pctconst c ->
              begin
                match c with
                | Big_endian -> make_const_bool Arch.big_endian eid
                | Word_size -> make_const_int (8*Arch.size_int) eid
                | Ostype_unix -> make_const_bool (Sys.os_type = "Unix") eid
                | Ostype_win32 -> make_const_bool (Sys.os_type = "Win32") eid
                | Ostype_cygwin -> make_const_bool (Sys.os_type = "Cygwin") eid
              end
          | _ ->
              expr, Value_unknown
          end
      | [Value_constptr x; Value_constptr y] ->
          begin match p with
            Psequand -> make_const_bool(x <> 0 && y <> 0) eid
          | Psequor  -> make_const_bool(x <> 0 || y <> 0) eid
          | _ ->
              expr, Value_unknown
          end
      | _ ->
          expr, Value_unknown

let rec loop env tree =
  let f, descr = loop_direct env tree in
  f, really_import descr

and loop_direct (env:env) tree : 'a flambda * descr =
  let aux v = fst (loop env v) in
  match tree with
  | Fsymbol (sym,_) -> tree, import_symbol sym
  | Fvar (id,_) -> tree, find_unknwon id env
  | Fconst (cst,_) -> tree, const_approx cst
  | Fapply (funct, args, direc, dbg, annot) ->
      apply env funct args dbg annot
  | Fclosure (ffuns, fv, annot) ->
      let fv = IdentMap.map (loop env) fv in
      let closure_env = IdentMap.fold
          (fun id (_,desc) env -> add_approx id desc env) fv (local_env env) in
      let ffuns =
        { ffuns with
          funs = IdentMap.map
              (fun ffun -> { ffun with body = fst(loop closure_env ffun.body) })
              ffuns.funs } in
      let closure =
        { ffunctions = ffuns;
          bound_var = IdentMap.fold (fun id (_,desc) map ->
              OffsetMap.add { off_unit = ffuns.unit; off_id = id } desc map)
              fv OffsetMap.empty } in
      Fclosure (ffuns, IdentMap.map fst fv, annot),
      Value_unoffseted_closure closure
  | Foffset (flam, off, rel, annot) ->
      let flam, approx = loop env flam in
      let ret_approx = match approx with
        | Value_unoffseted_closure closure ->
            Value_closure { fun_id = off; closure }
        | _ -> Value_unknown
      in
      Foffset (flam, off, rel, annot), ret_approx
  | Fenv_field (fenv_field, annot) ->
      Fenv_field ({ fenv_field with env = aux fenv_field.env }, annot),
      Value_unknown
  | Flet(str, id, lam, body, annot) ->
      let lam, lapprox = loop env lam in
      let body_env = match str with
        | Variable -> env
        | _ -> add_approx id lapprox env in
      let body, approx = loop body_env body in
      Flet (str, id, lam, body, annot),
      approx
  | Fletrec(defs, body, annot) ->
      let defs, body_env = List.fold_left (fun (defs, env_acc) (id,lam) ->
          let lam, approx = loop env lam in
          let defs = (id,lam) :: defs in
          let env_acc = add_approx id approx env_acc in
          defs, env_acc) ([],env) defs in
      let body, approx = loop body_env body in
      Fletrec (defs, body, annot),
      approx
  | Fprim(Pgetglobal id, [], dbg, annot) as expr ->
    let approx =
      if Ident.is_predef_exn id
      then Value_unknown
      else import_global id in
    expr, approx
  | Fprim(Pgetglobalfield(id,i), [], dbg, annot) as expr ->
      let approx =
        if id = Compilenv.current_unit_id ()
        then find_global i env
        else get_field i [really_import (import_global id)] in
      check_constant_result expr approx
  | Fprim(Psetglobalfield i, [arg], dbg, annot) as expr ->
      let arg', approx = loop env arg in
      let expr = if arg == arg' then expr
        else Fprim(Psetglobalfield i, [arg'], dbg, annot) in
      add_global i approx env;
      expr, Value_unknown
  | Fprim(p, args, dbg, annot) as expr ->
      let (args', approxs) = loop_list env args in
      let expr = if args' == args then expr else Fprim(p, args', dbg, annot) in
      simplif_prim_pure p (args, approxs) expr dbg
  | Fstaticfail(i, args, annot) ->
      let args = List.map aux args in
      Fstaticfail (i, args, annot),
      Value_unknown
  | Fcatch (i, vars, body, handler, annot) ->
      let body = aux body in
      let handler = aux handler in
      Fcatch (i, vars, body, handler, annot),
      Value_unknown
  | Ftrywith(body, id, handler, annot) ->
      let body = aux body in
      let handler = aux handler in
      Ftrywith(body, id, handler, annot),
      Value_unknown
  | Fifthenelse(arg, ifso, ifnot, annot) ->
      let arg = aux arg in
      let ifso = aux ifso in
      let ifnot = aux ifnot in
      Fifthenelse(arg, ifso, ifnot, annot),
      Value_unknown
  | Fsequence(lam1, lam2, annot) ->
      let lam1 = aux lam1 in
      let lam2 = aux lam2 in
      Fsequence(lam1, lam2, annot),
      Value_unknown
  | Fwhile(cond, body, annot) ->
      let cond = aux cond in
      let body = aux body in
      Fwhile(cond, body, annot),
      Value_unknown
  | Fsend(kind, met, obj, args, dbg, annot) ->
      let met = aux met in
      let obj = aux obj in
      let args = List.map aux args in
      Fsend(kind, met, obj, args, dbg, annot),
      Value_unknown
  | Ffor(id, lo, hi, dir, body, annot) ->
      let lo = aux lo in
      let hi = aux hi in
      let body = aux body in
      Ffor(id, lo, hi, dir, body, annot),
      Value_unknown
  | Fassign(id, lam, annot) ->
      let lam = aux lam in
      Fassign(id, lam, annot),
      Value_unknown
  | Fswitch(arg, sw, annot) ->
      let arg = aux arg in
      let sw =
        { sw with
          fs_failaction = Misc.may_map aux sw.fs_failaction;
          fs_consts = List.map (fun (i,v) -> i, aux v) sw.fs_consts;
          fs_blocks = List.map (fun (i,v) -> i, aux v) sw.fs_blocks; } in
      Fswitch(arg, sw, annot),
      Value_unknown
  | Funreachable _ -> tree, Value_bottom

and loop_list env l = match l with
  | [] -> [], []
  | h::t ->
      let t', approxs = loop_list env t in
      let h', approx = loop env h in
      let approxs = approx :: approxs in
      if t' == t && h' == h
      then l, approxs
      else h' :: t', approxs

and apply env funct args dbg eid =
  let funct, fapprox = loop env funct in
  let args, approxs = loop_list env args in
  match fapprox with
  | Value_closure { fun_id; closure } ->
      let clos = closure.ffunctions in
      assert(fun_id.off_unit = clos.unit);
      let func = IdentMap.find fun_id.off_id clos.funs in
      let nargs = List.length args in
      if nargs = func.arity
      then direct_apply env clos funct fun_id func args dbg eid
      else
        Fapply (funct, args, None, dbg, eid),
        Value_unknown
  | _ ->
      Fapply (funct, args, None, dbg, eid),
      Value_unknown

and direct_apply env clos funct fun_id func args dbg eid =
  if not clos.recursives && not (func.kind = Tupled) && lambda_smaller func.body
       ((!Clflags.inline_threshold + List.length func.params) * 2)
  then
    (* try inlining if the function is not too far above the threshold *)
    let body, approx = inline env clos funct fun_id func args dbg eid in
    if lambda_smaller body
        (!Clflags.inline_threshold + List.length func.params)
    then
      (* if the definitive size is small enought: keep it *)
      body, approx
    else Fapply (funct, args, Some fun_id, dbg, eid),
         Value_unknown
         (* do not use approximation: there can be renamed offsets *)
  else Fapply (funct, args, Some fun_id, dbg, eid),
       Value_unknown

and inline env clos lfunc fun_id func args dbg eid =
  let clos_id = Ident.create "inlined_closure" in
  let args' = List.map2 (fun id arg -> id, Ident.rename id, arg)
      func.params args in
  let fv' = IdentSet.fold (fun id l -> (id, Ident.rename id) :: l)
      func.closure_params [] in
  let arg_subst = List.fold_left
      (fun sb (id,id',_) -> IdentMap.add id id' sb) IdentMap.empty args' in
  let fv_subst = IdentMap.of_list fv' in
  let closure_fun_subst = (* other functions from the closure *)
    let other_functions =
      IdentMap.filter (fun id _ -> not (Ident.same id fun_id.off_id))
        clos.funs in
    IdentMap.mapi (fun id _ -> Ident.rename id) other_functions
  in
  let subst =
    IdentMap.disjoint_union closure_fun_subst
      (IdentMap.disjoint_union arg_subst fv_subst) in
  let body =
    Flambdasubst.substitute subst func.body
    |> List.fold_right (fun (_,id',arg) body ->
        Flet(Strict, id', arg, body, ExprId.create ~name:"inline arg" ()))
      args'
    |> List.fold_right (fun (id,id') body ->
        Flet(Strict, id',
             Fenv_field ({ env = Fvar(clos_id, ExprId.create ());
                           env_fun_id = fun_id;
                           env_var = { off_unit = clos.unit;
                                       off_id = id } },
                         ExprId.create ()),
             body, ExprId.create ()))
      fv'
    |> IdentMap.fold (fun id id' body ->
        Flet(Strict, id',
             Foffset (Fvar(clos_id, ExprId.create ()),
                      {fun_id with off_id = id},
                      Some fun_id,
                      ExprId.create ()),
             body, ExprId.create ()))
      closure_fun_subst
  in
  loop env (Flet(Strict, clos_id, lfunc, body, ExprId.create ()))

(* module Export = struct *)
(*   type t = descr *)
(*   open Flambdaexport *)

(*   type acc = *)
(*     { mapping : descr EidMap.t } *)

(*   let empty_acc = { mapping = EidMap.empty } *)

(*   let add ex descr acc = *)
(*     { mapping = EidMap.add ex descr acc.mapping } *)

(*   let make_ex () = *)
(*     ExportId.create (Compilenv.current_unit_name ()) *)

(*   let rec prepare_descr : acc -> t -> (descr * acc) option = fun acc -> function *)
(*     | Value_int i -> Some (Value_int i, acc) *)
(*     | Value_constptr i -> Some (Value_constptr i, acc) *)
(*     | Value_block (tag, fields) -> *)
(*         let fields, acc = Array.fold_right *)
(*             (fun descr (l,acc) -> *)
(*                let v, acc' = prepare_approx acc descr in *)
(*                (v::l, acc')) *)
(*             fields ([],acc) in *)
(*         Some (Value_block(tag,Array.of_list fields), acc) *)

(*     | Value_unknown *)
(*     | Value_bottom -> None *)

(*     | _ -> None *)
(*     (\* | Value_unoffseted_closure of value_closure *\) *)
(*     (\* | Value_closure of value_offset *\) *)
(*     (\* | Value_extern of Flambdaexport.ExportId.t *\) *)

(*   and prepare_approx : acc -> t -> approx * acc = fun acc -> function *)
(*     | Value_extern ex -> *)
(*         let id_symbol = (Compilenv.approx_env ()).ex_id_symbol in *)
(*         let approx = *)
(*           try Value_symbol (EidMap.find ex id_symbol) with *)
(*           | Not_found -> Value_unknown in *)
(*         approx, acc *)
(*     | t -> match prepare_descr acc t with *)
(*       | None -> Value_unknown, acc *)
(*       | Some (descr,acc) -> *)
(*           let ex = make_ex () in *)
(*           let acc = add ex descr acc in *)
(*           Value_id ex, acc *)

(*   let prepare_export global = *)
(*     let size_global = *)
(*       1 + (Hashtbl.fold (fun k _ acc -> max k acc) global (-1)) in *)
(*     let fields = Array.init size_global (fun i -> *)
(*         try Hashtbl.find global i with Not_found -> (Value_unknown:t)) in *)
(*     let root, acc = prepare_approx empty_acc (Value_block (0,fields)) in *)
(*     root, acc.mapping *)

(* end *)

let simplify tree =
  let env = empty_env () in
  let result, _ = loop env tree in
  (* let root, mapping = Export.prepare_export env.global in *)
  result
(* , root, mapping *)
