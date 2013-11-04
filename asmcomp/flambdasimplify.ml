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

open Ext_types
open Lambda
open Asttypes
open Flambda

let make_function id lam params =
  let fv = Flambdaiter.free_variables lam in
  let param_set = List.fold_right IdentSet.add params IdentSet.empty in
  let fv = IdentSet.diff fv param_set in
  let sb = IdentSet.fold (fun id sb -> IdentMap.add id (Ident.rename id) sb)
      (IdentSet.union fv param_set) IdentMap.empty in
  let body = Flambdasubst.substitute sb lam in
  let closure =
    { label = Flambdagen.make_function_lbl id;
      stub = false;
      arity = List.length params;
      params = List.map (fun id -> IdentMap.find id sb) params;
      closure_params = IdentSet.map (fun id -> IdentMap.find id sb) fv;
      body;
      dbg = Debuginfo.none } in
  let unit = Compilenv.current_unit () in
  let unit_name = Compilenv.current_unit_name () in
  let fv' = IdentMap.fold (fun id id' fv' ->
      IdentMap.add id' (Fvar(id,ExprId.create ())) fv')
      (IdentMap.filter (fun id _ -> IdentSet.mem id fv) sb)
      IdentMap.empty in
  Fclosure
    ({ ident = FunId.create unit_name;
       funs = IdentMap.singleton id closure;
       recursives = false;
       closed = IdentSet.is_empty fv;
       unit },
     fv', ExprId.create ())

let rec map_rem f l1 l2 =
  match l1, l2 with
  | [], _ -> [], l2
  | h::t, [] -> raise (Invalid_argument "map_length_1")
  | h1::t1, h2::t2 ->
      let h = f h1 h2 in
      let (t,rem) = map_rem f t1 t2 in
      h::t, rem

let fvar id = Fvar(id,ExprId.create ())
let foffset (lam, off_id) =
  Foffset(lam, { off_id; off_unit = Compilenv.current_unit ()},
          None, ExprId.create ())

type tag = int

type descr =
  | Value_block of tag * approx array
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
    bound_var : approx OffsetMap.t }

and approx =
  { descr : descr;
    var : Ident.t option;
    symbol : symbol option }

let rec print_descr ppf = function
  | Value_int i -> Format.pp_print_int ppf i
  | Value_constptr i -> Format.fprintf ppf "%ia" i
  | Value_block (tag,fields) ->
    let p ppf fields =
      Array.iter (fun v -> Format.fprintf ppf "%a@ " print_approx v) fields in
    Format.fprintf ppf "[%i:@ @[<1>%a@]]" tag p fields
  | Value_unknown -> Format.fprintf ppf "?"
  | Value_bottom -> Format.fprintf ppf "bottom"
  | Value_extern id -> Format.fprintf ppf "_%a_" Flambdaexport.ExportId.print id
  | Value_symbol sym -> Format.fprintf ppf "%a" Symbol.print sym
  | Value_closure { fun_id } ->
    Format.fprintf ppf "(fun:@ %a)" Offset.print fun_id
  | _ -> Format.fprintf ppf "TODO"

and print_approx ppf { descr } = print_descr ppf descr

let approx descr = { descr; var = None; symbol = None }

let value_unknown = approx Value_unknown
let value_int i = approx (Value_int i)
let value_constptr i = approx (Value_constptr i)
let value_closure c = approx (Value_closure c)
let value_unoffseted_closure c = approx (Value_unoffseted_closure c)
let value_block (t,b) = approx (Value_block (t,b))
let value_extern ex = approx (Value_extern ex)
let value_symbol sym = approx (Value_symbol sym)
let value_bottom = approx Value_bottom

module Import = struct
  type t = approx
  open Flambdaexport
  let rec import_ex ex : t =
    let ex_info = Compilenv.approx_env () in
    try match EidMap.find ex ex_info.ex_values with
      | Value_int i -> value_int i
      | Value_block (tag, fields) ->
          value_block (tag, Array.map import_approx fields)
      | Value_closure { fun_id; closure = { closure_id; bound_var } } ->
        let bound_var = OffsetMap.map import_approx bound_var in
        value_closure
          { fun_id;
            closure =
              { ffunctions = Compilenv.imported_closure closure_id;
                bound_var } }
      | _ -> value_unknown
    with Not_found -> value_unknown

  and import_approx (ap:Flambdaexport.approx) : t =
    match ap with
    | Value_unknown -> value_unknown
    | Value_id ex -> value_extern ex
    | Value_symbol sym -> value_symbol sym

  let import_symbol ((id,_) as sym) : t =
    if Ident.is_predef_exn id
    then
      value_unknown
    else
      let symbol_id_map =
        (Compilenv.approx_for_global id).ex_symbol_id in
      try import_ex (SymbolMap.find sym symbol_id_map) with
      | Not_found ->
        value_unknown

  let import_symbol sym =
    let r = import_symbol sym in
    (* Format.printf "import %a:@[<1>@ %a@]@." Symbol.print sym print_descr r; *)
    r

  let rec really_import = function
    | Value_extern ex -> really_import_ex ex
    | Value_symbol sym -> really_import_symbol sym
    | r -> r

  and really_import_ex ex =
    really_import (import_ex ex).descr

  and really_import_symbol sym =
    really_import (import_symbol sym).descr

  let import_global id =
    import_approx
      (IdentMap.find id (Compilenv.approx_for_global id).ex_globals)

end

open Import

type env =
  { env_approx : approx IdentMap.t;
    global : (int, approx) Hashtbl.t }

let empty_env () =
  { env_approx = IdentMap.empty;
    global = Hashtbl.create 10 }

let local_env env =
  { env with env_approx = IdentMap.empty }

type ret =
  { approx : approx;
    used_variables : IdentSet.t;
    used_staticfail : IntSet.t }

let ret (acc:ret) approx = { acc with approx }

let use_var acc var =
  { acc with used_variables = IdentSet.add var acc.used_variables }

let exit_scope acc var =
  { acc with used_variables = IdentSet.remove var acc.used_variables }

let use_staticfail acc i =
  { acc with used_staticfail = IntSet.add i acc.used_staticfail }

let exit_scope_catch acc i =
  { acc with used_staticfail = IntSet.remove i acc.used_staticfail }

let init_r () =
  { approx = value_unknown;
    used_variables = IdentSet.empty;
    used_staticfail = IntSet.empty }

let make_const_int r n eid = Fconst(Fconst_base(Const_int n),eid), ret r (value_int n)
let make_const_ptr r n eid = Fconst(Fconst_pointer n,eid), ret r (value_constptr n)
let make_const_bool r b eid = make_const_ptr r (if b then 1 else 0) eid

let find id env = IdentMap.find id env.env_approx
let find_unknwon id env =
  try find id env
  with Not_found -> value_unknown
let present id env = IdentMap.mem id env.env_approx
let add_approx id approx env =
  let approx =
    match approx.var with
    | Some var when present var env ->
      approx
    | _ ->
      { approx with var = Some id }
  in
  { env with env_approx = IdentMap.add id approx env.env_approx }

let add_global i approx env =
  Hashtbl.add env.global i approx
let find_global i env =
  try Hashtbl.find env.global i with
  | Not_found -> value_unknown

let const_approx = function
  | Fconst_base const ->
      begin match const with
      | Const_int i -> value_int i
      | Const_char c -> value_int (Char.code c)
      | Const_string _ -> value_unknown
      | Const_float  _ -> value_unknown
      | Const_int32  _ -> value_unknown
      | Const_int64  _ -> value_unknown
      | Const_nativeint  _ -> value_unknown
      end
  | Fconst_pointer i -> value_constptr i
  | Fconst_float_array _ -> value_unknown
  | Fconst_immstring _ -> value_unknown

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

let check_constant_result r lam approx =
  match approx.descr with
    Value_int n when no_effects lam -> make_const_int r n (data lam)
  | Value_constptr n when no_effects lam -> make_const_ptr r n (data lam)
  | Value_symbol sym when no_effects lam -> Fsymbol(sym, data lam), ret r approx
  | _ -> (lam, ret r approx)

let check_var_and_constant_result env r lam approx =
  let res = match approx.var with
    | None ->
      lam
    | Some var ->
      if present var env
      then Fvar(var, data lam)
      else lam
  in
  let expr, r = check_constant_result r res approx in
  let r = match expr with
    | Fvar(var,_) -> use_var r var
    | _ -> r
  in
  expr, r

let get_field i = function
  | [{descr = Value_block (tag, fields)}] ->
    if i >= 0 && i < Array.length fields
    then fields.(i)
    else value_unknown
  | _ -> value_unknown

let descrs approxs = List.map (fun v -> v.descr) approxs

let make_const_int' expr r n eid =
  if no_effects expr
  then make_const_int r n eid
  else expr, ret r (value_int n)
let make_const_ptr' expr r n eid =
  if no_effects expr
  then make_const_ptr r n eid
  else expr, ret r (value_constptr n)
let make_const_bool' expr r b eid =
  make_const_ptr' expr r (if b then 1 else 0) eid

let simplif_prim r p (args, approxs) expr dbg : 'a flambda * ret =
  match p with
  | Pmakeblock(tag, Immutable) ->
      expr, ret r (value_block(tag, Array.of_list approxs))
  | _ ->
      let eid = data expr in
      match descrs approxs with
        [Value_int x] ->
          begin match p with
            Pidentity -> make_const_int' expr r x eid
          | Pnegint -> make_const_int' expr r (-x) eid
          | Pbswap16 ->
              make_const_int' expr r (((x land 0xff) lsl 8) lor
                                      ((x land 0xff00) lsr 8)) eid
          | Poffsetint y -> make_const_int' expr r (x + y) eid
          | _ ->
              expr, ret r value_unknown
          end
      | [Value_int x; Value_int y] ->
          begin match p with
            Paddint -> make_const_int' expr r (x + y) eid
          | Psubint -> make_const_int' expr r (x - y) eid
          | Pmulint -> make_const_int' expr r (x * y) eid
          | Pdivint when y <> 0 -> make_const_int' expr r (x / y) eid
          | Pmodint when y <> 0 -> make_const_int' expr r (x mod y) eid
          | Pandint -> make_const_int' expr r (x land y) eid
          | Porint -> make_const_int' expr r (x lor y) eid
          | Pxorint -> make_const_int' expr r (x lxor y) eid
          | Plslint -> make_const_int' expr r (x lsl y) eid
          | Plsrint -> make_const_int' expr r (x lsr y) eid
          | Pasrint -> make_const_int' expr r (x asr y) eid
          | Pintcomp cmp ->
              let result = match cmp with
                  Ceq -> x = y
                | Cneq -> x <> y
                | Clt -> x < y
                | Cgt -> x > y
                | Cle -> x <= y
                | Cge -> x >= y in
              make_const_bool' expr r result eid
          | _ ->
              expr, ret r value_unknown
          end
      | [Value_constptr x] ->
          begin match p with
            Pidentity -> make_const_ptr' expr r x eid
          | Pnot -> make_const_bool' expr r (x = 0) eid
          | Pisint -> make_const_bool' expr r true eid
          | Pctconst c ->
              begin
                match c with
                | Big_endian -> make_const_bool' expr r Arch.big_endian eid
                | Word_size -> make_const_int' expr r (8*Arch.size_int) eid
                | Ostype_unix -> make_const_bool' expr r (Sys.os_type = "Unix") eid
                | Ostype_win32 -> make_const_bool' expr r (Sys.os_type = "Win32") eid
                | Ostype_cygwin -> make_const_bool' expr r (Sys.os_type = "Cygwin") eid
              end
          | _ ->
              expr, ret r value_unknown
          end
      | [Value_constptr x; Value_constptr y] ->
          begin match p with
            Psequand -> make_const_bool' expr r (x <> 0 && y <> 0) eid
          | Psequor  -> make_const_bool' expr r (x <> 0 || y <> 0) eid
          | _ ->
              expr, ret r value_unknown
          end
      | _ ->
          expr, ret r value_unknown

let sequence l1 l2 annot =
  if no_effects l1
  then l2
  else Fsequence(l1,l2,annot)

let really_import_approx approx =
  { approx with descr = really_import approx.descr }

let rec loop env r tree =
  let f, r = loop_direct env r tree in
  f, ret r (really_import_approx r.approx)

and loop_direct (env:env) r tree : 'a flambda * ret =
  match tree with
  | Fsymbol (sym,_) ->
      check_constant_result r tree (import_symbol sym)
  | Fvar (id,_) ->
      check_var_and_constant_result env r tree (find_unknwon id env)
  | Fconst (cst,_) -> tree, ret r (const_approx cst)
  | Fapply (funct, args, direc, dbg, annot) ->
      let funct, ({ approx = fapprox } as r) = loop env r funct in
      let args, approxs, r = loop_list env r args in
      apply env r (funct,fapprox) (args,approxs) dbg annot

  | Fclosure (ffuns, fv, annot) ->
      closure env r ffuns fv annot

  | Foffset (flam, off, rel, annot) ->
      let flam, r = loop env r flam in
      offset r flam off rel annot

  | Fenv_field (fenv_field, annot) as expr ->
      let arg, r = loop env r fenv_field.env in
      let approx = match r.approx.descr with
        | Value_closure { closure = { bound_var } } ->
            (try OffsetMap.find fenv_field.env_var bound_var with
             | Not_found -> value_unknown)
        | _ -> value_unknown in
      let expr =
        if arg == fenv_field.env
        then expr
        else Fenv_field ({ fenv_field with env = arg }, annot) in
      check_var_and_constant_result env r expr approx
  | Flet(str, id, lam, body, annot) ->
      let init_used_var = r.used_variables in
      let lam, r = loop env r lam in
      let def_used_var = r.used_variables in
      let body_env = match str with
        | Variable -> env
        | _ -> add_approx id r.approx env in
      let r_body = { r with used_variables = init_used_var } in
      let body, r = loop body_env r_body body in
      let expr, r =
        if IdentSet.mem id r.used_variables
        then
          Flet (str, id, lam, body, annot),
          { r with used_variables =
                     IdentSet.union def_used_var r.used_variables }
        else if no_effects lam
        then body, r
        else Fsequence(lam, body, annot),
             { r with used_variables =
                        IdentSet.union def_used_var r.used_variables } in
      expr, exit_scope r id
  | Fletrec(defs, body, annot) ->
      let defs, body_env, r = List.fold_left (fun (defs, env_acc, r) (id,lam) ->
          let lam, r = loop env r lam in
          let defs = (id,lam) :: defs in
          let env_acc = add_approx id r.approx env_acc in
          defs, env_acc, r) ([],env,r) defs in
      let body, r = loop body_env r body in
      let r = List.fold_left (fun r (id,_) -> exit_scope r id) r defs in
      Fletrec (defs, body, annot),
      r
  | Fprim(Pgetglobal id, [], dbg, annot) as expr ->
    let approx =
      if Ident.is_predef_exn id
      then value_unknown
      else import_global id in
    expr, ret r approx
  | Fprim(Pgetglobalfield(id,i), [], dbg, annot) as expr ->
      let approx =
        if id = Compilenv.current_unit_id ()
        then find_global i env
        else get_field i [really_import_approx (import_global id)] in
      check_constant_result r expr approx
  | Fprim(Psetglobalfield i, [arg], dbg, annot) as expr ->
      let arg', r = loop env r arg in
      let expr = if arg == arg' then expr
        else Fprim(Psetglobalfield i, [arg'], dbg, annot) in
      add_global i r.approx env; (* TODO: add global to r, not to env *)
      expr, ret r value_unknown
  | Fprim(Pfield i, [arg], dbg, annot) as expr ->
      let arg', r = loop env r arg in
      let expr =
        if arg == arg' then expr
        else Fprim(Pfield i, [arg'], dbg, annot) in
      let approx = get_field i [r.approx] in
      check_var_and_constant_result env r expr approx
  | Fprim(p, args, dbg, annot) as expr ->
      let (args', approxs, r) = loop_list env r args in
      let expr = if args' == args then expr else Fprim(p, args', dbg, annot) in
      simplif_prim r p (args, approxs) expr dbg
  | Fstaticfail(i, args, annot) ->
      let args, _, r = loop_list env r args in
      let r = use_staticfail r i in
      Fstaticfail (i, args, annot),
      ret r value_bottom
  | Fcatch (i, vars, body, handler, annot) ->
      let body, r = loop env r body in
      if not (IntSet.mem i r.used_staticfail)
      then body, r
      else
        let env = List.fold_left (fun env id -> add_approx id value_unknown env)
            env vars in
        let handler, r = loop env r handler in
        let r = List.fold_left exit_scope r vars in
        let r = exit_scope_catch r i in
        Fcatch (i, vars, body, handler, annot),
        ret r value_unknown
  | Ftrywith(body, id, handler, annot) ->
      let body, r = loop env r body in
      let env = add_approx id value_unknown env in
      let handler, r = loop env r handler in
      let r = exit_scope r id in
      Ftrywith(body, id, handler, annot),
      ret r value_unknown
  | Fifthenelse(arg, ifso, ifnot, annot) ->
      let arg, r = loop env r arg in
      begin match r.approx.descr with
      | Value_constptr 0 ->
          let ifnot, r = loop env r ifnot in
          sequence arg ifnot annot, r
      | Value_constptr _
      | Value_block _ ->
          let ifso, r = loop env r ifso in
          sequence arg ifso annot, r
      | _ ->
          let ifso, r = loop env r ifso in
          let ifnot, r = loop env r ifnot in
          Fifthenelse(arg, ifso, ifnot, annot),
          ret r value_unknown
      end
  | Fsequence(lam1, lam2, annot) ->
      let lam1, r = loop env r lam1 in
      let lam2, r = loop env r lam2 in
      sequence lam1 lam2 annot,
      r
  | Fwhile(cond, body, annot) ->
      let cond, r = loop env r cond in
      let body, r = loop env r body in
      Fwhile(cond, body, annot),
      ret r value_unknown
  | Fsend(kind, met, obj, args, dbg, annot) ->
      let met, r = loop env r met in
      let obj, r = loop env r obj in
      let args, _, r = loop_list env r args in
      Fsend(kind, met, obj, args, dbg, annot),
      ret r value_unknown
  | Ffor(id, lo, hi, dir, body, annot) ->
      let lo, r = loop env r lo in
      let hi, r = loop env r hi in
      let body, r = loop env r body in
      let r = exit_scope r id in
      Ffor(id, lo, hi, dir, body, annot),
      ret r value_unknown
  | Fassign(id, lam, annot) ->
      let lam, r = loop env r lam in
      let r = use_var r id in
      Fassign(id, lam, annot),
      ret r value_unknown
  | Fswitch(arg, sw, annot) ->
      let arg, r = loop env r arg in
      let get_failaction () = match sw.fs_failaction with
        | None -> Funreachable (ExprId.create ())
        | Some f -> f in
      begin match r.approx.descr with
      | Value_constptr i ->
          let lam = try List.assoc i sw.fs_consts with
            | Not_found -> get_failaction () in
          let lam, r = loop env r lam in
          sequence arg lam annot, r
      | Value_block(tag,_) ->
          let lam = try List.assoc tag sw.fs_blocks with
            | Not_found -> get_failaction () in
          let lam, r = loop env r lam in
          sequence arg lam annot, r
      | _ ->
          let f (i,v) (acc, r) =
            let lam, r = loop env r v in
            ((i,lam)::acc, r) in
          let fs_consts, r = List.fold_right f sw.fs_consts ([], r) in
          let fs_blocks, r = List.fold_right f sw.fs_blocks ([], r) in
          let fs_failaction, r = match sw.fs_failaction with
            | None -> None, r
            | Some l -> let l, r = loop env r l in Some l, r in
          let sw =
            { sw with fs_failaction; fs_consts; fs_blocks; } in
          Fswitch(arg, sw, annot),
          ret r value_unknown
      end
  | Funreachable _ -> tree, ret r value_bottom

and loop_list env r l = match l with
  | [] -> [], [], r
  | h::t ->
      let t', approxs, r = loop_list env r t in
      let h', r = loop env r h in
      let approxs = r.approx :: approxs in
      if t' == t && h' == h
      then l, approxs, r
      else h' :: t', approxs, r

and closure env r ffuns fv annot =
  let fv, r = IdentMap.fold (fun id lam (fv,r) ->
      let lam, r = loop env r lam in
      IdentMap.add id (lam, r.approx) fv, r) fv (IdentMap.empty, r) in
  (* we use the previous closure for evaluating the functions *)
  let off off_id = { off_unit = ffuns.unit; off_id } in
  let internal_closure =
    { ffunctions = ffuns;
      bound_var = IdentMap.fold (fun id (_,desc) map ->
          OffsetMap.add (off id) desc map)
          fv OffsetMap.empty } in
  let closure_env = IdentMap.fold
      (fun id _ env -> add_approx id
          (value_closure { fun_id = (off id);
                           closure = internal_closure }) env)
      ffuns.funs (local_env env) in
  let closure_env = IdentMap.fold
      (fun id (_,desc) env -> add_approx id desc env) fv closure_env in
  let funs, r = IdentMap.fold (fun fid ffun (funs,r) ->
      let closure_env = List.fold_left (fun env id ->
          add_approx id value_unknown env) closure_env ffun.params in
      let body, r = loop closure_env r ffun.body in
      let r = List.fold_left exit_scope r ffun.params in
      let r = IdentSet.fold (fun id r -> exit_scope r id)
          ffun.closure_params r in
      IdentMap.add fid { ffun with body } funs, r)
      ffuns.funs (IdentMap.empty, r) in
  let ffuns = { ffuns with funs } in
  let closure = { internal_closure with ffunctions = ffuns } in
  let r = IdentMap.fold (fun id _ r -> exit_scope r id) ffuns.funs r in
  Fclosure (ffuns, IdentMap.map fst fv, annot),
  ret r (value_unoffseted_closure closure)

and offset r flam off rel annot =
  let ret_approx = match r.approx.descr with
    | Value_unoffseted_closure closure ->
        value_closure { fun_id = off; closure }
    | _ -> value_unknown
  in
  Foffset (flam, off, rel, annot), ret r ret_approx

and apply env r (funct,fapprox) (args,approxs) dbg eid =
  match fapprox.descr with
  | Value_closure { fun_id; closure } ->
      let clos = closure.ffunctions in
      assert(fun_id.off_unit = clos.unit);
      let func = IdentMap.find fun_id.off_id clos.funs in
      let nargs = List.length args in
      if nargs = func.arity
      then direct_apply env r clos funct fun_id func args dbg eid
      else
        if nargs > 0 && nargs < func.arity
        then
          loop env r (partial_apply funct fun_id func args dbg eid)
        else
          Fapply (funct, args, None, dbg, eid),
          ret r value_unknown
  | _ ->
      Fapply (funct, args, None, dbg, eid),
      ret r value_unknown

and partial_apply funct fun_id func args dbg eid =
  let remaining_args = func.arity - (List.length args) in
  assert(remaining_args > 0);
  let param_sb = List.map (fun id -> Ident.rename id) func.params in
  let applied_args, remaining_args = map_rem
    (fun arg id' -> id', arg) args param_sb in
  let call_args = List.map (fun id' -> fvar id') param_sb in
  let funct_id = Ident.create "partial_called_fun" in
  let new_fun_id = Ident.create "partial_fun" in
  let expr = Fapply (funct, call_args, Some fun_id, dbg, ExprId.create ()) in
  let fclosure = make_function new_fun_id expr remaining_args in
  let offset = foffset (fclosure, new_fun_id) in
  let with_args = List.fold_right (fun (id', arg) expr ->
      Flet(Strict, id', arg, expr, ExprId.create ()))
      applied_args offset in
  Flet(Strict, funct_id, funct, with_args, ExprId.create ())

and direct_apply env r clos funct fun_id func args dbg eid =
  if func.stub ||
     (not clos.recursives && lambda_smaller func.body
        ((!Clflags.inline_threshold + List.length func.params) * 2))
  then
    (* try inlining if the function is not too far above the threshold *)
    let body, r_inline = inline env r clos funct fun_id func args dbg eid in
    if func.stub ||
       (lambda_smaller body
          (!Clflags.inline_threshold + List.length func.params))
    then
      (* if the definitive size is small enought: keep it *)
      body, r_inline
    else Fapply (funct, args, Some fun_id, dbg, eid),
         ret r value_unknown
         (* do not use approximation: there can be renamed offsets *)
  else Fapply (funct, args, Some fun_id, dbg, eid),
       ret r value_unknown

and inline env r clos lfunc fun_id func args dbg eid =
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
  loop env r (Flet(Strict, clos_id, lfunc, body, ExprId.create ()))

let simplify tree =
  let env = empty_env () in
  let result, r = loop env (init_r ()) tree in
  if not (IdentSet.is_empty r.used_variables)
  then Format.printf "remaining variables: %a@." IdentSet.print r.used_variables;
  assert(IdentSet.is_empty r.used_variables);
  assert(IntSet.is_empty r.used_staticfail);
  result
