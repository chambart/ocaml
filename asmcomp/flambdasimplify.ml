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
open Misc

let fvar id = Fvar(id,ExprId.create ())
let foffset (lam, off_id) =
  Foffset(lam, { off_id; off_unit = Compilenv.current_unit ()},
          None, ExprId.create ())

let add_unit_name name =
  let unit = Compilenv.current_unit_name () in
  unit ^ "^" ^ name

let reid id =
  if id.Ident.stamp >= 1000 (* naze: trouver mieux *)
  then
    { id with Ident.name = add_unit_name id.Ident.name }
  else id

let id_create name =
  Ident.create (add_unit_name name)

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
    bound_var : approx OffsetMap.t;
    fv_subst_renaming : offset OffsetMap.t;
    fun_subst_renaming : offset OffsetMap.t }

and approx =
  { descr : descr;
    var : Ident.t option;
    symbol : symbol option }

let rec print_descr ppf = function
  | Value_int i -> Format.pp_print_int ppf i
  | Value_constptr i -> Format.fprintf ppf "%ia" i
  | Value_block (tag,fields) ->
    let p ppf fields =
      Array.iter (fun v -> Format.fprintf ppf "%a@ " print_approx' v) fields in
    Format.fprintf ppf "[%i:@ @[<1>%a@]]" tag p fields
  | Value_unknown -> Format.fprintf ppf "?"
  | Value_bottom -> Format.fprintf ppf "bottom"
  | Value_extern id -> Format.fprintf ppf "_%a_" Flambdaexport.ExportId.print id
  | Value_symbol sym -> Format.fprintf ppf "%a" Symbol.print sym
  | Value_closure { fun_id } ->
    Format.fprintf ppf "(fun:@ %a)" Offset.print fun_id
  | Value_unoffseted_closure { ffunctions = { funs } } ->
    Format.fprintf ppf "(unoffseted:@ %a)"
      (fun ppf -> IdentMap.iter (fun id _ -> Ident.print ppf id)) funs

and print_approx' ppf { descr } = print_descr ppf descr

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

    let unit_id = Ident.create_persistent (ExportId.unit ex) in
    ignore(Compilenv.approx_for_global unit_id);

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
                bound_var;
                fv_subst_renaming = OffsetMap.empty;
                fun_subst_renaming = OffsetMap.empty } }
      | Value_unoffseted_closure { closure_id; bound_var } ->
        let bound_var = OffsetMap.map import_approx bound_var in
        value_unoffseted_closure
          { ffunctions = Compilenv.imported_closure closure_id;
            bound_var;
            fv_subst_renaming = OffsetMap.empty;
            fun_subst_renaming = OffsetMap.empty }
      | _ ->
          value_unknown
    with Not_found ->
      (* Format.printf "id not found %a@." ExportId.print ex; *)
      value_unknown

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
        (* Format.printf "symbol not found@ %a@." Symbol.print sym; *)
        value_unknown

  let import_symbol sym =
    let r = import_symbol sym in
    (* Format.printf "import %a:@[<1>@ %a@]@." Symbol.print sym print_approx' r; *)
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

(* There are two types of informations propagated.
   - propagating top-down: in the env type
   - propagating following approximatively the evaluation order ~ bottom-up:
     in the ret type *)

type sb = { sb_var : Ident.t IdentMap.t;
            sb_sym : Ident.t SymbolMap.t;
            sb_exn : int IntMap.t;
            back_var : Ident.t list IdentMap.t;
            back_sym : symbol list IdentMap.t;
            (* Used to handle substitution sequence: we cannot call
               the substitution recursively because there can be name
               clash *)
          }

type env =
  { env_approx : approx IdentMap.t;
    global : (int, approx) Hashtbl.t;
    escaping : bool;
    (* Wether the current expression is in a position to escape:
       i.e. if the current expression is a Fvar, will the variable
       will be considered as escaping. *)
    current_functions : FunSet.t;
    (* The functions currently being declared: used to avoid inlining
       recursively *)
    inlining_level : int;
    (* Number of times "inline" has been called recursively *)
    substitution_context : Flambdasubst.context;
    sb : sb;
    substitute : bool;
    inline_threshold : int;
    closure_depth : int;
  }

let empty_sb = { sb_var = IdentMap.empty;
                 sb_sym = SymbolMap.empty;
                 sb_exn = IntMap.empty;
                 back_var = IdentMap.empty;
                 back_sym = IdentMap.empty }

let empty_env () =
  { env_approx = IdentMap.empty;
    global = Hashtbl.create 10;
    escaping = true;
    current_functions = FunSet.empty;
    inlining_level = 0;
    substitution_context = Flambdasubst.empty_context;
    sb = empty_sb;
    substitute = false;
    inline_threshold = !Clflags.inline_threshold;
    closure_depth = 0}

let local_env env =
  { env with
    env_approx = IdentMap.empty;
    sb = empty_sb }

type ret =
  { approx : approx;
    used_variables : IdentSet.t;
    used_staticfail : IntSet.t;
    escape_variables : IdentSet.t;
    tmp_escape_variables : IdentSet.t list;
    not_kept_param : IdentSet.t
    (* Variables used for a recursive call with a different argument
       than the one used to enter the function. *)
  }

(* Utility functions *)

(* substitution utility functions *)

let add_sb_sym' sym id' sb =
  let back_sym =
    let l = try IdentMap.find id' sb.back_sym with Not_found -> [] in
    IdentMap.add id' (sym :: l) sb.back_sym in
  { sb with sb_sym = SymbolMap.add sym id' sb.sb_sym;
            back_sym }

let rec add_sb_var' id id' sb =
  let sb = { sb with sb_var = IdentMap.add id id' sb.sb_var } in
  let sb =
    try let pre_vars = IdentMap.find id sb.back_var in
      List.fold_left (fun sb pre_id -> add_sb_var' pre_id id' sb) sb pre_vars
    with Not_found -> sb in
  let sb =
    try let pre_sym = IdentMap.find id sb.back_sym in
      List.fold_left (fun sb pre_sym -> add_sb_sym' pre_sym id' sb) sb pre_sym
    with Not_found -> sb in
  let back_var =
    let l = try IdentMap.find id' sb.back_var with Not_found -> [] in
    IdentMap.add id' (id :: l) sb.back_var in
  { sb with back_var }


let add_sb_var id id' env = { env with sb = add_sb_var' id id' env.sb }
let add_sb_sym sym id' env = {env with sb = add_sb_sym' sym id' env.sb }
let add_sb_exn i i' env = { env with sb = { env.sb with sb_exn = IntMap.add i i' env.sb.sb_exn } }

let sb_exn i env = try IntMap.find i env.sb.sb_exn with Not_found -> i

let new_subst_exn i env =
  if env.substitute
  then
    let i' = Lambda.next_raise_count () in
    let env = add_sb_exn i i' env in
    i', env
  else i, env

let new_subst_id id env =
  if env.substitute
  then
    (* allows to prepare a substitution to know the final name *)
    (* let id' = try IdentMap.find id env.sb.sb_var with Not_found -> Ident.rename id in *)

    let id' = Ident.rename id in
    let env = add_sb_var id id' env in
    (* if id'.Ident.stamp = 1323 || id'.Ident.stamp = 1322 *)
    (* then Format.printf "%a -> %a@." Ident.print id Ident.print id'; *)
    id', env
  else id, env

let new_subst_ids defs env =
  List.fold_right (fun (id,lam) (defs, env) ->
      let id', env = new_subst_id id env in
      (id',lam) :: defs, env) defs ([],env)

let new_subst_ids' ids env =
  List.fold_right (fun id (ids,env) ->
      let id', env = new_subst_id id env in
      id' :: ids, env) ids ([],env)

let find_subst' id env =
  try IdentMap.find id env.sb.sb_var with
  | Not_found ->
      fatal_error (Format.asprintf "find_subst': can't find %a@." Ident.print id)

let subst_var env id =
  try IdentMap.find id env.sb.sb_var with
  | Not_found -> id

type offset_subst =
  { os_fv : offset OffsetMap.t;
    os_fun : offset OffsetMap.t }

let empty_offset_subst =
  { os_fv = OffsetMap.empty; os_fun = OffsetMap.empty }

let new_subst_off id off_unit env off_sb =
  if env.substitute
  then
    let id' = Ident.rename id in
    (* Format.printf "rename off: %a -> %a@." Ident.print id Ident.print id'; *)
    let env = add_sb_var id id' env in
    let off = { off_unit; off_id = id } in
    let off' = { off_unit = Compilenv.current_unit (); off_id = id' } in
    let off_sb = OffsetMap.add off off' off_sb in
    id', env, off_sb
  else id, env, off_sb

let new_subst_fv_off id off_unit env off_sb =
  (* Format.printf "rename off fv %a@." Ident.print id; *)
  let id, env, os_fv = new_subst_off id off_unit env off_sb.os_fv in
  id, env, { off_sb with os_fv }

let new_subst_fun_off id off_unit env off_sb =
  (* Format.printf "rename off fun %a@." Ident.print id; *)
  let id, env, os_fun = new_subst_off id off_unit env off_sb.os_fun in
  id, env, { off_sb with os_fun }

(* approximation utility functions *)

let ret (acc:ret) approx = { acc with approx }

let escaping ?(b=true) env = { env with escaping = b }

let escape_var acc var =
  { acc with escape_variables = IdentSet.add var acc.escape_variables }

let tmp_escape_var acc var =
  match acc.tmp_escape_variables with
  | [] -> assert false
  | set :: q ->
      { acc with
        tmp_escape_variables = IdentSet.add var set :: q }

let start_escape_region acc =
  { acc with tmp_escape_variables = IdentSet.empty :: acc.tmp_escape_variables }

let end_escape_region acc =
  match acc.tmp_escape_variables with
  | [] -> assert false
  | set :: q -> set, { acc with tmp_escape_variables = q }

let merge_escape set acc =
  { acc with escape_variables = IdentSet.union acc.escape_variables set }

let use_var acc var =
  (* Format.printf "use var %a@." Ident.print var; *)
  { acc with used_variables = IdentSet.add var acc.used_variables }

let exit_scope acc var =
  { acc with
    used_variables = IdentSet.remove var acc.used_variables;
    escape_variables = IdentSet.remove var acc.escape_variables }

let use_staticfail acc i =
  { acc with used_staticfail = IntSet.add i acc.used_staticfail }

let exit_scope_catch acc i =
  { acc with used_staticfail = IntSet.remove i acc.used_staticfail }

let not_kept_param id acc =
  { acc with not_kept_param = IdentSet.add id acc.not_kept_param }

let init_r () =
  { approx = value_unknown;
    used_variables = IdentSet.empty;
    used_staticfail = IntSet.empty;
    escape_variables = IdentSet.empty;
    tmp_escape_variables = [];
    not_kept_param = IdentSet.empty }

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

let inlining_level_up env = { env with inlining_level = env.inlining_level + 1 }

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

(* Utility function to duplicate an expression and makes a function from it *)

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
      kept_params = IdentSet.empty;
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
     fv', IdentMap.empty, ExprId.create ())

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
  | Praise _ -> 4
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

let lambda_smaller' lam threshold =
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
    | Fclosure(ffuns, fv, _, _) ->
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
    lambda_size lam;
    if !size <= threshold then Some !size
    else None
  with Exit ->
    None

let lambda_smaller lam threshold =
  lambda_smaller' lam threshold <> None

(* Simple effectfull test, should be replace by call to Purity module *)

let no_effects_prim = function
    Psetglobal _ | Psetfield _ | Psetfloatfield _ | Pduprecord _ |
    Pccall _ | Praise _ | Poffsetref _ | Pstringsetu | Pstringsets |
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
  | Fclosure (_, fv, _, _) ->
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
      then
        begin
          (* Format.printf "in env %a@." Ident.print var; *)
          Fvar(var, data lam)
        end
      else lam
  in
  let expr, r = check_constant_result r res approx in
  let r = match expr with
    | Fvar(var,_) ->
        let r =
          if env.escaping
          then escape_var r var
          else tmp_escape_var r var in
        (* Format.printf "use var %a@." Ident.print var; *)
        use_var r var
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

let split_list n l =
  let rec aux n acc l =
    if n = 0
    then List.rev acc, l
    else
      match l with
      | [] -> assert false
      | t::q ->
          aux (n-1) (t::acc) q in
  aux n [] l

(* The main functions: iterate on the expression rewriting it and
   propagating up an approximation of the value *)

(* let rec sb_var id env = *)
(*   try sb_var (IdentMap.find id env.sb.sb_var) env with *)
(*   | Not_found -> id *)

let rec loop env r tree =
  loop_no_escape (escaping env) r tree

and loop_substitute env r tree =
  loop { env with substitute = true } r tree

and loop_no_escape env r tree =
  let f, r = loop_direct env r tree in
  f, ret r (really_import_approx r.approx)

and loop_direct (env:env) r tree : 'a flambda * ret =
  (* TODO: changer le nom de tree pour être sûr qu'il n'est pas utilisé: avec la substitution
     il faut passer sur tout pour être sur de ne pas oublier de renomer une variable.
     bon en fait non ici, mais dans les sous fonctions ? *)
  match tree with
  | Fsymbol (sym,annot) ->
      let id' = try Some (SymbolMap.find sym env.sb.sb_sym) with Not_found -> None in
      begin match id' with
      | Some id' -> loop_direct env r (Fvar(id',annot))
      | None -> check_constant_result r tree (import_symbol sym)
      end
  | Fvar (id,annot) ->
      let id, tree =
        try
          let id' = IdentMap.find id env.sb.sb_var in
          (* Format.printf "subst: %a -> %a@." Ident.print id Ident.print id'; *)
          id', Fvar(id',annot) with
        | Not_found -> id, tree
      in
      check_var_and_constant_result env r tree (find_unknwon id env)
  | Fconst (cst,_) -> tree, ret r (const_approx cst)


  (* TODO VERIFY: we should not specialise non kept recursive parameters *)

  (* The only form to allow specialising function to parameters: no escaping guaranteed.
     if the function was an escaping recursive function, the transformation wouldn't
     have been allowed. *)
  | Fapply (Foffset (Fclosure (ffuns, fv, spec_args, a_clo), off, (None as rel), a_off),
            args, direc, dbg, a_app) ->
      (* Format.printf "known apply %a@." Offset.print off; *)
      (* suppose no strange effect in fv: eval args before closure *)
      let args, approxs, r = loop_list env r args in
      let r = start_escape_region r in
      let clos, r = closure env r ffuns fv spec_args a_clo in
      let funct, ({ approx = fapprox } as r) = offset r clos off rel a_off in
      let tmp_escape, r = end_escape_region r in
      apply ~local:true env r tmp_escape (funct,fapprox) (args,approxs) dbg a_app

  | Fapply (funct, args, direc, dbg, annot) ->
      let r = start_escape_region r in
      let funct, ({ approx = fapprox } as r) = loop_no_escape (escaping ~b:false env) r funct in
      let tmp_escape, r = end_escape_region r in
      let args, approxs, r = loop_list env r args in
      apply ~local:false env r tmp_escape (funct,fapprox) (args,approxs) dbg annot

  | Fclosure (ffuns, fv, spec_args, annot) ->
      closure env r ffuns fv spec_args annot
  | Foffset (flam, off, rel, annot) ->
      (* Format.printf "foffset before@ %a@." Printflambda.flambda flam; *)
      let flam, r = loop env r flam in
      offset r flam off rel annot
  | Fenv_field (fenv_field, annot) as expr ->
      let fun_off_id off closure =
        try
          let off' = OffsetMap.find off closure.fun_subst_renaming in
          (* Format.printf "enf_field fun %a -> %a@." Offset.print off Offset.print off'; *)
          off'
        with Not_found -> off in
      let fv_off_id off closure =
        try
          let off' = OffsetMap.find off closure.fv_subst_renaming in
          (* Format.printf "enf_field fv %a -> %a@." Offset.print off Offset.print off'; *)
          off'
        with Not_found -> off in

      let arg, r = loop env r fenv_field.env in
      let closure, approx_fun_id = match r.approx.descr with
        | Value_closure { closure; fun_id } -> closure, fun_id
        | Value_unknown ->
            Format.printf "Value unknown: %a@.%a@.%a@."
              Printflambda.flambda expr
              Printflambda.flambda arg
              Printflambda.flambda fenv_field.env;
            assert false
        | _ -> assert false in
      let env_var = fv_off_id fenv_field.env_var closure in
      let env_fun_id = fun_off_id fenv_field.env_fun_id closure in

      assert(Offset.equal env_fun_id approx_fun_id);

      let approx =
        try OffsetMap.find env_var closure.bound_var with
        | Not_found ->
            Format.printf "no field %a in closure %a@ %a@."
              Offset.print env_var
              Offset.print env_fun_id
              Printflambda.flambda arg;
            assert false in

      let expr =
        if arg == fenv_field.env
        then expr (* if the argument didn't change, the names didn't also *)
        else Fenv_field ({ env = arg; env_fun_id; env_var }, annot) in
      check_var_and_constant_result env r expr approx
  | Flet(str, id, lam, body, annot) ->
      let init_used_var = r.used_variables in
      (* Format.printf "flet: %a ->@ %a@." Ident.print id Printflambda.flambda lam; *)
      (* Format.printf "body:@ %a@." Printflambda.flambda body; *)
      let lam, r = loop env r lam in
      (* Format.printf "Flet %a -> " Ident.print id; *)
      let id, env = new_subst_id id env in
      (* Format.printf "%a@." Ident.print id; *)
      let def_used_var = r.used_variables in
      let body_env = match str with
        | Variable -> env
        | _ -> add_approx id r.approx env in
      let r_body = { r with used_variables = init_used_var } in
      let body, r = loop body_env r_body body in
      (* Format.printf "used: %a %b@." Ident.print id (IdentSet.mem id r.used_variables); *)
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
      (* Format.printf "flet_out: %a -> %a@." Ident.print id Printflambda.flambda lam; *)
      expr, exit_scope r id
  | Fletrec(defs, body, annot) ->
      let defs, env = new_subst_ids defs env in
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
      let i = sb_exn i env in
      let args, _, r = loop_list env r args in
      let r = use_staticfail r i in
      Fstaticfail (i, args, annot),
      ret r value_bottom
  | Fcatch (i, vars, body, handler, annot) ->
      let i, env = new_subst_exn i env in
      let body, r = loop env r body in
      if not (IntSet.mem i r.used_staticfail)
      then body, r
      else
        let vars, env = new_subst_ids' vars env in
        let env = List.fold_left (fun env id -> add_approx id value_unknown env)
            env vars in
        let handler, r = loop env r handler in
        let r = List.fold_left exit_scope r vars in
        let r = exit_scope_catch r i in
        Fcatch (i, vars, body, handler, annot),
        ret r value_unknown
  | Ftrywith(body, id, handler, annot) ->
      let body, r = loop env r body in
      let id, env = new_subst_id id env in
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
      let id, env = new_subst_id id env in
      let body, r = loop env r body in
      let r = exit_scope r id in
      Ffor(id, lo, hi, dir, body, annot),
      ret r value_unknown
  | Fassign(id, lam, annot) ->
      let lam, r = loop env r lam in
      let id = try IdentMap.find id env.sb.sb_var with
        | Not_found -> id in
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

and subst_free_vars unit fv env =
  IdentMap.fold (fun id lam (fv, env, off_sb) ->
      let id, env, off_sb = new_subst_fv_off id unit env off_sb in
      IdentMap.add id lam fv, env, off_sb)
    fv (IdentMap.empty, env, empty_offset_subst)

and ffuns_subst env ffuns off_sb =
  if env.substitute
  then
    (* only the structure of ffunction, the body is substituted later *)
    let subst_ffunction fun_id ffun env =
      let label = Flambdagen.make_function_lbl fun_id in
      let closure_params = IdentSet.fold (fun id set -> IdentSet.add (find_subst' id env) set)
          ffun.closure_params IdentSet.empty in
      (* Format.printf "add closure params %a -> %a@." *)
      (*   IdentSet.print ffun.closure_params *)
      (*   IdentSet.print closure_params; *)
      let params, env = new_subst_ids' ffun.params env in
      let kept_params = IdentSet.map (fun id -> find_subst' id env) ffun.kept_params in
      (* It is not a problem to share the substitution of parameter
         names between function: There should be no clash *)
      { ffun with
        closure_params;
        kept_params;
        params;
        label }, env
    in
    let funs, env, off_sb =
      IdentMap.fold (fun id ffun (funs, env, off_sb) ->

          (* let closure_symbol = Compilenv.closure_symbol *)
          (*     { off_unit = ffuns.unit; off_id = id } in *)

          let id, env, off_sb = new_subst_fun_off id ffuns.unit env off_sb in

          (* let env = add_sb_sym closure_symbol id env in *)

          (* Format.printf "subst fun name %a -> %a@." *)
          (*   Symbol.print closure_symbol *)
          (*   Ident.print id; *)
          let ffun, env = subst_ffunction id ffun env in
          let funs = IdentMap.add id ffun funs in
          funs, env, off_sb)
        ffuns.funs (IdentMap.empty,env,off_sb) in
    { ffuns with
      ident = FunId.create ((Compilenv.current_unit_name ()));
      unit = Compilenv.current_unit ();
      funs }, env, off_sb
  else ffuns, env,off_sb

and closure env r ffuns fv spec_args annot =
  let env = { env with closure_depth = env.closure_depth + 1 } in
  (* IdentMap.iter (fun key id -> *)
  (*     Format.printf "clos %a with %a -> %a@." *)
  (*       Ident.print key *)
  (*       Ident.print id *)
  (*       Ident.print (subst_var env id)) spec_args; *)
  let spec_args = IdentMap.map (subst_var env) spec_args in
  let approxs = IdentMap.map (fun id -> find_unknwon id env) spec_args in

  let fv, r = IdentMap.fold (fun id lam (fv,r) ->
      let lam, r = loop env r lam in
      IdentMap.add id (lam, r.approx) fv, r) fv (IdentMap.empty, r) in

  let env = local_env env in

  let prev_closure_symbols = IdentMap.fold (fun id _ map ->
      let sym = Compilenv.closure_symbol { off_unit = ffuns.unit; off_id = id } in
      SymbolMap.add sym id map) ffuns.funs SymbolMap.empty in

  (* Format.printf "funid %a ->" FunId.print ffuns.ident; *)
  let fv, env, off_sb = subst_free_vars ffuns.unit fv env in
  let ffuns, env, off_sb = ffuns_subst env ffuns off_sb in
  (* Format.printf "%a@." FunId.print ffuns.ident; *)

  let spec_args = IdentMap.map_keys (subst_var env) spec_args in
  let approxs = IdentMap.map_keys (subst_var env) approxs in
  let prev_closure_symbols = SymbolMap.map (subst_var env) prev_closure_symbols in

  let env = { env with current_functions = FunSet.add ffuns.ident env.current_functions } in
  (* we use the previous closure for evaluating the functions *)
  let off off_id = { off_unit = Compilenv.current_unit (); off_id } in
  (* let off off_id = { off_unit = ffuns.unit; off_id } in *)
  let internal_closure =
    { ffunctions = ffuns;
      bound_var = IdentMap.fold (fun id (_,desc) map ->
          OffsetMap.add (off id) desc map)
          fv OffsetMap.empty;
      fv_subst_renaming = off_sb.os_fv;
      fun_subst_renaming = off_sb.os_fun } in
  let closure_env = IdentMap.fold
      (fun id _ env -> add_approx id
          (value_closure { fun_id = (off id);
                           closure = internal_closure }) env)
      ffuns.funs env in
  let funs, used_params, r = IdentMap.fold (fun fid ffun (funs,used_params,r) ->
      (* Format.printf "ffun %a@." Ident.print fid; *)
      let closure_env = IdentMap.fold
          (fun id (_,desc) env ->
             if IdentSet.mem id ffun.closure_params
             then begin
               (* Format.printf "add closure param %a@." Ident.print id; *)
               add_approx id desc env
             end
             else env) fv closure_env in
      let closure_env = List.fold_left (fun env id ->
          let approx = try IdentMap.find id approxs
            with Not_found ->
              (* Format.printf "approx not found %a@." Ident.print id; *)
              value_unknown in
          add_approx id approx env) closure_env ffun.params in

      (***** TODO: find something better
             Warning if multiply recursive function ******)
      (* Format.printf "body:@ %a@." Printflambda.flambda ffun.body; *)
      let body = Flambdaiter.map_toplevel (function
          | Fsymbol (sym,_) when SymbolMap.mem sym prev_closure_symbols ->
              (* Format.printf "rec replace here! %a@." Symbol.print sym; *)
              Fvar(SymbolMap.find sym prev_closure_symbols,ExprId.create ())
          | e -> e) ffun.body in
      (* Format.printf "after:@ %a@." Printflambda.flambda body; *)
      (* We replace recursive calls using the function symbol
         This is done before substitution because we could have something like:
           List.iter (List.iter some_fun) l
         And we need to distinguish the inner iter from the outer one
      *)

      let body, r = loop closure_env r body in
      let used_params = List.fold_left (fun acc id ->
          if IdentSet.mem id r.used_variables
          then IdentSet.add id acc
          else acc) used_params ffun.params in
      let kept_params =
        if (IdentMap.cardinal ffuns.funs = 1) &&
           (* multiply recursive functions are not handled yet *)
           ffuns.recursives && not (IdentSet.mem fid r.escape_variables)
        then begin
          let kept_params = List.filter
              (fun id ->
                 IdentSet.mem id r.used_variables &&
                 not (IdentSet.mem id r.not_kept_param))
              ffun.params in
          IdentSet.of_list kept_params
        end
        else ffun.kept_params in
      let r = List.fold_left exit_scope r ffun.params in
      let r = IdentSet.fold (fun id r -> exit_scope r id)
          ffun.closure_params r in
      IdentMap.add fid { ffun with body; kept_params } funs, used_params, r)
      ffuns.funs (IdentMap.empty, IdentSet.empty, r) in

  let spec_args = IdentMap.filter
      (fun id _ -> IdentSet.mem id used_params)
      spec_args in

  let r = IdentMap.fold (fun id' v acc ->
      (* Format.printf "use %a (for %a)@." Ident.print v Ident.print id'; *)
      use_var acc v) spec_args r in
  let ffuns = { ffuns with funs } in
  let closure = { internal_closure with ffunctions = ffuns } in
  let r = IdentMap.fold (fun id _ r -> exit_scope r id) ffuns.funs r in
  Fclosure (ffuns, IdentMap.map fst fv, spec_args, annot),
  ret r (value_unoffseted_closure closure)

and offset r flam off rel annot =
  let off_id closure off =
    try
      let off' = OffsetMap.find off closure.fun_subst_renaming in
      (* Format.printf "offset %a -> %a@." Offset.print off Offset.print off'; *)
      off'
    with Not_found ->
      (* Format.printf "no rename offset %a %i@." Offset.print off *)
      (*   (OffsetMap.cardinal closure.fun_subst_renaming); *)
      off in
  let off_id closure off =
    let off = off_id closure off in
    if not (IdentMap.mem off.off_id closure.ffunctions.funs)
    then fatal_error (Format.asprintf "no function %a in the closure@ %a@."
                        Offset.print off Printflambda.flambda flam);
    off
  in
  let closure = match r.approx.descr with
    | Value_unoffseted_closure closure -> closure
    | Value_closure { closure } -> closure
    | _ ->
        Format.printf "%a@.%a@." Offset.print off Printflambda.flambda flam;
        assert false in
  let off = off_id closure off in
  let rel = Misc.may_map (off_id closure) rel in
  let ret_approx = value_closure { fun_id = off; closure } in

  Foffset (flam, off, rel, annot), ret r ret_approx

(* Apply a function to its parameters: if the function is known, we will go to the special cases:
   direct apply of parial apply
   local: if local is true, the application is of the shape: apply (offset (closure ...)).
          i.e. it should not duplicate the function
 *)
and apply env r ~local tmp_escape (funct,fapprox) (args,approxs) dbg eid =
  match fapprox.descr with
  | Value_closure { fun_id; closure } ->
      let clos = closure.ffunctions in

      if not (fun_id.off_unit = clos.unit)
      then Format.printf "not same unit %a %a@."
          Symbol.print fun_id.off_unit
          Symbol.print clos.unit;
      assert(fun_id.off_unit = clos.unit);

      let func =
        try IdentMap.find fun_id.off_id clos.funs with
        | Not_found ->
            Format.printf "missing %a@." Ident.print fun_id.off_id;
            assert false
      in
      let nargs = List.length args in
      if nargs = func.arity
      then direct_apply env r ~local clos funct fun_id func fapprox closure (args,approxs) dbg eid
      else
      if nargs > func.arity
      then
        let h_args, q_args = split_list func.arity args in
        let h_approxs, q_approxs = split_list func.arity approxs in
        let expr, r = direct_apply env r ~local clos funct fun_id func fapprox closure (h_args,h_approxs)
            dbg (ExprId.create ()) in
        loop env r (Fapply(expr, q_args, None, dbg, eid))
      else
        let r = merge_escape tmp_escape r in
        if nargs > 0 && nargs < func.arity
        then
          (* !!! TEST ***)

          loop env r (partial_apply funct fun_id func args dbg eid)
          (* Fapply (funct, args, None, dbg, eid), *)
          (* ret r value_unknown *)
        else
          Fapply (funct, args, None, dbg, eid),
          ret r value_unknown
  | _ ->
      let r = merge_escape tmp_escape r in
      Fapply (funct, args, None, dbg, eid),
      ret r value_unknown

and partial_apply funct fun_id func args dbg eid =
  let remaining_args = func.arity - (List.length args) in
  assert(remaining_args > 0);
  let param_sb = List.map (fun id -> Ident.rename id) func.params in
  let applied_args, remaining_args = Misc.map2_head
    (fun arg id' -> id', arg) args param_sb in
  let call_args = List.map (fun id' -> fvar id') param_sb in
  let funct_id = id_create "partial_called_fun" in
  let new_fun_id = id_create "partial_fun" in
  let expr = Fapply (funct, call_args, Some fun_id, dbg, ExprId.create ()) in
  let fclosure = make_function new_fun_id expr remaining_args in
  let offset = foffset (fclosure, new_fun_id) in
  let with_args = List.fold_right (fun (id', arg) expr ->
      Flet(Strict, id', arg, expr, ExprId.create ()))
      applied_args offset in
  Flet(Strict, funct_id, funct, with_args, ExprId.create ())

and functor_like env clos approxs =
  let b =
    env.closure_depth = 0 &&
    List.for_all (function { descr = Value_unknown } -> false | _ -> true) approxs &&
    not clos.recursives in
  (* if b then *)
  (*   Format.printf "functor_like %s@." ((snd (IdentMap.choose clos.funs)).label:>string); *)
  b

and direct_apply env r ~local clos funct fun_id func fapprox closure (args,approxs) dbg eid =
  let r =
    List.fold_left2 (fun r approx id ->
        match approx.var with
        | Some id' when Ident.same id id' -> r
        | _ -> not_kept_param id r) r approxs func.params
  in
  let max_level = 3 in
  (* if env.inlining_level > max_level then
     Format.printf "current level: %i in %a@." env.inlining_level Offset.print fun_id; *)
  let fun_size =
    if func.stub || functor_like env clos approxs
    then Some 0
    else lambda_smaller' func.body
        ((env.inline_threshold + List.length func.params) * 2) in
  match fun_size with
  | None ->
      Fapply (funct, args, Some fun_id, dbg, eid),
      ret r value_unknown
  | Some fun_size ->
      let env = { env with inline_threshold = env.inline_threshold - fun_size } in
      if func.stub || functor_like env clos approxs ||
         (not clos.recursives && env.inlining_level <= max_level)
      then
        (* try inlining if the function is not too far above the threshold *)
        let body, r_inline = inline env r clos funct fun_id func args dbg eid in
        if func.stub || functor_like env clos approxs ||
           (lambda_smaller body
              (env.inline_threshold + List.length func.params))
        then
          (* if the definitive size is small enought: keep it *)
          body, r_inline
        else Fapply (funct, args, Some fun_id, dbg, eid),
             ret r value_unknown
             (* do not use approximation: there can be renamed offsets.
             A better solution would be to use the generic approximation
                of the function *)
      else
        (* if false (\* Recursive function specialisation temporarily disabled *\) *)
        (* let () = Format.printf "current functions: %a@ %a@." *)
        (*     FunId.print clos.ident *)
        (*     FunSet.print env.current_functions in *)
      if clos.recursives && not (FunSet.mem clos.ident env.current_functions) &&
         not (IdentSet.is_empty func.kept_params)
         && OffsetMap.is_empty closure.bound_var (* closed *)
      then begin
        let f id approx acc =
          match approx.descr with
          | Value_unknown
          | Value_bottom -> acc
          | _ ->
              if IdentSet.mem id func.kept_params
              then IdentMap.add id approx acc
              else acc in
        let worth = List.fold_right2 f func.params approxs IdentMap.empty in
        if not (IdentMap.is_empty worth) && not local
        then
          duplicate_apply env r funct clos fun_id func fapprox closure
            (args,approxs) dbg
        else
          Fapply (funct, args, Some fun_id, dbg, eid),
          ret r value_unknown
      end
      else
        Fapply (funct, args, Some fun_id, dbg, eid),
        ret r value_unknown

(* Inlining for recursive functions: duplicates the function
   declaration and specialise it *)
and duplicate_apply env r funct clos fun_id func fapprox closure_approx
    (args,approxs) dbg =
  let env = inlining_level_up env in
  (* Format.printf "duplicate@."; *)
  let clos_id = id_create "dup_closure" in
  let make_fv { off_id = id } _ fv =
    (* Format.printf "fv clos:%a fun:%a off:%a @." *)
    (*   Ident.print clos_id Offset.print fun_id Offset.print *)
    (*   { off_unit = clos.unit; *)
    (*     off_id = id }; *)
    IdentMap.add id
      (Fenv_field ({ env = Fvar(clos_id, ExprId.create ());
                     env_fun_id = fun_id;
                     env_var = { off_unit = clos.unit;
                                 off_id = id } },
                   ExprId.create ())) fv
  in
  let fv = OffsetMap.fold make_fv closure_approx.bound_var IdentMap.empty in

  let env = add_approx clos_id fapprox env in

  (* TODO: remove specialisation from here and factorise with the other case *)

  let (spec_args, args, env_func) =
    let f (id,arg) approx (spec_args,args,env_func) =
      let new_id = Ident.rename id in
      (* Format.printf "rename %a -> %a@." Ident.print id Ident.print new_id; *)
      let args = (new_id, arg) :: args in
      let env_func = add_approx new_id approx env_func in
      let spec_args =
        match approx.descr with
        | Value_unknown
        | Value_bottom -> spec_args
        | _ ->
            if IdentSet.mem id func.kept_params
            then IdentMap.add id new_id spec_args
            else spec_args in
      spec_args, args, env_func
    in
    let params = List.combine func.params args in
    List.fold_right2 f params approxs (IdentMap.empty,[],env) in

  let args_exprs = List.map (fun (id,_) -> Fvar(id,ExprId.create ())) args in

  let clos_expr = (Fclosure(clos, fv, spec_args, ExprId.create ())) in

  let r = exit_scope r clos_id in
  let expr = Foffset(clos_expr, fun_id,
                     None, ExprId.create ()) in
  let expr = Fapply (expr, args_exprs, Some fun_id, dbg, ExprId.create ()) in
  let expr = List.fold_left
      (fun expr (id,arg) ->
         (* Format.printf "flet %a -> %a@." Ident.print id Printflambda.flambda arg; *)
         Flet(Strict, id, arg, expr, ExprId.create ()))
      expr args in
  let expr = Flet(Strict, clos_id, funct, expr, ExprId.create ()) in
  let r = List.fold_left (fun r (id,_) -> exit_scope r id) r args in
  loop_substitute env r expr
  (* expr, r *)

(* Duplicates the body of the called function *)
and inline env r clos lfunc fun_id func args dbg eid =
  let env = inlining_level_up env in
  let clos_id = id_create "inlined_closure" in

  (* let fv', env = IdentSet.fold (fun id (l,env) -> *)
  (*     let id' = Ident.rename id in *)
  (*     let env = add_sb_var id id' env in *)
  (*     (id, id') :: l, env) *)
  (*     func.closure_params ([],env) in *)

  let body =
    func.body
    |> List.fold_right2 (fun id arg body ->
        Flet(Strict, id, arg, body, ExprId.create ~name:"inline arg" ()))
      func.params args
    |> IdentSet.fold (fun id body ->
        (* Format.printf "inline closure param %a@." Ident.print id; *)
        Flet(Strict, id,
             Fenv_field ({ env = Fvar(clos_id, ExprId.create ());
                           env_fun_id = fun_id;
                           env_var = { off_unit = clos.unit;
                                       off_id = id } },
                         ExprId.create ()),
             body, ExprId.create ()))
      func.closure_params
    |> IdentMap.fold (fun id _ body ->
        Flet(Strict, id,
             Foffset (Fvar(clos_id, ExprId.create ()),
                      {fun_id with off_id = id},
                      Some fun_id,
                      ExprId.create ()),
             body, ExprId.create ()))
      clos.funs
  in
  loop_substitute env r (Flet(Strict, clos_id, lfunc, body, ExprId.create ()))

(*
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
  let body, substitution_context =
    (* Format.printf "subst inline body %a@." Offset.print fun_id; *)
    (* Flambdasubst.substitute' subst env.substitution_context func.body *)
    Flambdasubst.substitute' subst Flambdasubst.empty_context func.body
  in
  let env = { env with substitution_context } in
  let body =
    body
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
*)

let simplify tree =
  let env = empty_env () in
  let result, r = loop env (init_r ()) tree in
  (* let result, r = loop_substitute env (init_r ()) tree in *)
  if not (IdentSet.is_empty r.used_variables)
  then Format.printf "remaining variables: %a@." IdentSet.print r.used_variables;
  assert(IdentSet.is_empty r.used_variables);
  assert(IntSet.is_empty r.used_staticfail);
  result
