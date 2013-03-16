(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                      Pierre Chambart (OCamlPro)                        *)
(*                                                                        *)
(*   Copyright 2013 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

(* Introduction of closures *)

open Misc
open Lambda
open Flambda

let make_function_lbl id =
  let label = Compilenv.make_symbol (Some (Ident.unique_name id)) in
  make_function_label label

let rec add_debug_info ev f =
  match ev.lev_kind with
  | Lev_after _ ->
      begin match f with
        | Fapply(fn, args, direct, dinfo ,v) ->
          Fapply(fn, args, direct, Debuginfo.from_call ev, v)
        | Fprim(Praise, args, dinfo, v) ->
          Fprim(Praise, args, Debuginfo.from_call ev, v)
        | Fsend(kind, f1, f2, args, dinfo, v) ->
          Fsend(kind, f1, f2, args, Debuginfo.from_call ev, v)
        | Fsequence(f1, f2, v) ->
          Fsequence(f1, add_debug_info ev f2, v)
        | _ -> f
      end
  | _ -> f

(* shouldn't be recursive: insertion should be instead *)
let rec subst sb id =
  try subst sb (IdentMap.find id sb)
  with Not_found -> id

let add_subst sb ~replace ~by =
  IdentMap.add replace by sb

let nid () = ExprId.create ()

let rec close sb = function
    Lvar id -> Fvar (subst sb id, nid ())
  | Lconst cst -> close_const sb cst
  | Llet(str, id, lam, body) ->
    Flet(str, id, close_named id sb lam, close sb body, nid ())
  | Lfunction(kind, params, body) as funct ->
    let id = Ident.create "fun" in
    Foffset(close_functions sb [id,funct],id, nid ())
  | Lapply(funct, args, loc) ->
    Fapply(close sb funct, close_list sb args, None, Debuginfo.none, nid ())
  | Lletrec(defs, body) ->
    if List.for_all
        (function (id, Lfunction(_, _, _)) -> true | _ -> false)
        defs
    then
      let clos = close_functions sb defs in
      let clos_ident = Ident.create "clos" in
      let body = List.fold_left (fun body (id,_) ->
          Flet(Strict, id, Foffset(Fvar (clos_ident, nid ()), id, nid ()), body, nid ()))
          (close sb body) defs in
      Flet(Strict, clos_ident, clos, body, nid ())
    else
      let fdefs = List.map (fun (id,def) -> id, close_named id sb def) defs in
      Fletrec(fdefs, close sb body, nid ())
  | Lsend(kind, met, obj, args, _) ->
    Fsend(kind, close sb met, close sb obj,
        close_list sb args, Debuginfo.none, nid ())
  | Lprim(Pdirapply loc,[funct;arg])
  | Lprim(Prevapply loc,[arg;funct]) ->
    close sb (Lapply(funct, [arg], loc))
  | Lprim(Praise, [Levent(arg, ev)]) ->
    Fprim(Praise, [close sb arg], Debuginfo.from_raise ev, nid ())
  | Lprim(p, args) ->
    Fprim(p, close_list sb args, Debuginfo.none, nid ())
  | Lswitch(arg, sw) ->
    let aux (i,lam) = i, close sb lam in
    Fswitch(close sb arg,
        { fs_numconsts = sw.sw_numconsts;
          fs_consts = List.map aux sw.sw_consts;
          fs_numblocks = sw.sw_numblocks;
          fs_blocks = List.map aux sw.sw_blocks;
          fs_failaction = may_map (close sb) sw.sw_failaction }, nid ())
  | Lstaticraise (i, args) ->
    Fstaticfail (i, close_list sb args, nid ())
  | Lstaticcatch(body, (i, vars), handler) ->
    Fcatch (i, vars, close sb body, close sb handler, nid ())
  | Ltrywith(body, id, handler) ->
    Ftrywith(close sb body, id, close sb handler, nid ())
  | Lifthenelse(arg, ifso, ifnot) ->
    Fifthenelse(close sb arg, close sb ifso, close sb ifnot, nid ())
  | Lsequence(lam1, lam2) ->
    Fsequence(close sb lam1, close sb lam2, nid ())
  | Lwhile(cond, body) ->
    Fwhile(close sb cond, close sb body, nid ())
  | Lfor(id, lo, hi, dir, body) ->
    Ffor(id, close sb lo, close sb hi, dir, close sb body, nid ())
  | Lassign(id, lam) ->
    Fassign(id, close sb lam, nid ())
  | Levent(lam, ev) ->
    add_debug_info ev (close sb lam)
  | Lifused _ ->
    assert false

and close_functions (sb:Ident.t IdentMap.t) (fun_defs:(Ident.t * lambda) list) =
  let fv = List.fold_left
      (fun set (_,body) -> IdentSet.union (free_variables body) set)
      IdentSet.empty fun_defs in
  let fun_ids = List.fold_left (fun set (id,_) -> IdentSet.add id set)
      IdentSet.empty fun_defs in
  (* let fv = free_variables (Lletrec(fun_defs, lambda_unit)) in *)
  (* the recursive call to the function should not be added to clos_var *)
  let recursives = ref false in
  let sb,clos_var = List.fold_left (fun (sb,clos_var) id ->
      let id = subst sb id in
      let id' = Ident.rename id in
      if IdentSet.mem id fun_ids
      then (recursives := true;
        (sb,clos_var))
      else (add_subst sb ~replace:id ~by:id',
        IdentMap.add id' (Fvar (id, nid ())) clos_var))
      (sb,IdentMap.empty)
      (IdentSet.elements fv) in
  let close_one = function
      | (id, Lfunction(kind, params, body)) ->
        let dbg = match body with
          | Levent (_,({lev_kind=Lev_function} as ev)) ->
            Debuginfo.from_call ev
          | _ -> Debuginfo.none in
        { label = make_function_lbl id;
          kind;
          arity = List.length params;
          params;
          closure_params = fv;
          body = close sb body;
          dbg }
      | (_, _) -> fatal_error "Flambdagen.close_functions"
  in
  let functions = List.fold_left (fun map (id, f) ->
      IdentMap.add id (close_one (id,f)) map) IdentMap.empty fun_defs in
  let ident = FunId.create () in
  Fclosure ({ ident; funs = functions; recursives = !recursives },
    clos_var, nid ())

and close_list sb l = List.map (close sb) l

and close_named id sb = function
  | Lfunction(kind, params, body) as funct ->
    Foffset(close_functions sb [id,funct],id, nid ())
  | lam ->
    close sb lam

and close_const sb cst =
  let rec aux = function
    | Const_base c -> Fconst(Fconst_base c, nid ())
    | Const_pointer c -> Fconst(Fconst_pointer c, nid ())
    | Const_block (tag,l) ->
      Fprim(Pmakeblock(tag, Asttypes.Immutable),
          List.map aux l, Debuginfo.none, nid ())
    | Const_float_array c -> Fconst(Fconst_float_array c, nid ())
    | Const_immstring c -> Fconst(Fconst_immstring c, nid ())
  in
  aux cst


let intro size lam =
  let flam = close IdentMap.empty lam in
  flam
