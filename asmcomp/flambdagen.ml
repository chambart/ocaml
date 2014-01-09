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

open Misc
open Ext_types
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
      | Fprim(Praise k, args, dinfo, v) ->
        Fprim(Praise k, args, Debuginfo.from_call ev, v)
      | Fsend(kind, f1, f2, args, dinfo, v) ->
        Fsend(kind, f1, f2, args, Debuginfo.from_call ev, v)
      | Fsequence(f1, f2, v) ->
        Fsequence(f1, add_debug_info ev f2, v)
      | _ -> f
    end
  | _ -> f

let rec subst sb id =
  try subst sb (IdentMap.find id sb)
  with Not_found -> id

let add_subst sb ~replace ~by =
  IdentMap.add replace by sb

let nid = ExprId.create

let foffset (lam, off_id, v) =
  Foffset(lam, { off_id; off_unit = Compilenv.current_unit ()}, None, v)

let rec close sb = function
    Lvar id ->
    Fvar (subst sb id,
        nid ~name:(Printf.sprintf "var_%s" (Ident.unique_name id)) ())
  | Lconst cst -> close_const sb cst
  | Llet(str, id, lam, body) ->
    let str = match str with
      | Variable -> Variable
      | _ -> Strict in
    Flet(str, id, close_named id sb lam, close sb body, nid ~name:"let" ())
  | Lfunction(kind, params, body) as funct ->
    let id = Ident.create "fun" in
    let id' = Ident.rename id in
    foffset(close_functions sb [id,id',funct],id', nid ~name:"offset" ())
  | Lapply(funct, args, loc) ->
    Fapply(close sb funct, close_list sb args, None, Debuginfo.none,
        nid ~name:"apply" ())
  | Lletrec(defs, body) ->
    if List.for_all
        (function (id, Lfunction(_, _, _)) -> true | _ -> false)
        defs
    then
      (* When all the binding are functions, we build a single closure
         for all the functions *)
      let defs = List.map (fun (id,lam) -> id, Ident.rename id, lam) defs in
      let clos = close_functions sb defs in
      let clos_ident = Ident.create "clos" in
      let body = List.fold_left (fun body (id,id',_) ->
          Flet(Strict, id, foffset(Fvar (clos_ident, nid ()), id', nid ()),
               body, nid ()))
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
  | Lprim(Praise k, [Levent(arg, ev)]) ->
    Fprim(Praise k, [close sb arg], Debuginfo.from_raise ev, nid ())
  | Lprim(Pfield i, [Lprim(Pgetglobal id, [])])
    when id.Ident.name = Compilenv.current_unit_name () ->
    Fprim(Pgetglobalfield(id,i), [], Debuginfo.none,
          nid ~name:"getglobalfield" ())
  | Lprim(Psetfield(i,_), [Lprim(Pgetglobal id, []); lam]) ->
    assert(id.Ident.name = Compilenv.current_unit_name ());
    Fprim(Psetglobalfield i, [close sb lam], Debuginfo.none,
          nid ~name:"setglobalfield" ())
  | Lprim(Pgetglobal id, [])
    when not (Ident.is_predef_exn id) &&
         id.Ident.name <> Compilenv.current_unit_name () ->
    let symbol = id, Compilenv.symbol_for_global id in
    Fsymbol (symbol,nid ~name:"external_global" ())
  | Lprim(p, args) ->
    Fprim(p, close_list sb args, Debuginfo.none,
        nid ~name:"prim" ())
  | Lswitch(arg, sw) ->
    let aux (i,lam) = i, close sb lam in
    let rec set n = if n < 0 then IntSet.empty
      else IntSet.add n (set (n-1)) in
    Fswitch(close sb arg,
      { fs_numconsts = set (sw.sw_numconsts - 1);
        fs_consts = List.map aux sw.sw_consts;
        fs_numblocks = set (sw.sw_numblocks - 1);
        fs_blocks = List.map aux sw.sw_blocks;
        fs_failaction = may_map (close sb) sw.sw_failaction },
      nid ~name:"switch" ())
  | Lstaticraise (i, args) ->
    Fstaticfail (i, close_list sb args, nid ())
  | Lstaticcatch(body, (i, vars), handler) ->
    Fcatch (i, vars, close sb body, close sb handler, nid ())
  | Ltrywith(body, id, handler) ->
    Ftrywith(close sb body, id, close sb handler, nid ())
  | Lifthenelse(arg, ifso, ifnot) ->
    Fifthenelse(close sb arg, close sb ifso, close sb ifnot,
        nid ~name:"if" ())
  | Lsequence(lam1, lam2) ->
    Fsequence(close sb lam1, close sb lam2,
        nid ~name:"seq" ())
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

and close_functions (sb:Ident.t IdentMap.t)
    (fun_defs:(Ident.t * (* function id used externaly *)
               Ident.t * (* function id to be used internaly *)
               lambda) list) =
  let fv = List.fold_left
      (fun set (_,_,body) -> IdentSet.union (free_variables body) set)
      IdentSet.empty fun_defs in
  let fun_ids = List.fold_left (fun set (id,_,_) -> IdentSet.add id set)
      IdentSet.empty fun_defs in
  let recursives = ref false in
  (* clos_var contains all free variables that are not functions
     declared in this closure *)
  let sb,clos_var = List.fold_left (fun (sb,clos_var) id ->
      let id = subst sb id in
      if IdentSet.mem id fun_ids
      then (recursives := true;
        (sb,clos_var))
      else
        let id' = Ident.rename id in
        (add_subst sb ~replace:id ~by:id',
         IdentMap.add id' (Fvar (id, nid ())) clos_var))
      (sb,IdentMap.empty)
      (IdentSet.elements fv) in
  (* Rename function variables according to the substitution
     provided (in fun_defs) *)
  let sb = List.fold_left
      (fun sb (id,id',_) -> add_subst sb ~replace:id ~by:id')
      sb fun_defs in
  let closure_params =
    IdentMap.fold (fun id _ set -> IdentSet.add id set)
      clos_var IdentSet.empty in
  let unit = Compilenv.current_unit () in
  let close_one map = function
    | (id, Lfunction(kind, params, body)) ->
      let dbg = match body with
        | Levent (_,({lev_kind=Lev_function} as ev)) ->
          Debuginfo.from_call ev
        | _ -> Debuginfo.none in
      begin match kind with
      | Curried ->
          IdentMap.add id
            { label = make_function_lbl id;
              stub = false;
              arity = List.length params;
              params;
              closure_params;
              kept_params = IdentSet.empty;
              body = close sb body;
              dbg } map
      | Tupled ->
          let fun_var = Ident.rename id in
          let fun_off = { off_unit = unit; off_id = fun_var } in
          let tupled_stub = tupled_function_call_stub id params closure_params fun_off in
          let map = IdentMap.add id tupled_stub map in
          IdentMap.add fun_var
            { label = make_function_lbl fun_var;
              stub = false;
              arity = List.length params;
              params;
              closure_params;
              kept_params = IdentSet.empty;
              body = close sb body;
              dbg } map
      end
    | (_, _) -> fatal_error "Flambdagen.close_functions"
  in
  let functions = List.fold_left (fun map (_, id', f) -> close_one map (id',f))
      IdentMap.empty fun_defs in
  let ident = FunId.create (Compilenv.current_unit_name ()) in
  Fclosure ({ ident; funs = functions; recursives = !recursives;
              closed = false;
              unit },
    clos_var, nid ())

and tupled_function_call_stub id params closure_params fun_off =
  let tuple_param = Ident.create "tupled_stub_param" in
  let params' = List.map (fun p -> Ident.rename p) params in
  let call_params = List.map (fun p' -> Fvar(p',nid ())) params' in
  let call = Fapply(Fvar(fun_off.off_id,nid ()), call_params, Some fun_off,
                    Debuginfo.none, nid ()) in
  let _, body =
    List.fold_left (fun (pos,body) p' ->
        let lam = Fprim(Pfield pos, [Fvar(tuple_param, nid ())],
                        Debuginfo.none, nid ()) in
        pos+1,
        Flet(Strict,p',lam,body,nid ()))
      (0,call) params' in
  { label = make_function_lbl id;
    stub = true;
    arity = 1;
    params = [tuple_param];
    closure_params;
    kept_params = IdentSet.empty;
    body;
    dbg = Debuginfo.none }

and close_list sb l = List.map (close sb) l

and close_named id sb = function
  | Lfunction(kind, params, body) as funct ->
    let id' = Ident.rename id in
    foffset(close_functions sb [id,id',funct],id', nid ())
  | lam ->
    close sb lam

and close_const sb cst =
  let rec aux = function
    | Const_base c -> Fconst(Fconst_base c, nid ~name:"cst" ())
    | Const_pointer c -> Fconst(Fconst_pointer c, nid ~name:"cstptr" ())
    | Const_block (tag,l) ->
      Fprim(Pmakeblock(tag, Asttypes.Immutable),
          List.map aux l, Debuginfo.none, nid ~name:"cstblock" ())
    | Const_float_array c -> Fconst(Fconst_float_array c, nid ~name:"float" ())
    | Const_immstring c -> Fconst(Fconst_immstring c, nid ~name:"immstring" ())
  in
  aux cst


let intro size lam =
  let flam = close IdentMap.empty lam in
  flam
