open Misc
open Asttypes
open Lambda
open Flambda
open Values

let splitn n l =
  let rec aux n l acc =
    if n = 0
    then List.rev acc, l
    else match l with
      | [] -> assert false
      | t::q -> aux (n-1) q (t::acc) in
  aux n l []

let list_functions t =
  let r = ref FunMap.empty in
  let rec aux = function
    | Fclosure (funcs,_,_) ->
      r := FunMap.add funcs.ident funcs !r;
      IdentMap.iter (fun _ ffunc -> iter_tree ffunc.body) funcs.funs
    | _ -> ()
  and iter_tree t =
    Flambdautils.iter_flambda aux t
  in
  iter_tree t;
  !r

let global_size t =
  let r = ref (-1) in
  let globals = ref IdentSet.empty in
  let aux = function
    | Flet(_,id,Fprim(Pgetglobal gid, arg, _, _), _, _) ->
      globals := IdentSet.add id !globals
    | Fprim(Psetfield(n, _), [Fvar(id,_); lam], _, _) ->
      if IdentSet.mem id !globals
      then r := max !r n
    | Fprim(Psetfield(n, _), [Fprim(Pgetglobal id, arg, _, _); lam], _, _) ->
      r := max !r n;
    | _ -> ()
  in
  Flambdautils.iter_flambda aux t;
  !r + 1

type code_informations = {
  functions : ExprId.t ffunctions FunMap.t;
  global_size : int;
}

let prepare_informations t =
  { functions = list_functions t;
    global_size = global_size t }

type ret =
  | New of values
  | Old of ValSet.t
  | NewOld of (values option * ValSet.t)

type analysis_result = {
  info : code_informations;
  (* associate each binding with its potential values *)
  bindings : ValSet.t IdentMap.t;

  (* associate each value identifier to its contents *)
  values : Values.values ValMap.t;

  (* set of variables potentialy returned by expressions *)
  expr : ValSet.t ExprMap.t;

  (* when an expression can produce a value it is here *)
  expr_val : ValId.t ExprMap.t;

  (* values created for identifiers *)
  var_val : ValId.t IdentMap.t;

  (* values potentially returned by static fails *)
  staticfails : ValSet.t list IntMap.t;

  (* the value representing the global module *)
  global_val : ValId.t;
  (* a special value representing values comming from outside the
     analysis *)
  external_val : ValId.t;
}

module type Fparam = sig
  val tree : ExprId.t Flambda.flambda
end

module Run(Param:Fparam) = struct

  let global_val = ValId.create ()
  let external_val = ValId.create ()
  let external_eid = ExprId.create ()

  let info = prepare_informations Param.tree

  let bindings : ValSet.t IdentTbl.t = IdentTbl.create 1000

  let val_tbl : values ValTbl.t = ValTbl.create 1000

  let expr : ValSet.t ExprTbl.t = ExprTbl.create 1000
  (* set of values potentially returned by an expression *)

  let expr_val : ValId.t ExprTbl.t = ExprTbl.create 1000
  (* value constructed by an expression *)

  let var_val : ValId.t IdentTbl.t = IdentTbl.create 1000
  (* value constructed by an id *)

  let staticfails : ValSet.t list IntTbl.t = IntTbl.create 20

  let fun_call_count : int IdentTbl.t = IdentTbl.create 100

  let closures = Queue.create ()

  let value v =
    try ValTbl.find val_tbl v with
    | Not_found -> empty_value

  let set_global v =
    ValTbl.replace val_tbl global_val v

  let () =
    set_global (empty_block 0 info.global_size);
    ValTbl.replace val_tbl external_val unknown_value

  let mu exp =
    let eid = data exp in
    try ExprTbl.find expr eid with
    | Not_found ->
      fatal_error (Printf.sprintf "return value not available for expr %s"
            (ExprId.to_string eid))

  let set_union set =
    let l = ValSet.elements set in
    let vl = List.map value l in
    Values.list_union vl

  let val_union exp =
    let ids = mu exp in
    let l = ValSet.elements ids in
    let vl = List.map value l in
    Values.list_union vl

  (* create a value associated to a variable:
     'for' 'try' 'closure' variables *)
  let var_vid id =
    let desc = Ident.name id in
    try IdentTbl.find var_val id with
    | Not_found ->
      let vid = ValId.create ~name:desc () in
      IdentTbl.add var_val id vid;
      (* set an unknown value
         TODO use type to refine it *)
      ValTbl.replace val_tbl vid unknown_value;
      vid

  let expr_vid exp =
    let eid = data exp in
    let desc = string_desc exp in
    try ExprTbl.find expr_val eid with
    | Not_found ->
      let vid = ValId.create ~name:desc () in
      ExprTbl.add expr_val eid vid;
      vid

  let find_var id =
    try IdentTbl.find bindings id with
    | Not_found -> ValSet.empty

  let bind id values =
    let vids = find_var id in
    if not (ValSet.subset values vids)
    then IdentTbl.replace bindings id (ValSet.union vids values)

  let find_staticfail i =
    try Some (IntTbl.find staticfails i) with
    | Not_found -> None

  let bind_staticfail i values_list =
    let l = match find_staticfail i with
      | None -> values_list
      | Some l ->
        assert(List.length values_list = List.length l);
        List.map2 ValSet.union l values_list in
    IntTbl.replace staticfails i l

  let find_global id =
    (* TODO: remove *)
    let global_approx id =
      if Ident.is_predef_exn id
      then New unknown_value
      else
        ((* ignore (Compilenv.global_approx id); *)
         New unknown_value) in

    if id.Ident.name = Compilenv.current_unit_name ()
    then Old (ValSet.singleton global_val)
    else global_approx id
  (* TODO change:
     Compilenv.global_approx id *)

  let rec aux exp =
    let eid = data exp in

    let vids =
      let insert vid values =
        let v = value vid in
        ValTbl.replace val_tbl vid (Values.union v values);
      in
      match aux' exp with
      | NewOld (None, val_ids)
      | Old val_ids -> val_ids
      | New values ->
        let vid = expr_vid exp in
        insert vid values;
        ValSet.singleton vid
      | NewOld (Some values, val_ids) ->
        let vid = expr_vid exp in
        insert vid values;
        ValSet.add vid val_ids
    in

    try
      let old_vids = ExprTbl.find expr eid in
      if not (ValSet.subset vids old_vids)
      then (ExprTbl.replace expr eid (ValSet.union old_vids vids))
    with
    | Not_found -> ExprTbl.replace expr eid vids

  and aux' = function
    | Fvar (id,_) ->
      (try Old (IdentTbl.find bindings id) with
       | Not_found -> New unknown_value)
        (* TODO: forbid Not_found case *)

    | Fconst (cst,_) -> New (Values.const cst)

    | Flet(str, id, lam, body, _) ->
      aux lam;
      (match str with
       | Variable ->
         (* BOF *)
         bind id (ValSet.singleton external_val)
       | Strict | Alias | StrictOpt ->
         bind id (mu lam));
      aux body;
      Old (mu body)

    | Fletrec(defs, body, _) ->
      List.iter (fun (id,lam) ->
          aux lam;
          bind id (mu lam)) defs;
      aux body;
      Old (mu body)

    | Fprim(Pmakeblock(tag, mut), lams, _, _) ->
      List.iter (aux) lams;
      (match mut with
        | Mutable ->
          New value_mutable
        | Immutable ->
          let vlam = List.map mu lams in
          New (value_block tag (Array.of_list vlam)))
    | Fprim(Pfield n, args, _, _) ->
      (match args with
       | [] | _ :: _ :: _ -> assert false
       | [lam] ->
         aux lam;
         NewOld(Values.field n (val_union lam)))

    | Fprim(Pgetglobal id, arg, _, _) ->
      assert(arg = []);
      find_global id


    | Fprim(Psetfield(n, _), args, _, _) ->
      List.iter (aux) args;
      (match args with
       | [] | [_] | _::_::_::_ -> assert false
       | [a;arg] ->
         if ValSet.equal (mu a) (ValSet.singleton global_val)
         then begin
           set_global (set_field n (value global_val) (mu arg))
         end;
         New value_unit)

    | Fprim(Pmakearray kind, args, _, _) ->
      List.iter (aux) args;
      (match kind with
       | Pgenarray | Paddrarray | Pintarray ->
         New value_mutable
       | Pfloatarray ->
         New value_floatarray)

    | Fprim(Pccall desc, args, _, _) ->
      List.iter (aux) args;
      if desc.Primitive.prim_native_float
      then New value_float
      else New unknown_value

    | Fprim(Praise, args, _, _) ->
      List.iter (aux) args;
      New value_bottom

    | Fprim(Pduprecord _, args, _, _) ->
      List.iter (aux) args;
      New value_mutable

    | Fprim(p, args, _, eid) ->
      List.iter (aux) args;
      New (Values.prim p (List.map val_union args))

    | Fsequence(lam1, lam2, _) ->
      aux lam1;
      aux lam2;
      Old (mu lam2)

    | Fifthenelse(arg, ifso, ifnot, _) ->
      aux arg;
      let varg = val_union arg in
      begin match if_value varg with
        | True ->
          aux ifso;
          Old (mu ifso)
        | False ->
          aux ifnot;
          Old (mu ifnot)
        | TrueAndFalse ->
          aux ifso;
          aux ifnot;
          Old (ValSet.union (mu ifso) (mu ifnot))
        | Neither ->
          Old ValSet.empty
      end
    | Fassign(id, lam, _) ->
      aux lam;
      New value_unit

    | Fclosure (functions,fv,eid) ->
      IdentMap.iter (fun id lam ->
          aux lam;
          bind id (mu lam)) fv;
      let fv' = IdentMap.map mu fv in
      Queue.push (functions.ident,fv') closures;
      New (value_unoffseted_closure functions.ident fv')

    | Fapply ( func, args, _, dbg, eid) ->
      aux func;
      List.iter aux args;
      New unknown_value

    | Foffset(lam, id, eid) ->
      aux lam;
      let c = Values.set_closure_funid (val_union lam) id in
      (* Queue.push c closures; *)
      New (c)

    | Fenv_field({env; env_var}, _) ->
      aux env;
      NewOld(env_field env_var (val_union env))

    | Fswitch(arg, sw, _) ->
      aux arg;
      (* TODO: comme pour if *)
      let branches =
        let l = List.map snd sw.fs_consts
          @ List.map snd sw.fs_blocks in
        match sw.fs_failaction with
        | None -> l
        | Some b -> b :: l in
      List.iter (aux) branches;
      let l = List.fold_left ValSet.union
          ValSet.empty (List.map mu branches) in
      Old l

    | Fwhile (cond, body, _) ->
      aux cond;
      aux body;
      New value_unit

    | Ffor(id, lo, hi, dir, body, _) ->
      aux lo;
      aux hi;
      let vid = var_vid id in
      bind id (ValSet.singleton vid);
      aux body;
      New value_unit

    | Fstaticfail(i, args, _) ->
      List.iter (aux) args;
      bind_staticfail i (List.map mu args);
      New value_bottom

    | Fcatch(i, ids, body, handler, _) ->
      aux body;
      begin match find_staticfail i with
        | None -> Old (mu body)
        | Some l ->
          List.iter2 bind ids l;
          aux handler;
          Old (ValSet.union (mu body) (mu handler))
      end

    | Ftrywith(body, id, handler, _) ->
      aux body;
      let vid = var_vid id in
      bind id (ValSet.singleton vid);
      aux handler;
      Old (ValSet.union (mu body) (mu handler))

    | Fsend (kind ,meth ,obj ,args , dbg, _) ->
      let exprs = meth :: obj :: args in
      List.iter (aux) exprs;
      New (unknown_value)

    | Funreachable pid ->
      New (empty_value)

  and aux_apply1' (clos_id, free_vars) =

    let ffunctions =
      try FunMap.find clos_id info.functions
      with
      | Not_found as e ->
        Printf.printf "not_found %a\n%!"
          FunId.output clos_id;
        raise e
    in
    IdentMap.iter (aux_apply' free_vars) ffunctions.funs

  and aux_apply' free_vars fun_id ffunction =

    List.iter (fun id ->
      let vid = var_vid id in
      bind id (ValSet.singleton vid))
      ffunction.params;

    IdentMap.iter bind free_vars;

    aux_count ffunction.body fun_id

  and aux_count lam fun_id =
    let count = try IdentTbl.find fun_call_count fun_id with Not_found -> 0 in
    if count < 1
    then begin
      IdentTbl.replace fun_call_count fun_id (count+1);
      aux lam
    end

  let result () =
    { info = info;
      bindings = IdentTbl.to_map bindings;
      values = ValTbl.to_map val_tbl;
      expr = ExprTbl.to_map expr;
      expr_val = ExprTbl.to_map expr_val;
      var_val = IdentTbl.to_map var_val;
      staticfails = IntTbl.to_map staticfails;

      global_val;
      external_val;
    }

  let rec loop_closures () =
    if not (Queue.is_empty closures)
    then begin
      let c = Queue.pop closures in
      aux_apply1' c;
      loop_closures ()
    end

  let res =
    aux Param.tree;
    loop_closures ();
    result ()

end

let analyse tree =
  let module A = struct
    let tree = tree
  end in
  let module R = Run(A) in
  R.res
