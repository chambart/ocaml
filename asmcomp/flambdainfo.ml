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

let global_size t =
  let r = ref (-1) in
  let aux = function
    | Fprim(Psetglobalfield n, _, _, _) ->
      r := max !r n;
    | _ -> ()
  in
  Flambdautils.iter_flambda aux t;
  !r + 1

let offset off_id = {off_id; off_unit = Compilenv.current_unit_id ()}

let to_offset_map idmap =
  IdentMap.fold (fun id v map -> OffsetMap.add (offset id) v map)
    idmap OffsetMap.empty

type code_informations = {
  functions : ExprId.t ffunctions FunMap.t;
  global_size : int;
}

let prepare_informations t =
  { functions = Flambdautils.list_functions t;
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

  (* external symbols associated to a value *)
  symbols : Symbol.t ValMap.t;

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
  let symbols : ValId.t SymbolTbl.t = SymbolTbl.create 10
  let back_symbols : Symbol.t ValTbl.t = ValTbl.create 10
  let imported : ValId.t Flambdaexport.EidTbl.t = Flambdaexport.EidTbl.create 10

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

  let expr_vid' exp =
    let eid = data exp in
    try Some (ExprTbl.find expr_val eid) with
    | Not_found -> None

  let expr_vid exp =
    let eid = data exp in
    try ExprTbl.find expr_val eid with
    | Not_found ->
      let desc = string_desc exp in
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

  let rec import_export_id exid =
    let open Flambdaexport in
    let desc exid =
      try EidMap.find exid (Compilenv.approx_env ()).ex_values
      with Not_found -> Format.printf "%a@." ExportId.print exid;
        raise Not_found in
    try EidTbl.find imported exid with
    | Not_found ->
      (* Format.printf "import %a@." ExportId.print exid; *)
      match desc exid with
      | Value_symbol sym ->
        let _ = Compilenv.global_approx_info (fst sym) in
        (* Format.printf "val sym %s@." (snd sym); *)
        import_sym sym
      | _ ->
        let vid = ValId.create ~name:"imported" () in
        EidTbl.add imported exid vid;
        begin
          try
            let sym = EidMap.find exid (Compilenv.approx_env ()).ex_id_symbol in
            SymbolTbl.add symbols sym vid;
            ValTbl.add back_symbols vid sym
          with Not_found -> ()
        end;
        let value = match desc exid with
          | Value_predef_exn _ -> Values.unknown_value
          | Value_int i -> Values.value_int i
          | Value_constptr i -> Values.value_constptr i
          | Value_block (tag, fields) ->
            let fields = Array.map import_value fields in
            Values.value_block tag fields
          | Value_closure { fun_id; closure } ->
            let bound_var = OffsetMap.map import_value closure.bound_var in
            let clos = value_unoffseted_closure closure.closure_id bound_var in
            set_closure_funid clos fun_id
          | Value_symbol sym -> assert false
        in
        ValTbl.replace val_tbl vid value;
        vid
  and import_value v =
    let open Flambdaexport in
    match v with
    | Value_unknown ->
      let vid = ValId.create ~name:"unknown" () in
      ValTbl.replace val_tbl vid unknown_value;
      ValSet.singleton vid
    | Value_id exid ->
      ValSet.singleton (import_export_id exid)
  and import_sym sym =
    (* Format.printf "import sym %s@." (snd sym); *)
    let symbol_map = Compilenv.symbol_map () in
    let exid = SymbolMap.find sym symbol_map in
    import_export_id exid

  let import_symbol sym =
    try SymbolTbl.find symbols sym with
    | Not_found ->
      let (modul,name) = sym in
      (* Format.printf "import %s@." name; *)
      let _ = Compilenv.global_approx_info modul in
      let symbol_map = Compilenv.symbol_map () in
      let exid = SymbolMap.find sym symbol_map in
      let vid = import_export_id exid in
      vid

  let find_global id =
    let global_approx id =
      if Ident.is_predef_exn id
      then New unknown_value
      else
        let info = Compilenv.global_approx_info id in
        (* Printf.printf "import %s\n%!" (Ident.name id); *)
        let approx = IdentMap.find id info.Flambdaexport.ex_globals in
        Old (import_value approx)
    in

    if id.Ident.name = Compilenv.current_unit_name ()
    then Old (ValSet.singleton global_val)
    else global_approx id

  let rec aux ?set_id exp =
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
        let vid = match set_id with
          | None -> expr_vid exp
          | Some id ->
            match expr_vid' exp with
            | Some id -> id
            | None -> id
        in
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
    | Fsymbol (sym, _) ->
      Old (ValSet.singleton (import_symbol sym))

    | Fvar (id,_) ->
      (try Old (IdentTbl.find bindings id) with
       | Not_found ->
         (* Printf.printf "unknown %s\n%!" (Ident.unique_name id); *)
         New unknown_value)
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

      (* Hack to have some recursive informations with a single loop. *)
      let compare_closure_first f1 f2 = match f1, f2 with
        | Fclosure _, Fclosure _ -> 0
        | Fclosure _, _ -> -1
        | _, Fclosure _ -> 1
        | Foffset _, Foffset _ -> 0
        | Foffset _, _ -> -1
        | _, Foffset _ -> 1
        | _, _ -> 0
      in
      let defs = List.sort (fun (_,lam1) (_,lam2) ->
          compare_closure_first lam1 lam2) defs in
      let defs = List.map (fun (id,lam) ->
          let vid = expr_vid lam in
          (* Printf.printf "loopid %s %s\n%!" *)
          (*   (Ident.unique_name id) *)
          (*   (ValId.to_string vid); *)
          bind id (ValSet.singleton vid);
          id, lam, vid) defs in
      List.iter (fun (id,lam,set_id) ->
          aux ~set_id lam;
          (* Printf.printf "returned %s %a\n%!" *)
          (*   (Ident.unique_name id) *)
          (*   ValSet.output (mu lam); *)
          bind id (mu lam)) defs;

      (* List.iter (fun (id,lam) -> *)
      (*     aux lam; *)
      (*     bind id (mu lam)) defs; *)

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

    | Fprim(Pgetglobalfield(id,n), arg, _, _) ->
      assert(arg = []);
      if id.Ident.name = Compilenv.current_unit_name ()
      then NewOld(Values.field n (value global_val))
      else New unknown_value

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
      assert(List.length args = 2);
      New value_unit

    | Fprim(Psetglobalfield n, args, _, _) ->
      List.iter (aux) args;
      (match args with
       | [arg] ->
         set_global (set_field n (value global_val) (mu arg));
         New value_unit
       | _ -> assert false)

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
      let fv' = to_offset_map (IdentMap.map mu fv) in
      Queue.push (functions.ident,fv') closures;
      New (value_unoffseted_closure functions.ident fv')

    | Fapply ( func, args, _, dbg, eid) ->
      aux func;
      List.iter aux args;
      New unknown_value

    | Foffset(lam, id, eid) ->
      aux lam;
      (* WARNING: This is the only place where an id can be bound to
         the closure (there are none for recursive calls). Fix this by
         adding that declaration at closure declaration time. And
         after that forbid having the same identifier for closure
         inside its function body and outside. *)
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

    OffsetMap.iter (fun {off_id} v -> bind off_id v) free_vars;

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
      symbols = ValTbl.to_map back_symbols;

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

