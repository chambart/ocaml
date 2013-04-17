open Lambda
open Flambda
open Data_dependency


let basic_graph tree =

  let graph = empty_graph () in
  let bindings = IdentTbl.create 10 in
  let static_fail_bindings = IntTbl.create 10 in

  let global_bindings = IntTbl.create 10 in

  let unit = Const (Fconst_pointer 0) in

  let global_value = make_value ~name:"global" graph [] in

  let todo_functions = Queue.create () in

  let add_functions ffunctions =
    IdentMap.iter (fun id ffunc ->
      IdentTbl.add graph.functions id ffunc;
      Queue.push (id,ffunc) todo_functions)
      ffunctions.funs in

  let union l =
    Union (List.fold_left (fun l -> function
        | None -> l
        | Some v -> v :: l) [] l)
  in

  let rec aux constr_stack exp =
    let aux1 exp = aux constr_stack exp in
    let aux_ignore exp = let (_:v option) = aux1 exp in () in
    let aux' exp = match aux1 exp with
      | None -> failwith "expected data"
      | Some v -> v in
    let new_expr' ?new_value term =
      let eid = Flambda.data exp in
      new_expr ?new_value ~location:([],eid) graph constr_stack term in
    let new_expr ?new_value term = Some (new_expr' ?new_value term)
    in match exp with
    | Fconst (cst,_) ->
      new_expr (Const cst)

    | Flet (str, id, lam, body, _) ->
      assert(str = Strict);
      let v = aux' lam in
      IdentTbl.add bindings id v;
      aux1 body

    | Fletrec (defs, body, _) ->
      let l = List.map (fun (id,lam) ->
          let v = make_value ~name:"letrec_union" graph constr_stack in
          IdentTbl.add bindings id v;
          ignore_need graph v;
          lam,v) defs in
      List.iter (fun (lam,new_value) ->
        let v' = aux' lam in
        ignore(new_expr ~new_value (Union [v']):v option)) l;
      aux1 body

    | Fvar (id, _) ->
      Some (IdentTbl.find bindings id)

    | Fsequence (lam1, lam2, _) ->
      aux_ignore lam1;
      aux1 lam2

    | Fprim( Psequand, [arg1;arg2], _, _) ->
      let v1 = aux' arg1 in
      let v2 = aux' arg2 in
      new_expr (Sequand (v1,v2))

    | Fprim( Psequor, [arg1;arg2], _, _) ->
      let v1 = aux' arg1 in
      let v2 = aux' arg2 in
      new_expr (Sequor (v1,v2))

    | Fprim( Pnot, [arg], _, _) ->
      let v = aux' arg in
      new_expr (Not v)

    | Fprim(( Paddint | Psubint | Pmulint | Pdivint | Pmodint | Pandint
            | Porint  | Pxorint | Plslint | Plsrint | Pasrint ) as op,
          [arg1;arg2], _, _) ->
      let v1 = aux' arg1 in
      let v2 = aux' arg2 in
      let op' = match op with
        | Paddint -> Addint
        | Psubint -> Subint
        | Pmulint -> Mulint
        | Pdivint -> Divint
        | Pmodint -> Modint
        | Pandint -> Andint
        | Porint -> Orint
        | Pxorint -> Xorint
        | Plslint -> Lslint
        | Plsrint -> Lsrint
        | Pasrint -> Asrint
        | _ -> assert false
      in
      new_expr (Intbinop(op',v1,v2))

    | Fprim(Pintcomp cmp, [arg1;arg2], _, _) ->
      let v1 = aux' arg1 in
      let v2 = aux' arg2 in
      new_expr (Cmpint (cmp,v1,v2))

    | Fprim(Parraylength _, [arg], _, _) ->
      let v = aux' arg in
      new_expr (Arraylength v)

    | Fprim(Pmakearray kind, args, _, _) ->
      let vals = List.map aux' args in
      let tag = match kind with
        | Pgenarray | Paddrarray | Pintarray -> 0
        | Pfloatarray -> Obj.double_array_tag
      in
      new_expr (Makeblock (tag,Asttypes.Mutable,vals))

    | Fprim(Pmakeblock (tag,mut), args, _, _) ->
      let vals = List.map aux' args in
      new_expr (Makeblock (tag,mut,vals))

    | Fprim(Pfield i, [arg], _, _) ->
      let v = aux' arg in
      new_expr (Field (i,v))

    | Fprim(Psetfield(n, _), [Fprim(Pgetglobal id, [], _, _); lam], _, _) ->
      let v = aux' lam in
      IntTbl.add global_bindings n v;
      new_expr unit

    | Fprim(Pgetglobal id, [], _, _) ->
      if id.Ident.name = Compilenv.current_unit_name ()
      then (* current module *)
        Some (global_value)
      else
        failwith "TODO: global not current"
    (*   if Ident.is_predef_exn id *)
    (*   then trivial_expr traces (B.Predef_exn id) bindings *)
    (*   else failwith "TODO" (\* other module *\) *)

    | Fprim( Psetfield(n,_), [arg1;arg2], _, _) ->
      let v1 = aux' arg1 in
      let v2 = aux' arg2 in
      new_expr (Setfield (n,v1,v2))

    | Fprim(Pccall prim_desc, args, _, _) ->
      let vl = List.map aux' args in
      new_expr (Unknown_primitive (prim_desc.Primitive.prim_name, vl))

    | Fclosure(funct, fv, _) ->
      let fv' = IdentMap.mapi (fun id lam ->
          let v = aux' lam in
          IdentTbl.add bindings id v;
          v) fv in
      add_functions funct;
      let funs = IdentMap.fold (fun k _ set -> IdentSet.add k set)
          funct.funs IdentSet.empty in
      new_expr (Closure (funs, fv'))

    | Foffset(lam, id, _) ->
      let v = aux' lam in
      new_expr (Offset (id, v))

    | Fifthenelse(cond, ifso, ifnot, _) ->
      let v_cond = aux' cond in
      let stack b = (v_cond, Bool b) :: constr_stack in
      let v_ifso = aux (stack true) ifso in
      let v_ifnot = aux (stack false) ifnot in
      new_expr (union [v_ifso;v_ifnot])

    | Fswitch(cond, sw, _) ->
      let v_cond = aux' cond in
      let cst_cases =
        List.map (fun (tag,lam) ->
          let stack = (v_cond, SwitchCst tag) :: constr_stack in
          aux stack lam) sw.fs_consts in
      let block_cases =
        List.map (fun (tag,lam) ->
          let stack = (v_cond, SwitchTag tag) :: constr_stack in
          aux stack lam) sw.fs_blocks in
      new_expr (union (cst_cases @ block_cases))

    | Fapply(func, params, _, _, _) ->
      let v_func = aux' func in
      let v_params = List.map aux' params in
      new_expr (Apply (v_func, v_params))

    | Fstaticfail(n, args, _) ->
      let vl = List.map aux' args in
      let l = try IntTbl.find static_fail_bindings n with
        | Not_found -> [] in
      IntTbl.add static_fail_bindings n (vl::l);
      None

    | Fcatch(n, ids, body, handler, _) ->
      let previous_bindings = try Some (IntTbl.find static_fail_bindings n) with
        | Not_found -> None in
      IntTbl.remove static_fail_bindings n;
      let v_body = aux1 body in
      let body_bindings = try IntTbl.find static_fail_bindings n with
        | Not_found -> [] in
      let ret = match body_bindings with
        | [] -> v_body (* this should probably never happen *)
        | t::q ->
          let body_bindings = List.fold_left
              (fun acc l -> List.map2 (fun acc v -> v :: acc) acc l)
              (List.map (fun _ -> []) t) body_bindings in
          List.iter2 (fun id vals ->
            IdentTbl.add bindings id
              (new_expr' (union (List.map (fun v -> Some v) vals))))
            ids body_bindings;
          let v_handler = aux1 handler in
          new_expr (union [v_body; v_handler])
      in
      (match previous_bindings with
       | None -> IntTbl.remove static_fail_bindings n
       | Some previous_bindings ->
         IntTbl.replace static_fail_bindings n previous_bindings);
      ret

    | e ->
      let desc = string_desc e in
      failwith (Printf.sprintf "TODO graph: %s" desc)
  in

  let _ = aux [] tree in

  let global_value_term =
    let max_global = IntTbl.fold (fun i _ m -> max i m)
        global_bindings (-1) in
    let l = List.map (fun i -> IntTbl.find global_bindings i)
        (0 --> max_global) in
    Makeblock_module l in

  associate_term graph global_value global_value_term;

  let aux_fun fun_id func =
    let call_info = new_function graph fun_id func.arity in
    List.iter2 (fun id v -> IdentTbl.add bindings id v) func.params
      call_info.parameters;
    match aux [] func.body with
    | None -> ()
    | Some fun_ret ->
      ignore (add_virtual_union graph fun_ret call_info.return:bool);
      ()
  in

  while not (Queue.is_empty todo_functions) do
    let (fun_id, func) = Queue.pop todo_functions in
    aux_fun fun_id func
  done;

  global_value, graph
