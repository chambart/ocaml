open Lambda
open Flambda
open Data_dependency

exception Expecting_data

let basic_graph tree =

  let graph = empty_graph () in
  let bindings = IdentTbl.create 10 in
  let static_fail_bindings = IntTbl.create 10 in

  let global_bindings = IntTbl.create 10 in

  let predef_exception_bindings = IdentTbl.create 5 in

  let unit = Const (Fconst_pointer 0) in

  let global_value = make_value ~name:"global" graph graph.toplevel_block in

  let todo_functions = Queue.create () in

  let external_global_table = IdentTbl.create 10 in

  let get_external_global id =
    try IdentTbl.find external_global_table id with
    | Not_found ->
      let v = match Compilenv.global_ai id with
        | None -> new_expr graph graph.toplevel_block Unknown
        | Some (global_id,_) -> global_id
      in
      IdentTbl.add external_global_table id v;
      v
  in

  let predef_exn id =
    try IdentTbl.find predef_exception_bindings id with
    | Not_found ->
      let name = Printf.sprintf "predef_exn_%s" (Ident.name id) in
      let v1 = new_expr ~name graph graph.toplevel_block (Predef_exn id) in
      let v2 = new_expr graph graph.toplevel_block
          (Makeblock (0,Asttypes.Immutable,[v1])) in
      IdentTbl.add predef_exception_bindings id v2;
      v2
  in

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

  let rec aux block exp =
    let aux1 exp = aux block exp in
    let aux_ignore exp = let (_:v option) = aux1 exp in () in
    let aux' exp = match aux1 exp with
      | None -> raise Expecting_data
      | Some v -> v in
    let new_expr' ?new_value term =
      let eid = Flambda.data exp in
      new_expr ?new_value ~location:([],eid) graph block term in
    let new_expr ?new_value term = Some (new_expr' ?new_value term)
    in try match exp with
    | Fconst (cst,_) ->
      new_expr (Const cst)

    | Flet (str, id, lam, body, _) ->
      let v =
        match str with
        | Strict | Alias -> aux' lam
        | Variable ->
          failwith "TODO: variable"
          (* let v = aux' lam in *)
          (* new_expr' (Union [v]) *)
        | StrictOpt -> assert false
      in
      IdentTbl.add bindings id v;
      aux1 body

    | Fletrec (defs, body, _) ->
      let l = List.map (fun (id,lam) ->
          let v = make_value ~name:"letrec_union" graph block in
          IdentTbl.add bindings id v;
          ignore_need graph v;
          lam,v) defs in
      List.iter (fun (lam,new_value) ->
        let v' = aux' lam in
        ignore(new_expr' ~new_value (Union [v']):v)) l;
      aux1 body

    | Fvar (id, _) ->
      Some (IdentTbl.find bindings id)

    | Fsequence (lam1, lam2, _) ->
      aux_ignore lam1;
      aux1 lam2

    | Fprim( Pignore, [arg1], _, _ ) ->
      ignore(aux' arg1:v);
      new_expr unit

    | Fprim( Pidentity, [arg1], _, _ ) ->
      let v = aux' arg1 in
      new_expr (Identity v)

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

    | Fprim( Pnegint, [arg], _, _) ->
      let v = aux' arg in
      new_expr (Negint v)

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

    | Fprim(Pphyscomp cmp, [arg1;arg2], _, _) ->
      let v1 = aux' arg1 in
      let v2 = aux' arg2 in
      new_expr (Cmpphys (cmp,v1,v2))

    | Fprim(Pintcomp cmp, [arg1;arg2], _, _) ->
      let v1 = aux' arg1 in
      let v2 = aux' arg2 in
      new_expr (Cmpint (cmp,v1,v2))

    | Fprim(Pstringlength, [arg], _, _) ->
      let v = aux' arg in
      new_expr (Stringlength v)

    | Fprim(Pstringrefu, [arg1;arg2], _, _) ->
      let v1 = aux' arg1 in
      let v2 = aux' arg2 in
      new_expr (Stringget (false,v1,v2))

    | Fprim(Pstringrefs, [arg1;arg2], _, _) ->
      let v1 = aux' arg1 in
      let v2 = aux' arg2 in
      new_expr (Stringget (true,v1,v2))

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
      let f arg = new_expr' (union [aux1 arg]) in
      let vals = List.map f args in
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
        if Ident.is_predef_exn id
        then Some (predef_exn id)
        else Some (get_external_global id)
          (* failwith "TODO: global not current" *)
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
      let make_block b = new_block graph block (Printf.sprintf "if_%b" b) [v_cond, Bool b] in
      let v_ifso = aux (make_block true) ifso in
      let v_ifnot = aux (make_block false) ifnot in
      new_expr (union [v_ifso;v_ifnot])

    | Fswitch(cond, sw, _) ->
      let v_cond = aux' cond in
      let cst_cases =
        List.map (fun (tag,lam) ->
          let branch_block = new_block graph block (Printf.sprintf "switch_cst_%i" tag) [v_cond, SwitchCst tag] in
          aux branch_block lam) sw.fs_consts in
      let block_cases =
        List.map (fun (tag,lam) ->
          let branch_block = new_block graph block (Printf.sprintf "switch_block_%i" tag) [v_cond, SwitchTag tag] in
          aux branch_block lam) sw.fs_blocks in
      new_expr (union (cst_cases @ block_cases))

    | Fapply(func, params, _, _, _) ->
      let v_func = aux' func in
      let v_params = List.map aux' params in
      new_expr (Apply (v_func, v_params))

    | Fprim(Praise, [arg], _, _) ->
      let v = aux' arg in
      add_block_exception graph block v;
      None

    | Ftrywith(body, id, handler, _) ->
      let body_block = new_block graph block "try_body" [] in
      let v_body = aux body_block body in
      let exn = get_block_exception graph body_block in
      Printf.printf "exn value: %a\n%!" ValId.output exn;
      IdentTbl.add bindings id exn;
      let handler_block = new_block graph block "try_handler" [exn, Exists] in
      let v_handler = aux handler_block handler in
      new_expr (union [v_body; v_handler])

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

    | Fwhile(cond, body, _) ->
      let v_cond = aux' cond in
      let body_block = new_block graph block "while" [v_cond, Bool true] in
      let (_ : v option) = aux body_block body in
      add_block_exception graph block (get_block_exception graph body_block);
      new_expr unit

    | Ffor(id, lo, hi, _, body, _) ->
      let v_lo = aux' lo in
      let v_hi = aux' hi in
      let v_interval = new_expr' (Interval (v_lo, v_hi)) in
      IdentTbl.add bindings id v_interval;
      let body_block = new_block graph block "for" [] in
      let (_ : v option) = aux body_block body in
      add_block_exception graph block (get_block_exception graph body_block);
      new_expr unit

    | Fprim(p, args, _, _) ->
      let _ = Format.flush_str_formatter () in
      Format.fprintf Format.str_formatter "TODO prim: %a" Printlambda.primitive p;
      let s = Format.flush_str_formatter () in
      failwith s

    | e ->
      let desc = string_desc e in
      failwith (Printf.sprintf "TODO graph: %s" desc)
    with Expecting_data -> None
  in

  let _ = aux graph.toplevel_block tree in

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
    match aux call_info.call_block func.body with
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
