(* open Misc *)
(* open Asttypes *)
open Lambda
open Flambda

let rec (-->) i j =
  if i > j
  then []
  else i :: ((i+1) --> j)

let list_functions t =
  let r = ref IdentMap.empty in
  let rec aux = function
    | Fclosure (funcs,_,_) ->
      IdentMap.iter (fun id ffunc ->
          r := IdentMap.add id ffunc !r;
          iter_tree ffunc.body) funcs.funs
    | _ -> ()
  and iter_tree t =
    Flambdautils.iter_flambda aux t
  in
  iter_tree t;
  !r

let global_size t =
  let r = ref (-1) in
  let aux = function
    | Fprim(Psetfield(n, _), [Fprim(Pgetglobal id, arg, _, _); lam], _, _) ->
      r := max !r n;
    | _ -> ()
  in
  Flambdautils.iter_flambda aux t;
  !r + 1

type code_informations = {
  functions : ExprId.t ffunction IdentMap.t;
  global_size : int;
}

let prepare_informations t =
  { functions = list_functions t;
    global_size = global_size t }

module Body = struct
  type body = Ident.t
  let compare = Idt.compare
end

module Name = struct
  type name = string
end

module B = Branching.BasicInterp(Body)(Name)

module type Fparam = sig
  val tree : ExprId.t Flambda.flambda
  val call_per_round : int
  val test_if : bool
end

type carried_informations

type bindings = {
  global_bindings : B.values IntMap.t;
  local_bindings : B.values IdentMap.t;
  expr_bindings : (B.values*B.traces) list ExprMap.t;
}

let merge_bindings b1 b2 =
  let aux _ v1 v2 = match v1, v2 with
    | None, v | v, None -> v
    | Some v1, Some v2 ->
      assert(B.same_values v1 v2);
      Some v1 in
  let aux_list _ v1 v2 = match v1, v2 with
    | None, v | v, None -> v
    | Some v1, Some v2 ->
      Some (v1 @ v2) in
  { global_bindings = IntMap.merge aux b1.global_bindings b2.global_bindings;
    local_bindings = IdentMap.merge aux b1.local_bindings b2.local_bindings;
    expr_bindings = ExprMap.merge aux_list b1.expr_bindings b2.expr_bindings; }

module Run(Param:Fparam) = struct

  let info = prepare_informations Param.tree

  let empty_bindings = {
    global_bindings = IntMap.empty;
    local_bindings = IdentMap.empty;
    expr_bindings = ExprMap.empty;
  }

  type funinfo = {
    funinfo_bindings : B.fixed_value IdentMap.t;
    funinfo_return : B.fixed_value;
    escape : bool;
  }

  let funinfo_tbl : funinfo IdentTbl.t = IdentTbl.create 100

  let get_funinfo fun_id = try IdentTbl.find funinfo_tbl fun_id with
    | Not_found ->
      { funinfo_bindings = IdentMap.empty;
        funinfo_return = B.empty_value;
        escape = false; }

  let set_call_param : Ident.t -> B.traces -> B.values IdentMap.t -> unit =
    fun fun_id traces bindings ->
      let funinfo = get_funinfo fun_id in

      let bindings = IdentMap.map (B.fixed_value traces) bindings in

      let aux_merge _ v1 v2 = match v1, v2 with
        | None, v | v, None -> v
        | Some v1, Some v2 -> Some (B.fixed_value_union [v1;v2]) in

      let bindings = IdentMap.merge aux_merge bindings
          funinfo.funinfo_bindings in

      let funinfo = { funinfo with funinfo_bindings = bindings } in

      IdentTbl.replace funinfo_tbl fun_id funinfo

  let set_call_return fun_id traces return =
    let funinfo = get_funinfo fun_id in
    let return = B.fixed_value traces return in
    let funinfo_return = B.fixed_value_union [return;funinfo.funinfo_return] in
    let funinfo = { funinfo with funinfo_return } in
    IdentTbl.replace funinfo_tbl fun_id funinfo

  let find_var bindings id =
    IdentMap.find id bindings.local_bindings

  let bind_var bindings id v =
    { bindings with
      local_bindings = IdentMap.add id v bindings.local_bindings }

  let bind_global bindings i v =
    { bindings with
      global_bindings = IntMap.add i v bindings.global_bindings }

  let bind_expr bindings eid traces v =
    let l = try ExprMap.find eid bindings.expr_bindings with
      | Not_found -> [] in
    let l = (v,traces)::l in
    { bindings with
      expr_bindings = ExprMap.add eid l bindings.expr_bindings }

  let trivial_expr traces texpr bindings =
    let values, traces = B.trivial_expr traces texpr in
    values, traces, bindings

  let global_record traces bindings =
    let aux_field i (l,traces,bindings) =
      try
        let values = IntMap.find i bindings.global_bindings in
        values::l, traces, bindings
      with
      | Not_found ->
        let values, traces, bindings =
          trivial_expr traces B.Unknown bindings in
        values::l, traces, bindings
    in
    let values_list, traces, bindings =
      List.fold_right aux_field (0 --> (info.global_size - 1))
        ([],traces,bindings) in
    trivial_expr traces (B.Makeblock(0,Asttypes.Immutable,values_list)) bindings

  let rec aux' traces bindings exp = match exp with
    | Fvar (id,_) ->
      find_var bindings id, traces, bindings
    | Fconst (cst,_) ->
      trivial_expr traces (B.Const cst) bindings
    | Fsequence(e1, e2, _) ->
      let _, traces, bindings = aux traces bindings e1 in
      let values, traces, bindings = aux traces bindings e2 in
      values, traces, bindings
    | Flet(str, id, lam, body, _) ->
      let values, traces, bindings = aux traces bindings lam in
      let bindings = bind_var bindings id values in
      aux traces bindings body

    | Fprim(Psetfield(n, _), [Fprim(Pgetglobal id, arg, _, _); lam], _, _) ->
      assert(id.Ident.name = Compilenv.current_unit_name ());
      assert(arg = []);
      let values, traces, bindings = aux traces bindings lam in
      let unit, traces, bindings = trivial_expr traces B.Unit bindings in
      unit, traces, bind_global bindings n values

    | Fprim(Pgetglobal id, arg, _, _) ->
      assert(arg = []);
      if id.Ident.name = Compilenv.current_unit_name ()
      then (* current module *)
        global_record traces bindings
      else
      if Ident.is_predef_exn id
      then trivial_expr traces (B.Predef_exn id) bindings
      else failwith "TODO" (* other module *)

    | Fprim(Paddint, [arg1;arg2], _, _) ->
      let val2, traces, bindings = aux traces bindings arg2 in
      let val1, traces, bindings = aux traces bindings arg1 in
      trivial_expr traces (B.Addint (val1,val2)) bindings

    | Fprim(Pmakeblock (tag,mut), args, _, _) ->
      let values_list, traces, bindings = aux_list traces bindings args in
      trivial_expr traces (B.Makeblock (tag,mut,values_list)) bindings

    | Fprim(Pfield n, [arg], _, _) ->
      let values, traces, bindings = aux traces bindings arg in
      trivial_expr traces (B.Field (n,values)) bindings

    | Fifthenelse (cond, ifso, ifnot, _) ->
      let vcond, parent_traces, bindings = aux traces bindings cond in
      let name = "if" in
      let ifbranches = B.if_branch name vcond parent_traces in
      let branches_ifso = match ifbranches.B.ifso with
        | None -> []
        | Some traces -> [traces, ifso]  in
      let branches_ifnot = match ifbranches.B.ifnot with
        | None -> []
        | Some traces -> [traces, ifnot] in
      let branches = branches_ifso @ branches_ifnot in
      let aux_branch (l, bindings) (traces,branch) =
        let values, traces, bindings = aux traces bindings branch in
        (values, traces)::l, bindings
      in
      let done_branches, bindings =
        List.fold_left aux_branch ([],bindings) branches in
      let values, traces = B.if_return name ~parent:parent_traces
          done_branches in
      values, traces, bindings

    | Fclosure(funct, fv, _) ->
      let aux_funct (id,ffunct) =
        { B.function_id = id;
          function_kind = ffunct.kind;
          function_param = ffunct.params;
          function_body = id } in
      let fl = List.map aux_funct (IdentMap.bindings funct.funs) in
      let aux_fv id exp (map, traces, bindings) =
        let values, traces, bindings = aux traces bindings exp in
        IdentMap.add id values map, traces, bindings in
      let values_map, traces, bindings =
        IdentMap.fold aux_fv fv (IdentMap.empty, traces, bindings) in
      trivial_expr traces (B.Closure (fl,values_map)) bindings

    | Foffset(expr, id, _) ->
      let values, traces, bindings = aux traces bindings expr in
      trivial_expr traces (B.Offset (values, id)) bindings

    | Fapply(func, args, _, _, _) ->
      let func_values, traces, bindings = aux traces bindings func in
      let args_values, traces, bindings = aux_list traces bindings args in
      aux_apply traces bindings func_values args_values

    | e ->
      failwith (Printf.sprintf "missing: %s" (Flambda.string_desc e))

  and aux traces bindings expr =
    let values, traces, bindings = aux' traces bindings expr in
    let eid = Flambda.data expr in
    let bindings = bind_expr bindings eid traces values in
    values, traces, bindings

  (* evaluates right to left *)
  and aux_list traces bindings exprs =
    List.fold_right (fun expr (l,traces,bindings) ->
      let values, traces, bindings = aux traces bindings expr in
      values :: l, traces, bindings)
      exprs ([],traces,bindings)

  and aux_apply parent_traces bindings func_values args_values =
    let name = "call" in
    let call_branches = B.call name ~func:func_values args_values parent_traces in
    let aux_call (values_traces_list, bindings) (apply_branch,traces) =
      match apply_branch with
      | B.Inside inside ->
        let values, traces, bindings = aux_inside_function inside traces bindings in
        (values, traces) :: values_traces_list, bindings
      | B.Return values ->
        (values, traces) :: values_traces_list, bindings
    in
    let values_traces_list, bindings =
      List.fold_left aux_call ([], bindings) call_branches in
    let values, traces = B.return name ~parent:parent_traces values_traces_list in
    values, traces, bindings

  and aux_inside_function inside traces bindings =
    set_call_param inside.B.body traces inside.B.bindings;
    let merge_local_bindings _ b1 b2 = match b1, b2 with
      | None, b | b, None -> b
      | Some _, Some _ -> b2 (* new binding have priority *)
    in
    let bindings =
      { bindings with
        local_bindings = IdentMap.merge
            merge_local_bindings bindings.local_bindings inside.B.bindings } in
    let func = IdentMap.find inside.B.body info.functions in

    let values, traces, bindings =
      match do_eval_function inside with
      | None ->
        let values, traces, bindings = aux traces bindings func.body in
        set_call_return inside.B.body traces values;
        values, traces, bindings
      | Some fixed_return ->
        trivial_expr traces (B.Fixed fixed_return) bindings
    in

    match inside.B.over_parameters with
    | [] -> values, traces, bindings
    | _::_ ->
      aux_apply traces bindings values inside.B.over_parameters

  and do_eval_function inside = None

  let result =
    let init_traces = B.start_branch "root" in
    let values, traces, bindings = aux init_traces empty_bindings Param.tree in
    traces, bindings

end

let print_result ppf traces bindings =
  IntMap.iter (fun i v ->
    Format.fprintf ppf "%i: %a@ "
      i (B.print_values traces) v) bindings.global_bindings;
  Format.fprintf ppf "@.";
  let print_list ppf l =
    List.iter (fun (values,traces) ->
      Format.fprintf ppf "%a@ " (B.print_values traces) values) l
  in
  ExprMap.iter (fun eid l ->
    Format.fprintf ppf "%a: %a@ "
      ExprId.print eid print_list l)
    bindings.expr_bindings;
  Format.fprintf ppf "@."

let run ppf tree =
  let module Param = struct
    let tree = tree
    let call_per_round = 2
    let test_if = true
  end in
  let module R = Run(Param) in
  let traces, bindings = R.result in
  print_result ppf traces bindings;
  traces, bindings
