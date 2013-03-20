open Lambda
open Flambda
open Absint
open Values

module type CleanerParam = sig
  val analysis : analysis_result
  val unpure_expr : ExprSet.t
end

module Cleaner(Param:CleanerParam) = struct
  let analysis = Param.analysis

  let is_pure expr =
    let eid = Flambda.data expr in
    not (ExprSet.mem eid Param.unpure_expr)

  let get_val_id v =
    try ValMap.find v analysis.values with
    | Not_found -> empty_value

  let value eid =
    if ExprMap.mem eid analysis.expr
    then
      let ids = ExprMap.find eid analysis.expr in
      let l = ValSet.elements ids in
      let vl = List.map get_val_id l in
      Values.list_union vl
    else
      Values.empty_value

  let expr_value exp =
    value (Flambda.data exp)

  let array_set_kind kind array elt =
    match kind with
    | Paddrarray | Pintarray | Pfloatarray ->
      kind
    | Pgenarray ->
      let elt_kinds = array_kind_of_element (expr_value elt) in
      let arr_kinds = array_kind_of_array (expr_value array) in
      let kinds = array_kind_intersection elt_kinds arr_kinds in
      match kinds with
      | { int_kind = true; block_kind = false; float_kind = false } ->
        Printf.printf "int kind\n%!";
        Pintarray
      | { int_kind = false; block_kind = true; float_kind = false } ->
        Printf.printf "block kind\n%!";
        Paddrarray
      | { int_kind = false; block_kind = false; float_kind = true } ->
        Printf.printf "float kind\n%!";
        Pfloatarray
      | { int_kind = false; block_kind = false; float_kind = false } ->
        (* arg raise an exception or couldn't be reached: should
           remove completely that: for that, need to know
           wether arg, index or array is evaluated first *)
        (* Printf.printf "genkind none\n%!"; *)
        Pgenarray
      | _ ->
        (* Printf.printf "genkind\n%!"; *)
        Pgenarray

  let array_ref_kind kind array =
    match kind with
    | Paddrarray | Pintarray | Pfloatarray ->
      kind
    | Pgenarray ->
      let kinds = array_kind_of_array (expr_value array) in
      match kinds with
      | { int_kind = true; block_kind = false; float_kind = false } ->
        Pintarray
      | { int_kind = false; block_kind = true; float_kind = false } ->
        Paddrarray
      | { int_kind = false; block_kind = false; float_kind = true } ->
        Pfloatarray
      | { int_kind = false; block_kind = false; float_kind = false } ->
        (* should remove completely that: for that, need to know
           wether index or array is evaluated first *)
        Pgenarray
      | _ ->
        Pgenarray

  let fsequence (e1, e2, eid) =
    if is_pure e1
    then e2
    else Fsequence(e1,e2,eid)

  let mapper1 tree = match tree with

    | Fsequence(e1,e2,eid) ->
      fsequence(e1,e2,eid)

    | Fifthenelse(arg,ifso,ifnot,eid) ->
      begin match if_value (expr_value arg) with
        | TrueAndFalse -> tree
        | True -> fsequence(arg,ifso,eid)
        | False -> fsequence(arg,ifnot,eid)
        | Neither ->
          (* happens when arg never returns *)
          arg
      end

    | Fswitch(arg,sw,eid) ->
      let sw_val = switch_value (expr_value arg) in
      let fs_consts = match sw_val.consts with
        | All_cases -> sw.fs_consts
        | Some_cases cases ->
          List.filter (fun (i,_) -> IntSet.mem i cases) sw.fs_consts in

      let fs_blocks = match sw_val.blocks with
        | All_cases -> sw.fs_blocks
        | Some_cases cases ->
          List.filter (fun (i,_) -> IntSet.mem i cases) sw.fs_blocks in

      begin match sw.fs_failaction, fs_consts, fs_blocks with
        | Some _ , _, _ ->
          (* need to check what to do when there is a failaction *)
          tree
        | None, [], [] ->
          arg
        | None, [_, branch], []
        | None, [], [_, branch] ->
          (* maybe keep then information about the branch somewhere:
             it provides informations about the tag that are maybe not
             known in the abstraction *)
          fsequence(arg, branch, eid)
        | None, consts, blocks ->
          let max_case l =
            1 +
            List.fold_left (fun v (i,_) -> max v i) (-1) l in
          Fswitch(arg,
              { fs_failaction = sw.fs_failaction;
                fs_numconsts = max_case fs_consts;
                fs_consts;
                fs_numblocks = max_case fs_blocks;
                fs_blocks; },
              eid)
      end

    | Fprim(Parraysets kind, [array;index;elt], dbg, eid) ->
      (* could also check if the index is negative *)
      Fprim(Parraysets (array_set_kind kind array elt),
          [array;index;elt], dbg, eid)

    | Fprim(Parraysetu kind, [array;index;elt], dbg, eid) ->
      Fprim(Parraysetu (array_set_kind kind array elt),
          [array;index;elt], dbg, eid)

    | Fprim(Parrayrefs kind, [array;index], dbg, eid) ->
      Fprim(Parrayrefs (array_ref_kind kind array), [array;index], dbg, eid)

    | Fprim(Parrayrefu kind, [array;index], dbg, eid) ->
      Fprim(Parrayrefu (array_ref_kind kind array), [array;index], dbg, eid)

    | Fapply(funct, args, _, dbg, eid) ->
      begin match possible_closure (expr_value funct) with
        | Many_functions -> tree
        | One_function f ->
          let fun_id = match f.fun_id with
            | None -> assert false
            | Some id -> id in
          let ffunctions = FunMap.find f.closure_funs analysis.info.functions in
          let ffunction = IdentMap.find fun_id ffunctions.funs in
          let closed = IdentSet.is_empty ffunction.closure_params
                       && f.partial_application = Known [] in
          let closed = if closed then Closed else NotClosed in
          let direct = Some(ffunction.label,closed) in
          Fapply(funct, args, direct, dbg, eid)
        | No_function ->
          (* Don't know what to do here *)
          tree
      end
    | e -> e

  let mapper tree =
    if is_pure tree
    then
      match simple_constant (expr_value tree) with
      | Some (Cint i) ->
        Fconst (Fconst_base (Asttypes.Const_int i), data tree)
      | Some (Cptr i) ->
        Fconst (Fconst_pointer i, data tree)
      | None ->
        mapper1 tree
    else
      mapper1 tree

  let clean tree = Flambdautils.map mapper tree

end

let clean analysis unpure_expr tree =
  let module P = struct
    let analysis = analysis
    let unpure_expr = unpure_expr
  end in
  let module C = Cleaner(P) in
  C.clean tree
