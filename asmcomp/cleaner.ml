open Misc
open Lambda
open Flambda
open Flambdainfo
open Values

let one_value_function analysis value =
  match possible_closure value with
  | No_function | Many_functions -> None
  | One_function f ->
    let fun_id = f.fun_id in
    let ffunctions = FunMap.find f.closure_funs analysis.info.functions in
    Some (f, IdentMap.find fun_id ffunctions.funs)

module type CleanerParam = sig
  val analysis : analysis_result
  val effectful_expr : ExprSet.t
end

module Cleaner(Param:CleanerParam) = struct
  let analysis = Param.analysis

  let is_effectless expr =
    let eid = Flambda.data expr in
    not (ExprSet.mem eid Param.effectful_expr)

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
        Pintarray
      | { int_kind = false; block_kind = true; float_kind = false } ->
        Paddrarray
      | { int_kind = false; block_kind = false; float_kind = true } ->
        Pfloatarray
      | { int_kind = false; block_kind = false; float_kind = false } ->
        (* arg raise an exception or couldn't be reached: should
           remove completely that: for that, need to know
           wether arg, index or array is evaluated first *)
        Pgenarray
      | _ ->
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
    if is_effectless e1
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
          Printf.printf "\n\n\n\nneither\n\n\n\n%!";
          (* happens when arg never returns *)
          arg
      end

    | Fswitch(arg,sw,eid) ->
      let sw_val = switch_value (expr_value arg) in

      let aux branches cases_set = function
        | All_cases -> branches, cases_set, true
        | Some_cases cases ->
          let cases_set = IntSet.filter (fun i -> IntSet.mem i cases) cases_set in
          let kept = List.filter (fun (i,_) -> IntSet.mem i cases) branches in
          let branch_set = List.fold_left (fun set (i,_) -> IntSet.add i set)
              cases_set branches in
          let need_failaction = not (IntSet.subset cases branch_set) in
          kept, cases_set, need_failaction in

      let fs_consts, fs_numconsts, need_failaction_const =
        aux sw.fs_consts sw.fs_numconsts sw_val.consts in

      let fs_blocks, fs_numblocks, need_failaction_block =
        aux sw.fs_blocks sw.fs_numblocks sw_val.blocks in

      let need_failaction = need_failaction_const || need_failaction_block in

      let fs_failaction =
        if need_failaction
        then sw.fs_failaction
        else None in

      begin match fs_failaction, fs_consts, fs_blocks with
        | Some failaction, [], [] ->
          fsequence(arg, failaction, eid)
        | None, [], [] ->
          arg
        | None, [_, branch], []
        | None, [], [_, branch] ->
          (* maybe keep then information about the branch somewhere:
             it provides informations about the tag that are maybe not
             known in the abstraction *)
          fsequence(arg, branch, eid)
        | _, consts, blocks ->
          Fswitch(arg,
              { fs_failaction;
                fs_numconsts;
                fs_consts;
                fs_numblocks;
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
      begin match one_value_function analysis (expr_value funct) with
        | None -> tree
        | Some (f, ffunction) ->
          let fun_id = f.fun_id in
          let ffunctions = FunMap.find f.closure_funs analysis.info.functions in
          let ffunction = IdentMap.find fun_id ffunctions.funs in
          if ffunction.arity = List.length args
          then
            let closed = IdentSet.is_empty ffunction.closure_params
                         && f.partial_application = Known [] in
            let closed = if closed then Closed else NotClosed in
            let direct = Some(ffunction.label,closed) in
            Fapply(funct, args, direct, dbg, eid)
          else tree
      end

    | e -> e

  let mapper tree =
    if is_effectless tree
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

module Rebinder(Param:CleanerParam) = struct
  let analysis = Param.analysis
  let external_val = analysis.external_val

  let is_effectless expr =
    let eid = Flambda.data expr in
    not (ExprSet.mem eid Param.effectful_expr)

  let fsequence (e1, e2, eid) =
    if is_effectless e1
    then e2
    else Fsequence(e1,e2,eid)

  let find_binding id =
    try Some (IdentMap.find id analysis.bindings) with Not_found -> None

  let find_values expr =
    let eid = data expr in
    try Some (ExprMap.find eid analysis.expr) with Not_found -> None

  let recover_binding map v =
    try Some (ValMap.find v map) with Not_found -> None

  let add map id =
    match find_binding id with
    | None -> map
    | Some set ->
      ValSet.fold (fun elt map ->
          if (ValId.equal elt external_val) ||
             (* if the value is the external val we can't do anything with it *)
             (ValMap.mem elt map)
          then map
          else ValMap.add elt id map) set map

  let map tree =
    let simplif map tree =
      match find_values tree with
      | None -> tree
      | Some v ->
        match ValSet.elements v with
        | [] | _::_::_ -> tree
        | [v] -> match recover_binding map v with
          | None -> tree
          | Some id ->
            match tree with
            | Fconst _ -> tree
            | Fvar (id',data) ->
              if not (Ident.same id id')
              then begin
                (* Printf.printf "\n\ncas redir\n\n%!"; *)
                Fvar (id,data)
              end
              else tree
            | _ ->
              (* Printf.printf "\n\ncas utile\n\n%!"; *)
              if is_effectless tree
              then Fvar (id,data tree)
              else tree
              (* let eid1 = ExprId.create () in *)
              (* let eid2 = ExprId.create () in *)
              (* fsequence (tree,Fvar (id,eid1),eid2) *)
              (* tree *)
    in
    let rec aux map tree =
      let exp = match tree with
        | Fvar (id,annot) -> tree
        | Fconst (cst,annot) -> tree
        | Fapply (funct, args, direc, dbg, annot) ->
          Fapply ((aux map) funct, List.map (aux map) args, direc, dbg, annot)
        | Fclosure (ffuns, fv, annot) ->
          let ffuns =
            { ffuns with
              funs = IdentMap.map
                  (fun ffun ->
                    let map = IdentSet.fold (fun id map -> add map id)
                        ffun.closure_params ValMap.empty in
                    let map = List.fold_left add map ffun.params in
                    { ffun with body = (aux map) ffun.body })
                  ffuns.funs } in
          let fv = IdentMap.map (aux map) fv in
          Fclosure (ffuns, fv, annot)
        | Foffset (flam, off, annot) ->
          Foffset ((aux map) flam, off, annot)
        | Fenv_field (fenv_field, annot) ->
          Fenv_field ({ fenv_field with env = (aux map) fenv_field.env }, annot)
        | Flet(str, id, lam, body, annot) ->
          let lam = (aux map) lam in
          let map = add map id in
          let body = (aux map) body in
          Flet (str, id, lam, body, annot)
        | Fletrec(defs, body, annot) ->
          let defs = List.map (fun (id,lam) -> id,(aux map) lam) defs in
          let body = (aux map) body in
          Fletrec (defs, body, annot)
        | Fprim(p, args, dbg, annot) ->
          let args = List.map (aux map) args in
          Fprim (p, args, dbg, annot)
        | Fstaticfail(i, args, annot) ->
          let args = List.map (aux map) args in
          Fstaticfail (i, args, annot)
        | Fcatch (i, vars, body, handler, annot) ->
          let body = (aux map) body in
          let handler = (aux map) handler in
          Fcatch (i, vars, body, handler, annot)
        | Ftrywith(body, id, handler, annot) ->
          let body = (aux map) body in
          let handler = (aux map) handler in
          Ftrywith(body, id, handler, annot)
        | Fifthenelse(arg, ifso, ifnot, annot) ->
          let arg = (aux map) arg in
          let ifso = (aux map) ifso in
          let ifnot = (aux map) ifnot in
          Fifthenelse(arg, ifso, ifnot, annot)
        | Fsequence(lam1, lam2, annot) ->
          let lam1 = (aux map) lam1 in
          let lam2 = (aux map) lam2 in
          Fsequence(lam1, lam2, annot)
        | Fwhile(cond, body, annot) ->
          let cond = (aux map) cond in
          let body = (aux map) body in
          Fwhile(cond, body, annot)
        | Fsend(kind, met, obj, args, dbg, annot) ->
          let met = (aux map) met in
          let obj = (aux map) obj in
          let args = List.map (aux map) args in
          Fsend(kind, met, obj, args, dbg, annot)
        | Ffor(id, lo, hi, dir, body, annot) ->
          let lo = (aux map) lo in
          let hi = (aux map) hi in
          let body = (aux map) body in
          Ffor(id, lo, hi, dir, body, annot)
        | Fassign(id, lam, annot) ->
          let lam = (aux map) lam in
          Fassign(id, lam, annot)
        | Fswitch(arg, sw, annot) ->
          let arg = (aux map) arg in
          let sw =
            { sw with
              fs_failaction = Misc.may_map (aux map) sw.fs_failaction;
              fs_consts = List.map (fun (i,v) -> i, (aux map) v) sw.fs_consts;
              fs_blocks = List.map (fun (i,v) -> i, (aux map) v) sw.fs_blocks; } in
          Fswitch(arg, sw, annot)
      in
      simplif map exp
    in
    aux ValMap.empty tree

  let rebind = map

end

(* BAAAAAAD ! *)
let fun_size ffun =
  (* check also that some parameters are constants *)
  let count = ref 0 in
  Flambdautils.iter_flambda (fun _ -> incr count) ffun.body;
  !count

let should_inline ffun = fun_size ffun < 30

let should_inline_minimal ffun =
  let max_size = 10 in
  let count = ref 0 in
  Flambdautils.iter_flambda (function
    | Fclosure _ -> count := !count + max_size
    | _ -> incr count) ffun.body;
  !count < max_size

let inline_simple func fun_id ffun args =
  let sb, free_vars = IdentSet.fold (fun id (sb,free_vars) ->
      let id' = Ident.rename id in
      let sb = IdentMap.add id id' sb in
      let free_vars = (id,id')::free_vars in
      (sb, free_vars)) ffun.closure_params (IdentMap.empty,[]) in

  let sb, params = List.fold_right (fun id (sb,params) ->
      let id' = Ident.rename id in
      let sb = IdentMap.add id id' sb in
      let params = id'::params in
      (sb,params)) ffun.params (sb,[]) in
  let body, _ = Flambdasubst.substitute sb ffun.body in
  let body =
    List.fold_right2 (fun id arg body ->
        Flet(Strict,id,arg,body,ExprId.create ()))
      params args body in
  let func_var = Ident.create "inlined_closure" in
  let body =
    List.fold_right (fun (id,id') body ->
        let field =
          { env = Fvar(func_var,ExprId.create ());
            env_fun_id = fun_id;
            env_var = id } in
        Flet(Strict,id',Fenv_field(field,ExprId.create ()),body,ExprId.create ()))
      free_vars body in
  Flet(Strict,func_var,func,body,ExprId.create ())

type inlining_kind =
  | Minimal
  | With_local_functions

let inlining inlining_kind analysis lam =

  let functions_map = ref analysis.info.functions in

  let get_val_id v =
    try ValMap.find v analysis.values with
    | Not_found -> empty_value in

  let value eid =
    if ExprMap.mem eid analysis.expr
    then
      let ids = ExprMap.find eid analysis.expr in
      let l = ValSet.elements ids in
      let vl = List.map get_val_id l in
      Values.list_union vl
    else
      Values.empty_value in

  let expr_value exp =
    value (Flambda.data exp) in

  (* let closure ffunctions fv data = *)
  (*   (\* update functions body for further inlining *\) *)
  (*   functions_map := FunMap.add ffunctions.ident ffunctions !functions_map; *)
  (*   Flambdautils.Data data in *)

  let apply ~func ~args ~dbg ~tree node_data =
    begin match one_value_function analysis (expr_value func) with
      | None -> tree
      | Some (f, ffunction) ->
        if ffunction.arity = List.length args
        then
          let fun_id = f.fun_id in
          let ffunctions = FunMap.find f.closure_funs
              !functions_map in
          let ffunction = IdentMap.find fun_id ffunctions.funs in
          let should_go = match inlining_kind with
            | Minimal -> should_inline_minimal ffunction
            | With_local_functions -> should_inline ffunction
          in
          if not ffunctions.recursives && should_go
          then inline_simple func fun_id ffunction args
          else tree
        else tree
    end
  in

  let mapper tree = match tree with
    | Fclosure (ffunctions, fv, _) ->
      functions_map := FunMap.add ffunctions.ident ffunctions !functions_map;
      tree
    | Fapply (func, args, _, dbg, eid) ->
      apply ~func ~args ~dbg ~tree eid
    | _ -> tree in

  Flambdautils.map mapper lam


let used_variables_and_offsets tree =
  let vars = ref IdentSet.empty in
  let rec aux = function
    | Fenv_field({env_var = id}, _) (* env_fun_id also ? *)
    | Foffset(_, id, _)
    | Fvar (id,_) ->
      if not (IdentSet.mem id !vars) then vars := IdentSet.add id !vars
    | Fclosure (funcs,_,_) ->
      IdentMap.iter (fun _ ffunc -> Flambdautils.iter_flambda aux ffunc.body)
        funcs.funs
    | _ -> ()
  in
  Flambdautils.iter_flambda aux tree;
  !vars

(* this is now done by dead code elimination *)

(* let remove_unused_closure_param tree = *)
(*   let used = used_variables_and_offsets tree in *)
(*   let mapper tree = match tree with *)
(*     | Fclosure(ffunctions,fv,eid) -> *)
(*       let fv = IdentMap.filter (fun id _ -> IdentSet.mem id used) fv in *)
(*       let ffunctions = *)
(*         { ffunctions with *)
(*           funs = IdentMap.map (fun ffun -> *)
(*               { ffun with closure_params = *)
(*                             IdentSet.inter ffun.closure_params used }) *)
(*               ffunctions.funs } in *)
(*       Fclosure(ffunctions,fv,eid) *)
(*     | _ -> tree *)
(*   in *)
(*   Flambdautils.map mapper tree *)

let remove_function_variables used fun_id ffunction =
  match ffunction.kind with
  | Lambda.Tupled ->
    (* Not sure how to do it correctly with tupled functions *)
    None
  | Lambda.Curried ->
    let new_ident = Ident.rename fun_id in
    let indent_in_closure = Ident.rename fun_id in
    let new_label = Flambdagen.make_function_lbl new_ident in

    let aux map id = IdentMap.add id (Ident.rename id) map in

    let subst = List.fold_left aux IdentMap.empty ffunction.params in
    let subst = List.fold_left aux subst
        (IdentSet.elements ffunction.closure_params) in

    let filter_used l = List.filter (fun id -> IdentSet.mem id used) l in

    let kept_params = filter_used ffunction.params in
    let kept_closure_params = filter_used
        (IdentSet.elements ffunction.closure_params) in
    let cleaned_params = kept_params @ kept_closure_params in

    let map_params l = List.map (fun id -> IdentMap.find id subst) l in

    (* TODO: if arity = 0 ajouter un paramêtre unit *)

    let empty_params = cleaned_params = [] in

    let cleaned_ffunction =
      { label = new_label;
        kind = ffunction.kind;
        arity =
          if empty_params
          then 1
          else List.length cleaned_params;
        params =
          if empty_params
          then [Ident.create "unit_param"]
          else cleaned_params;
        closure_params = IdentSet.empty;
        body = ffunction.body;
        dbg = ffunction.dbg; } in

    let eid = ExprId.create in
    let body =
      let apply_params =
        if empty_params
        then [Fconst (Fconst_pointer 0,ExprId.create ())]
        else List.map (fun id -> Fvar(id,eid ()))
            (map_params cleaned_params) in
      Fapply(
        Fvar (indent_in_closure, eid ()),
        apply_params,
        None,
        Debuginfo.none,
        eid ()) in

    let params = map_params ffunction.params in
    let closure_params = IdentSet.fold (fun id set ->
        IdentSet.add (IdentMap.find id subst) set)
        ffunction.closure_params IdentSet.empty in

    let new_ffunction =
      { ffunction with
        params;
        closure_params = IdentSet.add indent_in_closure closure_params;
        dbg = Debuginfo.none;
        body } in
    Some (new_ident, indent_in_closure, new_ffunction, cleaned_ffunction)

let remove_unused_function_param tree =
  let used = used_variables_and_offsets tree in
  let mapper tree = match tree with
    | Fclosure(ffunctions,fv,eid) ->
      if ffunctions.recursives
      then tree
      else begin
        let l = IdentMap.bindings ffunctions.funs in
        match l with
        | [] | _::_::_ -> assert false
        | [fun_id, ffunction] ->
          if List.length ffunction.params = 1 ||
             List.for_all (fun id -> IdentSet.mem id used) ffunction.params
          then tree
          else
            match remove_function_variables used fun_id ffunction with
            | None -> tree
            | Some (new_ident, indent_in_closure,
                    new_ffunction, cleaned_ffunction) ->
              let cleaned_ffunctions =
                { ident = FunId.create ();
                  funs = IdentMap.singleton new_ident cleaned_ffunction;
                  recursives = false } in
              let new_ffunctions =
                { ident = ffunctions.ident;
                  funs = IdentMap.singleton fun_id new_ffunction;
                  recursives = false } in
              let cleaned_closure =
                Fclosure(cleaned_ffunctions,IdentMap.empty,ExprId.create ()) in
              let fv = IdentMap.add indent_in_closure
                  (Fvar(new_ident,ExprId.create ())) fv in
              let new_closure = Fclosure(new_ffunctions,fv,ExprId.create ()) in
              let ret = Flet
                  (Strict,new_ident,
                   Foffset(cleaned_closure, new_ident, ExprId.create ()),
                   new_closure,
                   eid) in
              ret
      end
    | _ -> tree
  in
  Flambdautils.map mapper tree

let clean analysis effectful tree =
  let module P = struct
    let analysis = analysis
    let effectful_expr = effectful.Purity.effectful_expr
  end in
  let module C1 = Cleaner(P) in
  let tree = C1.clean tree in
  let module C2 = Rebinder(P) in
  let tree = C2.rebind tree in
  tree
  (* remove_unused_closure_param tree *)

let specialise analysis effectful tree =
  let module P = struct
    let analysis = analysis
    let effectful_expr = effectful.Purity.effectful_expr
  end in
  let module C1 = Cleaner(P) in
  C1.clean tree

let rebind analysis effectful tree =
  let module P = struct
    let analysis = analysis
    let effectful_expr = effectful.Purity.effectful_expr
  end in
  let module C2 = Rebinder(P) in
  C2.rebind tree

let extract_constants (constants:Constants.constant_result) tree =

  let global_var, global_index = Flambdautils.global_index tree in

  let global_id id =
    try
      let gid = IdentTbl.find global_var id in
      if gid.Ident.name = Compilenv.current_unit_name ()
      then Some gid
      else None
    with Not_found -> None in

  (* constants extracted *)
  let bindings = ref [] in
  let renaming = IdentTbl.create 10 in

  (* constant variables extracted in the current closure *)
  let current_acc = ref IdentSet.empty in

  let add_binding id lam =
    bindings := (id, lam) :: !bindings;
    current_acc := IdentSet.add id !current_acc
  in

  let rec mapper' iter env tree = mapper (fun tree -> iter () tree) tree
  and mapper iter tree = match tree with
    | Flet ( (StrictOpt|Strict|Alias) as kind, id, lam, body, eid) ->
      begin
        let b = bind iter (id,lam) in
        let body = mapper iter body in
        match b with
        | None -> body
        | Some (id,lam) ->
          Flet ( kind, id, lam, body, eid )
      end

    | Fletrec (defs, body, eid) ->
      let not_const_defs = map_filter (bind iter) defs in
      let body = mapper iter body in
      begin match not_const_defs with
        | [] -> body
        | _::_ -> Fletrec (not_const_defs, body, eid)
      end

    | Fclosure( ffunctions, fv, eid ) ->
      fclosure iter None ffunctions fv eid

    | _ -> iter tree

  and bind iter (id,lam) =
    if IdentSet.mem id constants.Constants.not_constant_id
    then
      let lam = mapper iter lam in
      Some (id, lam)
    else begin
      match lam with
      | Fvar(aliased_id, _) ->
        (* avoid let rec x = y and y = ... *)
        IdentTbl.add renaming id aliased_id;
        Some (id, lam)
      | Fclosure( ffunctions, fv, eid ) ->
        ignore(fclosure iter (Some id) ffunctions fv eid);
        None
      | _ ->
        let lam = mapper iter lam in
        add_binding id lam;
        None
    end

  and fclosure iter name ffunctions fv eid =
    let fv = IdentMap.map (mapper iter) fv in

    let previous_acc = !current_acc in
    current_acc := IdentSet.empty;

    let ffunctions =
      { ffunctions with
        funs = IdentMap.map
            (fun ffun ->
               let tmp = !current_acc in
               current_acc := IdentSet.empty;
               let body = mapper iter ffun.body in
               let new_free_var = !current_acc in
               current_acc := IdentSet.union new_free_var tmp;
               let closure_params = IdentSet.union new_free_var ffun.closure_params in
               let v = { ffun with body; closure_params } in
               v)
            ffunctions.funs } in

    let renamed_fv id expr =
      if not (IdentSet.mem id constants.Constants.not_constant_id)
      then begin
        (* si la variable libre est une constante, alors elle
           elle doit être renomée dans les bindings *)
        match expr with
        | Fvar(external_var,_) ->
          (* Printf.printf "rename %s => %s\n%!" *)
          (*   (Ident.unique_name id) (Ident.unique_name external_var); *)
          IdentTbl.add renaming id external_var
        | _ ->
          (* TODO, ou pas... en ANF ce cas la n'existe pas *)
          failwith "TODO quand la variable libre n'est pas directement une variable"
      end
    in
    IdentMap.iter renamed_fv fv;

    let local_acc = ref IdentSet.empty in

    let add_var var fv =
      let renamed = Ident.rename var in
      IdentTbl.add renaming var renamed;
      local_acc := IdentSet.add renamed !local_acc;
      (* Printf.printf "rename add %s => %s\n%!" *)
      (* (Ident.unique_name var) (Ident.unique_name renamed); *)
      IdentMap.add var (Fvar(renamed,ExprId.create ())) fv in

    let fv = IdentSet.fold add_var !current_acc fv in
    (* TODO: se démerder pour qu'il n'y ait pas de problème quand le
       code des clotures sont constants *)

    let res =
      if FunSet.mem ffunctions.ident constants.Constants.not_constant_closure
      then Fclosure( ffunctions, fv, eid )
      else begin
        let new_id = match name with
          | None -> Ident.create "constant_closure"
          | Some id -> id
        in
        bindings := (new_id, Fclosure( ffunctions, fv, ExprId.create () )) ::
                      !bindings;
        local_acc := IdentSet.add new_id !local_acc;
        Fvar(new_id, eid)
      end
    in

    current_acc := IdentSet.union !local_acc previous_acc;
    res

  in
  let rec rename id =
    try rename (IdentTbl.find renaming id)
    with Not_found -> id in
  let renaming tree =
    match tree with
    | Fvar(var,eid) ->
      (* let ren = rename var in *)
      (* Printf.printf "rebind %s => %s\n%!" *)
      (*   (Ident.unique_name var) (Ident.unique_name ren); *)
      Fvar(rename var,eid)
    | Fprim(Pfield i,[Fvar(id,_)], _, eid) ->
      begin match global_id id with
        | None -> tree
        | Some gid ->
          let var = IntTbl.find global_index i in
          Fvar(var, eid)
      end
    | _ -> tree in
  let tree = Flambdautils.map2 mapper' () tree in
  let bindings = List.fold_left (fun map (v,expr) ->
      let v = rename v in
      let expr = Flambdautils.map_no_closure renaming expr in
      IdentMap.add v expr map)
      IdentMap.empty !bindings in
  Flambdasort.rebuild_sorted_expr bindings tree



type count =
  | Zero
  | One
  | Many
  | Loop

let count_var expr =

  let add id env =
    try
      let count = match IdentTbl.find env id with
        | Zero -> One
        | One | Many -> Many
        | Loop -> Loop in
      IdentTbl.replace env id count
    with Not_found -> () in

  let promote ~orig_env ~loop_env =
    IdentTbl.iter (fun id count -> IdentTbl.replace orig_env id Loop) loop_env in

  let fresh_env () = IdentTbl.create 5 in

  let global_count = IdentTbl.create 5 in

  let rec f iter env = function
    | Flet(_,id,lam,body,_) ->
      f iter env lam;
      IdentTbl.add env id Zero;
      f iter env body;
      begin try
          let count = IdentTbl.find env id in
          IdentTbl.remove env id;
          IdentTbl.add global_count id count
        with Not_found -> () end

    | Fvar(id, _) -> add id env
    | Fwhile(fcond, floop, _) ->
      f iter env fcond;
      loop iter env floop
    | Ffor(_,flo,fhi,_,floop,_) ->
      f iter env flo;
      f iter env fhi;
      loop iter env floop
    | Fclosure (ffuns, fv, _) ->
      IdentMap.iter (fun _ ffun -> f iter env ffun.body) ffuns.funs;
      IdentMap.iter (fun _ expr -> f iter env expr) fv;
    | expr -> iter env expr

  and loop iter orig_env expr =
    let loop_env = fresh_env () in
    f iter loop_env expr;
    promote ~orig_env ~loop_env
  in

  let init_env = fresh_env () in
  Flambdautils.iter2_flambda f init_env expr;
  assert(IdentTbl.length init_env = 0);
  global_count

let elim_let constant unpure_expr expr =
  let count = count_var expr in
  let should_elim id eid =
    let count = try Some (IdentTbl.find count id) with _ -> None in
    match count with
    | None | Some (Zero | Many | Loop) -> false
    | Some One ->
      (IdentSet.mem id constant.Constants.not_constant_id) &&
      (* if the value is a constant, it will be compiled more
         efficiently by simply accessing its label *)
      not (ExprSet.mem eid unpure_expr.Purity.effectful_expr)
          (* if the expression is unpure, we cannot move it
             around without changing the semantics *)
  in

  let add_env id lam env = IdentMap.add id lam env in

  let rec mapper iter env expr = match expr with
    | Fvar(id, _) ->
      begin try IdentMap.find id env with Not_found -> expr end

    | Flet(_, id, lam, Fvar(id', _), _) when Ident.same id id' ->
      mapper iter env lam

    | Flet((StrictOpt|Strict|Alias), id, lam, body, _) ->
      if should_elim id (data lam)
      then
        let lam = mapper iter env lam in
        mapper iter (add_env id lam env) body
      else iter env expr

    | Fclosure (ffuns, fv, eid) ->
      let ffuns =
        { ffuns with
          funs = IdentMap.map
              (fun ffun ->
                 { ffun with body = mapper iter IdentMap.empty ffun.body })
              ffuns.funs } in
      let fv = IdentMap.map (mapper iter env) fv in
      Fclosure (ffuns, fv, eid)

    | _ -> iter env expr
  in

  Flambdautils.map2 mapper IdentMap.empty expr
