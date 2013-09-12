open Misc
open Lambda
open Flambda
open Flambdainfo
open Values

let find_funid id map =
  try FunMap.find id map with
  | Not_found ->
    Compilenv.find_funid id

let one_value_function analysis value =
  match possible_closure value with
  | No_function | Many_functions -> None
  | One_function f ->
    let fun_id = f.fun_id in
    let ffunctions = find_funid f.closure_funs analysis.info.functions in
    Some (f, IdentMap.find fun_id.off_id ffunctions.funs)

let offset off_id = {off_id; off_unit = Compilenv.current_unit_symbol ()}

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
          Funreachable eid
      end

    | Fswitch(arg,sw,eid) ->
      let sw_val = switch_value (expr_value arg) in

      let aux branches cases_set = function
        | All_cases -> branches, cases_set, true
        | Some_cases cases ->
          let cases_set = IntSet.filter (fun i -> IntSet.mem i cases) cases_set in
          let kept = List.filter (fun (i,_) -> IntSet.mem i cases) branches in
          let branch_set = List.fold_left (fun set (i,_) -> IntSet.add i set)
              IntSet.empty branches in
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
          Printf.printf "neither switch\n%!";
          Funreachable eid
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
        | None ->
          (* Format.eprintf "none: %a@." Printflambda.flambda funct; *)
          tree
        | Some (f, ffunction) ->
          (* Format.eprintf "some: %a@." Printflambda.flambda funct; *)
          let fun_id = f.fun_id in
          let ffunctions = find_funid f.closure_funs analysis.info.functions in
          let ffunction = IdentMap.find fun_id.off_id ffunctions.funs in
          match ffunction.kind with
          | Tupled -> tree (* TODO: tupled functions ! *)
          | Curried ->
            if ffunction.arity = List.length args &&
               f.partial_application = Known []
            then
              let direct = Some fun_id in
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

let print_info a =
  let open Format in
  ValMap.iter (fun id (_,sym) ->
      printf "%s -> %a@." sym ValId.print id) a.symbols

module Rebinder(Param:CleanerParam) = struct
  let analysis = Param.analysis
  let external_val = analysis.external_val

  (* let () = print_info analysis *)

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

  type binding =
    | Var of Ident.t
    | Symbol of Symbol.t
    | No_binding

  let recover_binding map v =
    try Var (ValMap.find v map) with
    | Not_found ->
      try
        let symbol = ValMap.find v analysis.symbols in
        Symbol symbol
      with Not_found -> No_binding

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
          | No_binding -> tree
          | Var id ->
            begin match tree with
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
            end
          | Symbol sym ->
            (* Printf.printf "found symbol %s\n%!" (snd sym); *)
            begin match tree with
              | Fconst _ -> tree
              | _ ->
                if is_effectless tree
                then Fsymbol (sym,data tree)
                else tree
            end
    in
    let rec aux map tree =
      let exp = match tree with
        | Fvar (id,annot) -> tree
        | Fsymbol (sym,annot) -> tree
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
        | Funreachable _ -> tree
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
  Flambdautils.iter_all (function
      | Fvar _ -> ()
      | Fsequence (e1,e2,_) -> ()
      | Flet (_,_,e1,e2,_) -> ()
      | _ -> incr count) ffun.body;
  !count

type toplevel = Toplevel | Deep

let should_inline constants toplevel ffunction args =
  let should_inline_size () = fun_size ffunction < 30 in
  let has_constant_param =
    List.exists (function
        | Fvar (id,_) ->
          (* the parameter is a constant *)
          not (IdentSet.mem id constants.Constants.not_constant_id)
        | Fconst _ -> true
        | _ -> fatal_error "not in ANF")
      args
  in
  (* Printf.printf "inline %s\n%!" (ffunction.label:>string); *)
  match toplevel with
  | Toplevel ->
    (* Printf.printf "toplevel\n"; *)
    has_constant_param || should_inline_size ()
  | Deep ->
    (* Printf.printf "deep\n"; *)
    has_constant_param && should_inline_size ()

let should_inline_minimal ffun =
  let max_size = 10 in
  let count = ref 0 in
  Flambdautils.iter_flambda (function
      | Fvar _ -> ()
      | Fsequence (e1,e2,_) -> ()
      | Flet (_,_,e1,e2,_) -> ()
      | Fclosure _ -> count := !count + max_size
      | _ -> incr count) ffun.body;
  !count < max_size

let inline_simple func fun_id ffun args =
  (* Printf.printf "inline %s\n%!" (ffun.label:>string); *)
  let sb, free_vars = IdentSet.fold (fun id (sb,free_vars) ->
      let id' = Ident.rename id in
      let sb = IdentMap.add id id' sb in
      let off = { off_id = id; off_unit = fun_id.off_unit } in
      let free_vars = (off,id')::free_vars in
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
    List.fold_right (fun (env_var,id') body ->
        let field =
          { env = Fvar(func_var,ExprId.create ());
            env_fun_id = fun_id;
            env_var } in
        Flet(Strict,id',Fenv_field(field,ExprId.create ()),body,ExprId.create ()))
      free_vars body in
  Flet(Strict,func_var,func,body,ExprId.create ())

type inlining_kind =
  | Minimal
  | With_local_functions of Constants.constant_result

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

  let apply ~func ~args ~dbg ~tree toplevel node_data =
    begin match one_value_function analysis (expr_value func) with
      | None ->
        (* Format.eprintf "none: %a@." Printflambda.flambda func; *)
        tree
      | Some (f, ffunction) ->
        (* Format.eprintf "some: %a@." Printflambda.flambda func; *)
        match ffunction.kind with
        | Tupled -> tree (* TODO: tupled functions ! *)
        | Curried ->
          if ffunction.arity <= List.length args
          then
            let fun_id = f.fun_id in
            let ffunctions = find_funid f.closure_funs
                !functions_map in
            let ffunction = IdentMap.find fun_id.off_id ffunctions.funs in
            let should_go = match inlining_kind with
              | Minimal -> should_inline_minimal ffunction
              | With_local_functions constants ->
                should_inline constants toplevel ffunction args
            in
            if not ffunctions.recursives && should_go
            then
              let used_args = take_n ffunction.arity args in
              let remaining_args = drop_n ffunction.arity args in
              let call_result = inline_simple func fun_id ffunction used_args in
              match remaining_args with
              | [] -> call_result
              | _::_ ->
                (* Format.eprintf "partial: %a@." Printflambda.flambda func; *)
                Fapply(call_result, remaining_args, None, dbg, node_data)
            else tree
          else tree
    end
  in

  let rec mapper iter env tree = match tree with
    | Fclosure (ffuns, fv, eid) ->
      let fv = IdentMap.map (fun lam -> mapper iter env lam) fv in
      let ffuns =
        { ffuns with
          funs = IdentMap.map
              (fun ffun -> { ffun with body = mapper iter Deep ffun.body })
              ffuns.funs } in
      functions_map := FunMap.add ffuns.ident ffuns !functions_map;
      Fclosure (ffuns, fv, eid)
    | _ ->
      begin match iter env tree with
        | Fapply (func, args, _, dbg, eid) ->
          apply ~func ~args ~dbg ~tree env eid
        | tree -> tree
      end
  in

  Flambdautils.map2 mapper Toplevel lam


let used_variables_and_offsets tree =
  let vars = ref IdentSet.empty in
  let offs = ref OffsetSet.empty in
  let rec aux = function
    | Fenv_field({env_var = off}, _) (* env_fun_id also ? *)
    | Foffset(_, off, _) ->
      if not (OffsetSet.mem off !offs) then offs := OffsetSet.add off !offs
    | Fvar (id,_) ->
      if not (IdentSet.mem id !vars) then vars := IdentSet.add id !vars
    | Fclosure (funcs,_,_) ->
      IdentMap.iter (fun _ ffunc -> Flambdautils.iter_flambda aux ffunc.body)
        funcs.funs
    | _ -> ()
  in
  Flambdautils.iter_flambda aux tree;
  !vars, !offs

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
  let used_var,used_offset = used_variables_and_offsets tree in
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
             List.for_all (fun id -> IdentSet.mem id used_var) ffunction.params ||
             List.for_all (fun id -> OffsetSet.mem (offset id) used_offset) ffunction.params
          then tree
          else
            match remove_function_variables used_var fun_id ffunction with
            | None -> tree
            | Some (new_ident, indent_in_closure,
                    new_ffunction, cleaned_ffunction) ->
              let cleaned_ffunctions =
                { ident = FunId.create ();
                  funs = IdentMap.singleton new_ident cleaned_ffunction;
                  recursives = false;
                  closed = false;
                  unit = ffunctions.unit } in
              let new_ffunctions =
                { ident = ffunctions.ident;
                  funs = IdentMap.singleton fun_id new_ffunction;
                  recursives = false;
                  closed = false;
                  unit = ffunctions.unit } in
              let cleaned_closure =
                Fclosure(cleaned_ffunctions,IdentMap.empty,ExprId.create ()) in
              let fv = IdentMap.add indent_in_closure
                  (Fvar(new_ident,ExprId.create ())) fv in
              let new_closure = Fclosure(new_ffunctions,fv,ExprId.create ()) in
              let ret = Flet
                  (Strict,new_ident,
                   Foffset(cleaned_closure, offset new_ident, ExprId.create ()),
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

let extract_constants (alias:Constants.alias_result) tree =

  let constants = alias.Constants.constant_result in

  let get_alias id =
    try Some (IdentMap.find id alias.Constants.constant_alias)
    with Not_found -> None in

  let global_var, global_index = Flambdautils.global_index tree in

  (* let global_id id = *)
  (*   try *)
  (*     let gid = IdentTbl.find global_var id in *)
  (*     if gid.Ident.name = Compilenv.current_unit_name () *)
  (*     then Some gid *)
  (*     else None *)
  (*   with Not_found -> None in *)

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
    | Flet ( kind, id, lam, body, eid) ->
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
      match get_alias id with
      | Some alias ->
        IdentTbl.add renaming id alias;
        (* Some (id, Fvar(alias, Flambda.data lam)) *)
        Some(id,lam)
      | None ->
        match lam with
        | Fvar(_, _) ->
          Printf.printf "alias var %s\n%!" (Ident.unique_name id);
          assert false
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

  (* renaming applied only on value in the definition of a let *)
  let renaming_rval = IdentTbl.create 10 in

  let rec rename id =
    try rename (IdentTbl.find renaming id)
    with Not_found -> id in
  let rec rename_rval id =
    let id = (rename id) in
    try rename_rval (IdentTbl.find renaming_rval id)
    with Not_found -> id in
  let renaming tree =
    match tree with
    | Fvar(var,eid) ->
      (* let ren = rename var in *)
      (* Printf.printf "rebind %s => %s\n%!" *)
      (*   (Ident.unique_name var) (Ident.unique_name ren); *)
      Fvar(rename_rval var,eid)
    | Fprim(Pgetglobalfield(id,i),[], _, eid) ->
      if (id.Ident.name = Compilenv.current_unit_name ()) &&
         (IntTbl.mem global_index i)
      then
        let var = IntTbl.find global_index i in
        let var = rename_rval var in
        Fvar(var, eid)
      else tree

    | _ -> tree in

  let tree = Flambdautils.map2 mapper' () tree in

  (* (\* list fields of constants: field(i,lam) of a constant lam is *)
  (*    considered to be constant, so we need to do the dereferencing *)
  (*    here to avoid problems later (rebinding will have problems with *)
  (*    recursive cases). Dereferencing is done by adding a renaming *)
  (*    applied only to the expression (not to the let definition) *\) *)
  (* let fields = IdentTbl.create 10 in *)
  (* let list_fields (id,expr) = match expr with *)
  (*   | Fprim(Pmakeblock _, lams, _, _) -> *)
  (*     IdentTbl.add fields id lams *)
  (*   (\* TODO: find a solution for cases where a constant block commes *)
  (*      from a field acces or let alias... better constant analysis ? *\) *)
  (*   | _ -> () *)
  (* in *)
  (* List.iter list_fields !bindings; *)
  (* let deref (id,expr) = match expr with *)
  (*   (\* assumes ANF *\) *)
  (*   | Fprim(Pfield i, [Fvar (block_id',_)], _, _) -> *)
  (*     begin match global_id block_id' with *)
  (*       | Some _ -> () *)
  (*       | None -> *)
  (*         let block_id = rename block_id' in *)
  (*         let block = try IdentTbl.find fields block_id with *)
  (*           | Not_found -> *)
  (*             fatal_error (Printf.sprintf "can't find constant block %s -> %s" *)
  (*                            (Ident.unique_name block_id') *)
  (*                            (Ident.unique_name block_id)) *)
  (*         in *)
  (*         (\* assumes ANF *\) *)
  (*         match List.nth block i with *)
  (*         | Fvar(new_var, _) -> *)
  (*           let id' = rename id in *)
  (*           (\* Printf.printf "rename %s -> %s: %s, %s\n%!" *\) *)
  (*           (\*   (Ident.unique_name id) *\) *)
  (*           (\*   (Ident.unique_name id') *\) *)
  (*           (\*   (Ident.unique_name new_var) *\) *)
  (*           (\*   (Ident.unique_name block_id); *\) *)
  (*           IdentTbl.add renaming_rval id' new_var; *)
  (*         | Fconst _ -> (\* not recursive: nothing to do *\) *)
  (*           () *)
  (*         | _ -> fatal_error "not in ANF" *)
  (*     end *)
  (*   | _ -> () *)
  (* in *)
  (* List.iter deref !bindings; *)

  let bindings = List.fold_left (fun map (v',expr') ->
      let v = rename v' in
      let expr = Flambdautils.map_no_closure renaming expr' in
      (* Format.printf "%s %a@. => %s %a@." *)
      (*   (Ident.unique_name v') *)
      (*   Printflambda.flambda expr' *)
      (*   (Ident.unique_name v) *)
      (*   Printflambda.flambda expr; *)
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
    let count =
      try IdentTbl.find env id with
      | Not_found -> Zero
    in
    let count = match count with
      | Zero -> One
      | One | Many -> Many
      | Loop -> Loop in
    IdentTbl.replace env id count in

  let promote ~orig_env ~loop_env =
    IdentTbl.iter (fun id count ->
        (* Printf.printf "promote %s\n%!" (Ident.unique_name id); *)
        IdentTbl.replace orig_env id Loop) loop_env in

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
  (* if IdentTbl.length init_env <> 0 *)
  (* then begin *)
  (* (\* functions variables and closures are still there *\) *)
  (*   IdentTbl.iter (fun id _ -> Printf.printf "remaining %s\n%!" *)
  (*                     (Ident.unique_name id)) init_env; *)
  (*   assert false; *)
  (* end; *)
  global_count

let elim_let constant unpure_expr expr =
  let count = count_var expr in
  let should_elim id eid =
    let count = try Some (IdentTbl.find count id) with _ -> None in
    match count with
    | None | Some (Zero) ->
      not (ExprSet.mem eid unpure_expr.Purity.effectful_expr)
    | Some (Many | Loop) -> false
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

(** convert variables in closure to parameters: add an intermediate
    function to call the closed function using the closure

{[
  let v = ... in
  let const = ... in (* a constant value *)
  let rec f x =
    ... const ...
    ... v ...
    ... f y ...
  in
  ... f z ...
]}

is transformed to

{[
  let v = ... in
  let const = ... in (* a constant value *)
  let rec f' v x =
    ... const ...
    ... v ...
    ... f v y ...
  in
  let f x = f' v x in
  ... f z ...
]}

f' has only constants in its closure hence it can be compiled to a
constant: no allocation needed for its closure.

If f is known at some point, it is certain to be inlined, sometimes
allowing to completely remove the allocation of the closure f.

*)

let should_unclose constants ffunctions =

  (* cases where this is not possible:
     {[let v = ... in
       let rec f x =
         List.map f v ]}

     {[let v = ... in
       let rec f x = incr v; f ]}

     basicaly: f used as a variable not in a Fapply
  *)

  let no_escaping_functions _ ffun =
    if ffun.kind = Tupled
    then false (* TODO: tupled functions ! *)
    else
      let rec check iter env tree = match tree with
        | Fvar(id, _) ->
          if IdentMap.mem id ffunctions.funs
          then raise Exit
        | Fapply(Fvar(id, _), args, _, _, _) ->
          List.iter (check iter env) args
        | tree -> iter env tree
      in
      try
        Flambdautils.iter2_flambda check () ffun.body;
        true
      with Exit -> false
  in

  (* If the closure is a constant, there is nothing to do.

     There is no particular reason to restrict to recursive functions,
     but it is most certainly a win for them.

     TODO: do not do that if that prevents tail calls (too many arguments)
     TODO: find heuristics to determine when this is profitable
  *)
  (* ffunctions.recursives && *)
  FunSet.mem ffunctions.ident constants.Constants.not_constant_closure &&
  IdentMap.for_all no_escaping_functions ffunctions.funs

let unclose_functions constants ffunctions
    (fv':Flambda.ExprId.t Flambda.flambda Flambda.IdentMap.t) =
  let nid = ExprId.create in

  (* we separate constants and not constants in the closure:
     constants will stay, others will be passed as parameters. *)
  let not_const_fv, const_fv =
    List.partition (fun (id,lam) ->
        IdentSet.mem id constants.Constants.not_constant_id)
      (IdentMap.bindings fv') in

  (* if there is no real variable in the closure then there is nothing
     to do (and this should have been handled by should_unclose) *)
  assert(not_const_fv <> []);

  let const_fv' = IdentMap.of_list const_fv in
  let const_fv_set = IdentSet.of_list (List.map fst const_fv) in
  let const_fv_renaming = IdentMap.mapi (fun id _ -> Ident.rename id) const_fv' in

  let added_params = List.map fst not_const_fv in

  (* id of the new closed version of functions *)
  let fun_renaming = IdentMap.mapi (fun id _ -> Ident.rename id) ffunctions.funs in

  let replace_calls args_renaming tree =
    let var_renaming = IdentMap.disjoint_union const_fv_renaming args_renaming in
    let mapper tree = match tree with

      | Fvar(id, eid) ->
        (* rename constant access to the closure to their renamed version,
           other are renamed to use function parameters *)
        (try Fvar(IdentMap.find id var_renaming, eid)
         with Not_found -> tree)

      | Fapply(Fvar(id,eid1), args, _, dbg, eid2) ->
        (* replace recursive calls by calls to the new version (with
           more parameters) *)
        if not (IdentMap.mem id fun_renaming)
        then tree
        else begin
          let new_id = IdentMap.find id fun_renaming in
          let new_args = List.map (fun id ->
              Fvar(IdentMap.find id args_renaming, nid ()))
              added_params in
          Fapply(Fvar(new_id,eid1), new_args @ args, None, dbg, eid2)
        end

      | _ -> tree

    in

    (* the identifiers for closure variables and renamed functions
       cannot appear in the body of local functions: we do not need to
       map on them (hence the use of map_no_closure) *)
    let tree = Flambdautils.map_no_closure mapper tree in

    (* only checking that no variable is left: this should have been
       ensured by should_unclose, but who knows...
       TODO should be run only in compiler debug mode *)
    let check tree = match tree with
      | Fvar(id, _) ->
        assert(not (IdentMap.mem id fun_renaming));
      | _ -> ()
    in
    Flambdautils.iter_flambda check tree;
    tree
  in

  let internal_ffunction id ffunction =
    let args_renaming =
      List.map (fun id ->
          let rid = Ident.rename id in
          (* Printf.printf "%s -> %s\n%!" (Ident.unique_name id) (Ident.unique_name rid); *)
          id, rid) (added_params @ ffunction.params) in
    { label = Flambdagen.make_function_lbl id;
      kind = ffunction.kind;
      arity = List.length added_params + ffunction.arity;
      params = List.map snd args_renaming;
      closure_params = IdentSet.map (IdentMap.rename const_fv_renaming)
          (IdentSet.inter const_fv_set ffunction.closure_params);
      body = replace_calls (IdentMap.of_list args_renaming) ffunction.body;
      dbg = ffunction.dbg
    } in

  (* fun_renaming: (id, new_id, clos_id) list
     id: original id in the closure
     new_id: id of the closed version of the function (in the closure)
     clos_id: variable of the closed version of the function in the
     closure of the external one *)
  let internal_ffunctions, fun_renaming =
    let ident = FunId.create ?name:(FunId.name ffunctions.ident) () in
    let funs_list = List.map (fun (id,ffunction) ->
        let new_fun_id = IdentMap.find id fun_renaming in
        id, new_fun_id, internal_ffunction new_fun_id ffunction)
        (IdentMap.bindings ffunctions.funs) in
    let funs = List.fold_left (fun map (_,new_fun_id, ffunction) ->
        IdentMap.add new_fun_id ffunction map)
        IdentMap.empty funs_list in
    let fun_renaming =
      List.map (fun (id,new_fun_id, _) -> (id,new_fun_id, Ident.rename id))
        funs_list in
    { ident; funs; recursives = ffunctions.recursives;
      closed = false;
      unit = ffunctions.unit }, fun_renaming
  in

  let external_ffunction ffunction internal_fun_id =
    let params = List.map (fun id -> Fvar(id,nid ()))
        (added_params @ ffunction.params) in
    let body =
      Fapply( Fvar(internal_fun_id, nid ()),
              params,
              None, (* We could fill it directly, but there will
                       be a pass doing that later *)
              Debuginfo.none, (* Should we take the one from
                                 the functions declaration ?
                                 TODO: test stack trace *)
              nid () )
    in

    let closure_params =
      IdentSet.union
        ffunction.closure_params
        (IdentSet.of_list (List.map (fun (_,_,clos_id) -> clos_id) fun_renaming))
    in

    { ffunction with
      body;
      closure_params }
  in

  let external_ffunctions =
    let funs = List.fold_left (fun map (id, _, clos_id) ->
        let ffunction = IdentMap.find id ffunctions.funs in
        IdentMap.add id (external_ffunction ffunction clos_id) map)
        IdentMap.empty fun_renaming in
    { ident = ffunctions.ident; funs;
      recursives = false; closed = false;
      unit = ffunctions.unit } in

  let closure_id = Ident.create "unclosed" in
  let external_fv' =
    List.fold_left (fun fv (_,new_id, clos_id) ->
       let expr =
         Flet(Strict, new_id,
              (* stupid hack to give a name to the expression and ensure
                 this is the same as the function id.
                 TODO: This should be removed when flambdainfo is fixed *)
              Foffset(Fvar(closure_id,nid ()), offset new_id, nid ()),
              Fvar(new_id, nid ()), nid ())
       in
       IdentMap.add clos_id expr fv)
      fv' fun_renaming
  in

  let renamed_const_fv' =
    IdentMap.of_list
      (List.map (fun (id, lam) -> IdentMap.rename const_fv_renaming id, lam)
         const_fv) in

  let internal_closure =
    Fclosure(internal_ffunctions, renamed_const_fv', nid ()) in
  let external_closure =
    Fclosure(external_ffunctions, external_fv', nid ()) in

  Flet(Strict, closure_id, internal_closure,
       external_closure, nid ())

let unclose constants tree =
  let mapper tree = match tree with
    | Fclosure(ffunctions,fv,eid) ->
      if should_unclose constants ffunctions
      then begin
        (* let k, _ = IdentMap.min_binding ffunctions.funs in *)
        (* Printf.printf "unclose %s\n%!" (Ident.unique_name k); *)
        unclose_functions constants ffunctions fv
      end
      else tree
    | _ -> tree
  in
  Flambdautils.map mapper tree
