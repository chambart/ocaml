open Ext_types
open Flambda

module StringMap = Map.Make(String)

module type Param = sig
  val sb : Ident.t IdentMap.t
  val fv : Offset.t OffsetMap.t
  val func : Offset.t OffsetMap.t
end

module Subst(P:Param) = struct
  let offset_subst_table = ref P.fv
  let fun_offset_subst_table = ref P.func

  let add_offset id id' =
    let off_unit = Compilenv.current_unit () in
    let off_id' = { off_id = id'; off_unit } in
    offset_subst_table := OffsetMap.add id off_id' !offset_subst_table

  let add_fun_offset id id' =
    let off_unit = Compilenv.current_unit () in
    let off_id' = { off_id = id'; off_unit } in
    fun_offset_subst_table := OffsetMap.add id off_id' !fun_offset_subst_table

  type sb = { var : Ident.t IdentMap.t;
              sym : Ident.t SymbolMap.t }

  let empty_sb = { var = IdentMap.empty; sym = SymbolMap.empty }

  let add_var id id' sb = { sb with var = IdentMap.add id id' sb.var }
  let add_sym sym id' sb = { sb with sym = SymbolMap.add sym id' sb.sym }

  let sb_union sb1 sb2 =
    { var = IdentMap.disjoint_union sb1.var sb2.var;
      sym = SymbolMap.disjoint_union sb1.sym sb2.sym }

  let subst_var sb id =
    try IdentMap.find id sb.var with Not_found -> id

  let rec aux exn_sb (sb:sb) orig = match orig with
    | Fvar (id,annot) ->
      begin try Fvar(IdentMap.find id sb.var,annot) with
        | Not_found -> orig end
    | Fsymbol (sym,annot) ->
      begin try
        Fvar(SymbolMap.find sym sb.sym,annot)
      with
        | Not_found -> orig end
    | Fconst _ -> orig
    | Fapply (funct, args, direct, dbg, annot) ->
      let args = aux_list exn_sb sb args in
      let funct = aux exn_sb sb funct in
      let direct = match direct with
        | None -> None
        | Some offset ->
          try
            let offset = OffsetMap.find offset !fun_offset_subst_table in
            (* Format.printf "subst offset %a@." Offset.print offset; *)
            Some offset
          with
          | Not_found ->
            (* Format.printf "no subst offset %a@." Offset.print offset; *)
            direct
      in
      Fapply (funct, args, direct, dbg, annot)
    | Fclosure (ffuns, fv, spec_args, annot) ->
      let sb_fv, fv = subst_free_vars ffuns.unit fv in
      let ffuns, sb_params = aux_closure sb_fv ffuns in
      let spec_args =
        IdentMap.map_keys (subst_var sb_params)
          (IdentMap.map (subst_var sb) spec_args) in
      Fclosure (ffuns, IdentMap.map (aux exn_sb sb) fv, spec_args, annot)
    | Foffset (flam, off, rel, annot) ->
      let flam = aux exn_sb sb flam in
      let off =
        try OffsetMap.find off !fun_offset_subst_table with
        | Not_found -> off in
      let rel =
        match rel with
        | None -> rel
        | Some rel ->
            try Some (OffsetMap.find rel !fun_offset_subst_table) with
            | Not_found -> Some rel in
      Foffset (flam, off, rel, annot)

    | Fenv_field ({ env = flam; env_var = off; env_fun_id }, annot) ->
      let flam = aux exn_sb sb flam in
      let off =
        try
          let v = OffsetMap.find off !offset_subst_table in
          (* Format.printf "found %a@." Offset.print off; *)
          v
        with
        | Not_found ->
          (* Format.printf "not found %a@." Offset.print off; *)
          off in
      let env_fun_id =
        try
          let e = OffsetMap.find env_fun_id !fun_offset_subst_table in
          (* Format.printf "rename env_fun_id %a -> %a@." *)
          (*   Offset.print env_fun_id *)
          (*   Offset.print e; *)
          e
        with
        | Not_found ->
          (* Format.printf "not found env_fun_id %a@." *)
          (*   Offset.print env_fun_id; *)
          env_fun_id in
      Fenv_field ({ env = flam; env_var = off; env_fun_id }, annot)

    | Flet(str, id, lam, body, annot) ->
      let lam = aux exn_sb sb lam in
      let id' = Ident.rename id in
      (* Printf.printf "let rename: %s => %s\n%!" (Ident.unique_name id) (Ident.unique_name id'); *)
      let sb = add_var id id' sb in
      Flet(str, id', lam, aux exn_sb sb body, annot)
    | Fletrec(defs, body, annot) ->
      let new_ids = List.map Ident.rename (List.map fst defs) in
      let sb = List.fold_left2 (fun sb (id,def) id' ->
          (* Printf.printf "let rec rename: %s => %s\n%!" (Ident.unique_name id) (Ident.unique_name id'); *)
          add_var id id' sb) sb defs new_ids in
      let defs = List.map2 (fun id' (_,def) -> id', aux exn_sb sb def) new_ids defs in
      let body = aux exn_sb sb body in
      Fletrec(defs, body, annot)

    | Fprim(p, args, dbg, annot) ->
      Fprim(p, aux_list exn_sb sb args, dbg, annot)
    | Fstaticfail(i, args, annot) ->
      let i' = IntMap.find i exn_sb in
      Fstaticfail(i', aux_list exn_sb sb args, annot)
    | Fifthenelse(arg, ifso, ifnot, annot) ->
      Fifthenelse(aux exn_sb sb arg, aux exn_sb sb ifso, aux exn_sb sb ifnot, annot)
    | Fsequence(lam1, lam2, annot) ->
      Fsequence(aux exn_sb sb lam1, aux exn_sb sb lam2, annot)
    | Fwhile(cond, body, annot) ->
      Fwhile(aux exn_sb sb cond, aux exn_sb sb body, annot)
    | Fsend(kind, met, obj, args, dbg, annot) ->
      Fsend(kind, aux exn_sb sb met, aux exn_sb sb obj, aux_list exn_sb sb args, dbg, annot)
    | Ffor(id, lo, hi, dir, body, annot) ->
      let id' = Ident.rename id in
      let sb = add_var id id' sb in
      Ffor(id', aux exn_sb sb lo, aux exn_sb sb hi, dir, aux exn_sb sb body, annot)
    | Fassign(id, lam, annot) ->
      let lam = aux exn_sb sb lam in
      begin try Fassign(IdentMap.find id sb.var, lam, annot) with
        | Not_found -> orig end
    | Fcatch (i, vars, body, handler, annot) ->
      let i' = Lambda.next_raise_count () in
      let body =
        let exn_sb = IntMap.add i i' exn_sb in
        aux exn_sb sb body in
      let vars' = List.map Ident.rename vars in
      let sb = List.fold_left2 (fun sb id id' ->
          add_var id id' sb) sb vars vars' in
      let handler = aux exn_sb sb handler in
      Fcatch (i', vars', body, handler, annot)
    | Ftrywith(body, id, handler, annot) ->
      let body = aux exn_sb sb body in
      let id' = Ident.rename id in
      let sb = add_var id id' sb in
      let handler = aux exn_sb sb handler in
      Ftrywith(body, id', handler, annot)
    | Fswitch(arg, sw, annot) ->
      let sw =
        { sw with
          fs_failaction = Misc.may_map (aux exn_sb sb) sw.fs_failaction;
          fs_consts = List.map (fun (i,v) -> i, aux exn_sb sb v) sw.fs_consts;
          fs_blocks = List.map (fun (i,v) -> i, aux exn_sb sb v) sw.fs_blocks; } in
      Fswitch(aux exn_sb sb arg, sw, annot)
    | Funreachable _ -> orig

  and subst_free_vars unit fv : sb * 'a =
    let sb, fv' =
      IdentMap.fold (fun id value (sb, map) ->
        let id' = Ident.rename id in
        (* Printf.printf "rename closure params: %s => %s\n%!" (Ident.unique_name id) (Ident.unique_name id'); *)
        add_offset { off_id = id; off_unit = unit } id';
        let map = IdentMap.add id' value map in
        let sb = add_var id id' sb in
        sb, map) fv (empty_sb, IdentMap.empty) in
    sb, fv'

  and aux_closure (sb_fv:sb) ffuns =
    let fun_id_subst =
      IdentMap.fold (fun id _ map ->
        let id' = Ident.rename id in
        let off = { off_id = id; off_unit = ffuns.unit } in
        add_fun_offset off id';
        (* Format.printf "rename fun: %a => %s@." *)
        (*   Offset.print off (Ident.unique_name id'); *)
        add_var id id' map)
        ffuns.funs sb_fv in
    let aux_ffunction orig_id fun_id ffun =
      let label = Flambdagen.make_function_lbl fun_id in
      (* only handle simply recursive functions: TODO multiple ones *)
      let closure_symbol = Compilenv.closure_symbol
          { off_unit = ffuns.unit; off_id = orig_id } in
      let fun_id_subst = add_sym closure_symbol fun_id fun_id_subst in
      let sb_param, params = List.fold_right (fun id (sb,l) ->
          let id' = Ident.rename id in
          (* Printf.printf "rename params: %s => %s\n%!" (Ident.unique_name id) (Ident.unique_name id'); *)
          let sb = add_var id id' sb in
          let l = id'::l in
          sb,l) ffun.params (empty_sb, []) in
      let sb = sb_union sb_param fun_id_subst in
      let closure_params = IdentSet.fold (fun id set -> IdentSet.add (IdentMap.find id sb.var) set)
          ffun.closure_params IdentSet.empty in
      let kept_params = IdentSet.map (fun id -> IdentMap.find id sb.var) ffun.kept_params in
      let body = aux IntMap.empty sb ffun.body in
      { ffun with label; params; closure_params; kept_params; body }, sb_param
    in
    let funs, sb_params =
      IdentMap.fold (fun id ffun (map, sb_params) ->
        let id' = IdentMap.find id fun_id_subst.var in
        let ffunc, sb_param = aux_ffunction id id' ffun in
        let sb_params = sb_union sb_param sb_params in
        IdentMap.add id' ffunc map, sb_params)
        ffuns.funs (IdentMap.empty, empty_sb) in
    { ident = FunId.create ((Compilenv.current_unit_name ()));
      (* this ident is used only in value approximations,
         no need to propagate it *)
      funs;
      recursives = ffuns.recursives;
      closed = false;
      unit = Compilenv.current_unit () }, sb_params

  and aux_list exn_sb sb l = List.map (aux exn_sb sb) l

  let expr lam =
    aux IntMap.empty { empty_sb with var = P.sb } lam

  let closures clos =
    List.map (aux_closure { empty_sb with var = P.sb }) clos

end

let substitute sb lam =
  let module P = struct
    let sb = sb
    let fv = OffsetMap.empty
    let func = OffsetMap.empty
  end in
  let module S = Subst(P) in
  S.expr lam

let substitute_closures sb clos =
  let module P = struct
    let sb = sb
    let fv = OffsetMap.empty
    let func = OffsetMap.empty
  end in
  let module S = Subst(P) in
  List.map fst (S.closures clos)

type context =
  { fv : Offset.t OffsetMap.t;
    func : Offset.t OffsetMap.t }

let empty_context =
  { fv = OffsetMap.empty;
    func = OffsetMap.empty }

let substitute' sb context lam =
  let module P = struct
    let sb = sb
    let fv = context.fv
    let func = context.func
  end in
  let module S = Subst(P) in
  let expr = S.expr lam in
  let context = { fv = !S.offset_subst_table;
                  func = !S.fun_offset_subst_table } in
  expr, context
