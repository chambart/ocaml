open Flambda

module StringMap = Map.Make(String)

module type Param = sig
  val sb : IdentSet.elt IdentMap.t
end

module Subst(P:Param) = struct
  let offset_subst_table = ref IdentMap.empty
  let fun_label_subst_table = ref StringMap.empty

  let rec aux exn_sb sb orig = match orig with
    | Fvar (id,annot) ->
      begin try Fvar(IdentMap.find id sb,annot) with
        | Not_found -> orig end
    | Fsymbol _ | Fconst _ -> orig
    | Fapply (funct, args, direct, dbg, annot) ->
      let args = aux_list exn_sb sb args in
      let funct = aux exn_sb sb funct in
      let direct = match direct with
        | None -> None
        | Some (lbl,closed) ->
          try
            let label = StringMap.find (lbl:>string) !fun_label_subst_table in
            Some (label,closed)
          with
          | Not_found -> direct
      in
      Fapply (funct, args, direct, dbg, annot)
    | Fclosure (ffuns, fv, annot) ->
      let sb_fv, fv = subst_free_vars fv in
      Fclosure (aux_closure sb_fv ffuns,
          IdentMap.map (aux exn_sb sb) fv, annot)
    | Foffset (flam, off, annot) ->
      let flam = aux exn_sb sb flam in
      let off =
        try IdentMap.find off !offset_subst_table with
        | Not_found -> off in
      Foffset (flam, off, annot)

    | Fenv_field ({ env = flam; env_var = off; env_fun_id }, annot) ->
      let flam = aux exn_sb sb flam in
      let off =
        try IdentMap.find off !offset_subst_table with
        | Not_found ->
          (* Printf.printf "not found %s\n%!" (Ident.unique_name off); *)
          off in
      let env_fun_id =
        try
          let e = IdentMap.find env_fun_id !offset_subst_table in
          (* Printf.printf "rename %s -> %s\n%!" *)
          (*   (Ident.unique_name env_fun_id) *)
          (*   (Ident.unique_name e); *)
          e
        with
        | Not_found ->
          (* Printf.printf "not found fun %s\n%!" (Ident.unique_name env_fun_id); *)
          env_fun_id in
      Fenv_field ({ env = flam; env_var = off; env_fun_id }, annot)

    | Flet(str, id, lam, body, annot) ->
      let lam = aux exn_sb sb lam in
      let id' = Ident.rename id in
      (* Printf.printf "let rename: %s => %s\n%!" (Ident.unique_name id) (Ident.unique_name id'); *)
      let sb = IdentMap.add id id' sb in
      Flet(str, id', lam, aux exn_sb sb body, annot)
    | Fletrec(defs, body, annot) ->
      let new_ids = List.map Ident.rename (List.map fst defs) in
      let sb = List.fold_left2 (fun sb (id,def) id' ->
          (* Printf.printf "let rec rename: %s => %s\n%!" (Ident.unique_name id) (Ident.unique_name id'); *)
          IdentMap.add id id' sb) sb defs new_ids in
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
      let sb = IdentMap.add id id' sb in
      Ffor(id', aux exn_sb sb lo, aux exn_sb sb hi, dir, aux exn_sb sb body, annot)
    | Fassign(id, lam, annot) ->
      let lam = aux exn_sb sb lam in
      begin try Fassign(IdentMap.find id sb, lam, annot) with
        | Not_found -> orig end
    | Fcatch (i, vars, body, handler, annot) ->
      let i' = Lambda.next_raise_count () in
      let body =
        let exn_sb = IntMap.add i i' exn_sb in
        aux exn_sb sb body in
      let vars' = List.map Ident.rename vars in
      let sb = List.fold_left2 (fun sb id id' ->
          IdentMap.add id id' sb) sb vars vars' in
      let handler = aux exn_sb sb handler in
      Fcatch (i', vars', body, handler, annot)
    | Ftrywith(body, id, handler, annot) ->
      let body = aux exn_sb sb body in
      let id' = Ident.rename id in
      let sb = IdentMap.add id id' sb in
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

  and subst_free_vars fv =
    let sb, fv' =
      IdentMap.fold (fun id value (sb, map) ->
        let id' = Ident.rename id in
        (* Printf.printf "rename closure params: %s => %s\n%!" (Ident.unique_name id) (Ident.unique_name id'); *)
        offset_subst_table := IdentMap.add id id' !offset_subst_table;
        let map = IdentMap.add id' value map in
        let sb = IdentMap.add id id' sb in
        sb, map) fv (IdentMap.empty, IdentMap.empty) in
    sb, fv'

  and aux_closure sb_fv ffuns =
    let fun_id_subst =
      IdentMap.fold (fun id _ map ->
        let id' = Ident.rename id in
        (* Printf.printf "rename: %s => %s\n%!" (Ident.unique_name id) (Ident.unique_name id'); *)
        offset_subst_table := IdentMap.add id id' !offset_subst_table;
        IdentMap.add id id' map)
        ffuns.funs sb_fv in
    let aux_ffunction fun_id ffun =
      let label = Flambdagen.make_function_lbl fun_id in
      fun_label_subst_table :=
        StringMap.add (ffun.label:>string) label !fun_label_subst_table;
      let sb, params = List.fold_right (fun id (sb,l) ->
          let id' = Ident.rename id in
          (* Printf.printf "rename params: %s => %s\n%!" (Ident.unique_name id) (Ident.unique_name id'); *)
          let sb = IdentMap.add id id' sb in
          let l = id'::l in
          sb,l) ffun.params (fun_id_subst, []) in
      let closure_params = IdentSet.fold (fun id set -> IdentSet.add (IdentMap.find id sb) set)
          ffun.closure_params IdentSet.empty in
      let body = aux IntMap.empty sb ffun.body in
      { ffun with label; params; closure_params; body }
    in
    let funs =
      IdentMap.fold (fun id ffun map ->
        let id' = IdentMap.find id fun_id_subst in
        IdentMap.add id' (aux_ffunction id' ffun) map)
        ffuns.funs IdentMap.empty in
    { ident = FunId.create ();
      (* this ident is used only in value approximations,
         no need to propagate it *)
      funs;
      recursives = ffuns.recursives }

  and aux_list exn_sb sb l = List.map (aux exn_sb sb) l

  let expr lam =
    let res = aux IntMap.empty P.sb lam in
    res, !offset_subst_table

  let closures clos =
    let res = List.map (aux_closure P.sb) clos in
    res, !offset_subst_table

end

let substitute sb lam =
  let module P = struct let sb = sb end in
  let module S = Subst(P) in
  S.expr lam

let substitute_closures sb clos =
  let module P = struct let sb = sb end in
  let module S = Subst(P) in
  S.closures clos
