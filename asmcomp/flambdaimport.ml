open Flambda
open Flambdaexport

let import exported =
  let closure_id_map = FunMap.map (fun _ -> FunId.create ~name:"imported" ())
      exported.ex_functions in
  let eid_map = EidMap.map (fun _ ->
      let e = ExportId.create ~name:"imported" () in
      (* Format.printf "get %a@." ExportId.print e; *)
      e)
      exported.ex_values in
  let rename = function
    | Value_unknown as v -> v
    | Value_id id -> Value_id (EidMap.rename eid_map id)
  in
  let closures = FunTbl.create 10 in
  (* let subst = ref IdentMap.empty in *)
  (* let add_rename_id id = *)
  (*   try IdentMap.find id !subst with *)
  (*   | Not_found -> *)

  (*     id *)
  (*     (\* TODO: correct substitution !!! *\) *)

  (*     (\* let new_id = Ident.rename id in *\) *)
  (*     (\* subst := IdentMap.add id new_id !subst; *\) *)
  (*     (\* new_id *\) *)

  (* in *)
  let map_closure closure =
    try FunTbl.find closures closure.closure_id with
    | Not_found ->
      { closure_id = FunMap.rename closure_id_map closure.closure_id;
        bound_var = OffsetMap.map rename closure.bound_var }
  in
  let rename_val = function
    | Value_int _ | Value_constptr _ | Value_predef_exn _ as v -> v
    | Value_symbol sym as v ->
      (* Format.printf "get val_sym %a@." Symbol.print sym; *)
      v
    | Value_block (tag, fields) ->
      Value_block (tag, Array.map rename fields)
    | Value_closure {fun_id; closure} ->
      Value_closure {fun_id = fun_id;
                     closure = map_closure closure }
  in
  let ex_id_symbol =
    List.map (fun (id, f) -> EidMap.rename eid_map id, f)
      (EidMap.bindings exported.ex_id_symbol) in
  let ex_values =
    List.map (fun (id, v) -> EidMap.rename eid_map id, rename_val v)
      (EidMap.bindings exported.ex_values) in
  let ex_functions =
    let l =
      List.map (fun (id, f) -> { f with ident = FunMap.rename closure_id_map id})
        (FunMap.bindings exported.ex_functions) in
    (* Maybe substitution is not nescessary *)
    (* let l, _ = Flambdasubst.substitute_closures !subst l in *)
    List.fold_left (fun map f -> FunMap.add f.ident f map) FunMap.empty l in
  { ex_values = EidMap.of_list ex_values;
    ex_globals = IdentMap.map rename exported.ex_globals;
    ex_functions = ex_functions;
    ex_id_symbol = EidMap.of_list ex_id_symbol;
    ex_offset_fun = exported.ex_offset_fun;
    ex_offset_fv = exported.ex_offset_fv;
    ex_constants = exported.ex_constants }

let reverse_symbol_map exported =
  EidMap.fold (fun id sym map -> SymbolMap.add sym id map)
    exported.ex_id_symbol SymbolMap.empty

let merge e1 e2 =
  (* print_endline "values"; *)
  (* let _ = EidMap.disjoint_union e1.ex_values e2.ex_values in *)
  (* print_endline "globals"; *)
  (* let _ = IdentMap.disjoint_union e1.ex_globals e2.ex_globals in *)
  (* print_endline "functions"; *)
  (* let _ = FunMap.disjoint_union e1.ex_functions e2.ex_functions in *)
  (* print_endline "symbols"; *)
  (* let _ = EidMap.disjoint_union e1.ex_id_symbol e2.ex_id_symbol in *)
  (* print_endline "offset_fun"; *)
  (* let _ = OffsetMap.disjoint_union e1.ex_offset_fun e2.ex_offset_fun in *)
  (* print_endline "offset_fv"; *)
  (* let _ = OffsetMap.disjoint_union e1.ex_offset_fv e2.ex_offset_fv in *)

  (* Format.printf "%a@." (EidMap.print (fun _ _ -> ())) e1.ex_values; *)
  (* Format.printf "%a@." (EidMap.print (fun _ _ -> ())) e2.ex_values; *)
  (* let e = EidMap.disjoint_union e1.ex_values e2.ex_values in *)
  (* Format.printf "%a@." (EidMap.print (fun _ _ -> ())) e; *)
  (* Format.printf "%a@." (OffsetMap.print (fun _ _ -> ())) e1.ex_offset; *)
  (* Format.printf "%a@." (OffsetMap.print (fun _ _ -> ())) e2.ex_offset; *)
  (* let e = OffsetMap.disjoint_union e1.ex_offset e2.ex_offset in *)
  (* Format.printf "%a@." (OffsetMap.print (fun _ _ -> ())) e; *)
  { ex_values = EidMap.last_union e1.ex_values e2.ex_values;
    (* ex_globals = IdentMap.disjoint_union e1.ex_globals e2.ex_globals; *)
    ex_globals = IdentMap.empty;
    ex_functions = FunMap.last_union e1.ex_functions e2.ex_functions;
    ex_id_symbol = EidMap.last_union e1.ex_id_symbol e2.ex_id_symbol;
    ex_offset_fun = OffsetMap.last_union e1.ex_offset_fun e2.ex_offset_fun;
    ex_offset_fv = OffsetMap.last_union e1.ex_offset_fv e2.ex_offset_fv;
    ex_constants = SymbolSet.union e1.ex_constants e2.ex_constants }

(* TODO FIX: shouldn't use last_union: should use disjoint_union
   and have no clash! *)

let prefix_offset off global_lbl =
  { off with off_unit = global_lbl ^ "_" ^ off.off_unit }

let map_ffuns unit_lbls units global_id global_lbl funs =
  (* let import_offset off = *)
  (*   None *)
  (*   (\* there is no reason to prefix something like that... *\) *)

  (*   (\* if StringSet.mem off.off_unit unit_lbls *\) *)
  (*   (\* then Some (prefix_offset off global_lbl) *\) *)
  (*   (\* else None *\) *)
  (* in *)
  let mapper = function
    | Fsymbol ((modul,sym),d) as v ->
      if IdentSet.mem modul units
      then Fsymbol ((global_id,sym),d)
      else v

    (* | Foffset (f,off,d) as v -> *)
    (*   begin match import_offset off with *)
    (*     | None -> v *)
    (*     | Some off -> Foffset (f,off,d) *)
    (*   end *)

    (* | Fapply (f,arg,off_opt,dbg,d) as v-> *)
    (*   begin match Misc.may_map import_offset off_opt with *)
    (*     | None | Some None -> v *)
    (*     | Some off_opt -> Fapply (f,arg,off_opt,dbg,d) *)
    (*   end *)

    (* | Fenv_field (env_field, d) as v -> *)
    (*   begin match import_offset env_field.env_fun_id, *)
    (*               import_offset env_field.env_var with *)
    (*   | None, None -> v *)
    (*   | None, Some _ | Some _, None -> assert false *)
    (*   | Some env_fun_id, Some env_var -> *)
    (*     Fenv_field ({ env_field with env_fun_id; env_var }, d) *)
    (*   end *)

    | v -> v in
  let aux ffun = { ffun with body = Flambdautils.map mapper ffun.body } in
  (* print_endline funs.unit; *)
  { funs with
    unit = funs.unit; (* global_lbl; *)
    funs = IdentMap.map aux funs.funs }

let import_pack units unit_lbls global_lbl global_id unit =
  let map_val = function
    | Value_symbol (modul,sym) as v ->
      if IdentSet.mem modul units
      then Value_symbol (global_id,sym)
      else v
    (* | Value_closure ({ fun_id; closure } as clos) -> *)
    (*   Value_closure { clos with *)
    (*                   fun_id = { fun_id with off_unit = global_lbl } } *)
    | v -> v
  in
  let map_funs = map_ffuns unit_lbls units global_id global_lbl in
  (* let import_offset off = *)
  (*   off *)
  (*   (\* if StringSet.mem off.off_unit unit_lbls *\) *)
  (*   (\* then prefix_offset off global_lbl *\) *)
  (*   (\* else off *\) *)
  (* in *)
  (* let map_offset map = *)
  (*   OffsetMap.fold (fun off v acc -> *)
  (*       OffsetMap.add (import_offset off) v acc) map OffsetMap.empty in *)
  let ex_functions = FunMap.map map_funs unit.ex_functions in
  let ex_values = EidMap.map map_val unit.ex_values in
  let ex_id_symbol =
    EidMap.map (fun (_,lbl) -> global_id, lbl) unit.ex_id_symbol in
  let ex_constants =
    SymbolSet.map (fun (_,lbl) -> global_id, lbl) unit.ex_constants in
  let unit = { unit with
               ex_id_symbol;
               ex_values;
               ex_functions;
               (* ex_offset_fun = map_offset unit.ex_offset_fun; *)
               (* ex_offset_fv = map_offset unit.ex_offset_fv *)
               ex_constants;
             } in
  unit

  (* let r = merge unit pack in *)
  (* { r with ex_globals = pack.ex_globals } *)


let merge_symbol_map m1 m2 = SymbolMap.last_union m1 m2
