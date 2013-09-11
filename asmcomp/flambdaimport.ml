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
    ex_offset_fv = exported.ex_offset_fv; }

let reverse_symbol_map exported =
  EidMap.fold (fun id sym map -> SymbolMap.add sym id map)
    exported.ex_id_symbol SymbolMap.empty

let merge e1 e2 =
  (* Format.printf "%a@." (EidMap.print (fun _ _ -> ())) e1.ex_values; *)
  (* Format.printf "%a@." (EidMap.print (fun _ _ -> ())) e2.ex_values; *)
  (* let e = EidMap.disjoint_union e1.ex_values e2.ex_values in *)
  (* Format.printf "%a@." (EidMap.print (fun _ _ -> ())) e; *)
  (* Format.printf "%a@." (OffsetMap.print (fun _ _ -> ())) e1.ex_offset; *)
  (* Format.printf "%a@." (OffsetMap.print (fun _ _ -> ())) e2.ex_offset; *)
  (* let e = OffsetMap.disjoint_union e1.ex_offset e2.ex_offset in *)
  (* Format.printf "%a@." (OffsetMap.print (fun _ _ -> ())) e; *)
  { ex_values = EidMap.disjoint_union e1.ex_values e2.ex_values;
    ex_globals = IdentMap.disjoint_union e1.ex_globals e2.ex_globals;
    ex_functions = FunMap.disjoint_union e1.ex_functions e2.ex_functions;
    ex_id_symbol = EidMap.disjoint_union e1.ex_id_symbol e2.ex_id_symbol;
    ex_offset_fun = OffsetMap.disjoint_union e1.ex_offset_fun e2.ex_offset_fun;
    ex_offset_fv = OffsetMap.disjoint_union e1.ex_offset_fv e2.ex_offset_fv }

let merge_symbol_map m1 m2 = SymbolMap.disjoint_union m1 m2
