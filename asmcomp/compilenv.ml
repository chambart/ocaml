(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Compilation environments for compilation units *)

open Config
open Misc
open Clambda
open Symbol
open Abstract_identifiers
open Cmx_format

type error =
    Not_a_unit_info of string
  | Corrupted_unit_info of string
  | Illegal_renaming of string * string * string

exception Error of error

let global_infos_table =
  (Hashtbl.create 17 : (string, unit_infos option) Hashtbl.t)
let export_infos_table =
  (Hashtbl.create 10 : (string, Flambdaexport.exported) Hashtbl.t)

let imported_closure_table =
  (FunTbl.create 10
   : ExprId.t Flambda.function_declarations FunTbl.t)

let structured_constants =
  ref ([] : (string * bool * Clambda.ustructured_constant) list)

let current_unit_id = ref (Ident.create_persistent "___UNINITIALIZED___")
let merged_environment = ref Flambdaexport.empty_export

let current_unit =
  { ui_name = "";
    ui_symbol = "";
    ui_defines = [];
    ui_imports_cmi = [];
    ui_imports_cmx = [];
    ui_approx = Value_unknown;
    ui_curry_fun = [];
    ui_apply_fun = [];
    ui_send_fun = [];
    ui_force_link = false;
    ui_export_info = Flambdaexport.empty_export }

let symbolname_for_pack pack name =
  match pack with
  | None -> name
  | Some p ->
      let b = Buffer.create 64 in
      for i = 0 to String.length p - 1 do
        match p.[i] with
        | '.' -> Buffer.add_string b "__"
        |  c  -> Buffer.add_char b c
      done;
      Buffer.add_string b "__";
      Buffer.add_string b name;
      Buffer.contents b

let unit_id_from_name name = Ident.create_persistent name

let reset ?packname name =
  Hashtbl.clear global_infos_table;
  FunTbl.clear imported_closure_table;
  let symbol = symbolname_for_pack packname name in
  current_unit_id := unit_id_from_name name;
  current_unit.ui_name <- name;
  current_unit.ui_symbol <- symbol;
  current_unit.ui_defines <- [symbol];
  current_unit.ui_imports_cmi <- [];
  current_unit.ui_imports_cmx <- [];
  current_unit.ui_curry_fun <- [];
  current_unit.ui_apply_fun <- [];
  current_unit.ui_send_fun <- [];
  current_unit.ui_force_link <- false;
  current_unit.ui_export_info <- Flambdaexport.empty_export;
  merged_environment := Flambdaexport.empty_export;
  Hashtbl.clear export_infos_table;
  structured_constants := []

let current_unit_infos () =
  current_unit

let current_unit_name () =
  current_unit.ui_name

let current_unit_id () = !current_unit_id

let make_symbol ?(unitname = current_unit.ui_symbol) idopt =
  let prefix = "caml" ^ unitname in
  match idopt with
  | None -> prefix
  | Some id -> prefix ^ "__" ^ id

let read_unit_info filename =
  let ic = open_in_bin filename in
  try
    let buffer = input_bytes ic (String.length cmx_magic_number) in
    if buffer <> cmx_magic_number then begin
      close_in ic;
      raise(Error(Not_a_unit_info filename))
    end;
    let ui = (input_value ic : unit_infos) in
    let crc = Digest.input ic in
    close_in ic;
    (ui, crc)
  with End_of_file | Failure _ ->
    close_in ic;
    raise(Error(Corrupted_unit_info(filename)))

let read_library_info filename =
  let ic = open_in_bin filename in
  let buffer = input_bytes ic (String.length cmxa_magic_number) in
  if buffer <> cmxa_magic_number then
    raise(Error(Not_a_unit_info filename));
  let infos = (input_value ic : library_infos) in
  close_in ic;
  infos


(* Read and cache info on global identifiers *)

let cmx_not_found_crc =
  "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let get_global_info global_ident = (
  let modname = Ident.name global_ident in
  if modname = current_unit.ui_name then
    Some current_unit
  else begin
    try
      Hashtbl.find global_infos_table modname
    with Not_found ->
      let (infos, crc) =
        try
          let filename =
            find_in_path_uncap !load_path (modname ^ ".cmx") in
          let (ui, crc) = read_unit_info filename in
          if ui.ui_name <> modname then
            raise(Error(Illegal_renaming(modname, ui.ui_name, filename)));
          (Some ui, crc)
        with Not_found ->
          (None, cmx_not_found_crc) in
      current_unit.ui_imports_cmx <-
        (modname, crc) :: current_unit.ui_imports_cmx;
      Hashtbl.add global_infos_table modname infos;
      infos
  end
)

let cache_unit_info ui =
  Hashtbl.add global_infos_table ui.ui_name (Some ui)

(* Return the approximation of a global identifier *)

let toplevel_approx = Hashtbl.create 16

let record_global_approx_toplevel id =
  Hashtbl.add toplevel_approx current_unit.ui_name current_unit.ui_approx

let global_approx id =
  if Ident.is_predef_exn id then Value_unknown
  else try Hashtbl.find toplevel_approx (Ident.name id)
  with Not_found ->
    match get_global_info id with
      | None -> Value_unknown
      | Some ui -> ui.ui_approx

let get_unit_name id =
  match get_global_info id with
  | None -> Ident.name id
  | Some ui -> ui.ui_symbol

(* Return the symbol used to refer to a global identifier *)

let symbol_for_global id =
  if Ident.is_predef_exn id then
    "caml_exn_" ^ Ident.name id
  else begin
    match get_global_info id with
    | None -> make_symbol ~unitname:(Ident.name id) None
    | Some ui -> make_symbol ~unitname:ui.ui_symbol None
  end

let unit_for_global id =
  let sym_label = linkage_name (symbol_for_global id) in
  Compilation_unit.create (Ident.name id) sym_label

let predefined_exception_compilation_unit =
  Compilation_unit.create "__dummy__" (linkage_name "__dummy__")

let is_predefined_exception sym =
  Compilation_unit.equal
    predefined_exception_compilation_unit
    sym.sym_unit

let symbol_for_global' id =
  let open Symbol in
  let sym_label = linkage_name (symbol_for_global id) in
  if Ident.is_predef_exn id then
    { sym_unit = predefined_exception_compilation_unit;
      sym_label }
  else
    { sym_unit = unit_for_global id;
      sym_label }

let current_unit_linkage_name () =
  linkage_name
    (make_symbol ~unitname:current_unit.ui_symbol None)

(* Register the approximation of the module being compiled *)

let set_global_approx approx =
  current_unit.ui_approx <- approx

(* Exporting and importing cross module informations *)

let set_export_info export_info =
  current_unit.ui_export_info <- export_info

let approx_for_global comp_unit =
  let id = Compilation_unit.get_persistent_ident comp_unit in
  if (Compilation_unit.equal
      predefined_exception_compilation_unit
      comp_unit)
     || Ident.is_predef_exn id
     || not (Ident.global id)
  then invalid_arg (Format.asprintf "approx_for_global %a" Ident.print id);
  let modname = Ident.name id in
  try Hashtbl.find export_infos_table modname with
  | Not_found ->
    let exported = match get_global_info id with
      | None -> Flambdaexport.empty_export
      | Some ui -> ui.ui_export_info in
    Hashtbl.add export_infos_table modname exported;
    merged_environment := Flambdaexport.merge !merged_environment exported;
    exported

let approx_env () = !merged_environment

(* Record that a currying function or application function is needed *)

let need_curry_fun n =
  if not (List.mem n current_unit.ui_curry_fun) then
    current_unit.ui_curry_fun <- n :: current_unit.ui_curry_fun

let need_apply_fun n =
  if not (List.mem n current_unit.ui_apply_fun) then
    current_unit.ui_apply_fun <- n :: current_unit.ui_apply_fun

let need_send_fun n =
  if not (List.mem n current_unit.ui_send_fun) then
    current_unit.ui_send_fun <- n :: current_unit.ui_send_fun

(* Write the description of the current unit *)

let write_unit_info info filename =
  let oc = open_out_bin filename in
  output_string oc cmx_magic_number;
  output_value oc info;
  flush oc;
  let crc = Digest.file filename in
  Digest.output oc crc;
  close_out oc

let save_unit_info filename =
  current_unit.ui_imports_cmi <- Env.imported_units();
  write_unit_info current_unit filename

let current_unit_linkage_name () =
  linkage_name
    (make_symbol ~unitname:current_unit.ui_symbol None)

let current_unit () =
  Compilation_unit.create
    (Ident.name (current_unit_id ()))
    (current_unit_linkage_name ())

let current_unit_symbol () =
  { sym_unit = current_unit ();
    sym_label = current_unit_linkage_name () }

let const_label = ref 0

let new_const_label () =
  incr const_label;
  !const_label

let new_const_symbol () =
  incr const_label;
  make_symbol (Some (string_of_int !const_label))

let new_const_symbol' () =
  let sym_label = new_const_symbol () in
  { sym_unit = current_unit ();
    sym_label = linkage_name sym_label }

let concat_symbol unitname id =
  unitname ^ "__" ^ id

let closure_symbol fv =
  let compilation_unit = Closure_function.get_compilation_unit fv in
  let unitname =
    Symbol.string_of_linkage_name
      (Compilation_unit.get_linkage_name compilation_unit) in
  { Symbol.sym_unit = compilation_unit;
    sym_label =
      linkage_name
        (concat_symbol unitname
           ((Closure_function.unique_name fv) ^ "_closure")) }

let function_label fv =
  let open Symbol in
  let compilation_unit = Closure_function.get_compilation_unit fv in
  let unitname =
    string_of_linkage_name
      (Compilation_unit.get_linkage_name compilation_unit) in
  (concat_symbol unitname (Closure_function.unique_name fv))


let new_structured_constant cst global =
  let lbl = new_const_symbol() in
  structured_constants := (lbl, global, cst) :: !structured_constants;
  lbl

let add_structured_constant lbl cst global =
  structured_constants := (lbl, global, cst) :: !structured_constants

let clear_structured_constants () = structured_constants := []

let structured_constants () = !structured_constants

let imported_closure =
  let open Symbol in
  let open Flambda in
  let import_closure clos =

    let orig_var_map clos =
      VarMap.fold
        (fun id _ acc ->
           let fun_id = Closure_function.wrap id in
           let sym = closure_symbol fun_id in
           SymbolMap.add sym id acc)
        clos.funs SymbolMap.empty in

    let sym_map = orig_var_map clos in

    let f = function
      | Fsymbol (sym, ()) as e ->
          (try Fvar(SymbolMap.find sym sym_map,()) with
           | Not_found -> e)
      | e -> e in

    { clos with
      funs =
        VarMap.map
          (fun ff ->
             let body = Flambdaiter.map_toplevel f ff.body in
             let body = Flambdaiter.map_data ExprId.create body in
             let free_variables = Flambdaiter.free_variables body in
             { ff with body; free_variables })
          clos.funs } in
  let aux fun_id =
    let ex_info = approx_env () in
    let closure = FunMap.find fun_id ex_info.Flambdaexport.ex_functions in
    let cl = import_closure closure in
    cl
  in
  FunTbl.memoize imported_closure_table aux

(* Error report *)

open Format

let report_error ppf = function
  | Not_a_unit_info filename ->
      fprintf ppf "%a@ is not a compilation unit description."
        Location.print_filename filename
  | Corrupted_unit_info filename ->
      fprintf ppf "Corrupted compilation unit description@ %a"
        Location.print_filename filename
  | Illegal_renaming(name, modname, filename) ->
      fprintf ppf "%a@ contains the description for unit\
                   @ %s when %s was expected"
        Location.print_filename filename name modname
