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
open Cmx_format

type error =
    Not_a_unit_info of string
  | Corrupted_unit_info of string
  | Illegal_renaming of string * string * string

exception Error of error

let global_infos_table =
  (Hashtbl.create 17 : (string, unit_infos option) Hashtbl.t)
let global_approx_infos_table =
  (Hashtbl.create 17 : (string, Flambdaexport.exported) Hashtbl.t)

let structured_constants =
  ref ([] : (string * bool * Clambda.ustructured_constant) list)

let merged_environment = ref Flambdaexport.empty_export
let merged_symbol_map = ref Flambda.SymbolMap.empty
let fun_table = Flambda.FunTbl.create 10

let symbol_alias : (string,(bool*string) list) Hashtbl.t = Hashtbl.create 10
let symbol_back_alias : (string,string) Hashtbl.t = Hashtbl.create 10

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
    ui_approx_info = Flambdaexport.empty_export }

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


let reset ?packname name =
  Hashtbl.clear global_infos_table;
  Hashtbl.clear global_approx_infos_table;
  merged_environment := Flambdaexport.empty_export;
  merged_symbol_map := Flambda.SymbolMap.empty;
  Flambda.FunTbl.clear fun_table;
  let symbol = symbolname_for_pack packname name in
  current_unit.ui_name <- name;
  current_unit.ui_symbol <- symbol;
  current_unit.ui_defines <- [symbol];
  current_unit.ui_imports_cmi <- [];
  current_unit.ui_imports_cmx <- [];
  current_unit.ui_curry_fun <- [];
  current_unit.ui_apply_fun <- [];
  current_unit.ui_send_fun <- [];
  current_unit.ui_force_link <- false;
  current_unit.ui_approx_info <- Flambdaexport.empty_export;
  structured_constants := [];
  Hashtbl.clear symbol_alias;
  Hashtbl.clear symbol_back_alias

let current_unit_infos () =
  current_unit

let current_unit_name () =
  current_unit.ui_name

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
  failwith "TODO: toplevel"
  (* Hashtbl.add toplevel_approx current_unit.ui_name current_unit.ui_approx *)

let global_approx id =
  if Ident.is_predef_exn id then Value_unknown
  else try Hashtbl.find toplevel_approx (Ident.name id)
  with Not_found ->
    match get_global_info id with
      | None -> Value_unknown
      | Some ui -> ui.ui_approx

let global_approx_info id =
  let modname = Ident.name id in
  try Hashtbl.find global_approx_infos_table modname with
  | Not_found ->
    let exported = match get_global_info id with
      | None -> Flambdaexport.empty_export
      | Some ui -> ui.ui_approx_info
    in
    let imported = Flambdaimport.import exported in
    let symbol_map = Flambdaimport.reverse_symbol_map imported in
    merged_environment := Flambdaimport.merge imported !merged_environment;
    merged_symbol_map := Flambdaimport.merge_symbol_map
        symbol_map !merged_symbol_map;
    Hashtbl.add global_approx_infos_table modname imported;
    imported

let approx_env () = !merged_environment
let symbol_map () = !merged_symbol_map

let find_funid id =
  try Flambda.FunTbl.find fun_table id with
  | Not_found ->
    let funcs = Flambda.FunMap.find id
        (approx_env ()).Flambdaexport.ex_functions in
    let funcs = Flambdautils.map_index_ffunctions
        (fun () -> Flambda.ExprId.create ())
        funcs in
    Flambda.FunTbl.add fun_table id funcs;
    funcs

(* Return the symbol used to refer to a global identifier *)

let symbol_for_global id =
  if Ident.is_predef_exn id then
    "caml_exn_" ^ Ident.name id
  else begin
    match get_global_info id with
    | None -> make_symbol ~unitname:(Ident.name id) None
    | Some ui -> make_symbol ~unitname:ui.ui_symbol None
  end

(* Register the approximation of the module being compiled *)

let set_global_approx approx =
  current_unit.ui_approx <- approx

let set_global_approx_info approx =
  current_unit.ui_approx_info <- approx

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



let const_label = ref 0

let new_const_label () =
  incr const_label;
  !const_label

let new_const_symbol () =
  incr const_label;
  make_symbol (Some (string_of_int !const_label))

let add_structured_constant lbl cst global =
  structured_constants := (lbl, global, cst) :: !structured_constants

let new_structured_constant cst global =
  let lbl = new_const_symbol() in
  add_structured_constant lbl cst global;
  lbl

let clear_structured_constants () = structured_constants := []

let structured_constants () = !structured_constants

let set_symbol_alias ~orig ~alias global =
  let l = try Hashtbl.find symbol_alias orig with Not_found -> [] in
  Hashtbl.replace symbol_alias orig ((global,alias)::l);
  assert(not (Hashtbl.mem symbol_back_alias alias));
  Hashtbl.add symbol_back_alias alias orig

let rec new_symbol_alias ~orig ~alias global =
  match (try Some (Hashtbl.find symbol_back_alias orig) with _ -> None) with
  | Some orig -> new_symbol_alias ~orig ~alias global
  | None -> set_symbol_alias ~orig ~alias global

let symbol_alias s =
  let rec aux (_,s) =
    let l = try Hashtbl.find symbol_alias s with Not_found -> [] in
    List.concat (l::(List.map aux l))
  in
  aux (false,s)


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
      fprintf ppf "%a@ contains the description for unit @ %s when %s was expected"
        Location.print_filename filename name modname
