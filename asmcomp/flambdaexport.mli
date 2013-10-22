(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                      Pierre Chambart (OCamlPro)                        *)
(*                                                                        *)
(*   Copyright 2013 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

(** Exported informations about a compilation unit *)

open Ext_types

module ExportId : UnitId
module EidSet : ExtSet with module M := ExportId
module EidMap : ExtMap with module M := ExportId
module EidTbl : ExtHashtbl with module M := ExportId

type tag = int

type descr =
    Value_block of tag * approx array
  | Value_int of int
  | Value_constptr of int
  | Value_closure of value_offset
  | Value_unoffseted_closure of value_closure
  | Value_predef_exn of Ident.t

and value_offset = { fun_id : Flambda.offset; closure : value_closure; }

and value_closure = {
  closure_id : Flambda.FunId.t;
  bound_var : approx Flambda.OffsetMap.t;
}

and approx =
    Value_unknown
  | Value_id of ExportId.t
  | Value_symbol of Flambda.symbol

type exported = {
  ex_functions : unit Flambda.ffunctions Flambda.FunMap.t;
  ex_functions_off : unit Flambda.ffunctions Flambda.OffsetMap.t;
  ex_values : descr EidMap.t;
  ex_globals : approx Flambda.IdentMap.t;
  ex_id_symbol : Flambda.symbol EidMap.t;
  ex_symbol_id : ExportId.t Flambda.SymbolMap.t;
  ex_offset_fun : int Flambda.OffsetMap.t;
  ex_offset_fv : int Flambda.OffsetMap.t;
  ex_constants : Flambda.SymbolSet.t;
}

val empty_export : exported

val merge : exported -> exported -> exported

val import_for_pack :
  pack_units:Flambda.IdentSet.t -> pack:Ident.t -> exported -> exported

(**/**)
(* debug printing functions *)

val print_approx : Format.formatter -> exported -> unit

val print_symbols : Format.formatter -> exported -> unit
