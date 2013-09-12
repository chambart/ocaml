open Flambdaexport

val import : exported -> exported

val reverse_symbol_map : exported -> ExportId.t Flambda.SymbolMap.t

val merge : exported -> exported -> exported

val import_pack : Flambda.IdentSet.t -> Flambda.StringSet.t ->
  string ->
  Ident.t -> exported -> exported
(** [merge_pack units units_lbl global_lbl global_id unit] *)

val merge_symbol_map :
  ExportId.t Flambda.SymbolMap.t -> ExportId.t Flambda.SymbolMap.t ->
  ExportId.t Flambda.SymbolMap.t
