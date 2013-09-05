open Flambdaexport

val import : exported -> exported

val reverse_symbol_map : exported -> ExportId.t Flambda.SymbolMap.t

val merge : exported -> exported -> exported

val merge_symbol_map :
  ExportId.t Flambda.SymbolMap.t -> ExportId.t Flambda.SymbolMap.t ->
  ExportId.t Flambda.SymbolMap.t
