open Flambda

module ExportId : Id = Id(Empty)
module EidMap = ExtMap(ExportId)

type symbol = string
module SymbolMap = Map.Make(String)

type tag = int

type descr =
  | Value_block of tag * approx array
  | Value_int of int
  | Value_constptr of int
  | Value_closure of value_closure

and value_closure =
  { closure_id : FunId.t;
    fun_id : Ident.t;
    bound_var : approx IdentMap.t }

and approx =
  | Value_unknown
  | Value_id of ExportId.t

type exported = {
  ex_functions : ExprId.t ffunctions FunMap.t;
  ex_values : descr EidMap.t;
  ex_global : approx;
  ex_id_symbol : symbol EidMap.t;
  ex_symbol_id : ExportId.t SymbolMap.t;
}

let empty_export = {
  ex_functions = FunMap.empty;
  ex_values = EidMap.empty;
  ex_global = Value_unknown;
  ex_id_symbol = EidMap.empty;
  ex_symbol_id = SymbolMap.empty;
}
