
type constant_result = {
  not_constant_id : Flambda.IdentSet.t;
  not_constant_closure : Flambda.FunSet.t;
}

val not_constants : for_clambda:bool -> 'a Flambda.flambda -> constant_result
(** [not_constant ~for_clambda expr] *)(** [not_constant ~for_clambda expr]
    If for_clambda is true, are marked constant only expressions that can
    effectively be compiled to constants by Clambdagen.
    i.e. field access to a constant are not considered constant *)

type alias_result =
  { constant_result : constant_result;
    constant_alias : Ident.t Flambda.IdentMap.t;
    constant_symbol : Flambda.Symbol.t Flambda.IdentMap.t }

val alias : 'a Flambda.flambda -> alias_result
(** [alias expr] *)

type export_result =
  { export_constant : constant_result;
    export_global : Flambdaexport.approx;
    export_values : Flambdaexport.descr Flambdaexport.EidMap.t;
    export_mapping : Flambdaexport.approx Flambda.IdentMap.t }

val export_info : 'a Flambda.flambda -> export_result
