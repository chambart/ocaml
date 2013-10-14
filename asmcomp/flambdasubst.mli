open Flambda

val substitute : Ident.t IdentMap.t -> 'a flambda -> 'a flambda

val substitute_closures : Ident.t IdentMap.t ->
  'a ffunctions list -> 'a ffunctions list
