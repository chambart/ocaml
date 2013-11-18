open Flambda

val substitute : Ident.t IdentMap.t -> 'a flambda -> 'a flambda

val substitute_closures : Ident.t IdentMap.t ->
  'a ffunctions list -> 'a ffunctions list

type context =
  { fv : Offset.t OffsetMap.t;
    func : Offset.t OffsetMap.t }

val empty_context : context

val substitute' : Ident.t IdentMap.t -> context -> 'a flambda -> 'a flambda * context
