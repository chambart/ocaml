
val substitute : Flambda.IdentSet.elt Flambda.IdentMap.t ->
  'a Flambda.flambda -> 'a Flambda.flambda

val substitute_closures : Flambda.IdentSet.elt Flambda.IdentMap.t ->
  'a Flambda.ffunctions list ->
  'a Flambda.ffunctions list
