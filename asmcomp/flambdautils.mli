val iter_flambda : ('a Flambda.flambda -> unit) -> 'a Flambda.flambda -> unit

val iter2_flambda :
  (('env -> 'a Flambda.flambda -> unit) -> 'env -> 'a Flambda.flambda -> unit) ->
  'env -> 'a Flambda.flambda -> unit

val map_no_closure :
  ('a Flambda.flambda -> 'a Flambda.flambda) ->
  'a Flambda.flambda -> 'a Flambda.flambda

val map :
  ('a Flambda.flambda -> 'a Flambda.flambda) ->
  'a Flambda.flambda -> 'a Flambda.flambda

val map2 :
  (('a Flambda.flambda -> 'a Flambda.flambda) ->
   'a Flambda.flambda -> 'a Flambda.flambda) ->
  'a Flambda.flambda -> 'a Flambda.flambda

val anf :
  Flambda.ExprId.t Flambda.flambda -> Flambda.ExprId.t Flambda.flambda

val reindex :
  Flambda.ExprId.t Flambda.flambda -> Flambda.ExprId.t Flambda.flambda

val assigned_var : 'a Flambda.flambda -> Flambda.IdentSet.t

val free_variables : 'a Flambda.flambda -> Flambda.IdentSet.t
