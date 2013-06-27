val iter_all : ('a Flambda.flambda -> unit) -> 'a Flambda.flambda -> unit

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
  (('env -> 'a Flambda.flambda -> 'a Flambda.flambda) ->
   'env -> 'a Flambda.flambda -> 'a Flambda.flambda) ->
  'env -> 'a Flambda.flambda -> 'a Flambda.flambda

val anf :
  Flambda.ExprId.t Flambda.flambda -> Flambda.ExprId.t Flambda.flambda

val reindex :
  Flambda.ExprId.t Flambda.flambda -> Flambda.ExprId.t Flambda.flambda

val assigned_var : 'a Flambda.flambda -> Flambda.IdentSet.t

val free_variables : 'a Flambda.flambda -> Flambda.IdentSet.t

val global_var : 'a Flambda.flambda -> Ident.t Flambda.IdentTbl.t
(** [global_var expr] returns the association table between variables
    and the potential global module they reference

    It assumes expr is in ANF. *)

val global_index : 'a Flambda.flambda ->
  Ident.t Flambda.IdentTbl.t * Ident.t Flambda.IntTbl.t
(** [global_index expr] returns the association table between
    variables and the potential global module they reference and the
    table mapping global field to the variable set into it.

    It assumes expr is in ANF. *)
