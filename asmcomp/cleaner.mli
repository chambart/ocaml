open Flambda

val clean : Flambdainfo.analysis_result ->
  ExprSet.t -> ExprSet.elt flambda -> ExprSet.elt flambda
(** [clean value_analysis unpure_expressions expression] *)

type inlining_kind =
  | Minimal
  | With_local_functions

val inlining : inlining_kind -> Flambdainfo.analysis_result ->
  ExprSet.elt flambda -> ExprSet.elt flambda
(** [inlining value_analysis expression] *)

val specialise : Flambdainfo.analysis_result ->
  ExprSet.t -> ExprSet.elt flambda -> ExprSet.elt flambda

val rebind : Flambdainfo.analysis_result ->
  ExprSet.t -> ExprSet.elt flambda -> ExprSet.elt flambda

val remove_unused_closure_param : 'a flambda -> 'a flambda
