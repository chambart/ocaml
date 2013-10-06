open Flambda

val clean : Flambdainfo.analysis_result ->
  ExprSet.t -> ExprSet.elt flambda -> ExprSet.elt flambda
(** [clean value_analysis unpure_expressions expression] *)

val inlining : Flambdainfo.analysis_result ->
  ExprSet.elt flambda -> ExprSet.elt flambda
(** [inlining value_analysis expression] *)
