open Flambda

val clean : Flambdainfo.analysis_result ->
  Purity.effectful -> ExprId.t flambda -> ExprId.t flambda
(** [clean value_analysis unpure_expressions expression] *)

type inlining_kind =
  | Minimal
  | With_local_functions

val inlining : inlining_kind -> Flambdainfo.analysis_result ->
  ExprId.t flambda -> ExprId.t flambda
(** [inlining value_analysis expression] *)

val specialise : Flambdainfo.analysis_result ->
  Purity.effectful -> ExprId.t flambda -> ExprId.t flambda

val rebind : Flambdainfo.analysis_result ->
  Purity.effectful -> ExprId.t flambda -> ExprId.t flambda

(* val remove_unused_closure_param : 'a flambda -> 'a flambda *)

val remove_unused_function_param : ExprId.t flambda -> ExprId.t flambda

val extract_constants : Constants.constant_result ->
  ExprId.t flambda -> ExprId.t flambda

