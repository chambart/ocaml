(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                     Pierre Chambart, OCamlPro                       *)
(*                                                                     *)
(*  Copyright 2014 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

val check : current_compilation_unit:Flambda.symbol -> 'a Flambda.flambda -> unit
(** Run all tests, raises Fatal_error if a test fails *)

type 'a counter_example =
  | No_counter_example
  | Counter_example of 'a

val every_used_identifier_is_bound :
  'a Flambda.flambda -> Ident.t counter_example

val no_identifier_bound_multiple_times :
  'a Flambda.flambda -> Ident.t counter_example

val no_assign_on_variable_of_kind_strict :
  'a Flambda.flambda -> Ident.t counter_example

val no_variable_within_closure_is_bound_multiple_times :
  'a Flambda.flambda -> Flambda.variable_within_closure counter_example

val no_function_within_closure_is_bound_multiple_times :
  'a Flambda.flambda -> Flambda.function_within_closure counter_example

val every_declared_closure_is_from_current_compilation_unit :
  current_compilation_unit:Flambda.symbol -> 'a Flambda.flambda ->
  Flambda.symbol counter_example

val every_used_function_from_current_compilation_unit_is_declared :
  current_compilation_unit:Flambda.symbol -> 'a Flambda.flambda ->
  Flambda.ClosureFunctionSet.t counter_example

val every_used_variable_in_closure_from_current_compilation_unit_is_declared :
  current_compilation_unit:Flambda.symbol -> 'a Flambda.flambda ->
  Flambda.ClosureVariableSet.t counter_example

val every_static_exception_is_caught :
  'a Flambda.flambda -> int counter_example

val every_static_exception_is_caught_at_a_single_position :
  'a Flambda.flambda -> int counter_example
