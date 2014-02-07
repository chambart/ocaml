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

val check : current_unit:Flambda.symbol -> 'a Flambda.flambda -> unit
(**
   Well formedness checking
   Ensures that:
    * No identifier is bound multiple times
    * every used identifier is bound
    * At most one place can catch a static exception
    * Staticfail are correctly enclosed inside a catch
 *)

type 'a counter_example =
  | No_counter_example
  | Counter_example of 'a

val every_used_identifier_is_bound :
  'a Flambda.flambda -> Ident.t counter_example

val no_identifier_bound_multiple_times :
  'a Flambda.flambda -> Ident.t counter_example

val no_assign_on_variable_of_kind_strict :
  'a Flambda.flambda -> Ident.t counter_example
