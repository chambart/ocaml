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

type every_used_identifier_is_bound =
  | Every_used_identifier_is_bound
  | Some_used_unbound_variable of Ident.t

val every_used_identifier_is_bound :
  'a Flambda.flambda -> every_used_identifier_is_bound
