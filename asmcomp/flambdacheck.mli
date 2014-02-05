(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                     Pierre Chambart, OCamlPro                       *)
(*                                                                     *)
(*  Copyright 2013 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Flambda

val check : current_unit:symbol -> 'a flambda -> unit
(**
   Well formedness checking
   Ensures that:
    * No identifier is bound multiple times
    * every used identifier is bound
    * At most one place can catch a static exception
    * Staticfail are correctly enclosed inside a catch
 *)
