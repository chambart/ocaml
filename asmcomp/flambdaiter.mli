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

(** Iterators on flambda *)

val iter : f:('a Flambda.flambda -> unit) -> 'a Flambda.flambda -> unit

val iter_toplevel : f:('a Flambda.flambda -> unit) -> 'a Flambda.flambda -> unit
(** Like iter, but does not apply f on functions body *)

val map :
  ('a Flambda.flambda -> 'a Flambda.flambda) ->
  'a Flambda.flambda -> 'a Flambda.flambda

val map_data : ('a -> 'b) -> 'a Flambda.flambda -> 'b Flambda.flambda

val free_variables : 'a Flambda.flambda -> Flambda.IdentSet.t
