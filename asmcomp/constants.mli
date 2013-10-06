(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                      Pierre Chambart (OCamlPro)                        *)
(*                                                                        *)
(*   Copyright 2013 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

type constant_result = {
  not_constant_id : Flambda.IdentSet.t;
  not_constant_closure : Flambda.FunSet.t;
}

val not_constants : for_clambda:bool -> 'a Flambda.flambda -> constant_result
(** [not_constant ~for_clambda expr]
    If for_clambda is true, are marked constant only expressions that can
    effectively be compiled to constants by Clambdagen.
    i.e. field access to a constant are not considered constant *)
