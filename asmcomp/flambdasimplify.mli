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

(* Simple constant propagation and simplifications:
- mark direct calls
- simple inlining
- duplicates recursive functions for specialisation
- eliminates unused staticcatch
- build an explicit closure for partial direct applications *)

open Flambda
open Abstract_identifiers

val simplify : ExprId.t flambda -> ExprId.t flambda

val eliminate_ref : ExprId.t flambda -> ExprId.t flambda

val passes : Flambdapasses.pass list
