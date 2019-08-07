(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

(*

val create
   : typing_env_left:Typing_env.t
  -> typing_env_right:Typing_env.t
  -> t

val typing_env_left : t -> Typing_env.t

val typing_env_right : t -> Typing_env.t

(** Note that we are now in the process of comparing the given two
    [Simple]s for equality. *)
val now_comparing : t -> Simple.t -> Simple.t -> t

(** Determine whether we are now in the process of comparing the given two
    [Simple]s for equality. The arguments do not have to be provided in the same
    order as when [now_comparing] was called. *)
val already_comparing : t -> Simple.t -> Simple.t -> bool

*)