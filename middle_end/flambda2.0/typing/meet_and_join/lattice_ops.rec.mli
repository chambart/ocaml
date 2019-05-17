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

(** Modules that are used for specialising generic meet-and-join operations
    to either meet or join.  The content of these modules typically corresponds
    to intersections for meets and unions for joins. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type meet_or_join = private Meet | Join

module For_meet : Either_meet_or_join_intf.S
  with type join_env := Join_env.t
  with type meet_env := Meet_env.t
  with type typing_env_extension := Typing_env_extension.t
  with type meet_or_join := meet_or_join

module For_join : Either_meet_or_join_intf.S
  with type join_env := Join_env.t
  with type meet_env := Meet_env.t
  with type typing_env_extension := Typing_env_extension.t
  with type meet_or_join := meet_or_join
