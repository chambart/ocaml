(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018--2019 OCamlPro SAS                                    *)
(*   Copyright 2018--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

include Continuation

let free_names t = Name_occurrences.singleton_continuation t

let apply_name_permutation t perm = Name_permutation.apply_continuation perm t

let rename _t = create ()

let add_to_name_permutation t1 t2 perm =
  Name_permutation.add_continuation perm t1 t2

let name_permutation t1 t2 =
  add_to_name_permutation t1 t2 Name_permutation.empty

let singleton_occurrence_in_terms t = Name_occurrences.singleton_continuation t

let add_occurrence_in_terms t occs = Name_occurrences.add_continuation occs t