(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Variables used in the backend, optionally equipped with "provenance"
    information, used for the emission of debugging information. *)

(* CR-soon mshinwell: Move this into the asmcomp/ directory and change
   [Clambda] to use [Variable].  This should be done once the Flambda gdb
   patches are ready, so we can add the necessary provenance info. to
   [Variable]. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

include module type of struct include Ident end

type backend_var = t

module With_provenance : sig
  (** Values of type [t] should be used for variables in binding position. *)
  type t

  val print : Format.formatter -> t -> unit

  val create : ?provenance:Provenance.t -> backend_var -> t

  val var : t -> backend_var
  val provenance : t -> Provenance.t option

  val name : t -> string

  val rename : t -> t
end
