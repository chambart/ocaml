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

open! Flambda.Import

type lifted_constants =
  (Symbol.t * (Flambda_type.t * Flambda_static.Static_part.t)) list

module type Env = sig
  (** Environments, following the lexical scope of the program, used during
      simplification. *)
  type t

  type result

  val invariant : t -> unit

  (** Print a human-readable version of the given environment. *)
  val print : Format.formatter -> t -> unit

  (** Create a new environment. *)
  val create
     : round:int
    -> backend:(module Flambda2_backend_intf.S)
    -> scope_level_for_lifted_constants:Scope_level.t
    -> t

  (** Obtain the first-class module that gives information about the
      compiler backend being used for compilation. *)
  val backend : t -> (module Flambda2_backend_intf.S)

  val resolver : t -> (Export_id.t -> Flambda_type.t option)

  val enter_closure : t -> t

  val increment_continuation_scope_level : t -> t

  val continuation_scope_level : t -> Scope_level.t

  val typing_env : t -> Flambda_type.Typing_env.t

  val add_variable : t -> Variable.t -> Flambda_type.t -> t

  val add_equation_on_variable : t -> Variable.t -> Flambda_type.t -> t

  val find_variable : t -> Variable.t -> Flambda_type.t

  val add_symbol : t -> Symbol.t -> Flambda_type.t -> t

  val add_parameters
     : t
    -> Kinded_parameter.t list
    -> arg_types:Flambda_type.t list
    -> t

  val add_parameters_with_unknown_types : t -> Kinded_parameter.t list -> t

  val add_continuation : t -> Continuation.t -> Flambda_arity.t -> t

  val add_unreachable_continuation
     : t
    -> Continuation.t
    -> Flambda_arity.t
    -> t

  val add_continuation_alias
     : t
    -> Continuation.t
    -> Flambda_arity.t
    -> alias_for:Continuation.t
    -> t

  val add_continuation_to_inline
     : t
    -> Continuation.t
    -> Flambda_arity.t
    -> Continuation_handler.t
    -> t

  val add_exn_continuation : t -> Exn_continuation.t -> t

  val mem_continuation : t -> Continuation.t -> bool

  val mem_exn_continuation : t -> Exn_continuation.t -> bool

  val find_continuation : t -> Continuation.t -> Continuation_in_env.t

  val resolve_continuation_aliases : t -> Continuation.t -> Continuation.t

  val continuation_arity : t -> Continuation.t -> Flambda_arity.t

  val extend_typing_environment : t -> Flambda_type.Typing_env_extension.t -> t

  val check_variable_is_bound : t -> Variable.t -> unit

  val check_symbol_is_bound : t -> Symbol.t -> unit

  val check_name_is_bound : t -> Name.t -> unit

  val check_exn_continuation_is_bound : t -> Exn_continuation.t -> unit

  (** Appends the locations of inlined call-sites to the given debuginfo
      and sets the resulting debuginfo as the current one in the
      environment. *)
  val add_inlined_debuginfo : t -> Debuginfo.t -> t

   (** If collecting inlining statistics, record an inlining decision for the
       call at the top of the closure stack stored inside the given
       environment. *)
  val record_decision
     : t
    -> Inlining_stats_types.Decision.t
    -> unit

  val round : t -> int

  (** Prevent function inlining from occurring in the given environment. *)
  val disable_function_inlining : t -> t

  (** Add the given lifted constants to the environment.  Symbols that are
      already defined in the environment are ignored. *)
  val add_lifted_constants : t -> lifted_constants -> t

  (** Like [add_lifted_constants], but takes the constants from the given
      result structure. *)
  val add_lifted_constants_from_r : t -> result -> t

  val set_scope_level_for_lifted_constants : t -> Scope_level.t -> t

  val can_inline : t -> bool
end

module type Result = sig
  (** The result structure used during simplification. *)

  type t

  type env

  val create : resolver:(Export_id.t -> Flambda_type.t option) -> t

  val add_continuation : t -> env -> Continuation.t -> t

  val record_continuation_use
     : t
    -> env
    -> Continuation.t
    -> arg_types:Flambda_type.t list
    -> t

  (* CR mshinwell: Add [record_exn_continuation_use]? *)

  val continuation_arg_types
     : t
    -> env
    -> Continuation.t
    -> Flambda_type.t list

  val new_lifted_constant
     : t
    -> name:string
    -> Flambda_type.t
    -> Flambda_static.Static_part.t
    -> Symbol.t * t

  val get_lifted_constants : t -> lifted_constants

  val imported_symbols : t -> Flambda_kind.t Symbol.Map.t
end