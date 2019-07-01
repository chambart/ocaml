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

module K = Flambda_kind
module T = Flambda_type
module TEE = Flambda_type.Typing_env_extension

let simplify_project_closure dacc ~original_term ~set_of_closures_ty closure_id
      ~result_var =
  let result = Simple.var result_var in
  let closures =
    Closure_id.Map.empty
    |> Closure_id.Map.add closure_id (T.alias_type_of K.value result)
  in
  Simplify_primitive_common.simplify_projection
    dacc ~original_term ~deconstructing:set_of_closures_ty
    ~shape:(T.set_of_closures_containing_at_least closures)
    ~result_var ~result_kind:K.value

let simplify_move_within_set_of_closures dacc ~original_term ~closure
      ~closure_ty ~move_from ~move_to ~result_var =
  (* CR mshinwell: We're assuming here that the argument to the move is
     always the closure whose ID is [move_from].  We should document this
     somewhere most probably, e.g. flambda_primitive.mli. *)
  (* CR mshinwell: We talked about enhancing [Row_like] so a tag could be
     specified in the "at least" cases.  In this case we would set the tag
     to [move_from].  Think again as to whether we really need this, as it
     will complicate [Row_like]. *)
  let result = Simple.var result_var in
  let closures =
    Closure_id.Map.empty
    |> Closure_id.Map.add move_from (T.alias_type_of K.value closure)
    |> Closure_id.Map.add move_to (T.alias_type_of K.value result)
  in
  Simplify_primitive_common.simplify_projection
    dacc ~original_term ~deconstructing:closure_ty
    ~shape:(T.set_of_closures_containing_at_least closures)
    ~result_var ~result_kind:K.value

let simplify_project_var dacc ~original_term ~closure_ty closure_element
      ~result_var =
  Simplify_primitive_common.simplify_projection
    dacc ~original_term ~deconstructing:closure_ty
    ~shape:(T.closure_containing_at_least closure_element
      ~closure_element_var:result_var)
    ~result_var ~result_kind:K.value

let simplify_unbox_number dacc ~original_term ~boxed_number_ty
      (boxable_number_kind : K.Boxable_number.t) ~result_var =
  let shape, result_kind =
    match boxable_number_kind with
    | Naked_float ->
      T.boxed_float_alias_to ~naked_float:result_var, K.naked_float
    | Naked_int32 ->
      T.boxed_int32_alias_to ~naked_int32:result_var, K.naked_int32
    | Naked_int64 ->
      T.boxed_int64_alias_to ~naked_int64:result_var, K.naked_int64
    | Naked_nativeint ->
      T.boxed_nativeint_alias_to ~naked_nativeint:result_var, K.naked_nativeint
  in
  Simplify_primitive_common.simplify_projection
    dacc ~original_term ~deconstructing:boxed_number_ty
    ~shape ~result_var ~result_kind

let simplify_box_number dacc ~original_term ~naked_number_ty
      (boxable_number_kind : K.Boxable_number.t) ~result_var =
  let ty =
    match boxable_number_kind with
    | Naked_float -> T.box_float naked_number_ty
    | Naked_int32 -> T.box_int32 naked_number_ty
    | Naked_int64 -> T.box_int64 naked_number_ty
    | Naked_nativeint -> T.box_nativeint naked_number_ty
  in
  Reachable.reachable original_term,
    TEE.one_equation (Name.var result_var) ty,
    dacc

let simplify_unary_primitive dacc (prim : Flambda_primitive.unary_primitive)
      arg dbg ~result_var =
(*
begin match (arg : Simple.t) with
| Name (Var arg) ->
Format.eprintf "simplify_unary_primitive: type of arg %a:@ %a@ Env:@ %a%!"
  Variable.print arg
  T.print (E.find_variable env arg)
  E.print env
| _ -> ()
end;
*)
  let min_occurrence_kind = Var_in_binding_pos.occurrence_kind result_var in
  let result_var = Var_in_binding_pos.var result_var in
  match Simplify_simple.simplify_simple dacc arg ~min_occurrence_kind with
  | Bottom, ty ->
    let env_extension = TEE.one_equation (Name.var result_var) ty in
    Reachable.invalid (), env_extension, dacc
  | Ok arg, arg_ty ->
    let original_term = Named.create_prim (Unary (prim, arg)) dbg in
    match prim with
    | Project_closure closure_id ->
      simplify_project_closure dacc ~original_term ~set_of_closures_ty:arg_ty
        closure_id ~result_var
    | Project_var closure_element ->
      simplify_project_var dacc ~original_term ~closure_ty:arg_ty
        closure_element ~result_var
    | Move_within_set_of_closures { move_from; move_to; } ->
      simplify_move_within_set_of_closures dacc ~original_term ~closure:arg
        ~closure_ty:arg_ty ~move_from ~move_to ~result_var
    | Unbox_number boxable_number_kind ->
      simplify_unbox_number dacc ~original_term ~boxed_number_ty:arg_ty
        boxable_number_kind ~result_var
    | Box_number boxable_number_kind ->
      simplify_box_number dacc ~original_term ~naked_number_ty:arg_ty
        boxable_number_kind ~result_var
    | _ ->
      (* CR mshinwell: temporary code *)
      let named = Named.create_prim (Unary (prim, arg)) dbg in
      let kind = Flambda_primitive.result_kind_of_unary_primitive' prim in
      let ty = T.unknown kind in
      let env_extension = TEE.one_equation (Name.var result_var) ty in
      Reachable.reachable named, env_extension, dacc