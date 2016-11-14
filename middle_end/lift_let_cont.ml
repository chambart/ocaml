(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

let params_do_not_overlap_free_variables ~params ~handler =
  let params = Variable.Set.of_list params in
  let handler_fvs = Flambda.free_variables handler in
  Variable.Set.is_empty (Variable.Set.inter params handler_fvs)

let params_do_not_overlap_free_variables_let ~params
      ~(let_expr : Flambda.let_expr) =
  let params = Variable.Set.of_list params in
  let defining_expr_fvs = let_expr.free_vars_of_defining_expr in
  Variable.Set.is_empty (Variable.Set.inter params defining_expr_fvs)

type thing_to_lift =
  | Continuation of Continuation.t * Flambda.let_cont_handler
  | Let of Variable.t * Flambda.named
  | Let_mutable of Mutable_variable.t * Variable.t * Lambda.value_kind

(* CR mshinwell: We should review to make sure lifting Const lets is
   the right thing to do.  Leo pointed out what we could also push
   non side effecting lets (e.g. makeblocks) downwards into let_cont
   handlers when the bound var isn't used in the body of such let_cont. *)

let rec find_things_to_lift (acc : thing_to_lift list) (expr : Flambda.t) =
  match expr with
  | Let_cont { name; body = body1; handler = Handler {
        params; recursive; handler = (Let_cont { name = _ (* name' *); body = _;
          handler = Handler { handler; _ }; _ }) as let_cont2; }; }
      when params_do_not_overlap_free_variables ~params ~handler ->
    (* The continuation [handler] can be lifted.  First we try to lift things
       (recursively) from inside [handler] itself.  (The recursive call will
       be the thing that actually adds [handler] to [acc].) *)
    let acc, body2 = find_things_to_lift acc let_cont2 in
    let acc =
      (Continuation (name, Handler {
          params; recursive; handler = body2; }))
        :: acc
    in
    acc, lift body1
  | Let_cont { name; body; handler = Handler {
      handler = Let_cont { handler = Alias alias_to; _ }; }; } ->
    (* Continuation aliases nested immediately inside another [Let_cont]'s
       handler can always be lifted. *)
    let acc = (Continuation (name, Alias alias_to)) :: acc in
    acc, lift body
  | Let_cont { name; body; handler = Handler {
        params; handler = Let let_expr; }; }
      when params_do_not_overlap_free_variables_let ~params ~let_expr
        && begin match let_expr.defining_expr with
           | Const _ -> true
           | Var _ | Prim _ | Assign _ | Read_mutable _ | Symbol _
           | Read_symbol_field _ | Allocated_const _ | Set_of_closures _
           | Project_closure _ | Move_within_set_of_closures _ | Project_var _
           | Proved_unreachable -> false
           end ->
    (* The let-bound expression [let_expr] can be lifted.  Since there cannot
       be any nested [Let] or [Let_cont] in the defining expression of the
       [Let], we don't need to recurse into that expression.
       Lifting let-bound expressions in this manner allows us to lift more
       continuations (that may depend on such lets). *)
    let acc, body2 = find_things_to_lift acc (Flambda.Let let_expr) in
    let acc =
      (Continuation (name, Handler {
          params; recursive = Nonrecursive; handler = body2; }))
        :: acc
    in
    acc, lift body
  | Let_cont { name; body; handler = Handler {
      params; recursive; handler; }; } ->
    (* The continuation [handler] cannot be lifted---but there might be
       sequences of [Let_cont]s or [Let]s inside [handler] that can be lifted
       (without bringing them out of [handler]). *)
    let handler = lift handler in
    let acc =
      (Continuation (name, Handler { params; recursive; handler; })) :: acc
    in
    acc, lift body
  | Let_cont { name; body; handler = Alias alias_to; } ->
    (* Similar to the previous case, but there's no handler. *)
    let acc = (Continuation (name, Alias alias_to)) :: acc in
    acc, lift body
  | Let { var; defining_expr; body; _ } ->
    (* Same as the previous case. *)
    let acc = (Let (var, defining_expr)) :: acc in
    acc, lift body
    (* Same as the previous case. *)
  | Apply _ | Apply_cont _ | Switch _ ->
    (* These things have no subexpressions, so we're done. *)
    acc, expr
  | Let_mutable { var; initial_value; contents_kind; body; } ->
    let acc = (Let_mutable (var, initial_value, contents_kind)) :: acc in
    acc, lift body

and lift (expr : Flambda.t) : Flambda.t =
  let defs, body = find_things_to_lift [] expr in
  List.fold_left (fun body (def : thing_to_lift) ->
      match def with
      | Continuation (name, handler) ->
        Flambda.Let_cont { name; body; handler; }
      | Let (var, Set_of_closures set) ->
        let set = Flambda_iterators.map_function_bodies set ~f:lift in
        Flambda.create_let var (Set_of_closures set) body
      | Let (var, defining_expr) ->
        Flambda.create_let var defining_expr body
      | Let_mutable (var, initial_value, contents_kind) ->
        Flambda.Let_mutable { var; initial_value; contents_kind; body; })
    body
    defs

let run program =
  Flambda_iterators.map_exprs_at_toplevel_of_program program ~f:lift