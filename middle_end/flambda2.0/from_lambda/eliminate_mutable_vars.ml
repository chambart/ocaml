(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2016--2019 OCamlPro SAS                                    *)
(*   Copyright 2016--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Env : sig
  type t

  val empty : t

  val add_mutable_and_make_new_id
     : t
    -> Ident.t
    -> Lambda.value_kind
    -> t * Ident.t

  val new_id_for_mutable : t -> Ident.t -> t * Ident.t * Lambda.value_kind

  type add_continuation_result = private {
    body_env : t;
    handler_env : t;
    extra_params : Ident.t list;
  }

  val add_continuation
     : t
    -> Continuation.t
    -> Asttypes.rec_flag
    -> add_continuation_result

  val rename_variable : t -> Ident.t -> Ident.t
  val rename_variables : t -> Ident.t list -> Ident.t list
end = struct
  type t = {
    current_values_of_mutables_in_scope
      : (Ident.t * Lambda.value_kind) Ident.Map.t;
    mutables_needed_by_continuations : Ident.Set.t Continuation.Map.t;
  }

  let empty =
    { current_values_of_mutables_in_scope = Ident.Map.empty;
      mutables_needed_by_continuations = Continuation.Map.empty;
  }

  let add_mutable_and_make_new_id t id kind =
    if Ident.Map.mem id t.current_values_of_mutables_in_scope then begin
      Misc.fatal_errorf "Redefinition of mutable variable %a"
        Ident.print id
    end;
    let new_id = Ident.rename id in
    let current_values_of_mutables_in_scope =
      Ident.Map.add id (new_id, kind) t.current_values_of_mutables_in_scope
    in
    let t =
      { t with
        current_values_of_mutables_in_scope;
      }
    in
    t, new_id

  let new_id_for_mutable t id =
    match Ident.Map.find id t.current_values_of_mutables_in_scope with
    | exception Not_found ->
      Misc.fatal_errorf "Mutable variable %a not in environment"
        Ident.print id
    | _old_id, kind ->
      let new_id = Ident.rename id in
      let current_values_of_mutables_in_scope =
        Ident.Map.add id (new_id, kind) t.current_values_of_mutables_in_scope
      in
      let t =
        { t with
          current_values_of_mutables_in_scope;
        }
      in
      t, new_id, kind

  let mutables_in_scope t =
    Ident.Map.keys t.current_values_of_mutables_in_scope

  type add_continuation_result = {
    body_env : t;
    handler_env : t;
    extra_params : Ident.t list;
  }

  let add_continuation t cont (recursive : Asttypes.rec_flag) =
    let body_env =
      let mutables_needed_by_continuations =
        Continuation.Map.add cont (mutables_in_scope t)
          t.mutables_needed_by_continuations
      in
      { t with
        mutables_needed_by_continuations;
      }
    in
    let current_values_of_mutables_in_scope =
      Ident.Map.mapi (fun mut_var (_outer_value, kind) ->
          Ident.rename mut_var, kind)
        t.current_values_of_mutables_in_scope
    in
    let handler_env =
      let handler_env =
        match recursive with
        | Nonrecursive -> t
        | Recursive -> body_env
      in
      { handler_env with
        current_values_of_mutables_in_scope;
      }
    in
    let extra_params =
      List.map snd
        (Continuation.Map.bindings t.current_values_of_mutables_in_scope)
    in
    { body_env;
      handler_env;
      extra_params;
    }

  let rename_variable t id =
    match Ident.Map.find id t.current_values_of_mutables_in_scope with
    | exception Not_found -> id
    | id -> id

  let rename_variables t ids =
    List.map (fun id -> rename_variable t id) ids
end

let add_exn_continuation_wrapper ~exn_continuation ~evaluate =
  evaluate exn_continuation

(* Required in the absence of backend changes
(* CR-someday mshinwell: Consider sharing the wrapper continuations. *)
  let extra_args = Env.extra_arguments_for_continuation env cont in
  match extra_args with
  | [] -> evaluate ~exn_continuation
  | extra_args ->
    let computation_cont = Continuation.create () in
    let wrapper_cont = Continuation.create () in
    let exn_bucket = Ident.create "exn" in
    Let_cont {
      name = wrapper_cont;
      administrative = false;
      is_exn_handler = true;
      params = [exn_bucket];
      recursive = Nonrecursive;
      handler = Apply_cont (cont, None, exn_bucket :: extra_args);
      body =
        Let_cont {
          name = computation_cont;
          administrative = false;
          is_exn_handler = false;
          params = [];
          recursive = Nonrecursive;
          handler = evaluate_primitive ~exn_continuation:wrapper_cont;
          body =
            Apply_cont (computation_cont,
              Some (Push { exn_handler = wrapper_cont; }),
              []);
        };
    }
*)

let rec transform_expr env (expr : Ilambda.t) : Ilambda.t =
  match expr with
  | Let (id, kind, named, body) ->
    transform_named env id kind named (fun env ->
      let body = transform_body env body in
      Let (id, kind, named, body))
  | Let_mutable let_mutable -> transform_let_mutable env let_mutable
  | Let_rec (func_decls, body) ->
    let func_decls =
      List.map (fun (id, func_decl) ->
          id, transform_function_declaration env func_decl)
        func_decls
    in
    let body = transform_body env body in
    Let_rec (func_decls, body)
  | Let_cont let_cont -> Let_cont (transform_let_cont env let_cont)
  | Apply ({ exn_continuation; _ } as apply) ->
    let apply = transform_apply env apply in
    let evaluate ~exn_continuation =
      Apply { apply with exn_continuation; }
    in
    add_exn_continuation_wrapper ~exn_continuation ~evaluate
  | Apply_cont (cont, trap_action, args) ->
    let args = Env.rename_variables env args in
    let extra_args = Env.extra_arguments_for_continuation env cont in
    Apply_cont (cont, trap_action, args @ extra_args)
  | Switch (id, switch) ->
    let id = Env.rename_variable env id in
    Switch (id, switch)
  | Event (expr, event) ->
    let expr = transform_expr env expr in
    Event (expr, event)

and transform_named env id kind (named : Ilambda.named) k : Ilambda.expr =
  let normal_case named = Let (id, kind, named, k env) in
  match named with
  | Var id -> normal_case (Var (Env.rename_variable env id))
  | Const _ -> normal_case named
  | Prim { prim; args; loc; exn_continuation; } ->
    let args = Env.rename_variables env args in
    let evaluate ~exn_continuation =
      normal_case (Prim { prim; args; loc; exn_continuation; })
    in
    begin match exn_continuation with
    | None -> evaluate ~exn_continuation
    | Some cont ->
      add_exn_continuation_wrapper ~exn_continuation ~evaluate
    end
  | Assign { being_assigned; new_value; } ->
    let env, new_id, new_kind = Env.new_id_for_mutable env being_assigned in
    Let (new_id, new_kind, Var new_value,
      Let (id, kind, Const (Const_base (Const_int 0)), k env))

and transform_let_mutable env
      ({ id; initial_value; contents_kind; body; } : Ilambda.let_mutable)
      : Ilambda.t =
  let env, new_id = Env.add_mutable env id contents_kind in
  let body = transform_expr env body in
  Let (new_id, contents_kind, Var initial_value, body)

and transform_let_cont env
      ({ name; administrative; is_exn_handler; params; recursive;
         body; handler;
       } : Ilambda.let_cont)
      : Ilambda.let_cont =
  let { body_env; handler_env; extra_params } =
    Env.add_continuation env name recursive
  in
  let is_exn_handler =
    is_exn_handler &&
      match extra_params with
      | [] -> true
      | _ -> false
  in
  { name; 
    administrative;
    is_exn_handler;
    params = params @ extra_params;
    recursive;
    body = transform_expr body_env body;
    handler = transform_expr handler_env handler;
  }

and transform_apply env
      ({ kind; func; args; continuation; exn_continuation;
         loc; should_be_tailcall; inlined; specialised;
       } : Ilambda.apply) : Ilambda.apply =
  let kind : Ilambda.apply_kind =
    match kind with
    | Function -> Function
    | Method { kind; obj; } ->
      Method {
        kind;
        obj = Env.rename_variable env obj;
      }
  in
  { kind;
    func = Env.rename_variable env func;
    args = Env.rename_variables env args;
    continuation;
    exn_continuation;
    loc;
    should_be_tailcall;
    inlined;
    specialised;
  }

let run (program : Ilambda.program) =
  let expr = transform_expr Env.empty program.expr in
  { program with
    expr;
  }