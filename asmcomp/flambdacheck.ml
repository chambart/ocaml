(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                     Pierre Chambart, OCamlPro                       *)
(*                                                                     *)
(*  Copyright 2014 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Ext_types
open Flambda

type 'a env = {
  current_unit : symbol;
  bound_variables : Ident.Set.t;
  seen_variables : Ident.Set.t ref;
  seen_fun_label : StringSet.t ref;
  seen_static_catch : IntSet.t ref;
  need_closure_var : ClosureVariableSet.t ref;
  seen_closure_var : ClosureVariableSet.t ref;
  need_function : ClosureFunctionSet.t ref;
  seen_function : ClosureFunctionSet.t ref;
  caught_static_exceptions : IntSet.t;
}

let fatal_error_f fmt = Printf.kprintf Misc.fatal_error fmt

let closure_env env =
  { env with
    bound_variables = Ident.Set.empty;
    caught_static_exceptions = IntSet.empty }

let record_var env id =
  if Ident.Set.mem id !(env.seen_variables)
  then fatal_error_f "Flambda.check: variable %s bound multiple times"
      (Ident.unique_name id);
  env.seen_variables := Ident.Set.add id !(env.seen_variables)

let add_var id env =
  { env with bound_variables = Ident.Set.add id env.bound_variables }

let bind_var id env =
  record_var env id;
  add_var id env

let check_var id env =
  if not (Ident.Set.mem id env.bound_variables)
  then fatal_error_f "Flambda.check: unbound variable %s" (Ident.unique_name id)

(* We can't easilly check here that a variable or a function are effectively
   available if they come from a different compilation unit *)
let need_closure_var var_offset env =
  if Symbol.equal (Closure_variable.compilation_unit var_offset) env.current_unit
  then env.need_closure_var := ClosureVariableSet.add var_offset !(env.need_closure_var)

let seen_closure_var var_offset env =
  env.seen_closure_var := ClosureVariableSet.add var_offset !(env.seen_closure_var)

let need_function offset env =
  if Symbol.equal (Closure_function.compilation_unit offset) env.current_unit
  then
    env.need_function := ClosureFunctionSet.add offset !(env.need_function)

let seen_function offset env =
  env.seen_function := ClosureFunctionSet.add offset !(env.seen_function)

let add_check_static_catch n env =
  if IntSet.mem n !(env.seen_static_catch)
  then fatal_error_f  "Flambda.check: static exception %i caught \
                       at multiple places" n;
  env.seen_static_catch := IntSet.add n !(env.seen_static_catch);
  { env with
    caught_static_exceptions = IntSet.add n env.caught_static_exceptions }

let record_fun_label lbl env =
  if StringSet.mem lbl !(env.seen_fun_label)
  then fatal_error_f "Flambda.check: function %s appear multiple times" lbl;
  env.seen_fun_label := StringSet.add lbl !(env.seen_fun_label)

let rec check env = function
  | Fsymbol _ -> ()
  | Fvar (id,_) -> check_var id env
  | Fconst (cst,_) -> ()
  | Flet(str, id, lam, body,_) ->
    check env lam;
    let env = bind_var id env in
    check env body
  | Fletrec(defs, body,_) ->
    let env = List.fold_left (fun env (id,lam) -> bind_var id env) env defs in
    List.iter (fun (_,def) -> check env def) defs;
    check env body
  | Fclosure({cl_fun;cl_free_var = fv;
              cl_specialised_arg = spec_arg}, _) ->
    Ident.Map.iter (fun _ lam -> check env lam) fv;
    Ident.Map.iter (fun param id' ->
        (* a specialised parameter must be a parameter of a function
           in the closure *)
        if not (Ident.Map.exists (fun _ ffun -> List.mem param ffun.params) cl_fun.funs)
        then fatal_error_f "Flambda.check: %s is not a function argument"
            (Ident.unique_name id');
        check_var id' env)
      spec_arg;
    (* The code inside a closure can't access variable bound outside of the closure,
       closure_env removes it *)
    check_closure (closure_env env) cl_fun fv
  | Ffunction({of_closure = lam; of_fun = offset;
               of_relative_to = relative_offset}, _) ->
    begin match relative_offset with
      | None -> ()
      | Some rel_offset ->
        need_function rel_offset env;
        if not (Symbol.equal
                  (Closure_function.compilation_unit offset)
                  (Closure_function.compilation_unit rel_offset))
        then Misc.fatal_error "Flambda.check relative offset from a different\
                               compilation unit"
    end;
    need_function offset env;
    check env lam
  | Fvariable_in_closure({ vc_closure = env_lam; vc_fun = env_fun_id;
                           vc_var = env_var },_) ->
    if not (Symbol.equal
              (Closure_function.compilation_unit env_fun_id)
              (Closure_variable.compilation_unit env_var))
    then Misc.fatal_error "Flambda.check closure variable and function comes\
                           from a different compilation units";
    need_closure_var env_var env;
    check env env_lam
  | Fapply({ap_function = funct; ap_arg = args},_) ->
    check env funct;
    List.iter (check env) args
  | Fswitch(arg, sw,_) ->
    check env arg;
    List.iter (fun (_,l) -> check env l) sw.fs_consts;
    List.iter (fun (_,l) -> check env l) sw.fs_blocks
  | Fsend(kind, met, obj, args, _,_) ->
    check env met;
    check env obj;
    List.iter (check env) args
  | Fprim(_, args, _,_) ->
    List.iter (check env) args
  | Fstaticfail (i, args,_) ->
    if not (IntSet.mem i env.caught_static_exceptions)
    then fatal_error_f "Flambda.check: uncaught static exception %i" i;
    List.iter (check env) args
  | Fcatch (i, vars, body, handler,_) ->
    let env' = add_check_static_catch i env in
    check env' body;
    let env = List.fold_right bind_var vars env in
    check env handler
  | Ftrywith(body, id, handler,_) ->
    check env body;
    let env = bind_var id env in
    check env handler
  | Fifthenelse(arg, ifso, ifnot,_) ->
    check env arg;
    check env ifso;
    check env ifnot
  | Fsequence(lam1, lam2,_) ->
    check env lam1;
    check env lam2
  | Fwhile(cond, body,_) ->
    check env cond;
    check env body
  | Ffor(id, lo, hi, dir, body,_) ->
    check env lo; check env hi;
    let env = bind_var id env in
    check env body
  | Fassign(id, lam,_) ->
    check env lam
  | Funreachable _ -> ()

and check_closure env funct fv =
  Ident.Map.iter (fun id _ ->
      seen_closure_var (Closure_variable.create funct.unit id) env;
      record_var env id) fv;
  let env =
    if funct.recursives
    then Ident.Map.fold (fun id _ env -> bind_var id env) funct.funs env
    else (Ident.Map.iter (fun id _ -> record_var env id) funct.funs;
          env)
  in
  Ident.Map.iter (fun fun_id func ->
      seen_function (Closure_function.create funct.unit fun_id) env;
      record_fun_label (func.label:>string) env;
      let env = Ident.Set.fold (fun id env ->
          if not (Ident.Map.mem id fv)
          then fatal_error_f "Flambda.check: variable %s not in \
                              the closure" (Ident.unique_name id);
          add_var id env) func.closure_params env in
      let env = List.fold_right bind_var func.params env in
      check env func.body)
    funct.funs

let check ~current_unit flam =
  let env = { current_unit;
              bound_variables = Ident.Set.empty;
              seen_variables = ref Ident.Set.empty;
              seen_fun_label = ref StringSet.empty;
              seen_static_catch = ref IntSet.empty;
              need_closure_var = ref ClosureVariableSet.empty;
              seen_closure_var = ref ClosureVariableSet.empty;
              need_function = ref ClosureFunctionSet.empty;
              seen_function = ref ClosureFunctionSet.empty;
              caught_static_exceptions = IntSet.empty } in
  check env flam;
  let diff = ClosureVariableSet.diff !(env.need_closure_var) !(env.seen_closure_var) in
  if not (ClosureVariableSet.is_empty diff)
  then fatal_error_f "Flambda.check: closure variable %s is needed but not provided"
      (ClosureVariableSet.to_string diff);
  let diff = ClosureFunctionSet.diff !(env.need_function) !(env.seen_function) in
  if not (ClosureFunctionSet.is_empty diff)
  then fatal_error_f "Flambda.check: function %s is needed but not provided"
      (ClosureFunctionSet.to_string diff)
