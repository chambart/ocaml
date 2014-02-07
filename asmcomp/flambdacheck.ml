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

let fatal_error_f fmt = Printf.kprintf Misc.fatal_error fmt

type 'a counter_example =
  | No_counter_example
  | Counter_example of 'a

exception Counter_example_id of Ident.t

let every_used_identifier_is_bound flam =
  let test var env =
    if not (Ident.Set.mem var env)
    then raise (Counter_example_id var) in
  let check env = function
    | Fassign(id,_,_)
    | Fvar(id,_) -> test id env
    | Fclosure({cl_specialised_arg},_) ->
      Ident.Map.iter (fun _ id -> test id env) cl_specialised_arg
    | _ -> ()
  in
  let rec dispach env = function
    | Flet(_,id,def,body,_) ->
      loop env def;
      loop (Ident.Set.add id env) body
    | Fletrec(defs,body,_) ->
      let env =
        List.fold_left (fun env (id,_) -> Ident.Set.add id env) env defs in
      List.iter (fun (_,def) -> loop env def) defs;
      loop env body
    | Fclosure ({cl_fun;cl_free_var},_) ->
      Ident.Map.iter (fun _ v -> loop env v) cl_free_var;
      let env = Ident.Set.empty in
      let env =
        if cl_fun.recursives
        then Ident.Map.fold (fun id _ env -> Ident.Set.add id env) cl_fun.funs env
        else env in
      Ident.Map.iter (fun _ { params; closure_params; body } ->
          let env = List.fold_right Ident.Set.add params env in
          let env = Ident.Set.union closure_params env in
          loop env body)
        cl_fun.funs
    | Ffor (id, lo, hi, _, body, _) ->
      loop env lo; loop env hi;
      loop (Ident.Set.add id env) body
    | Fcatch (i, vars, body, handler,_) ->
      loop env body;
      let env = List.fold_right Ident.Set.add vars env in
      loop env handler
    | Ftrywith(body, id, handler,_) ->
      loop env body;
      loop (Ident.Set.add id env) handler
    | exp -> loop env exp
  and loop env exp =
    check env exp;
    Flambdaiter.apply_on_subexpressions (dispach env) exp
  in
  let env = Ident.Set.empty in
  try
    loop env flam;
    No_counter_example
  with Counter_example_id var ->
    Counter_example var

let no_identifier_bound_multiple_times flam =
  let bound = ref Ident.Set.empty in
  let add_and_check id =
    if Ident.Set.mem id !bound
    then raise (Counter_example_id id)
    else bound := Ident.Set.add id !bound
  in
  let f = function
    | Flet(_,id,_,_,_) ->
      add_and_check id
    | Fletrec(defs,_,_) ->
      List.iter (fun (id,_) -> add_and_check id) defs
    | Fclosure ({cl_fun;cl_free_var},_) ->
      Ident.Map.iter (fun id _ -> add_and_check id) cl_free_var;
      Ident.Map.iter (fun _ { params } -> List.iter add_and_check params)
        cl_fun.funs
    | Ffor (id,_,_,_,_,_) ->
      add_and_check id
    | Fcatch (_,vars,_,_,_) ->
      List.iter add_and_check vars
    | Ftrywith(_, id,_,_) ->
      add_and_check id
    | _ -> ()
  in
  try
    Flambdaiter.iter f flam;
    No_counter_example
  with Counter_example_id var ->
    Counter_example var

let no_assign_on_variable_of_kind_strict flam =
  let test var env =
    if not (Ident.Set.mem var env)
    then raise (Counter_example_id var) in
  let check env = function
    | Fassign(id,_,_) -> test id env
    | _ -> ()
  in
  let rec dispach env = function
    | Flet(Variable,id,def,body,_) ->
      loop env def;
      loop (Ident.Set.add id env) body
    | Fclosure ({cl_fun;cl_free_var},_) ->
      Ident.Map.iter (fun _ v -> loop env v) cl_free_var;
      let env = Ident.Set.empty in
      Ident.Map.iter (fun _ { body } -> loop env body) cl_fun.funs
    | exp -> loop env exp
  and loop env exp =
    check env exp;
    Flambdaiter.apply_on_subexpressions (dispach env) exp
  in
  let env = Ident.Set.empty in
  try
    loop env flam;
    No_counter_example
  with Counter_example_id var ->
    Counter_example var

let declared_variable_within_closure flam =
  let bound = ref ClosureVariableSet.empty in
  let bound_multiple_times = ref None in
  let add_and_check var =
    if ClosureVariableSet.mem var !bound
    then bound_multiple_times := Some var;
    bound := ClosureVariableSet.add var !bound
  in
  let f = function
    | Fclosure ({cl_fun;cl_free_var},_) ->
      let compilation_unit = cl_fun.unit in
      Ident.Map.iter (fun id _ ->
          let var = Closure_variable.create ~compilation_unit id in
          add_and_check var) cl_free_var
    | _ -> ()
  in
  Flambdaiter.iter f flam;
  !bound, !bound_multiple_times

let no_variable_within_closure_is_bound_multiple_times flam =
  match declared_variable_within_closure flam with
  | _, Some var -> Counter_example var
  | _, None -> No_counter_example

exception Counter_example_sym of symbol

let every_declared_closure_is_from_current_compilation_unit
    ~current_compilation_unit flam =
  let f = function
    | Fclosure ({cl_fun = { unit }},_) ->
      if not (Symbol.equal unit current_compilation_unit)
      then raise (Counter_example_sym unit)
    | _ -> ()
  in
  try
    Flambdaiter.iter f flam;
    No_counter_example
  with Counter_example_sym sym ->
    Counter_example sym

let declared_function_within_closure flam =
  let bound = ref ClosureFunctionSet.empty in
  let bound_multiple_times = ref None in
  let add_and_check var =
    if ClosureFunctionSet.mem var !bound
    then bound_multiple_times := Some var;
    bound := ClosureFunctionSet.add var !bound
  in
  let f = function
    | Fclosure ({cl_fun},_) ->
      let compilation_unit = cl_fun.unit in
      Ident.Map.iter (fun id _ ->
          let var = Closure_function.create ~compilation_unit id in
          add_and_check var) cl_fun.funs
    | _ -> ()
  in
  Flambdaiter.iter f flam;
  !bound, !bound_multiple_times

let no_function_within_closure_is_bound_multiple_times flam =
  match declared_function_within_closure flam with
  | _, Some var -> Counter_example var
  | _, None -> No_counter_example

let used_function_within_closure flam =
  let used = ref ClosureFunctionSet.empty in
  let f = function
    | Ffunction ({fu_fun;fu_relative_to},_) ->
      used := ClosureFunctionSet.add fu_fun !used;
      (match fu_relative_to with
       | None -> ()
       | Some rel ->
         used := ClosureFunctionSet.add fu_fun !used)
    | Fvariable_in_closure ({vc_fun},_) ->
      used := ClosureFunctionSet.add vc_fun !used
    | _ -> ()
  in
  Flambdaiter.iter f flam;
  !used

let used_variable_within_closure flam =
  let used = ref ClosureVariableSet.empty in
  let f = function
    | Fvariable_in_closure ({vc_var},_) ->
      used := ClosureVariableSet.add vc_var !used
    | _ -> ()
  in
  Flambdaiter.iter f flam;
  !used

let every_used_function_from_current_compilation_unit_is_declared
    ~current_compilation_unit flam =
  let declared, _ = declared_function_within_closure flam in
  let used = used_function_within_closure flam in
  let used_from_current_unit =
    ClosureFunctionSet.filter (fun func ->
        Symbol.equal
          (Closure_function.compilation_unit func)
          current_compilation_unit)
      used in
  let counter_examples =
    ClosureFunctionSet.diff used_from_current_unit declared in
  if ClosureFunctionSet.is_empty counter_examples
  then No_counter_example
  else Counter_example counter_examples

let every_used_variable_in_closure_from_current_compilation_unit_is_declared
    ~current_compilation_unit flam =
  let declared, _ = declared_variable_within_closure flam in
  let used = used_variable_within_closure flam in
  let used_from_current_unit =
    ClosureVariableSet.filter (fun var ->
        Symbol.equal
          (Closure_variable.compilation_unit var)
          current_compilation_unit)
      used in
  let counter_examples =
    ClosureVariableSet.diff used_from_current_unit declared in
  if ClosureVariableSet.is_empty counter_examples
  then No_counter_example
  else Counter_example counter_examples

exception Counter_example_int of int

let every_static_exception_is_caught flam =
  let check env = function
    | Fstaticfail(exn,_,_) ->
      if not (IntSet.mem exn env)
      then raise (Counter_example_int exn)
    | _ -> ()
  in
  let rec dispach env = function
    | Fcatch (i, _, body, handler,_) ->
      loop env handler;
      let env = IntSet.add i env in
      loop env body
    | exp -> loop env exp
  and loop env exp =
    check env exp;
    Flambdaiter.apply_on_subexpressions (dispach env) exp
  in
  let env = IntSet.empty in
  try
    loop env flam;
    No_counter_example
  with Counter_example_int var ->
    Counter_example var

let every_static_exception_is_caught_at_a_single_position flam =
  let caught = ref IntSet.empty in
  let f = function
    | Fcatch (i, _, body, handler,_) ->
      if IntSet.mem i !caught
      then raise (Counter_example_int i);
      caught := IntSet.add i !caught
    | _ -> ()
  in
  try
    Flambdaiter.iter f flam;
    No_counter_example
  with Counter_example_int var ->
    Counter_example var

let test result fmt printer =
  match result with
  | No_counter_example -> ()
  | Counter_example ce ->
    Misc.fatal_error (Format.asprintf fmt printer ce)

let check ~current_compilation_unit flam =
  test (every_used_identifier_is_bound flam)
    "Unbound identifier %a" Ident.print;

  test (no_identifier_bound_multiple_times flam)
    "identifier bound multiple times %a" Ident.print;

  test (no_assign_on_variable_of_kind_strict flam)
    "variable %a of kind strict is assigned" Ident.print;

  test (no_variable_within_closure_is_bound_multiple_times flam)
    "variable within closure %a bound multiple times"
    Closure_variable.print;

  test (no_function_within_closure_is_bound_multiple_times flam)
    "function within closure %a bound multiple times"
    Closure_function.print;

  test (every_declared_closure_is_from_current_compilation_unit
         ~current_compilation_unit flam)
    "function declare using unit %a which is not the current one"
    Symbol.print;

  test (every_used_function_from_current_compilation_unit_is_declared
         ~current_compilation_unit flam)
    "functions %a from the current compilation unit are used but \
     not declared"
    ClosureFunctionSet.print;

  test (every_used_variable_in_closure_from_current_compilation_unit_is_declared
          ~current_compilation_unit flam)
    "variables %a from the current compilation unit are used but \
      not declared"
    ClosureVariableSet.print;

  test (every_static_exception_is_caught flam)
    "static exception %a can't be caught"
    Format.pp_print_int;

  test (every_static_exception_is_caught_at_a_single_position flam)
    "multiple catch point for exception %a"
    Format.pp_print_int
