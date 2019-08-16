[@@@ocaml.warning "-30"]

type location = Location.t

type closure_id = string * location

type symbol = string * location
type variable = string * location
type variable_opt = (string * location) option
type continuation = string * location
type func_sym = symbol

type immediate = string
type targetint = int64

type const =
  | Naked_immediate of immediate
  | Tagged_immediate of immediate
  | Naked_float of float
  | Naked_int32 of Int32.t
  | Naked_int64 of Int64.t
  | Naked_nativeint of targetint

type of_kind_value =
  | Symbol of symbol
  | Tagged_immediate of immediate
  | Dynamically_computed of variable

type mutable_or_immutable =
  | Mutable
  | Immutable

type is_recursive =
  | Nonrecursive
  | Recursive

type tag_scannable = int

type static_part =
  | Block of tag_scannable * mutable_or_immutable * of_kind_value list

type kind = unit
type flambda_type = unit

type static_structure = (symbol * kind * static_part)

type invalid_term_semantics =
  | Treat_as_unreachable
  | Halt_and_catch_fire

type trap_action
type typed_parameter = {
  param : variable;
  ty : flambda_type;
}

type name =
  | Var of variable
  | Symbol of symbol

type simple =
  | Var of variable
  | Symbol of symbol
  | Const of const

type unop =
  | Opaque_identity

type binop =
  | Plus | Plusdot
  | Minus | Minusdot

type prim =
  | Unop of unop * simple
  | Binop of binop * simple * simple
  | Block of tag_scannable * mutable_or_immutable * simple list

type named =
  | Simple of simple
  | Prim of prim
  (* | Set_of_closures of Set_of_closures.t *)
  | Assign of {
      being_assigned : variable;
      new_value : simple;
    }
  | Read_mutable of variable

type is_fabricated =
  | Value | Fabricated

type flambda_arity = kind list

type function_call =
  | Direct of {
      closure_id : closure_id;
      return_arity : flambda_arity;
    }
  | Indirect_unknown_arity
  | Indirect_known_arity of {
      param_arity : flambda_arity;
      return_arity : flambda_arity;
    }

type method_kind = Self | Public | Cached

type call_kind =
  | Function of function_call
  | Method of { kind : method_kind; obj : name; }
  | C_call of {
      alloc : bool;
      (* param_arity : flambda_arity; To recover from args *)
      return_arity : flambda_arity option;
    }

type apply = {
    func : name;
    continuation : continuation;
    exn_continuation : continuation;
    args : simple list;
    call_kind : call_kind;
    (* dbg : Debuginfo.t; *)
    (* inline : inline_attribute;
     * specialise : specialise_attribute; *)
  }

type expr =
  | Let of let_
  | Let_mutable of {
      var : variable;
      initial_value : simple;
      kind : kind;
      body : expr;
    }
  | Let_cont of let_cont
  | Apply of apply
  | Apply_cont of continuation * trap_action option * simple list
  | Switch of {
      scrutinee : name;
      is_fabricated : is_fabricated;
      cases : (int * continuation) list;
    }
  | Invalid of invalid_term_semantics

and let_ = {
    var : variable_opt;
    kind : kind;
    defining_expr : named;
    body : expr;
  }

and let_cont = {
  recursive : is_recursive;
  body : expr;
  handlers : let_cont_handlers;
}

and let_cont_handlers = continuation_handler list

and continuation_handler = {
  name : continuation;
  params : typed_parameter list;
  stub : bool;
  is_exn_handler : bool;
  handler : expr;
}

type computation = {
  expr : expr;
  return_cont : continuation;
  exception_cont : continuation;
  computed_values : (variable * kind) list;
}

type definition = {
  computation : computation option;
  static_structure : static_structure list;
}

type let_code = {
  name : func_sym;
  params : typed_parameter list;
  ret_cont : continuation;
  exn_cont : continuation option;
  ret_arity : flambda_arity option;
  expr : expr;
}

type program_body_elt =
  | Root of symbol
  | Let_code of let_code
  | Define_symbol of is_recursive * definition

type program = program_body_elt list