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

(** A variant of lambda code with explicit closures, where every dependency
    is explicit

    The particularities are:
    * symbolic closure: closure fields are referenced
        by unique identifiers (type offset)
    * explicit external constants access: (type symbol)
    * direct calls are explicit, but still keep an explicit
      reference to the closure.
    * recursive closure are annotated to avoid traversal to check
    * each node carry a value that can be used for term identifiers
    * no structured constants (represented as prim(makeblock) )
*)

open Ext_types

type variable

type linkage_name

type symbol = { sym_unit : Ident.t; sym_label : linkage_name }
(** A symbol is an identifier of a constant provided by another
    compilation unit or of top level module.
    [sym_unit] is the compilation unit containing the value.
    [sym_lablel] is the linking name of the variable.
    The label must be globaly unique: two compilation units linked
    in the same program must not share labels *)

type compilation_unit

type function_within_closure
type variable_within_closure

val linkage_name : string -> linkage_name

module Variable : sig
  include PrintableHashOrdered with type t = variable
  val create : compilation_unit:compilation_unit -> Ident.t -> t
  val make : compilation_unit:compilation_unit -> string -> t
  val compilation_unit : t -> compilation_unit
  val rename : compilation_unit:compilation_unit -> t -> t
end

module Closure_function : sig
  include PrintableHashOrdered with type t = function_within_closure
  val create : variable -> t
  val compilation_unit : t -> compilation_unit
end
module Closure_variable : sig
  include PrintableHashOrdered with type t = variable_within_closure
  val create : variable -> t
  val compilation_unit : t -> compilation_unit
end

module Symbol : PrintableHashOrdered with type t = symbol
module Compilation_unit : sig
  include PrintableHashOrdered with type t = compilation_unit
  val create : symbol -> t
end

module ExprId : Id
module FunId : UnitId with module Compilation_unit := Compilation_unit

module VarSet : sig
  include ExtSet with module M := Variable
  val of_ident_set : compilation_unit:compilation_unit -> Lambda.IdentSet.t -> t
end
module VarMap : ExtMap with module M := Variable
module VarTbl : ExtHashtbl with module M := Variable

module SymbolSet : ExtSet with module M := Symbol
module SymbolMap : ExtMap with module M := Symbol
module SymbolTbl : ExtHashtbl with module M := Symbol

module ExprSet : ExtSet with module M := ExprId
module ExprMap : ExtMap with module M := ExprId
module ExprTbl : ExtHashtbl with module M := ExprId

module FunSet : ExtSet with module M := FunId
module FunMap : ExtMap with module M := FunId
module FunTbl : ExtHashtbl with module M := FunId

module ClosureFunctionSet : ExtSet with module M := Closure_function
module ClosureFunctionMap : ExtMap with module M := Closure_function
module ClosureFunctionTbl : ExtHashtbl with module M := Closure_function

module ClosureVariableSet : ExtSet with module M := Closure_variable
module ClosureVariableMap : ExtMap with module M := Closure_variable
module ClosureVariableTbl : ExtHashtbl with module M := Closure_variable

type let_kind = Strict | Variable (** See Lambda.let_kind *)

type call_kind =
  | Indirect
  | Direct of function_within_closure

(* A data is attached to each node. It is often used to uniquely
   identify an expression *)
type 'a flambda =
    Fsymbol of symbol * 'a
  | Fvar of variable * 'a
  | Fconst of const * 'a
  | Fapply of 'a apply * 'a
  | Fclosure of 'a closure * 'a
  (** This represents an unspecified closure: multiple function can be
      present in a closure, to call a function in the closure, we must
      first select a function using Ffunction.

      Typical usage when compiling
      {[let rec f x = ...
        and g x = ... ]}

      is to represent it as:
      {[Flet( closure, Fclosure { f -> ...; g -> ... },
              Flet( f, Ffunction { fu_closure = closure; fu_fun = f },
              Flet( g, Ffunction { fu_closure = closure; fu_fun = g }, ...)))]}
  *)
  | Ffunction of 'a funct * 'a
  | Fvariable_in_closure of 'a variable_in_closure * 'a
  | Flet of let_kind * variable * 'a flambda * 'a flambda * 'a
  | Fletrec of (variable * 'a flambda) list * 'a flambda * 'a
  | Fprim of Lambda.primitive * 'a flambda list * Debuginfo.t * 'a
  | Fswitch of 'a flambda * 'a flambda_switch * 'a
  | Fstaticfail of int * 'a flambda list * 'a
  | Fcatch of int * variable list * 'a flambda * 'a flambda * 'a
  | Ftrywith of 'a flambda * variable * 'a flambda * 'a
  | Fifthenelse of 'a flambda * 'a flambda * 'a flambda * 'a
  | Fsequence of 'a flambda * 'a flambda * 'a
  | Fwhile of 'a flambda * 'a flambda * 'a
  | Ffor of variable * 'a flambda * 'a flambda * Asttypes.direction_flag *
            'a flambda * 'a
  | Fassign of variable * 'a flambda * 'a
  | Fsend of Lambda.meth_kind * 'a flambda * 'a flambda * 'a flambda list *
             Debuginfo.t * 'a
  | Funreachable of 'a
  (** Represent a code that has been proved to be unreachable *)

and const =
  (* notice: no structured constant *)
    Fconst_base of Asttypes.constant
  | Fconst_pointer of int
  | Fconst_float_array of string list
  | Fconst_immstring of string

and 'a flambda_switch =
  { fs_numconsts: IntSet.t; (** integer cases *)
    fs_consts: (int * 'a flambda) list; (** Integer cases *)
    fs_numblocks: IntSet.t; (** Number of tag block cases *)
    fs_blocks: (int * 'a flambda) list; (** Tag block cases *)
    fs_failaction : 'a flambda option } (** Action to take if none matched *)

and 'a apply =
  { ap_function: 'a flambda;
    ap_arg: 'a flambda list;
    ap_kind: call_kind;
    ap_dbg: Debuginfo.t }

and 'a closure =
  { cl_fun : 'a function_declarations;
    cl_free_var : 'a flambda VarMap.t;
    cl_specialised_arg : variable VarMap.t }

and 'a function_declaration = {
  stub : bool;
  (** A stub function is a generated function used to prepare
      arguments or return value to allow indirect calls to function
      with a special call convention. For instance indirect calls to
      tuplified function must go through a stub. Stubs will be
      unconditionnaly inlined. *)
  params : variable list;
  free_variables : VarSet.t;
  (** All free variables used in body, including function parameters,
      functions and variables declared in the closure.
      It is present for efficiency reasons. *)
  body : 'a flambda;
  dbg : Debuginfo.t;
}

and 'a function_declarations = {
  ident : FunId.t;
  funs : 'a function_declaration VarMap.t;
  (** The ident key correspond to off_id of offset type *)
  compilation_unit : compilation_unit;
  closed : bool;
}

and 'a funct = {
  fu_closure: 'a flambda;
  fu_fun: function_within_closure;
  fu_relative_to: function_within_closure option;
  (** Keeps track of the original function When specifying an already
      specified function. *)
}

and 'a variable_in_closure = {
  vc_closure : 'a flambda; (** A selected closure *)
  vc_fun : function_within_closure;
  vc_var : variable_within_closure;
}

(* utility functions *)

val function_arity : 'a function_declaration -> int

val can_be_merged : 'a flambda -> 'a flambda -> bool
(** If [can_be_merged f1 f2] is true, it is safe to merge switch
    branches containing [f1] and [f2] *)

val data_at_toplevel_node : 'a flambda -> 'a

val description_of_toplevel_node : 'a flambda -> string
