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

type symbol = Ident.t * string
(** A symbol is an identifier of an external constant or top level module.
    the [Ident.t] refer to the compilation unit containing the value.
    the [string] is the linking name of the variable *)

type offset = {
  off_id : Ident.t;
  off_unit : symbol;
}
(** An offset refer to a variable or a function inside a
    closure.
    [off_id] is the name of the variable.
    [off_unit] is the compilation unit of the closure *)

module Offset : PrintableHashOrdered with type t = offset
module Symbol : PrintableHashOrdered with type t = symbol
module Idt : PrintableHashOrdered with type t = Ident.t

module ExprId : Id
module FunId : UnitId

module SymbolSet : ExtSet with module M := Symbol
module SymbolMap : ExtMap with module M := Symbol
module SymbolTbl : ExtHashtbl with module M := Symbol

module ExprSet : ExtSet with module M := ExprId
module ExprMap : ExtMap with module M := ExprId
module ExprTbl : ExtHashtbl with module M := ExprId

module FunSet : ExtSet with module M := FunId
module FunMap : ExtMap with module M := FunId
module FunTbl : ExtHashtbl with module M := FunId

module IdentSet : sig
  include Set.S with type t = Lambda.IdentSet.t and type elt = Ident.t
  val of_list : Ident.t list -> t
  val map : (Ident.t -> Ident.t) -> t -> t
end
module IdentMap : ExtMap with module M := Idt
module IdentTbl : ExtHashtbl with module M := Idt

module OffsetSet : ExtSet with module M := Offset
module OffsetMap : ExtMap with module M := Offset
module OffsetTbl : ExtHashtbl with module M := Offset

type function_label = private string
val make_function_label : string -> function_label

(* A data is attached to each node. It is often used to uniquely
   identify an expression *)
type 'a flambda =
    Fsymbol of symbol * 'a (** an external constant value *)
  | Fvar of Ident.t * 'a
  | Fconst of const * 'a

  | Fapply of 'a flambda * 'a flambda list * offset option * Debuginfo.t * 'a
  (** closure * parameters * direct call informations
      Direct call informations being [Some offset] means that
      only one function can be called here, and offset is its identifer *)

  | Fclosure of 'a ffunctions * 'a flambda IdentMap.t * Ident.t IdentMap.t * 'a
  (** functions description * bound variables * specialised variables.
      It is an unoffseted closure: multiple function can be
      present in a closure, to use the closure, we must tell
      which function it refers to by using Foffset.

      If an internal variable rely on the value of an external one, it
      must appear in the specialised variables map. *)

  | Foffset of 'a flambda * offset * offset option * 'a
  (** Transform an unoffseted closure into an offseted one by choosing
      the referenced function (offset).

      Foffset(closure, id, previous_offset) access to the function 'id'
      from the closure. If previous_offset is Some(off) this represent
      an offset to an already offseted function: this is a relative offset.
      It can appear when inlining multiply recursive functions
  *)

  | Fenv_field of 'a fenv_field * 'a
  (** Access a variable inside an offseted closure *)

  | Flet of Lambda.let_kind * Ident.t * 'a flambda * 'a flambda * 'a
  (** let_kind must be Variable or Strict *)
  | Fletrec of (Ident.t * 'a flambda) list * 'a flambda * 'a
  | Fprim of Lambda.primitive * 'a flambda list * Debuginfo.t * 'a
  | Fswitch of 'a flambda * 'a flambda_switch * 'a
  | Fstaticfail of int * 'a flambda list * 'a
  | Fcatch of int * Ident.t list * 'a flambda * 'a flambda * 'a
  | Ftrywith of 'a flambda * Ident.t * 'a flambda * 'a
  | Fifthenelse of 'a flambda * 'a flambda * 'a flambda * 'a
  | Fsequence of 'a flambda * 'a flambda * 'a
  | Fwhile of 'a flambda * 'a flambda * 'a
  | Ffor of Ident.t * 'a flambda * 'a flambda * Asttypes.direction_flag *
            'a flambda * 'a
  | Fassign of Ident.t * 'a flambda * 'a
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

and 'a flambda_switch = {
  fs_numconsts : Ext_types.IntSet.t;
  fs_consts : (int * 'a flambda) list;
  fs_numblocks : Ext_types.IntSet.t;
  fs_blocks : (int * 'a flambda) list;
  fs_failaction : 'a flambda option;
}

and 'a ffunction = {
  label : function_label; (** an unique name used for linking *)
  stub : bool; (** If true, the function should be unconditionnaly inlined. *)
  arity : int;
  params : Ident.t list; (** internal identifiers of parameters. *)
  closure_params : IdentSet.t; (** free variables used in the function *)
  body : 'a flambda;
  dbg : Debuginfo.t;
}

and 'a ffunctions = {
  ident : FunId.t;
  funs : 'a ffunction IdentMap.t;
  (** The ident key correspond to off_id of offset type *)
  unit : symbol;
  (** The compilation unit containing the closure *)
  closed : bool;
  recursives : bool;
  (** true if any of the function inside the closure is recursive *)
}

and 'a fenv_field = {
  env : 'a flambda; (** the closure *)
  env_fun_id : offset; (** the offset applied to the closure *)
  env_var : offset; (** the accessed variable *)
}

(* utility functions *)

val same : 'a flambda -> 'a flambda -> bool
(** [same f1 f2] is true if it can prove that f1 and f2 are
    semanticaly equal *)

val data : 'a flambda -> 'a
(** [data flam] retrieve the data associated with the top-level node
    of [flam] *)

val string_desc : 'a flambda -> string
(** [string_desc flam] returns a small description of the top-level
    node of [flam]. It is usefull to generate new names for
    identifiers to help debuging *)
