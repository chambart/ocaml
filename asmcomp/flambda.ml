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

type linkage_name = string

type symbol = { sym_unit : Ident.t; sym_label : linkage_name }

module Symbol = struct
  type t = symbol
  let compare s1 s2 = String.compare s1.sym_label s2.sym_label
  (** Labels are unique, so comparing them is sufficient. It also could
      uncover bugs to consider same labels from different modules equal *)
  let output c s = output_string c s.sym_label
  let hash s = Hashtbl.hash s.sym_label
  let equal s1 s2 = s1.sym_label = s2.sym_label
  let print ppf s =
    Format.fprintf ppf "%a - %s" Ident.print s.sym_unit s.sym_label
end

module SymbolSet = ExtSet(Symbol)
module SymbolMap = ExtMap(Symbol)
module SymbolTbl = ExtHashtbl(Symbol)

module ExprId : Id = Id(struct end)
module ExprMap = ExtMap(ExprId)
module ExprSet = ExtSet(ExprId)
module ExprTbl = ExtHashtbl(ExprId)

module FunInnerid : Id = Id(struct end)
module FunId : UnitId = UnitId(FunInnerid)
module FunMap = ExtMap(FunId)
module FunSet = ExtSet(FunId)
module FunTbl = ExtHashtbl(FunId)

type closure_element = {
  ce_id : Ident.t;
  ce_unit : symbol;
}

type function_within_closure = closure_element
type variable_within_closure = closure_element

module Closure_element = struct
  type t = closure_element
  let compare x y =
    let c = Ident.compare x.ce_id y.ce_id in
    if c <> 0
    then c
    else Symbol.compare x.ce_unit y.ce_unit
  let output oc x =
    Printf.fprintf oc "%s.%a" x.ce_unit.sym_label
      Ident.output x.ce_id
  let print ppf x =
    Format.fprintf ppf "%s.%a" x.ce_unit.sym_label
      Ident.print x.ce_id
  let hash off = Hashtbl.hash off
  let equal o1 o2 = compare o1 o2 = 0

  let create ~compilation_unit ce_id = { ce_unit = compilation_unit; ce_id }
  let compilation_unit { ce_unit } = ce_unit
end

module Closure_function = Closure_element
module Closure_variable = Closure_element

module Function_label = struct
  include Ext_types.String_M
  let create str = str
end

type function_label = Function_label.t

module ClosureFunctionMap = ExtMap(Closure_function)
module ClosureFunctionSet = ExtSet(Closure_function)
module ClosureFunctionTbl = ExtHashtbl(Closure_function)

module ClosureVariableMap = ExtMap(Closure_variable)
module ClosureVariableSet = ExtSet(Closure_variable)
module ClosureVariableTbl = ExtHashtbl(Closure_variable)

type let_kind = Strict | Variable

type call_kind =
  | Indirect
  | Direct of function_within_closure

type 'a flambda =
  | Fsymbol of symbol * 'a
  | Fvar of Ident.t * 'a
  | Fconst of const * 'a
  | Fapply of 'a apply * 'a
  | Fclosure of 'a closure * 'a
  | Ffunction of 'a funct * 'a
  | Fvariable_in_closure of 'a variable_in_closure * 'a
  | Flet of let_kind * Ident.t * 'a flambda * 'a flambda * 'a
  | Fletrec of (Ident.t * 'a flambda) list * 'a flambda * 'a
  | Fprim of Lambda.primitive * 'a flambda list * Debuginfo.t * 'a
  | Fswitch of 'a flambda * 'a flambda_switch * 'a
  | Fstaticfail of int * 'a flambda list * 'a
  | Fcatch of int * Ident.t list * 'a flambda * 'a flambda * 'a
  | Ftrywith of 'a flambda * Ident.t * 'a flambda * 'a
  | Fifthenelse of 'a flambda * 'a flambda * 'a flambda * 'a
  | Fsequence of 'a flambda * 'a flambda * 'a
  | Fwhile of 'a flambda * 'a flambda * 'a
  | Ffor of Ident.t * 'a flambda * 'a flambda * Asttypes.direction_flag * 'a flambda * 'a
  | Fassign of Ident.t * 'a flambda * 'a
  | Fsend of Lambda.meth_kind * 'a flambda * 'a flambda * 'a flambda list * Debuginfo.t * 'a
  | Funreachable of 'a

and const =
  | Fconst_base of Asttypes.constant
  | Fconst_pointer of int
  | Fconst_float_array of string list
  | Fconst_immstring of string

and 'a flambda_switch =
  { fs_numconsts: IntSet.t;
    fs_consts: (int * 'a flambda) list;
    fs_numblocks: IntSet.t;
    fs_blocks: (int * 'a flambda) list;
    fs_failaction : 'a flambda option }

and 'a apply =
  { ap_function: 'a flambda;
    ap_arg: 'a flambda list;
    ap_kind: call_kind;
    ap_dbg: Debuginfo.t }

and 'a closure =
  { cl_fun : 'a ffunctions;
    cl_free_var : 'a flambda Ident.Map.t;
    cl_specialised_arg : Ident.t Ident.Map.t }

and 'a ffunction = {
  label  : function_label;
  stub   : bool;
  arity  : int;
  params : Ident.t list;
  free_variables : Ident.Set.t;
  body   : 'a flambda;
  dbg    : Debuginfo.t;
}

and 'a ffunctions = {
  ident  : FunId.t;
  funs   : 'a ffunction Ident.Map.t;
  compilation_unit : symbol;
  closed : bool;
  contains_recursive_function : bool;
}

and 'a funct = {
  fu_closure: 'a flambda;
  fu_fun: function_within_closure;
  fu_relative_to: function_within_closure option;
}

and 'a variable_in_closure = {
  vc_closure : 'a flambda;
  vc_fun : function_within_closure;
  vc_var : variable_within_closure;
}

let can_be_merged f1 f2 = match f1,f2 with
  | Fsymbol (sym1, _), Fsymbol (sym2, _) ->
    Symbol.equal sym1 sym2
  | Fvar (id1, _), Fvar (id2, _) ->
    Ident.equal id1 id2
  | Fconst (c1, _), Fconst (c2, _) -> begin
      let open Asttypes in
      match c1, c2 with
      | Fconst_base (Const_string _), _ ->
        false (* string constants can't be merged: they are mutable *)
      | Fconst_base (Const_int _ | Const_char _ | Const_float _ |
                     Const_int32 _ | Const_int64 _ | Const_nativeint _), _
      | Fconst_pointer _, _
      | Fconst_float_array _, _
      | Fconst_immstring _, _ -> c1 = c2
    end
  | _ -> false

(* utility functions *)

let data_at_toplevel_node = function
  | Fsymbol (_,data)
  | Fvar (_,data)
  | Fconst (_,data)
  | Flet(_,_,_,_,data)
  | Fletrec(_,_,data)
  | Fclosure(_,data)
  | Ffunction(_,data)
  | Fvariable_in_closure(_,data)
  | Fapply(_,data)
  | Fswitch(_,_,data)
  | Fsend(_,_,_,_,_,data)
  | Fprim(_,_,_,data)
  | Fstaticfail (_,_,data)
  | Fcatch (_,_,_,_,data)
  | Ftrywith(_,_,_,data)
  | Fifthenelse(_,_,_,data)
  | Fsequence(_,_,data)
  | Fwhile(_,_,data)
  | Ffor(_,_,_,_,_,data)
  | Fassign(_,_,data)
  | Funreachable data -> data

let description_of_toplevel_node = function
  | Fsymbol ({sym_label},_) -> Printf.sprintf "%%%s" sym_label
  | Fvar (id,data) -> Ident.unique_name id
  | Fconst (cst,data) -> "const"
  | Flet(str, id, lam, body,data) ->
    Printf.sprintf "let %s"
      (Ident.unique_name id)
  | Fletrec(defs, body,data) -> "letrec"
  | Fclosure(_,data) -> "closure"
  | Ffunction(_,data) -> "function"
  | Fvariable_in_closure(_,data) -> "variable_in_closure"
  | Fapply(_,data) -> "apply"
  | Fswitch(arg, sw,data) -> "switch"
  | Fsend(kind, met, obj, args, _,data) -> "send"
  | Fprim(_, args, _,data) -> "prim"
  | Fstaticfail (i, args,data) -> "staticfail"
  | Fcatch (i, vars, body, handler,data) -> "catch"
  | Ftrywith(body, id, handler,data) -> "trywith"
  | Fifthenelse(arg, ifso, ifnot,data) -> "if"
  | Fsequence(lam1, lam2,data) -> "seq"
  | Fwhile(cond, body,data) -> "while"
  | Ffor(id, lo, hi, dir, body,data) -> "for"
  | Fassign(id, lam,data) -> "assign"
  | Funreachable _ -> "unreachable"
