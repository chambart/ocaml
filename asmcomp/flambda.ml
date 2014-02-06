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

open Asttypes
open Lambda
open Ext_types

type symbol = { sym_unit : Ident.t; sym_label : string }

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

type offset = {
  off_id : Ident.t;
  off_unit : symbol;
}

module Offset = struct
  type t = offset
  let compare x y =
    let c = Ident.compare x.off_id y.off_id in
    if c <> 0
    then c
    else Symbol.compare x.off_unit y.off_unit
  let output oc x =
    Printf.fprintf oc "%s.%a" x.off_unit.sym_label
      Ident.output x.off_id
  let print ppf x =
    Format.fprintf ppf "%s.%a" x.off_unit.sym_label
      Ident.print x.off_id
  let hash off = Hashtbl.hash off
  let equal o1 o2 = compare o1 o2 = 0
end

module OffsetMap = ExtMap(Offset)
module OffsetSet = ExtSet(Offset)
module OffsetTbl = ExtHashtbl(Offset)

module M : sig
  type function_label = private string
  val make_function_label : string -> function_label
end = struct
  type function_label = string
  let make_function_label str = str
end

include M

type 'a flambda =
  | Fsymbol of symbol * 'a
  | Fvar of Ident.t * 'a
  | Fconst of const * 'a
  | Fapply of 'a flambda * 'a flambda list *
                offset option * Debuginfo.t * 'a
  | Fclosure of 'a ffunctions * 'a flambda Ident.Map.t * Ident.t Ident.Map.t * 'a
  | Foffset of 'a flambda * offset * offset option * 'a
  | Fenv_field of 'a fenv_field * 'a
  | Flet of let_kind * Ident.t * 'a flambda * 'a flambda * 'a
  | Fletrec of (Ident.t * 'a flambda) list * 'a flambda * 'a
  | Fprim of primitive * 'a flambda list * Debuginfo.t * 'a
  | Fswitch of 'a flambda * 'a flambda_switch * 'a
  | Fstaticfail of int * 'a flambda list * 'a
  | Fcatch of int * Ident.t list * 'a flambda * 'a flambda * 'a
  | Ftrywith of 'a flambda * Ident.t * 'a flambda * 'a
  | Fifthenelse of 'a flambda * 'a flambda * 'a flambda * 'a
  | Fsequence of 'a flambda * 'a flambda * 'a
  | Fwhile of 'a flambda * 'a flambda * 'a
  | Ffor of Ident.t * 'a flambda * 'a flambda * direction_flag * 'a flambda * 'a
  | Fassign of Ident.t * 'a flambda * 'a
  | Fsend of meth_kind * 'a flambda * 'a flambda * 'a flambda list * Debuginfo.t * 'a
  | Funreachable of 'a

and const =
  | Fconst_base of constant
  | Fconst_pointer of int
  | Fconst_float_array of string list
  | Fconst_immstring of string

and 'a flambda_switch =
  { fs_numconsts: IntSet.t;
    fs_consts: (int * 'a flambda) list;
    fs_numblocks: IntSet.t;
    fs_blocks: (int * 'a flambda) list;
    fs_failaction : 'a flambda option }

and 'a ffunction = {
  label  : function_label;
  stub   : bool;
  arity  : int;
  params : Ident.t list;
  closure_params : Ident.Set.t;
  body   : 'a flambda;
  dbg    : Debuginfo.t;
}

and 'a ffunctions = {
  ident  : FunId.t;
  funs   : 'a ffunction Ident.Map.t;
  unit   : symbol;
  closed : bool;
  recursives : bool;
}

and 'a fenv_field = {
  env : 'a flambda;
  env_fun_id : offset;
  env_var : offset;
}

let can_be_merged f1 f2 = match f1,f2 with
  | Fsymbol (sym1, _), Fsymbol (sym2, _) ->
    Symbol.equal sym1 sym2
  | Fvar (id1, _), Fvar (id2, _) ->
    Ident.equal id1 id2
  | Fconst (c1, _), Fconst (c2, _) -> begin
      match c1, c2 with
      | Fconst_base (Const_string c1), Fconst_base (Const_string c2) ->
        false
      | _, _ -> c1 = c2
    end
  | _ -> false

(* utility functions *)

let data = function
  | Fsymbol (_,data)
  | Fvar (_,data)
  | Fconst (_,data)
  | Flet(_,_,_,_,data)
  | Fletrec(_,_,data)
  | Fclosure(_,_,_, data)
  | Foffset(_,_,_,data)
  | Fenv_field(_,data)
  | Fapply(_,_,_,_,data)
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

let string_desc = function
  | Fsymbol ({sym_label},_) -> Printf.sprintf "%%%s" sym_label
  | Fvar (id,data) -> Ident.unique_name id
  | Fconst (cst,data) -> "const"
  | Flet(str, id, lam, body,data) ->
    Printf.sprintf "let %s"
      (Ident.unique_name id)
  | Fletrec(defs, body,data) -> "letrec"
  | Fclosure(funct, fv, spec_arg, data) -> "closure"
  | Foffset(lam,id,rel,data) -> "offset"
  | Fenv_field(_,data) -> "env_field"
  | Fapply(funct, args, _, _,data) -> "apply"
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
