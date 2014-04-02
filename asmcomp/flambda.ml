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

type static_exception = int

module Compilation_unit : sig
  include PrintableHashOrdered
  val create : Ident.t -> linkage_name -> t
  val to_ident : t -> Ident.t
  (* Should be used only in Compilenv *)
  val name : t -> string
end = struct
  type t =
    { id: Ident.t;
      linkage_name: linkage_name }
  (* multiple units can have the same id, if they are in different
     pack. To distinguish we also keep the linkage name which contains
     the pack name *)
  let compare v1 v2 =
    let c = Ident.compare v1.id v2.id in
    if c = 0
    then String.compare v1.linkage_name v2.linkage_name
    else c

  let equal x y = compare x y = 0

  let create id linkage_name =
    assert(Ident.persistent id);
    { id; linkage_name }

  let print ppf x = Ident.print ppf x.id
  let output oc x = Ident.output oc x.id
  let hash x = Ident.hash x.id

  let to_ident x = x.id
  let name x = x.id.Ident.name
end

type compilation_unit = Compilation_unit.t

let predefined_exception_compilation_unit =
  Compilation_unit.create (Ident.create_persistent "__dummy__") "__dummy__"

type symbol = { sym_unit : compilation_unit; sym_label : linkage_name }

let ident_of_compilation_unit = Compilation_unit.to_ident

let linkage_name s = s
let string_of_linkage_name s = s

module Symbol = struct
  type t = symbol
  let compare s1 s2 = String.compare s1.sym_label s2.sym_label
  (** Labels are unique, so comparing them is sufficient. It also could
      uncover bugs to consider same labels from different modules equal *)
  let output c s = output_string c s.sym_label
  let hash s = Hashtbl.hash s.sym_label
  let equal s1 s2 = s1.sym_label = s2.sym_label
  let print ppf s =
    Format.fprintf ppf "%a - %s" Compilation_unit.print s.sym_unit s.sym_label
end

type variable = { var_unit : compilation_unit; var_var : Ident.t }

module Variable = struct
  type t = variable
  let compare v1 v2 =
    let c = Ident.compare v1.var_var v2.var_var in
    if c = 0
    then Compilation_unit.compare v1.var_unit v2.var_unit
    else c
  let output c v = Ident.output c v.var_var
  let hash v = Ident.hash v.var_var
  let equal v1 v2 =
    Ident.same v1.var_var v2.var_var &&
    Compilation_unit.equal v1.var_unit v2.var_unit
  let print ppf v = Ident.print ppf v.var_var
  let create ~compilation_unit id =
    { var_unit = compilation_unit; var_var = id }
  let make ~compilation_unit name =
    { var_unit = compilation_unit; var_var = Ident.create name }
  let compilation_unit var = var.var_unit
  let rename ~compilation_unit var =
    { var_unit = compilation_unit;
      var_var = Ident.rename var.var_var }
  let to_string var = Format.asprintf "%a" print var
  let make_ident var =
    let open Ident in
    { var.var_var with
      name = Compilation_unit.name var.var_unit
             ^ "_" ^ var.var_var.name }
end

module SymbolSet = ExtSet(Symbol)
module SymbolMap = ExtMap(Symbol)
module SymbolTbl = ExtHashtbl(Symbol)

module VarSet = struct
  include ExtSet(Variable)
  let of_ident_set ~compilation_unit idset =
    Lambda.IdentSet.fold (fun id set ->
        add (Variable.create ~compilation_unit id) set)
      idset empty
end
module VarMap = ExtMap(Variable)
module VarTbl = ExtHashtbl(Variable)

module ExprId : Id = Id(struct end)
module ExprMap = ExtMap(ExprId)
module ExprSet = ExtSet(ExprId)
module ExprTbl = ExtHashtbl(ExprId)

module FunInnerid : Id = Id(struct end)
module FunId : UnitId with module Compilation_unit := Compilation_unit
  = UnitId(FunInnerid)(Compilation_unit)
module FunMap = ExtMap(FunId)
module FunSet = ExtSet(FunId)
module FunTbl = ExtHashtbl(FunId)

module Static_exception = struct
  include Int
  let of_int x = x
  let to_int x = x
  let create () = Lambda.next_raise_count ()
end

type closure_element = {
  ce_id : Ident.t;
  ce_unit : compilation_unit;
}

type function_within_closure = closure_element
type variable_within_closure = closure_element

module Closure_element = struct
  type t = closure_element
  let compare x y =
    let c = Ident.compare x.ce_id y.ce_id in
    if c = 0
    then Compilation_unit.compare x.ce_unit y.ce_unit
    else c
  let output oc x =
    Printf.fprintf oc "%a.%a"
      Compilation_unit.output x.ce_unit
      Ident.output x.ce_id
  let print ppf x =
    Format.fprintf ppf "%a.%a"
      Compilation_unit.print x.ce_unit
      Ident.print x.ce_id
  let hash off = Hashtbl.hash off
  let equal o1 o2 = compare o1 o2 = 0

  let create var = { ce_unit = var.var_unit; ce_id = var.var_var }
  let compilation_unit { ce_unit } = ce_unit

  let to_var { ce_unit; ce_id } = { var_unit = ce_unit; var_var = ce_id }

  let to_string t = Format.asprintf "%a" print t
end

let ident_of_function_within_closure { ce_id } = ce_id

module Closure_function = Closure_element
module Closure_variable = Closure_element

module ClosureFunctionMap = ExtMap(Closure_function)
module ClosureFunctionSet = ExtSet(Closure_function)
module ClosureFunctionTbl = ExtHashtbl(Closure_function)

module ClosureVariableMap = ExtMap(Closure_variable)
module ClosureVariableSet = ExtSet(Closure_variable)
module ClosureVariableTbl = ExtHashtbl(Closure_variable)

module StaticExceptionSet = ExtSet(Static_exception)
module StaticExceptionMap = ExtMap(Static_exception)
module StaticExceptionTbl = ExtHashtbl(Static_exception)

module CompilationUnitSet = ExtSet(Compilation_unit)
module CompilationUnitMap = ExtMap(Compilation_unit)
module CompilationUnitTbl = ExtHashtbl(Compilation_unit)

module IdentMap = ExtMap(Ident)

type let_kind =
  | Not_assigned
  | Assigned

type call_kind =
  | Indirect
  | Direct of function_within_closure

type 'a flambda =
  | Fsymbol of symbol * 'a
  | Fvar of variable * 'a
  | Fconst of const * 'a
  | Fapply of 'a apply * 'a
  | Fclosure of 'a closure * 'a
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
  | Ffor of variable * 'a flambda * 'a flambda * Asttypes.direction_flag * 'a flambda * 'a
  | Fassign of variable * 'a flambda * 'a
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
  { cl_fun : 'a function_declarations;
    cl_free_var : 'a flambda VarMap.t;
    cl_specialised_arg : variable VarMap.t }

and 'a function_declaration = {
  stub   : bool;
  params : variable list;
  free_variables : VarSet.t;
  body   : 'a flambda;
  dbg    : Debuginfo.t;
}

and 'a function_declarations = {
  ident  : FunId.t;
  funs   : 'a function_declaration VarMap.t;
  compilation_unit : compilation_unit;
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

(* access functions *)

let find_declaration cf { funs } =
  VarMap.find (Closure_function.to_var cf) funs

let find_declaration_variable cf { funs } =
  let var = Closure_function.to_var cf in
  if not (VarMap.mem var funs)
  then raise Not_found
  else var

let find_free_variable cv { cl_free_var } =
  VarMap.find (Closure_variable.to_var cv) cl_free_var

(* utility functions *)

let function_arity f = List.length f.params

let variables_bound_by_the_closure cf decls =
  let func = find_declaration cf decls in
  let params = VarSet.of_list func.params in
  let functions = VarMap.keys decls.funs in
  VarSet.diff
    (VarSet.diff func.free_variables params)
    functions

let can_be_merged f1 f2 = match f1,f2 with
  | Fsymbol (sym1, _), Fsymbol (sym2, _) ->
      Symbol.equal sym1 sym2
  | Fvar (id1, _), Fvar (id2, _) ->
      Variable.equal id1 id2
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
  | Fvar (id,data) -> Variable.to_string id
  | Fconst (cst,data) -> "const"
  | Flet(str, id, lam, body,data) ->
      Printf.sprintf "let %s"
        (Variable.to_string id)
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

module Var_connected_components =
  Sort_connected_components.Make(Variable)

let recursive_functions { funs } =
  let function_variables = VarMap.keys funs in
  let directed_graph =
    VarMap.map
      (fun ffun -> VarSet.inter ffun.free_variables function_variables)
      funs in
  let connected_components =
    Var_connected_components.connected_components_sorted_from_roots_to_leaf
      directed_graph in
  Array.fold_left (fun rec_fun -> function
      | Var_connected_components.No_loop _ ->
          rec_fun
      | Var_connected_components.Has_loop elts ->
          List.fold_right VarSet.add elts rec_fun)
    VarSet.empty connected_components
