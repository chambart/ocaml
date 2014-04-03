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

open Misc
open Ext_types
open Symbol
open Abstract_identifiers

type let_kind =
  | Not_assigned
  | Assigned

type call_kind =
  | Indirect
  | Direct of function_within_closure

type 'a flambda =
    Fsymbol of Symbol.t * 'a
  | Fvar of Fident.t * 'a
  | Fconst of const * 'a
  | Fapply of 'a fapply * 'a
  | Fclosure of 'a fclosure * 'a
  | Ffunction of 'a ffunction * 'a
  | Fvariable_in_closure of 'a fvariable_in_closure * 'a
  | Flet of let_kind * Fident.t * 'a flambda * 'a flambda * 'a
  | Fletrec of (Fident.t * 'a flambda) list * 'a flambda * 'a
  | Fprim of Lambda.primitive * 'a flambda list * Debuginfo.t * 'a
  | Fswitch of 'a flambda * 'a fswitch * 'a
  | Fstaticraise of static_exception * 'a flambda list * 'a
  | Fstaticcatch of
      static_exception * Fident.t list * 'a flambda * 'a flambda * 'a
  | Ftrywith of 'a flambda * Fident.t * 'a flambda * 'a
  | Fifthenelse of 'a flambda * 'a flambda * 'a flambda * 'a
  | Fsequence of 'a flambda * 'a flambda * 'a
  | Fwhile of 'a flambda * 'a flambda * 'a
  | Ffor of Fident.t * 'a flambda * 'a flambda * Asttypes.direction_flag *
            'a flambda * 'a
  | Fassign of Fident.t * 'a flambda * 'a
  | Fsend of Lambda.meth_kind * 'a flambda * 'a flambda * 'a flambda list *
             Debuginfo.t * 'a
  | Fevent of 'a flambda * Lambda.lambda_event * 'a
  | Funreachable of 'a

and const =
  | Fconst_base of Asttypes.constant
  | Fconst_pointer of int
  | Fconst_float_array of string list
  | Fconst_immstring of string

and 'a fapply =
  { ap_function: 'a flambda;
    ap_arg: 'a flambda list;
    ap_kind: call_kind;
    ap_dbg: Debuginfo.t }

and 'a fclosure =
  { cl_fun : 'a function_declarations;
    cl_free_var : 'a flambda FidentMap.t;
    cl_specialised_arg : Fident.t FidentMap.t }

and 'a function_declarations = {
  ident : FunId.t;
  funs : 'a function_declaration FidentMap.t;
  compilation_unit : compilation_unit;
}

and 'a function_declaration = {
  stub : bool;
  params : Fident.t list;
  free_variables : FidentSet.t;
  body : 'a flambda;
  dbg : Debuginfo.t;
}

and 'a ffunction = {
  fu_closure: 'a flambda;
  fu_fun: function_within_closure;
  fu_relative_to: function_within_closure option;
}

and 'a fvariable_in_closure = {
  vc_closure : 'a flambda;
  vc_fun : function_within_closure;
  vc_var : variable_within_closure;
}

and 'a fswitch =
  { fs_numconsts: IntSet.t;
    fs_consts: (int * 'a flambda) list;
    fs_numblocks: IntSet.t;
    fs_blocks: (int * 'a flambda) list;
    fs_failaction : 'a flambda option }

(* access functions *)

let find_declaration cf { funs } =
  FidentMap.find (Closure_function.unwrap cf) funs

let find_declaration_variable cf { funs } =
  let var = Closure_function.unwrap cf in
  if not (FidentMap.mem var funs)
  then raise Not_found
  else var

let find_free_variable cv { cl_free_var } =
  FidentMap.find (Closure_variable.unwrap cv) cl_free_var

(* utility functions *)

let function_arity f = List.length f.params

let variables_bound_by_the_closure cf decls =
  let func = find_declaration cf decls in
  let params = FidentSet.of_list func.params in
  let functions = FidentMap.keys decls.funs in
  FidentSet.diff
    (FidentSet.diff func.free_variables params)
    functions

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
  | Fstaticraise (_,_,data)
  | Fstaticcatch (_,_,_,_,data)
  | Ftrywith(_,_,_,data)
  | Fifthenelse(_,_,_,data)
  | Fsequence(_,_,data)
  | Fwhile(_,_,data)
  | Ffor(_,_,_,_,_,data)
  | Fassign(_,_,data)
  | Fevent(_,_,data)
  | Funreachable data -> data

let description_of_toplevel_node = function
  | Fsymbol ({sym_label},_) ->
      Printf.sprintf "%%%s" (string_of_linkage_name sym_label)
  | Fvar (id,data) -> Fident.to_string id
  | Fconst (cst,data) -> "const"
  | Flet(str, id, lam, body,data) ->
      Printf.sprintf "let %s"
        (Fident.to_string id)
  | Fletrec(defs, body,data) -> "letrec"
  | Fclosure(_,data) -> "closure"
  | Ffunction(_,data) -> "function"
  | Fvariable_in_closure(_,data) -> "variable_in_closure"
  | Fapply(_,data) -> "apply"
  | Fswitch(arg, sw,data) -> "switch"
  | Fsend(kind, met, obj, args, _,data) -> "send"
  | Fprim(_, args, _,data) -> "prim"
  | Fstaticraise (i, args,data) -> "staticraise"
  | Fstaticcatch (i, vars, body, handler,data) -> "catch"
  | Ftrywith(body, id, handler,data) -> "trywith"
  | Fifthenelse(arg, ifso, ifnot,data) -> "if"
  | Fsequence(lam1, lam2,data) -> "seq"
  | Fwhile(cond, body,data) -> "while"
  | Ffor(id, lo, hi, dir, body,data) -> "for"
  | Fassign(id, lam,data) -> "assign"
  | Fevent(lam, ev, data) -> "event"
  | Funreachable _ -> "unreachable"

let recursive_functions { funs } =
  let function_variables = FidentMap.keys funs in
  let directed_graph =
    FidentMap.map
      (fun ffun -> FidentSet.inter ffun.free_variables function_variables)
      funs in
  let connected_components =
    Fident_connected_components.connected_components_sorted_from_roots_to_leaf
      directed_graph in
  Array.fold_left (fun rec_fun -> function
      | Fident_connected_components.No_loop _ ->
          rec_fun
      | Fident_connected_components.Has_loop elts ->
          List.fold_right FidentSet.add elts rec_fun)
    FidentSet.empty connected_components

let rec same l1 l2 =
  match (l1, l2) with
  | Fsymbol(s1, _), Fsymbol(s2, _) -> Symbol.equal s1 s2
  | Fsymbol _, _ | _, Fsymbol _ -> false
  | Fvar(v1, _), Fvar(v2, _) -> Fident.equal v1 v2
  | Fvar _, _ | _, Fvar _ -> false
  | Fconst(c1, _), Fconst(c2, _) -> begin
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
  | Fconst _, _ | _, Fconst _ -> false
  | Fapply(a1, _), Fapply(a2, _) ->
      a1.ap_kind = a2.ap_kind &&
      same a1.ap_function a2.ap_function &&
      samelist same a1.ap_arg a2.ap_arg
  | Fapply _, _ | _, Fapply _ -> false
  | Fclosure (c1, _), Fclosure (c2, _) ->
      samelist sameclosure
        (FidentMap.bindings c1.cl_fun.funs) (FidentMap.bindings c2.cl_fun.funs)
  | Fclosure _, _ | _, Fclosure _ -> false
  | Ffunction (f1, _), Ffunction (f2, _) ->
      same f1.fu_closure f2.fu_closure &&
      Closure_function.equal f1.fu_fun f1.fu_fun &&
      sameoption Closure_function.equal f1.fu_relative_to f1.fu_relative_to
  | Ffunction _, _ | _, Ffunction _ -> false
  | Fvariable_in_closure (v1, _), Fvariable_in_closure (v2, _) ->
      same v1.vc_closure v2.vc_closure &&
      Closure_function.equal v1.vc_fun v2.vc_fun &&
      Closure_variable.equal v1.vc_var v2.vc_var
  | Fvariable_in_closure _, _ | _, Fvariable_in_closure _ -> false
  | Flet (k1, v1, a1, b1, _), Flet (k2, v2, a2, b2, _) ->
      k1 = k2 && Fident.equal v1 v2 && same a1 a2 && same b1 b2
  | Flet _, _ | _, Flet _ -> false
  | Fletrec (bl1, a1, _), Fletrec (bl2, a2, _) ->
      samelist samebinding bl1 bl2 && same a1 a2
  | Fletrec _, _ | _, Fletrec _ -> false
  | Fprim (p1, al1, _, _), Fprim (p2, al2, _, _) ->
      p1 = p2 && samelist same al1 al2
  | Fprim _, _ | _, Fprim _ -> false
  | Fswitch (a1, s1, _), Fswitch (a2, s2, _) ->
      same a1 a2 && sameswitch s1 s2
  | Fswitch _, _ | _, Fswitch _ -> false
  | Fstaticraise (e1, a1, _), Fstaticraise (e2, a2, _) ->
      Static_exception.equal e1 e2 && samelist same a1 a2
  | Fstaticraise _, _ | _, Fstaticraise _ -> false
  | Fstaticcatch (s1, v1, a1, b1, _), Fstaticcatch (s2, v2, a2, b2, _) ->
      Static_exception.equal s1 s2 && samelist Fident.equal v1 v2 &&
      same a1 a2 && same b1 b2
  | Fstaticcatch _, _ | _, Fstaticcatch _ -> false
  | Ftrywith (a1, v1, b1, _), Ftrywith (a2, v2, b2, _) ->
      same a1 a2 && Fident.equal v1 v2 && same b1 b2
  | Ftrywith _, _ | _, Ftrywith _ -> false
  | Fifthenelse (a1, b1, c1, _), Fifthenelse (a2, b2, c2, _) ->
      same a1 a2 && same b1 b2 && same c1 c2
  | Fifthenelse _, _ | _, Fifthenelse _ -> false
  | Fsequence (a1, b1, _), Fsequence (a2, b2, _) ->
      same a1 a2 && same b1 b2
  | Fsequence _, _ | _, Fsequence _ -> false
  | Fwhile (a1, b1, _), Fwhile (a2, b2, _) ->
      same a1 a2 && same b1 b2
  | Fwhile _, _ | _, Fwhile _ -> false
  | Ffor(v1, a1, b1, df1, c1, _), Ffor(v2, a2, b2, df2, c2, _) ->
      Fident.equal v1 v2 &&  same a1 a2 &&
      same b1 b2 && df1 = df2 && same c1 c2
  | Ffor _, _ | _, Ffor _ -> false
  | Fassign(v1, a1, _), Fassign(v2, a2, _) ->
      Fident.equal v1 v2 && same a1 a2
  | Fassign _, _ | _, Fassign _ -> false
  | Fsend(k1, a1, b1, cl1, _, _), Fsend(k2, a2, b2, cl2, _, _) ->
      k1 = k2 && same a1 a2 && same b1 b2 && samelist same cl1 cl2
  | Fsend _, _ | _, Fsend _ -> false
  | Funreachable _, Funreachable _ -> true
  | Funreachable _, _ | _, Funreachable _ -> false
  | Fevent _, Fevent _ -> false

and sameclosure (_, c1) (_, c2) =
  samelist Fident.equal c1.params c2.params &&
  same c1.body c2.body

and samebinding (v1, c1) (v2, c2) =
  Fident.equal v1 v2 && same c1 c2

and sameswitch fs1 fs2 =
  let samecase (n1, a1) (n2, a2) = n1 = n2 && same a1 a2 in
  fs1.fs_numconsts = fs2.fs_numconsts &&
  fs1.fs_numblocks = fs2.fs_numblocks &&
  samelist samecase fs1.fs_consts fs2.fs_consts &&
  samelist samecase fs1.fs_blocks fs2.fs_blocks &&
  (match (fs1.fs_failaction, fs2.fs_failaction) with
    | (None, None) -> true
    | (Some a1, Some a2) -> same a1 a2
    | _ -> false)

let can_be_merged = same
