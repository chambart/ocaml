(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                     Pierre Chambart, OCamlPro                       *)
(*                                                                     *)
(*  Copyright 2013 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Misc
open Asttypes
open Lambda
open Ext_types

type symbol = Ident.t * string (* module * label *)

module Symbol = struct
  type t = symbol
  let compare (_,s1) (_,s2) = String.compare s1 s2
  let output c (_,s) = output_string c s
  let hash (_,(i:string)) = Hashtbl.hash i
  let equal (_,(i:string)) (_,j) = i = j
  let print ppf (id,s) = Format.fprintf ppf "%a - %s" Ident.print id s
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

module Idt = struct
  type t = Ident.t
  let compare x y =
    let c = compare x.Ident.stamp y.Ident.stamp in
    if c = 0
    then compare x.Ident.name y.Ident.name
    else c
  let output oc id = output_string oc (Ident.unique_name id)
  let print = Ident.print
  let hash i = (Char.code i.Ident.name.[0]) lxor i.Ident.stamp
  let equal = Ident.same
end

module IdentSet =
struct
  include Lambda.IdentSet
  let of_list l = match l with
    | [] -> empty
    | [t] -> singleton t
    | t :: q -> List.fold_left (fun acc e -> add e acc) (singleton t) q
  let map f s = of_list (List.map f (elements s))
  let print ppf s =
    let elts ppf s = iter (fun e -> Format.fprintf ppf "@ %a" Ident.print e) s in
    Format.fprintf ppf "@[<1>{@[%a@ @]}@]" elts s
end
module IdentMap = ExtMap(Idt)
module IdentTbl = ExtHashtbl(Idt)

type offset = {
  off_id : Ident.t;
  off_unit : symbol;
}

module Offset = struct
  type t = offset
  let compare x y =
    let c = Idt.compare x.off_id y.off_id in
    if c <> 0
    then c
    else Symbol.compare x.off_unit y.off_unit
  let output oc x =
    Printf.fprintf oc "%s.%a"
      (snd x.off_unit)
      Idt.output x.off_id
  let print ppf x =
    Format.fprintf ppf "%s.%a"
      (snd x.off_unit)
      Idt.print x.off_id
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

(* A data is attached to each node. It is often used to uniquely
   identify an expression *)
type 'a flambda =
  | Fsymbol of symbol * 'a
  | Fvar of Ident.t * 'a
  | Fconst of const * 'a
  | Fapply of 'a flambda * 'a flambda list *
                offset option * Debuginfo.t * 'a
  | Fclosure of 'a ffunctions * 'a flambda IdentMap.t * 'a
  | Foffset of 'a flambda * offset * offset option * 'a
    (* Foffset(closure, id, previous_offset) access to the function 'id' from the closure.
       If previous_offset is Some(off) this represent an offset
       to an already offseted function: this is a relative offset.
       It can appear when inlining multiply recursive functions *)
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
  { fs_numconsts: IntSet.t;               (* integer cases *)
    fs_consts: (int * 'a flambda) list;    (* Integer cases *)
    fs_numblocks: IntSet.t;               (* Number of tag block cases *)
    fs_blocks: (int * 'a flambda) list;    (* Tag block cases *)
    fs_failaction : 'a flambda option }    (* Action to take if failure *)

and 'a ffunction = {
  label  : function_label;
  stub   : bool;
  arity  : int;
  params : Ident.t list;
  kept_params : IdentSet.t;
  closure_params : IdentSet.t;
  body   : 'a flambda;
  dbg    : Debuginfo.t;
}

and 'a ffunctions = {
  ident  : FunId.t;
  funs   : 'a ffunction IdentMap.t;
  unit   : symbol;
  closed : bool;
  recursives : bool;
}

and 'a fenv_field = {
  env : 'a flambda;
  env_fun_id : offset;
  env_var : offset;
}

let same f1 f2 =
  (* TODO ! used for switch compiling *)
  false


(* Well formedness checking *)

type 'a env = {
  current_unit : symbol;
  bound_variables : IdentSet.t;
  seen_variables : IdentSet.t ref;
  seen_fun_label : StringSet.t ref;
  seen_static_catch : IntSet.t ref;
  seen_env_var : IdentSet.t ref;
  seen_fun_env_var : IdentSet.t IdentMap.t ref;
  need_env_var : IdentSet.t IdentMap.t ref;
  caught_static_exceptions : IntSet.t;
  closure_variables : ('a ffunctions * 'a flambda IdentMap.t) IdentMap.t;
  (* variables bound to a closure *)
}

let add_check_env' env id =
  if IdentSet.mem id !(env.seen_variables)
  then fatal_error (Printf.sprintf "Flambda.check: variable %s bound \
                                    multiple times" (Ident.unique_name id));
  env.seen_variables := IdentSet.add id !(env.seen_variables)

let add_env id env =
  { env with
    bound_variables = IdentSet.add id env.bound_variables }

let add_check_env id env =
  add_check_env' env id;
  add_env id env

let need_env_var fun_id var_id env =
  let set = try IdentMap.find fun_id !(env.need_env_var) with
    | Not_found -> IdentSet.empty in
  env.need_env_var :=
    IdentMap.add fun_id (IdentSet.add var_id set) !(env.need_env_var)

let seen_env_var fun_id var_id env =
  let set = try IdentMap.find fun_id !(env.seen_fun_env_var) with
    | Not_found -> IdentSet.empty in
  env.seen_fun_env_var :=
    IdentMap.add fun_id (IdentSet.add var_id set) !(env.seen_fun_env_var)

let bind_var id lam env =
  let env = add_check_env id env in
  let rec aux lam =
    match lam with
    | Fvar (var,_) ->
      if IdentMap.mem var env.closure_variables
      then
        let closure_var = IdentMap.find var env.closure_variables in
        { env with closure_variables =
                     IdentMap.add id closure_var env.closure_variables }
    else env
    | Fclosure (ffun,fv,_) ->
      { env with closure_variables =
                   IdentMap.add id (ffun,fv) env.closure_variables }
    | Flet (_,_,_,lam,_) ->
      aux lam
    | _ -> env
  in
  aux lam

(* Adds without checks: the variable of the closure was already
   inserted in an environment, but will not be available inside the
   closure if we do not add it here also. *)
let add_rec_closure id env =
  { env with bound_variables = IdentSet.add id env.bound_variables }

let add_check_fun_label lbl env =
  if StringSet.mem lbl !(env.seen_fun_label)
  then fatal_error (Printf.sprintf "Flambda.check: function %s appear \
                                    multiple times" lbl);
  env.seen_fun_label := StringSet.add lbl !(env.seen_fun_label)

let add_check_static_catch n env =
  if IntSet.mem n !(env.seen_static_catch)
  then fatal_error (Printf.sprintf "Flambda.check: static exception %i \
                                    caught at multiple places" n);
  env.seen_static_catch := IntSet.add n !(env.seen_static_catch);
  { env with
    caught_static_exceptions = IntSet.add n env.caught_static_exceptions }

let empty_env env =
  { env with bound_variables = IdentSet.empty }

let rec check env = function
  | Fsymbol _ -> ()
  | Fvar (id,_) ->
    if not (IdentSet.mem id env.bound_variables)
    then fatal_error (Printf.sprintf "Flambda.check: unbound variable %s"
          (Ident.unique_name id))
  | Fconst (cst,_) -> ()
  | Flet(str, id, lam, body,_) ->
    begin match str with
    | Variable | Strict -> ()
    | _ -> fatal_error (Printf.sprintf "Flambda.check: let kind is not \
                                        Variable or strict")
    end;
    check env lam;
    let env = bind_var id lam env in
    check env body
  | Fletrec(defs, body,_) -> List.iter (function
      (* clambdagen assumes this *)
      | (_,Fvar(var_id,_)) ->
        List.iter (fun (id,_) ->
            if Ident.same var_id id
            then fatal_error (Printf.sprintf "Flambda.check: recursive alias to \
                                              var %s" (Ident.unique_name id)))
          defs
      | _ -> ())
      defs;
    let env = List.fold_left (fun env (id,lam) -> bind_var id lam env) env defs in
    List.iter (fun (_,def) -> check env def) defs;
    check env body
  | Fclosure(funct, fv,_) ->
    List.iter (fun (id, l) ->
        if IdentSet.mem id !(env.seen_env_var)
        then fatal_error (Printf.sprintf "Flambda.check: closure variable %s \
                                          bound in multiple closures"
              (Ident.unique_name id))
        else env.seen_env_var := IdentSet.add id !(env.seen_env_var);
        check env l) (IdentMap.bindings fv);
    check_closure env funct fv
  | Foffset(lam,{ off_unit; off_id = id },relative_offset, _) ->
    check env lam;
    (* let rec find_var_offset = function *)
    (*   | Fclosure (ffun,fv,_) -> ffun,fv *)
    (*   | Fvar(id,_) -> *)
    (*     (try IdentMap.find id env.closure_variables *)
    (*      with Not_found -> *)
    (*        fatal_error (Printf.sprintf "Flambda.check: Foffset on a variable \ *)
    (*                                     not bound to a closure: %s" *)
    (*                       (Ident.unique_name id))) *)
    (*   | Flet(_,_,_,body,_) -> find_var_offset body *)
    (*   | _ -> *)
    (*     fatal_error (Printf.sprintf "Flambda.check: Foffset on neither a \ *)
    (*                                  variable nor a closure") *)
    (* in *)
    begin match relative_offset with
    | None -> ()
    | Some { off_unit = rela_unit; off_id = rela_id } ->
        if not (off_unit = rela_unit)
        then fatal_error "Flambda.check relative offset"
    end

    (* TODO: change the checks ! *)

    (* if off_unit = env.current_unit *)
    (* then *)
    (*   let (ffun,fv) = find_var_offset lam in *)

    (*   (\* TODO: also check the recursive flag *\) *)
    (*   if not (IdentMap.mem id ffun.funs) *)
    (*   then fatal_error (Printf.sprintf "Flambda.check: Foffset function %s not \ *)
    (*                                     present in the closure" *)
    (*                       (Ident.unique_name id)) *)

  | Fenv_field({ env = env_lam; env_fun_id; env_var },_) ->
    if Symbol.equal env_var.off_unit env.current_unit &&
       Symbol.equal env_fun_id.off_unit env.current_unit
    then begin
      need_env_var env_fun_id.off_id env_var.off_id env;
      let closure = match env_lam with
        | Fclosure (ffun,fv,_) -> Some (ffun,fv)
        | Fvar(id,_) ->
          (try Some (IdentMap.find id env.closure_variables)
           with Not_found -> None)
        | _ -> None
      in
      begin match closure with
        | None -> () (* In recursive cases we can't know directly *)
        | Some (ffun,fv) ->
          if not (IdentMap.mem env_var.off_id fv)
          then fatal_error (Printf.sprintf "Flambda.check: Fenv_field var %s not \
                                            present in the closure"
                              (Ident.unique_name env_var.off_id));
          if not (IdentMap.mem env_fun_id.off_id ffun.funs)
          then fatal_error (Printf.sprintf "Flambda.check: Fenv_field function %s \
                                            not present in the closure"
                              (Ident.unique_name env_fun_id.off_id))
      end;
    end;
    check env env_lam
  | Fapply(funct, args, _, _,_) ->
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
    then fatal_error (Printf.sprintf "Flambda.check: uncaught static \
                                      exception %i" i);
    List.iter (check env) args
  | Fcatch (i, vars, body, handler,_) ->
    let env' = add_check_static_catch i env in
    check env' body;
    let env = List.fold_right add_check_env vars env in
    check env handler
  | Ftrywith(body, id, handler,_) ->
    check env body;
    let env = add_check_env id env in
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
    let env = add_check_env id env in
    check env body
  | Fassign(id, lam,_) ->
    check env lam
  | Funreachable _ -> ()

and check_closure orig_env funct fv' =
  let fv = List.map fst (IdentMap.bindings fv') in
  let funs = List.map fst (IdentMap.bindings funct.funs) in
  List.iter (add_check_env' orig_env) fv;
  let env = List.fold_right add_rec_closure funs (empty_env orig_env) in
  IdentMap.iter (fun fun_id func ->
    IdentSet.iter (fun id ->
      seen_env_var fun_id id env;
      if not (IdentMap.mem id fv')
      then
        fatal_error (Printf.sprintf "Flambda.check: variable %s not in \
                                     the closure" (Ident.unique_name id)))
      func.closure_params;
    let env = IdentSet.fold add_env func.closure_params env in
    let env = List.fold_right add_check_env func.params env in
    check env func.body) funct.funs

let check_fun_env_var need seen =
  IdentMap.iter (fun fun_id need_set ->
      let seen_set = try IdentMap.find fun_id seen with
        | Not_found ->
          fatal_error (Printf.sprintf "Flambda.check: closure variable needed \
                                       in function %s but not provided"
                         (Ident.unique_name fun_id))
      in
      let diff = IdentSet.diff need_set seen_set in
      IdentSet.iter (fun id ->
          fatal_error (Printf.sprintf "Flambda.check: var offset %s is needed \
                                       but not provided" (Ident.unique_name id)))
        diff) need

let check ~current_unit flam =
  let env = { current_unit;
              bound_variables = IdentSet.empty;
              seen_variables = ref IdentSet.empty;
              seen_fun_label = ref StringSet.empty;
              seen_static_catch = ref IntSet.empty;
              seen_env_var = ref IdentSet.empty;
              seen_fun_env_var = ref IdentMap.empty;
              need_env_var = ref IdentMap.empty;
              caught_static_exceptions = IntSet.empty;
              closure_variables = IdentMap.empty } in
  check env flam;
  check_fun_env_var !(env.need_env_var) !(env.seen_fun_env_var)

(* utility functions *)

let data = function
  | Fsymbol (_,data) -> data
  | Fvar (id,data) -> data
  | Fconst (cst,data) -> data
  | Flet(str, id, lam, body,data) -> data
  | Fletrec(defs, body,data) -> data
  | Fclosure(funct, fv,data) -> data
  | Foffset(lam,id,rel,data) -> data
  | Fenv_field(_,data) -> data
  | Fapply(funct, args, _, _,data) -> data
  | Fswitch(arg, sw,data) -> data
  | Fsend(kind, met, obj, args, _,data) -> data
  | Fprim(_, args, _,data) -> data
  | Fstaticfail (i, args,data) -> data
  | Fcatch (i, vars, body, handler,data) -> data
  | Ftrywith(body, id, handler,data) -> data
  | Fifthenelse(arg, ifso, ifnot,data) -> data
  | Fsequence(lam1, lam2,data) -> data
  | Fwhile(cond, body,data) -> data
  | Ffor(id, lo, hi, dir, body,data) -> data
  | Fassign(id, lam,data) -> data
  | Funreachable data -> data

let string_desc = function
  | Fsymbol ((_,symbol),_) -> Printf.sprintf "%%%s" symbol
  | Fvar (id,data) -> Ident.unique_name id
  | Fconst (cst,data) -> "const"
  | Flet(str, id, lam, body,data) ->
    Printf.sprintf "let %s"
      (Ident.unique_name id)
  | Fletrec(defs, body,data) -> "letrec"
  | Fclosure(funct, fv,data) -> "closure"
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
