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
open Ext_types
open Asttypes
open Lambda
open Switch
open Clambda
open Flambda

let list_closures expr constants =
  let closures = ref OffsetMap.empty in
  let aux expr = match expr with
    | Fclosure(functs, fv, data) ->
      let add off_id _ map =
        OffsetMap.add {off_unit = functs.unit; off_id} functs map in
      closures := IdentMap.fold add functs.funs !closures;
    | e -> ()
  in
  Flambdaiter.iter aux expr;
  SymbolMap.iter (fun _ flam -> Flambdaiter.iter aux flam) constants;
  !closures

let reexported_offset extern_fun_offset_table extern_fv_offset_table expr =
  let set_fun = ref OffsetSet.empty in
  let set_fv = ref OffsetSet.empty in
  let aux expr = match expr with
    | Fenv_field({env_var;env_fun_id}, _) ->
      set_fun := OffsetSet.add env_fun_id !set_fun;
      set_fv := OffsetSet.add env_var !set_fv;
    | Foffset(_,id, rel, _) ->
      let set = match rel with
        | None -> !set_fun
        | Some rel -> OffsetSet.add rel !set_fun in
      set_fun := OffsetSet.add id set;
    | e -> ()
  in
  Flambdaiter.iter aux expr;
  let f extern_map offset new_map =
    try
      OffsetMap.add offset (OffsetMap.find offset extern_map) new_map
    with Not_found -> new_map (* local function *)
  in
  let fun_map = OffsetSet.fold (f extern_fun_offset_table) !set_fun in
  let fv_map = OffsetSet.fold (f extern_fv_offset_table) !set_fv in
  fun_map, fv_map

(* functions that assumes that the refered value is declared in the
   current unit: usage should be justified *)
let to_offset off_id = {off_id; off_unit = Compilenv.current_unit_symbol ()}

module type Param1 = sig
  type t
  val expr : t flambda
  val constants : t flambda SymbolMap.t
end

module Offsets(P:Param1) = struct

  (* The offset table associate a function label to its offset
     inside a closure *)
  let fun_offset_table = ref OffsetMap.empty
  (* The offset table associate a free variable to its offset inside a
     closure *)
  let fv_offset_table = ref OffsetMap.empty

  let rec iter = function
    | Fclosure(funct, fv, _) ->
      iter_closure funct fv
    | _ -> ()

  and iter_closure functs fv =

    let funct = IdentMap.bindings functs.funs in
    let fv = IdentMap.bindings fv in

    (* build the table mapping the function to the offset of its code
       pointer inside the closure value *)
    let aux_fun_offset (map,env_pos) (id, func) =
      let pos = env_pos + 1 in
      let env_pos = env_pos + 1 +
          (if func.arity <> 1 then 3 else 2) in
      let map = OffsetMap.add (to_offset id) pos map in
      (map,env_pos)
    in
    let fun_offset, fv_pos =
      List.fold_left aux_fun_offset (!fun_offset_table, -1) funct in

    (* Adds the mapping of free variables to their offset. It is not
       used inside the body of the function: it is directly
       substituted here. But if the function is inlined, it is
       possible that the closure is accessed from outside its body. *)
    let aux_fv_offset (map,pos) (id, _) =
      let off = to_offset id in
      assert(not (OffsetMap.mem off map));
      let map = OffsetMap.add off pos map in
      (map,pos + 1)
    in
    let fv_offset, _ = List.fold_left aux_fv_offset
        (!fv_offset_table, fv_pos) fv in

    fun_offset_table := fun_offset;
    fv_offset_table := fv_offset;

    List.iter (fun (_,{body}) -> Flambdaiter.iter_toplevel iter body) funct

  let res =
    let run flam = Flambdaiter.iter_toplevel iter flam in
    run P.expr;
    SymbolMap.iter (fun _ -> run) P.constants;
    !fun_offset_table, !fv_offset_table

end

module type Param2 = sig
  include Param1
  val fun_offset_table : int OffsetMap.t
  val fv_offset_table : int OffsetMap.t
  val closures : t Flambda.ffunctions OffsetMap.t
end

type const_lbl =
  | Lbl of string
  | No_lbl
  | Not_const

module Conv(P:Param2) = struct
  (* The offset table associate a function label to its offset
     inside a closure *)
  let fun_offset_table = P.fun_offset_table
  let fv_offset_table = P.fv_offset_table
  let closures = P.closures

  (* offsets of functions and free variables in closures comming from
     a linked module *)
  let extern_fun_offset_table =
    (Compilenv.approx_env ()).Flambdaexport.ex_offset_fun
  let extern_fv_offset_table =
    (Compilenv.approx_env ()).Flambdaexport.ex_offset_fv
  let ex_closures =
    (Compilenv.approx_env ()).Flambdaexport.ex_functions_off

  let is_current_unit id =
    (Compilenv.current_unit_symbol ()) = id

  let get_fun_offset off =
    try
      if is_current_unit off.off_unit
      then OffsetMap.find off fun_offset_table
      else OffsetMap.find off extern_fun_offset_table
    with Not_found ->
      fatal_error (Format.asprintf "missing offset %a" Offset.print off)

  let get_fv_offset off =
    if is_current_unit off.off_unit
    then
      if not (OffsetMap.mem off fv_offset_table)
      then fatal_error (Format.asprintf "env field offset not found: %a\n%!"
                          Offset.print off)
      else OffsetMap.find off fv_offset_table
    else OffsetMap.find off extern_fv_offset_table

  let get_closed_and_label off =
    let closed_and_label clos off =
      clos.closed, (IdentMap.find off.off_id clos.funs).label in
    try closed_and_label (OffsetMap.find off closures) off with
    | Not_found ->
      try closed_and_label (OffsetMap.find off ex_closures) off with
      | Not_found ->
        fatal_error (Format.asprintf "missing closure %a"
                       Offset.print off)

  type env = ulambda IdentMap.t (* substitution *)

  let empty_env () = IdentMap.empty

  let add_sb id subst env = IdentMap.add id subst env

  let rec conv ?expected_symbol (env : env) = function
    | Fvar (id,_) ->
      begin
        (* If the variable is a recursive access to the function
           currently being defined: it is replaced by an offset in the
           closure. If the variable is bound by the closure, it is
           replace by a field access inside the closure *)
        try IdentMap.find id env
        with Not_found -> Uvar id
      end

    | Fsymbol ((_,lbl),_) ->
      Uconst (Uconst_label lbl, None)

    | Fconst (cst,_) ->
      Uconst (conv_const cst, None)

    | Flet(str, id, lam, body, _) ->
      Ulet(id, conv env lam, conv env body)

    | Fletrec(defs, body, _) ->
      let udefs = List.map (fun (id,def) -> id, conv env def) defs in
      Uletrec(udefs, conv env body)

    | Fclosure(funct, fv, _) ->
      conv_closure env ~expected_symbol funct fv

    | Foffset(lam,id,rel, _) ->
      let ulam = conv env lam in
      let offset = get_fun_offset id in
      let relative_offset = match rel with
        | None -> offset
        | Some rel -> offset - get_fun_offset rel
      in
      if relative_offset = 0
      then ulam
      (* compilation of let rec in cmmgen assumes
         that a closure is not offseted (Cmmgen.expr_size) *)
      else Uoffset(ulam, relative_offset)

    | Fenv_field({env = lam;env_var;env_fun_id}, _) ->
      let ulam = conv env lam in
      let fun_offset = get_fun_offset env_fun_id in
      let var_offset = get_fv_offset env_var in
      let pos = var_offset - fun_offset in
      Uprim(Pfield pos, [ulam], Debuginfo.none)

    | Fapply(funct, args, Some direct_func, dbg, _) ->
      conv_direct_apply (conv env funct) args direct_func dbg env

    | Fapply(funct, args, None, dbg, _) ->
      (* the closure parameter of the function is added by cmmgen, but
         it already appears in the list of parameters of the clambda
         function for generic calls. Notice that for direct calls it is
         added here. *)
      Ugeneric_apply(conv env funct, conv_list env args, dbg)

    | Fswitch(arg, sw, _) ->
      (* NB: failaction might get copied, thus it should be some Lstaticraise *)
      let const_index, const_actions =
        conv_switch env sw.fs_consts sw.fs_numconsts sw.fs_failaction
      and block_index, block_actions =
        conv_switch env sw.fs_blocks sw.fs_numblocks sw.fs_failaction in
      Uswitch(conv env arg,
        {us_index_consts = const_index;
         us_actions_consts = const_actions;
         us_index_blocks = block_index;
         us_actions_blocks = block_actions})

    | Fprim(Pgetglobal id, l, dbg, _) ->
      assert false

    | Fprim(Pgetglobalfield(id,i), l, dbg, _) ->
      assert(l = []);
      Uprim(Pfield i,
            [Uprim(Pgetglobal (Ident.create_persistent
                                 (Compilenv.symbol_for_global id)), [], dbg)],
            dbg)

    | Fprim(Psetglobalfield i, [arg], dbg, _) ->
      Uprim(Psetfield (i,false),
            [Uprim(Pgetglobal (Ident.create_persistent
                                 (Compilenv.make_symbol None)), [], dbg);
             conv env arg],
            dbg)

    | Fprim(Pmakeblock(tag, Immutable) as p, args, dbg, _) ->
      let args = conv_list env args in
      begin match constant_list args with
        | None ->
          Uprim(p, args, dbg)
        | Some l ->
          Uconst(Uconst_block (tag,l), None)
      end

    | Fprim(p, args, dbg, _) ->
      Uprim(p, conv_list env args, dbg)
    | Fstaticfail (i, args, _) ->
      Ustaticfail (i, conv_list env args)
    | Fcatch (i, vars, body, handler, _) ->
      Ucatch (i, vars, conv env body, conv env handler)
    | Ftrywith(body, id, handler, _) ->
      Utrywith(conv env body, id, conv env handler)
    | Fifthenelse(arg, ifso, ifnot, _) ->
      Uifthenelse(conv env arg, conv env ifso, conv env ifnot)
    | Fsequence(lam1, lam2, _) ->
      Usequence(conv env lam1, conv env lam2)
    | Fwhile(cond, body, _) ->
      Uwhile(conv env cond, conv env body)
    | Ffor(id, lo, hi, dir, body, _) ->
      Ufor(id, conv env lo, conv env hi, dir, conv env body)
    | Fassign(id, lam, _) ->
      Uassign(id, conv env lam)
    | Fsend(kind, met, obj, args, dbg, _) ->
      Usend(kind, conv env met, conv env obj, conv_list env args, dbg)

    | Funreachable _ ->
      (* shoudl'nt be executable, maybe build something else *)
      Uprim(Praise, [Uconst (Uconst_pointer 0, None)], Debuginfo.none)

  and conv_switch env cases num_keys default =
    let num_keys =
      if IntSet.cardinal num_keys = 0
      then 0
      else IntSet.max_elt num_keys + 1 in
    let index = Array.create num_keys 0
    and store = mk_store Flambda.same in

    (* First default case *)
    begin match default with
      | Some def when List.length cases < num_keys ->
        ignore (store.act_store def)
      | _ -> ()
    end ;
    (* Then all other cases *)
    List.iter (fun (key,lam) -> index.(key) <- store.act_store lam) cases;
    (* Compile action *)
    let actions = Array.map (conv env) (store.act_get ()) in
    match actions with
    | [| |] -> [| |], [| |] (* May happen when default is None *)
    | _     -> index, actions

  and conv_direct_apply ufunct args direct_func dbg env =
    let closed, label = get_closed_and_label direct_func in
    let uargs =
      let uargs = conv_list env args in
      if closed then uargs else uargs @ [ufunct] in

    let apply = Udirect_apply((label:>string), uargs, dbg) in

    (* This is usualy sufficient to detect closure with side effects *)
    let rec no_effect = function
      | Uvar _ | Uconst _ | Uprim(Pgetglobalfield _, _, _)
      | Uprim(Pgetglobal _, _, _) -> true
      | Uprim(Pfield _, [arg], _) -> no_effect arg
      | Uclosure _ ->
        (* if the function is closed, then it is a Uconst otherwise,
           we do not call this function *)
        assert false
      | _ -> false in

    (* if the function is closed, the closure is not in the parameters,
       so we must ensure that it is executed if it does some side effects *)
    if closed && not (no_effect ufunct)
    then Usequence(ufunct, apply)
    else apply

  and conv_closure env functs fv ~expected_symbol =
    (* Make the susbtitutions for variables bound by the closure:
       the variables bounds are the functions inside the closure and
       the free variables of the functions.

       For instance the closure for a code like

         let rec fun_a x =
           if x <= 0 then 0 else fun_b (x-1) v1
         and fun_b x y =
           if x <= 0 then 0 else v1 + v2 + y + fun_a (x-1)

       will be represented in memory as:

         [ closure header; fun_a; 1; infix header; fun caml_curry_2; 2; fun_b; v1; v2 ]

       fun_a and fun_b will take an additional parameter 'env' to access their closure.
       It will be shifted such that in the body of a function the env parameter points
       to its code pointer. i.e. in fun_b it will be shifted by 3 words.

       Hence accessing to v1 in the body of fun_a is accessing to the 6th field of 'env'
       and in the body of fun_b it is the 1st field.

       If the closure can be compiled to a constant, the env parameter is not always passed
       to the function (for direct calls). Inside the body of the function, we acces a constant
       globaly defined: there are label camlModule__id created to access the functions.
       fun_a can be accessed by 'camlModule__id' and fun_b by 'camlModule__id_3'
       (3 is the offset of fun_b in the closure).

       Inside a constant closure, there will be no access to the closure for the free variables,
       but if the function is inlined, some variables can be retrieved from the closure outside
       of its body, so constant closure still contains their free variables. *)

    let funct = IdentMap.bindings functs.funs in
    let fv = IdentMap.bindings fv in
    let closed = functs.closed in

    (* the environment variable used for non constant closures *)
    let env_var = Ident.create "env" in
    (* the label used for constant closures *)
    let closure_lbl = match expected_symbol with
      | None ->
        assert(not closed);
        Compilenv.new_const_symbol ()
      | Some (_,lbl) -> lbl
    in

    let fv_ulam = List.map (fun (id,lam) -> id,conv env lam) fv in

    let conv_function (id,func) =
      (* adds variables from the closure to the substitution environment *)
      let fun_offset = OffsetMap.find (to_offset id) fun_offset_table in

      (* inside the body of the function, we cannot access variables
         declared outside, so take a clean substitution table. *)
      let env = IdentMap.empty in

      let env =
        (* Add to the substitution the value of the free variables *)

        let add_env_variable env (id,lam) =
          match constant_label lam with
          | Not_const ->
              assert(not closed);
              let var_offset = OffsetMap.find
                  { off_unit = functs.unit; off_id = id } fv_offset_table in
              let pos = var_offset - fun_offset in
              add_sb id (Uprim(Pfield pos, [Uvar env_var], Debuginfo.none)) env
          | No_lbl
          | Lbl _ -> env
        in

        let env = List.fold_left add_env_variable env fv_ulam in

        (* Add to the substitution the value of the functions defined in
           the current closure:
           this can be retrieved by shifting the environment. *)
        if closed
        then env
        else
          let add_offset_subst pos env (id,_) =
            let offset = OffsetMap.find (to_offset id) fun_offset_table in
            let exp = Uoffset(Uvar env_var, offset - pos) in
            add_sb id exp env in
          List.fold_left (add_offset_subst fun_offset) env funct
      in

      { Clambda.label = (func.label:>string);
        arity = func.arity;
        params = if closed then func.params else func.params @ [env_var];
        body = conv env func.body;
        dbg = func.dbg } in

    let ufunct = List.map conv_function funct in

    if closed
    then
      match constant_list (List.map snd fv_ulam) with
      | None -> assert false
      | Some fv_const ->
        let cst = Uconst_closure (ufunct, closure_lbl, fv_const) in
        Uconst(cst,Some closure_lbl)
    else
      Uclosure (ufunct, List.map snd fv_ulam)

  and conv_list env l = List.map (conv env) l

  and conv_const = function
    | Fconst_base c -> Uconst_base c
    | Fconst_pointer c -> Uconst_pointer c
    | Fconst_float_array c -> Uconst_float_array c
    | Fconst_immstring c -> Uconst_immstring c

  and constant_list l =
    let rec aux acc = function
      | [] ->
        Some (List.rev acc)
      | Uconst(v,None) :: q ->
        aux (v :: acc) q
      | Uconst(_,Some lbl) :: q ->
        aux (Uconst_label lbl :: acc) q
      | _ -> None
    in
    aux [] l

  and constant_label : ulambda -> const_lbl = function
    | Uconst(
        (Uconst_base(Const_int _ | Const_char _)
        | Uconst_pointer _), _) -> No_lbl
    | Uconst(Uconst_label lbl, _)
    | Uconst(_, Some lbl) -> Lbl lbl
    | Uconst(cst, None) ->
      Lbl (Compilenv.new_structured_constant cst false)
    | Uprim(Pgetglobal id, [], _) ->
      Lbl (Ident.name id)
    | _ -> Not_const

  let structured_constant_for_symbol lbl = function
    | Uconst(
        (Uconst_base(Const_int _ | Const_char _)
        | Uconst_pointer _), _)
    | Uconst(Uconst_label _, _) -> assert false
    | Uconst(cst, Some lbl') -> assert(lbl = lbl'); cst
    | Uconst(cst, None) -> cst
    | _ -> assert false

  let constants =
    SymbolMap.mapi
      (fun sym lam ->
         let ulam = conv (empty_env ()) ~expected_symbol:sym lam in
         structured_constant_for_symbol (snd sym) ulam)
      P.constants

  let res = conv (empty_env ()) P.expr

end

let convert (type a)
    ~(constants:a flambda SymbolMap.t)
    ~exported
    (expr:a flambda) =
  let closures = list_closures expr constants in
  let module P1 = struct
    type t = a
    let expr = expr
    let constants = constants
  end in
  let fun_offset_table, fv_offset_table =
    let module O = Offsets(P1) in
    O.res
  in
  let extern_fun_offset_table =
    (Compilenv.approx_env ()).Flambdaexport.ex_offset_fun in
  let extern_fv_offset_table =
    (Compilenv.approx_env ()).Flambdaexport.ex_offset_fv in
  let add_ext_offset_fun, add_ext_offset_fv =
    reexported_offset extern_fun_offset_table extern_fv_offset_table expr in
  let module P2 = struct include P1
    let fun_offset_table = fun_offset_table
    let fv_offset_table = fv_offset_table
    let closures = closures
  end in
  let module C = Conv(P2) in
  let export = let open Flambdaexport in
    { exported with
      ex_offset_fun = add_ext_offset_fun fun_offset_table;
      ex_offset_fv = add_ext_offset_fv fv_offset_table }
  in
  Compilenv.set_export_info export;
  SymbolMap.iter
    (fun (_,lbl) cst -> Compilenv.add_structured_constant lbl cst true)
    C.constants;
  C.res
