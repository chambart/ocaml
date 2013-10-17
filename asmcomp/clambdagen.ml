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

(** annotate closed functions and build a label -> closed map *)
let mark_closed not_constants expr =
  let closures = ref OffsetMap.empty in
  let aux expr = match expr with
    | Fclosure(functs, fv, data) ->
      let closed =
        (* A function is considered to be closed if all the free variables can be converted
           to constant. i.e. if the whole closure will be compiled as a constant *)
        not (FunSet.mem functs.ident not_constants.Constants.not_constant_closure) in
      let functs = { functs with closed } in
      let add off_id _ map =
        OffsetMap.add {off_unit = functs.unit; off_id} functs map in
      closures := IdentMap.fold add functs.funs !closures;
      Fclosure(functs, fv, data)
    | e -> e
  in
  let expr = Flambdaiter.map aux expr in
  expr, !closures

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

let offseted_label (lbl,i) =
  assert(i>=0);
  if i = 0 then lbl else lbl ^ "_" ^ (string_of_int i)

(* functions that assumes that the refered value is declared in the
   current unit: usage should be justified *)
let to_offset off_id = {off_id; off_unit = Compilenv.current_unit_symbol ()}
let to_symbol lbl = (Compilenv.current_unit_id (), offseted_label lbl)

module type Param1 = sig
  type t
  val expr : t Flambda.flambda
  val not_constants : Constants.constant_result
end

module Offsets(P:Param1) = struct

  let not_constants = P.not_constants

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

(* TO BE UNCOMMENTED: when fenv_field access to a constant will be
   guaranteed not to use the closure (see also conv_function)

    (* only consider free variables that are not constants *)
    let fv = List.filter (fun (id, _) ->
        IdentSet.mem id not_constants.Constants.not_constant_id) fv in
*)

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
    Flambdaiter.iter_toplevel iter P.expr;
    !fun_offset_table, !fv_offset_table

end

module type Param2 = sig
  type t
  val expr : t Flambda.flambda
  val fun_offset_table : int OffsetMap.t
  val fv_offset_table : int OffsetMap.t
  val not_constants : Constants.constant_result
  val closures : t Flambda.ffunctions OffsetMap.t
end

type const_lbl =
  | Lbl of offseted_label
  | No_lbl
  | Not_const

module Conv(P:Param2) = struct
  open Flambdaexport
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

  (* let () = *)
  (*   OffsetMap.iter (fun off _ -> *)
  (*       Format.printf "fun off %a@." Offset.print off) *)
  (*     extern_fun_offset_table *)

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

  let get_arity off =
    let arity clos off = (IdentMap.find off.off_id clos.funs).arity in
    try arity (OffsetMap.find off closures) off with
    | Not_found ->
      try arity (OffsetMap.find off ex_closures) off with
      | Not_found ->
        fatal_error (Format.asprintf "missing closure %a"
                       Offset.print off)

  let not_constants = P.not_constants

  type env =
    { sb : ulambda IdentMap.t; (* substitution *)
      cm : offseted_label IdentMap.t; (* variables associated to constants *)
      global : (int, approx) Hashtbl.t;
      ex_table : descr EidMap.t ref;
      ex_symbol_id : ExportId.t SymbolMap.t ref;
      approx : approx IdentMap.t;
    }

  let empty_env () =
    { sb = IdentMap.empty;
      cm = IdentMap.empty;
      global = Hashtbl.create 10;
      ex_table = ref EidMap.empty;
      ex_symbol_id = ref SymbolMap.empty;
      approx = IdentMap.empty }

  let add_sb id subst env =
    { env with sb = IdentMap.add id subst env.sb }

  let add_cm id const env =
    { env with cm = IdentMap.add id const env.cm }

  let add_global i approx env =
    Hashtbl.add env.global i approx
  let get_global i env =
    (* /!\ si il n'y a pas on devrait mieux planter non ? /!\
       à changer plus tard *)
    try Hashtbl.find env.global i with Not_found -> Value_unknown

  let new_descr descr env =
    let id = ExportId.create (Compilenv.current_unit_name ()) in
    env.ex_table := EidMap.add id descr !(env.ex_table);
    id

  let add_approx id approx env =
    { env with approx = IdentMap.add id approx env.approx }
  let get_approx id env =
    try IdentMap.find id env.approx with Not_found -> Value_unknown

  let get_descr approx env =
    match approx with
    | Value_unknown -> None
    | Value_id ex -> Some (EidMap.find ex !(env.ex_table))
    | Value_symbol sym ->
      try
        let ex = SymbolMap.find sym !(env.ex_symbol_id) in
        Some (EidMap.find ex !(env.ex_table))
      with Not_found -> None

  let add_symbol sym id env =
    env.ex_symbol_id := SymbolMap.add sym id !(env.ex_symbol_id)

  let rec conv env expr = fst (conv_approx env expr)
  and conv_approx (env : env) = function
    | Fvar (id,_) ->
      begin
        (* If the variable reference a constant, it is replaced by the
           constant label *)
        try
          let lbl = IdentMap.find id env.cm in
          Uconst(Uconst_label lbl, None)
        with Not_found ->

          (* If the variable is a recursive access to the function
             currently being defined: it is replaced by an offset in the
             closure. If the variable is bound by the closure, it is
             replace by a field access inside the closure *)
          try IdentMap.find id env.sb
          with Not_found -> Uvar id
      end,
      get_approx id env

    | Fsymbol ((_,lbl) as sym,_) ->
      Uconst (Uconst_label (lbl,0), None),
      Value_symbol sym

    | Fconst (cst,_) ->
      let ucst, descr = conv_const cst in
      let approx = match descr with
        | None -> Value_unknown
        | Some descr -> Value_id (new_descr descr env) in
      Uconst (ucst, None),
      approx

    | Flet(str, id, lam, body, _) ->
      let lam, approx = conv_approx env lam in
      let env = add_approx id approx env in
      if IdentSet.mem id not_constants.Constants.not_constant_id
      then
        let ubody, body_approx = conv_approx env body in
        Ulet(id, lam, ubody), body_approx
      else begin match constant_label lam with
        | No_lbl ->
          (* no label: the value is an integer: substitute it *)
          conv_approx (add_sb id lam env) body
        | Lbl lbl ->
          (* label: the value is a block: reference it *)
          conv_approx (add_cm id lbl env) body
        | Not_const ->
          Format.printf "%a@." Ident.print id;
          Printclambda.clambda Format.std_formatter lam;
          assert false
      end

    | Fletrec(defs, body, _) ->
      let not_consts, consts =
        List.partition (fun (id,_) ->
          IdentSet.mem id not_constants.Constants.not_constant_id) defs in
      let env, consts = List.fold_left
          (fun (env, acc) (id,def) ->
            match def with
            | Fconst (( Fconst_pointer _
                      | Fconst_base (Const_int _ | Const_char _)), _) ->
              (* When the value is an integer constant, we cannot affect a label
                 to it: hence we must substitute it directly. *)
              add_sb id (conv env def) env, acc
            | Fvar (var_id, _) ->
              List.iter (fun (id,_) ->
                  if Ident.same var_id id
                  then Printf.printf "prob: %s %s\n%!"
                      (Ident.unique_name id)
                      (Ident.unique_name var_id)) consts;
              assert(List.for_all(fun (id,_) -> not (Ident.same var_id id)) consts);
              (* For variables: the variable could have been substituted to
                 a constant: avoid it by substituting it directly *)
              add_sb id (conv env def) env, acc
            | _ ->
              let lbl = Compilenv.new_const_symbol () in
              let env = add_cm id (lbl,0) env in (* !!!!!! Heuuu si c'est une fonction ? *)
              env, (id,lbl,def)::acc) (env,[]) consts in
      List.iter (fun (id,lbl,def) ->
          set_label lbl (conv env def))
        consts;

      let not_consts = List.map (fun (id,def) -> id, conv env def) not_consts in
      begin match not_consts with
        | [] -> conv env body
        | _ -> Uletrec(not_consts, conv env body) end,
      Value_unknown

    | Fclosure(funct, fv, _) ->
      conv_closure env funct fv

    | Foffset(lam,id,rel, _) ->
      (* For compiling offset inside a constant closure as a constant,
         we compile it as a direct label to the function:
         label ^ "_" ^ offset
         This works because we know that Foffset is by construction always
         applied to a variable or the closure on which we can recover the
         original label.
      *)
      let ulam, fun_approx = conv_approx env lam in
      let offset = get_fun_offset id in
      let relative_offset = match rel with
        | None -> offset
        | Some rel -> offset - get_fun_offset rel
      in
      let uoffset = make_offset ulam relative_offset in
      let approx = match get_descr fun_approx env with
        | Some (Value_unoffseted_closure closure)
        | Some (Value_closure { closure }) ->
          let ex = new_descr (Value_closure { fun_id = id; closure }) env in
          begin match constant_label uoffset with
          | No_lbl -> (* no label: the value is an integer: bug *)
            assert false
          | Lbl lbl ->
            (* it is ok to consider this label as a symbol exported by
               this unit because Foffset are always directly applied
               to the closure: in the same unit *)
            let sym = to_symbol lbl in
            add_symbol sym ex env;
            Value_symbol sym
          | Not_const ->
            Value_id ex
          end
        | Some _ -> assert false
        | None -> Value_unknown in
      uoffset, approx

    | Fenv_field({env = lam;env_var;env_fun_id}, _) ->
      let ulam = conv env lam in
      let fun_offset = get_fun_offset env_fun_id in
      let var_offset = get_fv_offset env_var in
      let pos = var_offset - fun_offset in
      Uprim(Pfield pos, [ulam], Debuginfo.none),
      Value_unknown

    | Fapply(funct, args, Some direct_func, dbg, _) ->
      conv_direct_apply (conv env funct) args direct_func dbg env

    | Fapply(funct, args, None, dbg, _) ->
      let ufunct, fun_approx = conv_approx env funct in
      begin match get_descr fun_approx env with
      | Some (Value_closure { fun_id }) when
          (get_arity fun_id) = List.length args ->
          conv_direct_apply ufunct args fun_id dbg env
      | _ ->
        (* the closure parameter of the function is added by cmmgen, but
           it already appears in the list of parameters of the clambda
           function for generic calls. Notice that for direct calls it is
           added here. *)
        Ugeneric_apply(ufunct, conv_list env args, dbg),
        Value_unknown
      end

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
         us_actions_blocks = block_actions}),
      Value_unknown

    | Fprim(Pgetglobal id, l, dbg, _) ->
      assert(l = []);
      let lbl = Compilenv.symbol_for_global id in
      let approx =
        if Ident.is_predef_exn id
        then Value_id (new_descr (Value_predef_exn id) env)
        else Value_symbol (id,lbl) in
      Uconst (Uconst_label (lbl,0), None),
      approx

    | Fprim(Pgetglobalfield(id,i), l, dbg, _) ->
      assert(l = []);
      Uprim(Pfield i,
            [Uprim(Pgetglobal (Ident.create_persistent
                                 (Compilenv.symbol_for_global id)), [], dbg)],
            dbg),
      get_global i env

    | Fprim(Psetglobalfield i, [arg], dbg, _) ->
      let uarg, approx = conv_approx env arg in
      add_global i approx env;
      Uprim(Psetfield (i,false),
            [Uprim(Pgetglobal (Ident.create_persistent
                                 (Compilenv.make_symbol None)), [], dbg);
             uarg],
            dbg),
      Value_unknown

    | Fprim(Pmakeblock(tag, Immutable) as p, args, dbg, _) ->
      let args, approxs = conv_list_approx env args in
      let ex = new_descr (Value_block (tag, Array.of_list approxs)) env in
      begin match constant_list args with
        | None ->
          Uprim(p, args, dbg),
          Value_id ex
        | Some l ->
          let cst = Uconst_block (tag,l) in
          let lbl = Compilenv.new_structured_constant cst true, 0 in
          (* building a value in the current unit: ok to use to_symbol *)
          let sym = to_symbol lbl in
          add_symbol sym ex env;
          Uconst(Uconst_label lbl, None),
          Value_symbol sym
      end

    | Fprim(p, args, dbg, _) ->
      Uprim(p, conv_list env args, dbg),
      Value_unknown
    | Fstaticfail (i, args, _) ->
      Ustaticfail (i, conv_list env args),
      Value_unknown
    | Fcatch (i, vars, body, handler, _) ->
      Ucatch (i, vars, conv env body, conv env handler),
      Value_unknown
    | Ftrywith(body, id, handler, _) ->
      Utrywith(conv env body, id, conv env handler),
      Value_unknown
    | Fifthenelse(arg, ifso, ifnot, _) ->
      Uifthenelse(conv env arg, conv env ifso, conv env ifnot),
      Value_unknown
    | Fsequence(lam1, lam2, _) ->
      let ulam1 = conv env lam1 in
      let ulam2, approx = conv_approx env lam2 in
      Usequence(ulam1, ulam2),
      approx
    | Fwhile(cond, body, _) ->
      Uwhile(conv env cond, conv env body),
      Value_unknown
    | Ffor(id, lo, hi, dir, body, _) ->
      Ufor(id, conv env lo, conv env hi, dir, conv env body),
      Value_unknown
    | Fassign(id, lam, _) ->
      Uassign(id, conv env lam),
      Value_unknown
    | Fsend(kind, met, obj, args, dbg, _) ->
      Usend(kind, conv env met, conv env obj, conv_list env args, dbg),
      Value_unknown

    | Funreachable _ ->
      (* shoudl'nt be executable, maybe build something else *)
      Uprim(Praise, [Uconst (Uconst_pointer 0, None)], Debuginfo.none),
      Value_unknown

  and make_offset ulam offset =
    match ulam with
    | Uconst(Uconst_label lbl,_)
    | Uconst(_,Some lbl) ->
      Uconst(Uconst_label (offset_label lbl offset),None)
    | _ ->
      assert(offset >= 0);
      if offset = 0
      then ulam
      (* compilation of let rec in cmmgen assumes
         that a closure is not offseted (Cmmgen.expr_size) *)
      else Uoffset(ulam, offset)

  and offset_label (lbl,orig) offset = (lbl,orig + offset)

  (* and offset_label lbl offset = *)
  (*   if offset = 0 *)
  (*   then lbl *)
  (*   else lbl ^ "_" ^ string_of_int offset *)

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
       so we must ensure that it is present if it does some side effects *)
    (if closed && not (no_effect ufunct)
     then Usequence(ufunct, apply)
     else apply),
    Value_unknown

  and conv_closure env functs fv =
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
    let closure_lbl = Compilenv.new_const_symbol () in

    let fv_ulam_approx = List.map (fun (id,lam) -> id,conv_approx env lam) fv in
    let fv_ulam = List.map (fun (id,(lam,_)) -> id, lam) fv_ulam_approx in
    let value_closure' =
      { closure_id = functs.ident;
        bound_var = List.fold_left (fun map (off_id,(_,approx)) ->
            OffsetMap.add { off_id; off_unit = functs.unit } approx map)
            OffsetMap.empty fv_ulam_approx } in
    let value_closure =
      Value_id (new_descr (Value_unoffseted_closure value_closure') env) in

    let conv_function (id,func) =
      (* adds variables from the closure to the substitution environment *)
      let fun_offset = OffsetMap.find (to_offset id) fun_offset_table in

      let env_param =
        if closed
        then
          (* This case not be used: when the function is closed every
             acces to the environment is directly substituted *)
          Uconst(Uconst_label (offset_label (closure_lbl,0) fun_offset), None)
        else Uvar env_var in

      (* inside the body of the function, we cannot access variables
         declared outside, so take a clean substitution table. *)
      let env = { env with sb = IdentMap.empty } in

      (* add informations about currently defined functions to
         allow direct call *)
      let env =
        IdentMap.fold (fun id _ map ->
            let fun_id = { off_unit = functs.unit; off_id = id } in
            let desc = Value_closure { fun_id; closure = value_closure' } in
            add_approx id (Value_id (new_descr desc env)) env)
          functs.funs env
      in

      let env =
        (* Add to the substitution the value of the free variables *)

        let add_env_variable env (id,lam) =
          match constant_label lam with
          | Not_const ->
              assert(not closed);
              let var_offset = OffsetMap.find
                  { off_unit = functs.unit; off_id = id } fv_offset_table in
              let pos = var_offset - fun_offset in
              add_sb id (Uprim(Pfield pos, [env_param], Debuginfo.none)) env
          | No_lbl ->
              add_sb id lam env
          | Lbl lbl ->
              add_cm id lbl env
        in

        let env = List.fold_left add_env_variable env fv_ulam in

        (* Add to the substitution the value of the functions defined in
           the current closure:
         * If the function is not closed, this can be retrieved by shifting
           the environment.
         * If the function is closed, we use a global variable named
           'closure_lbl_offset' defined. *)
        let add_offset_subst pos env (id,_) =
          let offset = OffsetMap.find (to_offset id) fun_offset_table in
          if closed
          then
            let lbl = offset_label (closure_lbl,0) offset in
            add_cm id lbl env
          else
            let exp = Uoffset(Uvar env_var, offset - pos) in
            add_sb id exp env in

        List.fold_left (add_offset_subst fun_offset) env funct in

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
        Compilenv.add_structured_constant closure_lbl cst true;
        Uconst(cst,Some (closure_lbl,0)),
        value_closure
    else

(* TO BE UNCOMMENTED: when fenv_field access to a constant will be
   guaranteed not to use the closure (see also iter_closure)

      let fv_ulam =
        (* only not constants are in the closure *)
        List.filter (fun (id, _) ->
            IdentSet.mem id not_constants.Constants.not_constant_id) fv_ulam in
*)

      Uclosure (ufunct, List.map snd fv_ulam),
      value_closure

  and conv_list env l = List.map (conv env) l
  and conv_list_approx env l =
    List.split (List.map (conv_approx env) l)

  and conv_const = function
    | Fconst_base c ->
      let descr = match c with
        | Const_int i -> Some (Value_int i)
        | Const_char c -> Some (Value_int (Char.code c))
        | Const_string _
        | Const_float _
        | Const_int32 _
        | Const_int64 _
        | Const_nativeint _ -> None
      in
      Uconst_base c, descr
    | Fconst_pointer c -> Uconst_pointer c, Some (Value_constptr c)
    | Fconst_float_array c -> Uconst_float_array c, None
    | Fconst_immstring c -> Uconst_immstring c, None

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
      let lbl = Compilenv.new_structured_constant cst false, 0 in
      Lbl lbl
    | Uprim(Pgetglobal id, [], _) ->
      Lbl (Ident.name id, 0)
    | _ -> Not_const

  and set_label set_lbl = function
    | Uconst(
        (Uconst_base(Const_int _ | Const_char _)
        | Uconst_pointer _), _) -> assert false
    | Uconst(Uconst_label (lbl,off_lbl), _)
    | Uconst(_, Some (lbl,off_lbl)) ->
      if off_lbl <> 0 then fatal_error "TODO: handle alias to offseted symbol";
      if lbl <> set_lbl
      then Compilenv.new_symbol_alias ~orig:lbl ~alias:set_lbl false
    | Uconst(cst, None) ->
      Compilenv.add_structured_constant set_lbl cst false
    | _ -> assert false

  let env = empty_env ()
  let res = conv env P.expr
  let root_id =
    let size_global =
      1 + (Hashtbl.fold (fun k _ acc -> max k acc) env.global (-1)) in
    let fields = Array.init size_global (fun i ->
        try Hashtbl.find env.global i with
        | Not_found -> Value_unknown) in
    new_descr (Value_block (0,fields)) env
  let root_approx = Value_id root_id
  let ex_values = !(env.ex_table)

  let module_symbol =
    let id = Compilenv.current_unit_id () in
    id, Compilenv.symbol_for_global id

  let ex_symbol_id =
    SymbolMap.add module_symbol root_id
      !(env.ex_symbol_id)
  let ex_id_symbol =
    SymbolMap.fold (fun sym id map -> EidMap.add id sym map)
      ex_symbol_id EidMap.empty

  let ex_functions =
    let unitify clos =
      { clos with
        funs =
          IdentMap.map
            (fun ff -> { ff with body = Flambdaiter.map_data ignore ff.body })
            clos.funs }
    in
    OffsetMap.fold (fun _ clos map -> FunMap.add clos.ident (unitify clos) map)
      closures FunMap.empty

  let ex_functions_off =
    let aux_fun ffunctions off_id _ map =
      OffsetMap.add {off_unit = ffunctions.unit; off_id} ffunctions map in
    let aux _ f map = IdentMap.fold (aux_fun f) f.funs map in
    FunMap.fold aux ex_functions OffsetMap.empty

end

let convert (type a) (expr:a Flambda.flambda) =
  let not_constants = Constants.not_constants ~for_clambda:true expr in
  let expr, closures = mark_closed not_constants expr in
  let module P1 = struct
    type t = a
    let expr = expr
    let not_constants = not_constants
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
    let not_constants = not_constants
    let closures = closures
  end in
  let module C = Conv(P2) in
  let export = let open Flambdaexport in
    { empty_export with
      ex_values = C.ex_values;
      ex_globals = IdentMap.singleton
          (Compilenv.current_unit_id ()) C.root_approx;
      ex_symbol_id = C.ex_symbol_id;
      ex_id_symbol = C.ex_id_symbol;
      ex_functions = C.ex_functions;
      ex_functions_off = C.ex_functions_off;
      ex_offset_fun = add_ext_offset_fun fun_offset_table;
      ex_offset_fv = add_ext_offset_fv fv_offset_table }
  in
  (* Format.printf "%a@.%a@." *)
  (*   Flambdaexport.print_approx export *)
  (*   Flambdaexport.print_symbols export; *)
  Compilenv.set_export_info export;
  C.res
