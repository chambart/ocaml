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

(*
Transform an expression to prepare conversion to clambda
- attributes symbols to structured constants
- replace access to constants from the current compilation unit by Fsymbol nodes,
  including access to fields from a constant closure inside the body of the function.
- mark closed functions
- Find used closure fields and remove unused ones.
- build value approximations for export

During symbol assignment, some alias can be created (when building let rec for instance).
They are replaced by their cannonical representent in the Prepare functor application.

Then the tables needed to build the Flambdaexport.exported type are build.
*)

open Misc
open Asttypes
open Lambda
open Flambda
open Flambdaexport

(** annotate closed functions and build a label -> closed map *)
let mark_closed not_constants expr =
  let closures = ref OffsetMap.empty in
  let aux expr = match expr with
    | Fclosure(functs, fv, spec_arg, data) ->
      let closed =
        (* A function is considered to be closed if all the free variables can be converted
           to constant. i.e. if the whole closure will be compiled as a constant *)
        not (FunSet.mem functs.ident not_constants.Constants.not_constant_closure) in
      let functs = { functs with closed } in
      let add off_id _ map =
        OffsetMap.add {off_unit = functs.unit; off_id} functs map in
      closures := IdentMap.fold add functs.funs !closures;
      Fclosure(functs, fv, spec_arg, data)
    | e -> e
  in
  let expr = Flambdaiter.map aux expr in
  expr, !closures

let find_used_env_field expr =
  let used = ref IdentSet.empty in
  let aux expr = match expr with
    | Fenv_field({ env_var },_) ->
      used := IdentSet.add env_var.off_id !used
    | e -> ()
  in
  Flambdaiter.iter aux expr;
  !used

(* functions that assumes that the refered value is declared in the
   current unit: usage should be justified *)
let to_offset off_id = {off_id; off_unit = Compilenv.current_unit ()}
let to_symbol lbl : symbol = (Compilenv.current_unit_id (), lbl)

module type Param1 = sig
  type t
  val expr : t Flambda.flambda
  val not_constants : Constants.constant_result
  val closures : t Flambda.ffunctions OffsetMap.t
end

type const_sym =
  | Lbl of symbol
  | No_lbl
  | Not_const
  | Const_closure

type infos =
  { global : (int, approx) Hashtbl.t;
    ex_table : descr EidMap.t ref;
    ex_symbol_id : ExportId.t SymbolMap.t ref;
    constants : unit flambda SymbolTbl.t;
    symbol_alias : symbol SymbolTbl.t;
    ex_functions : unit ffunctions FunMap.t ref }

let init_infos () =
  { global = Hashtbl.create 10;
    ex_table = ref EidMap.empty;
    ex_symbol_id = ref SymbolMap.empty;
    constants = SymbolTbl.create 10;
    symbol_alias = SymbolTbl.create 10;
    ex_functions = ref FunMap.empty }

let rec canonical_symbol s infos =
  try
    let s' = SymbolTbl.find infos.symbol_alias s in
    let s'' = canonical_symbol s' infos in
    if s' != s''
    then SymbolTbl.replace infos.symbol_alias s s'';
    s''
  with Not_found -> s

let new_descr descr infos =
  let id = ExportId.create (Compilenv.current_unit_name ()) in
  infos.ex_table := EidMap.add id descr !(infos.ex_table);
  id

module Conv(P:Param1) = struct
  open Flambdaexport
  let closures = P.closures

  (* functions comming from a linked module *)
  let ex_closures =
    (Compilenv.approx_env ()).Flambdaexport.ex_functions_off

  let used_env_field = find_used_env_field P.expr

  let function_closed_and_label off =
    let closed_and_label clos off =
      clos.closed, (IdentMap.find off.off_id clos.funs).label in
    try closed_and_label (OffsetMap.find off closures) off with
    | Not_found ->
      try closed_and_label (OffsetMap.find off ex_closures) off with
      | Not_found ->
        fatal_error (Format.asprintf "missing closure %a"
                       Offset.print off)

  let function_arity off =
    let arity clos off = (IdentMap.find off.off_id clos.funs).arity in
    try arity (OffsetMap.find off closures) off with
    | Not_found ->
      try arity (OffsetMap.find off ex_closures) off with
      | Not_found ->
        fatal_error (Format.asprintf "missing closure %a"
                       Offset.print off)

  let not_constants = P.not_constants
  let is_constant id = not (IdentSet.mem id not_constants.Constants.not_constant_id)

  let function_symbol off =
    Compilenv.closure_symbol off

  type env =
    { sb : unit flambda IdentMap.t; (* substitution *)
      cm : symbol IdentMap.t; (* variables associated to constants *)
      approx : approx IdentMap.t }

  let infos = init_infos ()

  let empty_env =
    { sb = IdentMap.empty;
      cm = IdentMap.empty;
      approx = IdentMap.empty }

  let canonical_symbol s = canonical_symbol s infos
  let set_symbol_alias s1 s2 =
    let s1' = canonical_symbol s1 in
    let s2' = canonical_symbol s2 in
    if s1' <> s2'
    then SymbolTbl.add infos.symbol_alias s1' s2'

  let add_sb id subst env =
    { env with sb = IdentMap.add id subst env.sb }

  let add_cm id const env =
    { env with cm = IdentMap.add id const env.cm }

  let add_global i approx =
    Hashtbl.add infos.global i approx
  let get_global i =
    try Hashtbl.find infos.global i
    with Not_found ->
      (* Value_unknown *)
      fatal_error (Format.asprintf "no global %i" i)

  let add_approx id approx env =
    { env with approx = IdentMap.add id approx env.approx }
  let get_approx id env =
    try IdentMap.find id env.approx with Not_found -> Value_unknown

  let extern_symbol_descr ((id,_) as sym) =
    if Ident.is_predef_exn id
    then None
    else
      let export = Compilenv.approx_for_global id in
      (* Format.printf "load approx for sym %a@." Ident.print id; *)
      try
        let id = SymbolMap.find sym export.ex_symbol_id in
        let descr = EidMap.find id export.ex_values in
        Some descr
      with
      | Not_found -> None

  let extern_id_descr ex =
    (* let unit_id = Ident.create_persistent (ExportId.unit ex) in *)
    (* Format.printf "load approx for ex %a %a@." *)
    (*   ExportId.print ex Ident.print unit_id; *)
    (* let export = Compilenv.approx_for_global unit_id in *)
    let export = Compilenv.approx_env () in
    try Some (EidMap.find ex export.ex_values)
    with Not_found ->
      (* Format.printf "Not found@."; *)
      None

  let get_descr approx =
    match approx with
    | Value_unknown -> None
    | Value_id ex ->
      (try Some (EidMap.find ex !(infos.ex_table)) with
       | Not_found ->
         extern_id_descr ex)
    | Value_symbol sym ->
      try
        let ex = SymbolMap.find sym !(infos.ex_symbol_id) in
        Some (EidMap.find ex !(infos.ex_table))
      with Not_found ->
        extern_symbol_descr sym

  let add_symbol sym id =
    infos.ex_symbol_id := SymbolMap.add sym id !(infos.ex_symbol_id)

  let symbol_id sym =
    try Some (SymbolMap.find sym !(infos.ex_symbol_id)) with Not_found -> None

  let add_constant lam =
    let sym = to_symbol (Compilenv.new_const_symbol ()) in
    (* Format.printf "add constant@ %a@ %a@." *)
    (*   Symbol.print sym *)
    (*   Printflambda.flambda lam; *)
    SymbolTbl.add infos.constants sym lam;
    sym

  let new_descr descr = new_descr descr infos
  let unit_approx () = Value_id (new_descr (Value_constptr 0))

  let rec conv env expr = fst (conv_approx env expr)
  and conv_approx (env : env) : P.t flambda -> unit flambda * approx = function
    | Fvar (id,_) ->
      begin
        (* If the variable reference a constant, it is replaced by the
           constant label *)
        try
          let lbl = IdentMap.find id env.cm in
          Fsymbol(lbl, ()), Value_symbol lbl
        with Not_found ->

          (* If the variable is a recursive access to the function
             currently being defined: it is replaced by an offset in the
             closure. If the variable is bound by the closure, it is
             replace by a field access inside the closure *)
          try
            let lam = IdentMap.find id env.sb in
            lam, get_approx id env
          with Not_found ->
            Fvar (id, ()), get_approx id env
      end

    | Fsymbol (sym,_) ->
      Fsymbol (sym,()),
      Value_symbol sym

    | Fconst (Fconst_base c as cst,_) ->
        begin match c with
        | Const_int i ->
            Fconst(cst, ()),
            Value_id (new_descr (Value_int i))
        | Const_char c ->
            Fconst(cst, ()),
            Value_id (new_descr (Value_int (Char.code c)))
        | Const_float _
        | Const_int32 _
        | Const_int64 _
        | Const_nativeint _ ->
            Fconst(cst, ()),
            Value_unknown
        | Const_string _ ->
            Fsymbol (add_constant (Fconst (cst,())),()),
            Value_unknown
        end

    | Fconst (Fconst_pointer c as cst,_) ->
      Fconst (cst, ()), Value_id (new_descr (Value_constptr c))
    | Fconst (Fconst_float_array c as cst, _) ->
      Fsymbol(add_constant (Fconst (cst,())),()),
      Value_unknown
    | Fconst (Fconst_immstring c as cst, _) ->
      Fsymbol(add_constant (Fconst (cst,())),()),
      Value_unknown

    | Flet(str, id, lam, body, _) ->
      let lam, approx = conv_approx env lam in
      let env = add_approx id approx env in
      if not (is_constant id)
      then
        let ubody, body_approx = conv_approx env body in
        Flet(str, id, lam, ubody, ()), body_approx
      else begin match constant_symbol lam with
        | No_lbl ->
          (* no label: the value is an integer: substitute it *)
          conv_approx (add_sb id lam env) body
        | Lbl lbl ->
          (* label: the value is a block: reference it *)
          conv_approx (add_cm id lbl env) body
        | Const_closure ->
          conv_approx env body
        | Not_const ->
          Format.printf "%a@." Ident.print id;
          Printflambda.flambda Format.std_formatter lam;
          assert false
      end

    | Fletrec(defs, body, _) ->
      let consts, not_consts =
        List.partition (fun (id,_) -> is_constant id) defs in

      let env, consts = List.fold_left
          (fun (env, acc) (id,def) ->
             match def with
             | Fconst (( Fconst_pointer _
                       | Fconst_base
                           (Const_int _ | Const_char _
                           | Const_float _ | Const_int32 _
                           | Const_int64 _ | Const_nativeint _)), _) ->
               (* When the value is an integer constant, we cannot affect a label
                  to it: hence we must substitute it directly.
                  For other numerical constant, a label could be attributed, but
                  unboxing doesn't handle it well *)
               add_sb id (conv env def) env, acc
             | Fvar (var_id, _) ->
               assert(List.for_all(fun (id,_) -> not (Ident.same var_id id)) consts);
               (* For variables: the variable could have been substituted to
                  a constant: avoid it by substituting it directly *)
               add_sb id (conv env def) env, acc
             | _ ->
               let sym = to_symbol (Compilenv.new_const_symbol ()) in
               let env = add_cm id sym env in
               env, (id,sym,def)::acc) (env,[]) consts in

      List.iter (fun (id,sym,def) ->
          match constant_symbol (conv env def) with
          | Lbl sym' ->
            (match symbol_id sym' with
             | None -> ()
             | Some eid -> add_symbol sym eid);
            set_symbol_alias sym sym'
          | _ ->
              fatal_error (Format.asprintf
                             "recursive constant value without symbol %a"
                             Ident.print id))
        consts;

      let not_consts, env =
        List.fold_right (fun (id,def) (not_consts,env') ->
            let flam, approx = conv_approx env def in
            let env' = add_approx id approx env' in
            (id, flam) :: not_consts, env') not_consts ([],env) in

      let body, approx = conv_approx env body in
      (match not_consts with
       | [] -> body
       | _ -> Fletrec(not_consts, body, ())),
      approx

    | Fclosure(funct, fv, spec_arg, _) ->
      let args_approx = IdentMap.map (fun id -> get_approx id env) spec_arg in
      conv_closure env funct args_approx fv

    | Foffset(lam,id,rel, _) as expr ->
      let ulam, fun_approx = conv_approx env lam in
      let closed, _ = function_closed_and_label id in
      if closed
      then
        let sym = function_symbol id in
        Fsymbol (sym,()),
        Value_symbol sym
      else
        let approx = match get_descr fun_approx with
          | Some (Value_unoffseted_closure closure)
          | Some (Value_closure { closure }) ->
            let ex = new_descr (Value_closure { fun_id = id; closure }) in
            Value_id ex
          | Some _ -> assert false
          | _ ->
            Format.printf "Unknown closure in offset %a@."
              Printflambda.flambda expr;
            assert false;
            Value_unknown
            (* TODO: export at least the function used, just put an unknown
                 closure *)
        in
        Foffset (ulam,id,rel,()), approx

    | Fenv_field({env = lam;env_var;env_fun_id}, _) as expr ->
      let ulam, fun_approx = conv_approx env lam in
      let approx = match get_descr fun_approx with
        | Some (Value_closure { closure = { bound_var } }) ->
          (try OffsetMap.find env_var bound_var with
           | Not_found ->
             Format.printf "Wrong closure in env_field %a@.%a@."
               Printflambda.flambda expr
               Printflambda.flambda ulam;
             assert false)
        | Some _ -> assert false
        | None ->
          Format.printf "Unknown closure in env_field %a@.%a@."
            Printflambda.flambda expr
            Printflambda.flambda ulam;
          assert false in
      Fenv_field({env = ulam;env_var;env_fun_id}, ()),
      approx

    (* TODO VERIFY: we should not specialise non kept recursive parameters *)

    | Fapply (Foffset (Fclosure (ffuns, fv, spec_arg, _), off, (None as rel), _),
              args, direc, dbg, _) ->
        (match direc with
         | None -> assert false
         | Some direc -> assert (Offset.equal off direc));
        let uargs, args_approx = conv_list_approx env args in
        let func =
          try IdentMap.find off.off_id ffuns.funs
          with Not_found -> assert false in
        assert(List.length uargs = List.length func.params);
        let args_approx = List.fold_right2 IdentMap.add func.params
            args_approx IdentMap.empty in
        let uffuns, fun_approx = conv_closure env ffuns args_approx fv in
        Fapply (Foffset (uffuns, off, rel, ()),
                uargs, direc, dbg, ()),
        Value_unknown
    (* TODO: use approximation of functions *)

    | Fapply(funct, args, direct, dbg, _) ->
      let ufunct, fun_approx = conv_approx env funct in
      let direct = match direct with
        | Some _ -> direct
        | None -> match get_descr fun_approx with
          (* We mark some calls as direct when it is unknown:
             for instance if simplify wasn't run before. *)
          | Some (Value_closure { fun_id }) when
              (function_arity fun_id) = List.length args ->
            Some fun_id
          | _ -> None
      in
      Fapply(ufunct, conv_list env args, direct, dbg, ()),
      Value_unknown
    (* TODO: use approximation of functions *)

    | Fswitch(arg, sw, _) ->
      Fswitch(conv env arg,
              { sw with
                fs_consts = List.map (fun (i,lam) -> i, conv env lam) sw.fs_consts;
                fs_blocks = List.map (fun (i,lam) -> i, conv env lam) sw.fs_blocks;
                fs_failaction = may_map (conv env) sw.fs_failaction }, ()),
      Value_unknown

    | Fprim(Pgetglobal id, l, dbg, _) ->
      assert(l = []);
      let sym = id, Compilenv.symbol_for_global id in
      let approx =
        if Ident.is_predef_exn id
        then Value_id (new_descr (Value_predef_exn id))
        else Value_symbol sym in
      Fsymbol (sym, ()),
      approx

    | Fprim(Pgetglobalfield(id,i), l, dbg, v) ->
      assert(l = []);
      let lam = Fprim(Pfield i, [Fprim(Pgetglobal id, l, dbg, v)], dbg, v) in
      if id = Compilenv.current_unit_id ()
      then
        conv env lam,
        get_global i
      else
        conv_approx env lam

    | Fprim(Psetglobalfield i, [arg], dbg, _) ->
      let uarg, approx = conv_approx env arg in
      add_global i approx;
      Fprim(Psetglobalfield i, [uarg], dbg, ()),
      Value_unknown

    | Fprim(Pmakeblock(tag, Immutable) as p, args, dbg, _) ->
      let args, approxs = conv_list_approx env args in
      let block = Fprim(p, args, dbg, ()) in
      let ex = new_descr (Value_block (tag, Array.of_list approxs)) in
      if not (List.for_all is_simple_constant args)
      then block, Value_id ex
      else
        let sym = add_constant block in
        add_symbol sym ex;
        Fsymbol(sym, ()), Value_symbol sym

    | Fprim(Pfield i, [arg], dbg, _) ->
      let block, block_approx = conv_approx env arg in
      let approx = match get_descr block_approx with
        | Some (Value_block (_,fields)) ->
          if i >= 0 && i < Array.length fields
          then fields.(i)
          else Value_unknown
        | _ -> Value_unknown
      in
      Fprim(Pfield i, [block], dbg, ()),
      approx

    | Fprim(p, args, dbg, _) ->
      Fprim(p, conv_list env args, dbg, ()),
      Value_unknown

    | Fstaticfail (i, args, _) ->
      Fstaticfail (i, conv_list env args, ()),
      Value_unknown

    | Fcatch (i, vars, body, handler, _) ->
      Fcatch (i, vars, conv env body, conv env handler, ()),
      Value_unknown

    | Ftrywith(body, id, handler, _) ->
      Ftrywith(conv env body, id, conv env handler, ()),
      Value_unknown

    | Fifthenelse(arg, ifso, ifnot, _) ->
      Fifthenelse(conv env arg, conv env ifso, conv env ifnot, ()),
      Value_unknown

    | Fsequence(lam1, lam2, _) ->
      let ulam1 = conv env lam1 in
      let ulam2, approx = conv_approx env lam2 in
      Fsequence(ulam1, ulam2, ()),
      approx

    | Fwhile(cond, body, _) ->
      Fwhile(conv env cond, conv env body, ()),
      unit_approx ()

    | Ffor(id, lo, hi, dir, body, _) ->
      Ffor(id, conv env lo, conv env hi, dir, conv env body, ()),
      unit_approx ()

    | Fassign(id, lam, _) ->
      Fassign(id, conv env lam, ()),
      unit_approx ()

    | Fsend(kind, met, obj, args, dbg, _) ->
      Fsend(kind, conv env met, conv env obj, conv_list env args, dbg, ()),
      Value_unknown

    | Funreachable _ ->
      Funreachable (),
      Value_unknown

  and conv_closure env functs param_approxs fv =
    let closed = functs.closed in

    let fv_ulam_approx = IdentMap.map (conv_approx env) fv in
    let fv_ulam = IdentMap.map (fun (lam,approx) -> lam) fv_ulam_approx in

    let kept_fv id =
      not (is_constant id) || (IdentSet.mem id used_env_field) in

    let used_fv_approx = IdentMap.filter (fun id _ -> kept_fv id) fv_ulam_approx in
    let used_fv = IdentMap.map (fun (lam,approx) -> lam) used_fv_approx in

    let value_closure' =
      { closure_id = functs.ident;
        bound_var =
          IdentMap.fold (fun off_id (_,approx) map ->
              OffsetMap.add { off_id; off_unit = functs.unit } approx map)
            used_fv_approx OffsetMap.empty } in
    let closure_ex_id = new_descr (Value_unoffseted_closure value_closure') in
    let value_closure = Value_id closure_ex_id in

    (* add informations about free variables *)
    let env =
      IdentMap.fold (fun id (_,approx) -> add_approx id approx)
        fv_ulam_approx env in

    let conv_function id func =

      (* inside the body of the function, we cannot access variables
         declared outside, so take a clean substitution table. *)
      let env = { env with sb = IdentMap.empty } in

      (* add informations about currently defined functions to
         allow direct call *)
      let env =
        IdentMap.fold (fun id _ env ->
            let fun_id = { off_unit = functs.unit; off_id = id } in
            let desc = Value_closure { fun_id; closure = value_closure' } in
            let ex = new_descr desc in
            if closed then add_symbol (function_symbol fun_id) ex;
            add_approx id (Value_id ex) env)
          functs.funs env
      in

      (* add informations about kept parameters *)
      let env =
        IdentMap.fold (fun id approx env ->
            if IdentSet.mem id func.kept_params
            then add_approx id approx env
            else env)
          param_approxs env in

      (* Add to the substitution the value of the free variables *)
      let add_env_variable id lam env =
        match constant_symbol lam with
        | Not_const ->
          assert(not closed);
          env
        | No_lbl ->
          add_sb id lam env
        | Lbl lbl ->
          add_cm id lbl env
        | Const_closure ->
          env
      in
      let env = IdentMap.fold add_env_variable fv_ulam env in

      let env =
        if closed
        then
          (* if the function is closed, recursive call access those constants *)
          IdentMap.fold (fun id _ env ->
              add_cm id (function_symbol (to_offset id)) env) functs.funs env
        else env
      in
      { func with
        closure_params = IdentSet.filter kept_fv func.closure_params;
        body = conv env func.body }
    in

    let ufunct =
      { functs with funs = IdentMap.mapi conv_function functs.funs } in

    infos.ex_functions := FunMap.add ufunct.ident ufunct !(infos.ex_functions);

    let expr =
      (* TODO: fill spec_arg *)
      let expr = Fclosure (ufunct, used_fv, IdentMap.empty, ()) in
      if ufunct.closed
      then
        let sym = add_constant expr in
        add_symbol sym closure_ex_id;
        Fsymbol(sym, ())
      else expr in
    expr, value_closure

  and conv_list env l = List.map (conv env) l
  and conv_list_approx env l =
    List.split (List.map (conv_approx env) l)

  and is_simple_constant = function
    | Fconst _
    | Fsymbol _ -> true
    | _ -> false

  and constant_symbol : unit flambda -> const_sym = function
    | Fsymbol(sym, ()) ->
      Lbl sym
    | Fconst(_, ()) ->
      No_lbl
    | Fclosure (clos,_,_,_) ->
      if clos.closed
      then Const_closure
      else Not_const
    | _ -> Not_const


  let expr = conv empty_env P.expr

end

module type Param2 = sig
  include Param1
  val infos : infos
  val expr : unit flambda
end

module Prepare(P:Param2) = struct
  open P

  (*** Preparing export informations: Replacing every symbol by its
       cannonical representant ***)

  let canonical_symbol s = canonical_symbol s infos

  (* Replace all symbols occurences by their representative *)
  let expr, constants =
    let use_canonical_symbols = function
      | Fsymbol(sym, ()) as expr ->
        let sym' = canonical_symbol sym in
        if sym == sym' then expr
        else Fsymbol(sym', ())
      | expr -> expr in
    let aux sym lam map =
      let sym' = canonical_symbol sym in
      SymbolMap.add sym' (Flambdaiter.map use_canonical_symbols lam) map
    in
    Flambdaiter.map use_canonical_symbols expr,
    SymbolTbl.fold aux infos.constants SymbolMap.empty

  (* Preparing export informations *)

  let canonical_approx = function
    | Value_unknown
    | Value_id _ as v -> v
    | Value_symbol sym ->
      Value_symbol (canonical_symbol sym)

  let rec canonical_descr = function
    | Value_block (tag, fields) ->
      Value_block (tag, Array.map canonical_approx fields)
    | Value_int _
    | Value_constptr _
    | Value_predef_exn _ as v -> v
    | Value_closure offset ->
      Value_closure { offset with closure = (aux_closure offset.closure) }
    | Value_unoffseted_closure clos ->
      Value_unoffseted_closure (aux_closure clos)

  and aux_closure clos =
    { clos with bound_var = OffsetMap.map canonical_approx clos.bound_var }

  let new_descr descr = new_descr descr infos

  (* build the approximation of the root module *)
  let root_id =
    let size_global =
      1 + (Hashtbl.fold (fun k _ acc -> max k acc) infos.global (-1)) in
    let fields = Array.init size_global (fun i ->
        try canonical_approx (Hashtbl.find infos.global i) with
        | Not_found -> Value_unknown) in
    new_descr (Value_block (0,fields))

  let root_approx =
    Value_id root_id

  (* replace symbol by their representative in value approximations *)
  let ex_values =
    EidMap.map canonical_descr !(infos.ex_table)

  (* build the symbol to id and id to symbol maps *)
  let module_symbol =
    let id = Compilenv.current_unit_id () in
    id, Compilenv.symbol_for_global id

  let ex_symbol_id =
    let aux sym ex map =
      let sym' = canonical_symbol sym in
      SymbolMap.add sym' ex map
    in
    SymbolMap.fold aux !(infos.ex_symbol_id) SymbolMap.empty

  let ex_symbol_id =
    SymbolMap.add module_symbol root_id
      ex_symbol_id
  let ex_id_symbol =
    SymbolMap.fold (fun sym id map -> EidMap.add id sym map)
      ex_symbol_id EidMap.empty

  let ex_functions = !(infos.ex_functions)

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
    let closures = closures
  end in
  let module C = Conv(P1) in
  let module P2 = struct
    include P1
    let expr = C.expr
    let infos = C.infos
  end in
  let module C2 = Prepare(P2) in

  let export = let open Flambdaexport in
    { empty_export with
      ex_values = C2.ex_values;
      ex_globals = IdentMap.singleton
          (Compilenv.current_unit_id ()) C2.root_approx;
      ex_symbol_id = C2.ex_symbol_id;
      ex_id_symbol = C2.ex_id_symbol;
      ex_functions = C2.ex_functions;
      ex_functions_off = C2.ex_functions_off }
  in
  C2.expr, C2.constants, export

