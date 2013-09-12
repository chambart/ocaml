open Misc
open Asttypes
open Lambda
open Switch
open Clambda
open Flambda

(* Converts a flambda expression to clambda.
   During the conversion it:
    * substitute variables bound by a closure by a field access
      inside the closure
    * replace symbolic closure offset by the real integer offset.
    * build the switch tables
    * add closure parameter for direct calls
    * detect constants values and transform them to Uconst
    * mark closed functions
    * assign label to exported variables
    * prepare exported informations
   For everything else, it is basically the identity.
*)

(** annotate closed functions and build a label -> closed map *)
let mark_closed not_constants expr =
  let aux expr = match expr with
    | Fclosure(functs, fv, data) ->
      let closed =
        (* A function is considered to be closed if all the free variables can be converted
           to constant. i.e. if the whole closure will be compiled as a constant *)
        not (FunSet.mem functs.ident not_constants.Constants.not_constant_closure) in
      let functs = { functs with closed } in
      Fclosure(functs, fv, data)
    | Fprim(Pgetglobalfield(id,i), l, dbg, v) ->
      Fprim(Pfield i, [
          Fsymbol((id,(Compilenv.symbol_for_global id)),v)
        ], dbg, v)
    | e -> e
  in
  Flambdautils.map aux expr

let to_offset off_id = {off_id; off_unit = Compilenv.current_unit_symbol ()}

let make_symbols info =
  let open Constants in
  let open Flambdaexport in
  let make_symbol id =
    Compilenv.make_symbol (Some ((Ident.unique_name id) ^ "_global")) in
  let not_constant id =
    IdentSet.mem id info.export_constant.not_constant_id in
  let desc eid = EidMap.find eid info.export_values in
  let eid_symbols = EidTbl.create 10 in
  let id_symbols = IdentTbl.create 10 in
  let aux id approx =
    if not (not_constant id)
    then match approx with
      | Value_unknown -> ()
      | Value_id eid ->
        try
          let symbol = EidTbl.find eid_symbols eid in
          IdentTbl.add id_symbols id symbol
        with Not_found ->
          match desc eid with
          | Value_symbol _ | Value_int _
          | Value_constptr _ | Value_predef_exn _ -> ()
          | Value_block _ ->
            let symbol = make_symbol id in
            EidTbl.add eid_symbols eid symbol;
            IdentTbl.add id_symbols id symbol
          | Value_closure { fun_id } ->
            let symbol = make_symbol fun_id.off_id in
            EidTbl.add eid_symbols eid symbol;
            IdentTbl.add id_symbols id symbol
  in
  IdentMap.iter aux info.export_mapping;
  let id_map = IdentTbl.fold (fun id v map -> IdentMap.add id v map)
      id_symbols IdentMap.empty in
  let eid_map = EidTbl.fold (fun eid v map -> EidMap.add eid v map)
      eid_symbols EidMap.empty in
  id_map, eid_map

let exported_offsets info ~fv_offset_table ~fun_offset_table =
  let open Flambdaexport in
  let aux _ desc ((map_fun, map_fv) as r) = match desc with
    | Value_closure { fun_id; closure = { bound_var } } ->
      let map_fun = OffsetMap.add fun_id
          (OffsetMap.find fun_id fun_offset_table) map_fun in
      let map_fv = OffsetMap.fold (fun off _ map_fv ->
          OffsetMap.add off
            (OffsetMap.find off fv_offset_table) map_fv)
          bound_var map_fv in
      (map_fun, map_fv)
    | _ -> r
  in
  EidMap.fold aux info.Constants.export_values (OffsetMap.empty, OffsetMap.empty)

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
  (* table associating closures to the first free variable offset in
     the closure *)
  let fv_pos_table = ref FunMap.empty

  let rec iter = function
    | Fclosure(funct, fv, _) ->
      iter_closure funct fv
    | _ -> ()

  and iter_closure functs fv =

    let funct = IdentMap.bindings functs.funs in
    let fv = IdentMap.bindings fv in
    (* only consider free variables that are not constants *)
    (* let fv = List.filter (fun (id, _) -> *)
    (*     IdentSet.mem id not_constants.Constants.not_constant_id) fv in *)

    (* build the table mapping the function to the offset of its code
       pointer inside the closure value *)
    let aux_fun_offset (map,env_pos) (id, func) =
      let pos = env_pos + 1 in
      let env_pos = env_pos + 1 +
          (if func.arity <> 1 then 3 else 2) in
      (* if IdentMap.mem id map *)
      (* then Printf.printf "seen offset %s\n%!" (Ident.unique_name id); *)
      (* assert(not (IdentMap.mem id map)); *)
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
    fv_pos_table := FunMap.add functs.ident fv_pos !fv_pos_table;

    List.iter (fun (_,{body}) -> Flambdautils.iter_flambda iter body) funct

  let res =
    Flambdautils.iter_flambda iter P.expr;
    !fun_offset_table, !fv_offset_table, !fv_pos_table

end

module type Param2 = sig
  type t
  val expr : t Flambda.flambda
  val fun_offset_table : int OffsetMap.t
  val fv_offset_table : int OffsetMap.t
  val fv_pos_table : int FunMap.t
  val not_constants : Constants.constant_result
  val assigned_symbols : string IdentMap.t
  val closures : unit Flambda.ffunctions OffsetMap.t
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
  let fv_pos_table = P.fv_pos_table
  let closures = P.closures

  (* offsets of functions and free variables in closures comming from
     a linked module *)
  let extern_fun_offset_table =
    (Compilenv.approx_env ()).Flambdaexport.ex_offset_fun
  let extern_fv_offset_table =
    (Compilenv.approx_env ()).Flambdaexport.ex_offset_fv


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

  let not_constants = P.not_constants

  let add_global_alias id lbl =
    try
      let assigned = IdentMap.find id P.assigned_symbols in
      Compilenv.new_symbol_alias ~orig:lbl ~alias:assigned true
    with Not_found -> ()

  let rec conv (sb:ulambda IdentMap.t) (cm:string IdentMap.t) = function
    | Fvar (id,_) ->
      begin
        (* If the variable reference a constant, it is replaced by the
           constant label *)
        try
          let lbl = IdentMap.find id cm in
          Uconst(Uconst_label lbl, None)
        with Not_found ->

          (* If the variable is a recursive access to the function
             currently being defined: it is replaced by an offset in the
             closure. If the variable is bound by the closure, it is
             replace by a field access inside the closure *)
          try IdentMap.find id sb
          with Not_found -> Uvar id
      end

    | Fsymbol ((_,sym),_) ->
      Uconst (Uconst_label sym, None)

    | Fconst (cst,_) ->
      Uconst (conv_const sb cm cst, None)

    | Flet(str, id, lam, body, _) ->
      let lam = conv sb cm lam in
      if IdentSet.mem id not_constants.Constants.not_constant_id
      then Ulet(id, lam, conv sb cm body)
      else begin match constant_label lam with
        | No_lbl ->
          (* no label: the value is an integer substitute it *)
          let sb = IdentMap.add id lam sb in
          conv sb cm body
        | Lbl lbl ->
          (* label: the value is a block: reference it *)
          let cm = IdentMap.add id lbl cm in
          add_global_alias id lbl;
          conv sb cm body
        | Not_const ->
          Format.printf "%a@." Ident.print id;
          Printclambda.clambda Format.std_formatter lam;
          assert false
      end

    | Fletrec(defs, body, _) ->
      let not_consts, consts =
        List.partition (fun (id,_) ->
          IdentSet.mem id not_constants.Constants.not_constant_id) defs in
      let sb, cm, consts = List.fold_left
          (fun (sb, cm, acc) (id,def) ->
            match def with
            | Fconst (( Fconst_pointer _
                      | Fconst_base (Const_int _ | Const_char _)), _) ->
              (* When the value is an integer constant, we cannot affect a label
                 to it: hence we must substitute it directly. *)
              IdentMap.add id (conv sb cm def) sb, cm, acc
            | Fvar (var_id, _) ->
              List.iter (fun (id,_) ->
                  if Ident.same var_id id
                  then Printf.printf "prob: %s %s\n%!"
                      (Ident.unique_name id)
                      (Ident.unique_name var_id)) consts;
              assert(List.for_all(fun (id,_) -> not (Ident.same var_id id)) consts);
              (* For variables: the variable could have been substituted to
                 a constant: avoid it by substituting it directly *)
              IdentMap.add id (conv sb cm def) sb, cm, acc
            | _ ->
              let lbl = Compilenv.new_const_symbol () in
              let cm = IdentMap.add id lbl cm in
              sb, cm, (id,lbl,def)::acc) (sb, cm,[]) consts in
      List.iter (fun (id,lbl,def) ->
          set_label lbl (conv sb cm def);
          add_global_alias id lbl)
        consts;

      let not_consts = List.map (fun (id,def) -> id, conv sb cm def) not_consts in
      begin match not_consts with
        | [] -> conv sb cm body
        | _ -> Uletrec(not_consts, conv sb cm body) end

    | Fclosure(funct, fv, _) ->
      conv_closure sb cm funct fv

    | Foffset(lam,id, _) ->
      (* For compiling offset inside a constant closure as a constant,
         we compile it as a direct label to the function:
         label ^ "_" ^ offset
         This works because we know that Foffset is by construction always
         applied to a variable or the closure on which we can recover the
         original label.

         TODO: verify that this still holds after substitution !
         add a check in Flambda.check
      *)
      let ulam = conv sb cm lam in
      let offset = get_fun_offset id in
      make_offset ulam offset

    | Fenv_field({env = lam;env_var;env_fun_id}, _) ->
      let ulam = conv sb cm lam in
      let fun_offset = get_fun_offset env_fun_id in
      let var_offset = get_fv_offset env_var in
      let pos = var_offset - fun_offset in
      Uprim(Pfield pos, [ulam], Debuginfo.none)

    | Fapply(funct, args, Some direct_func, dbg, _) ->
      let closed = try
          (OffsetMap.find direct_func closures).closed
        with
        | Not_found ->
          fatal_error (Format.asprintf "missing closure %a"
                         Offset.print direct_func)
      in
      let args = if closed then args else args @ [funct] in

      (* If usefull things are in ufunct they are eliminated: TODO
         replace funct field by an ident field *)
      let closure = OffsetMap.find direct_func closures in
      let func = IdentMap.find direct_func.off_id closure.funs in
      Udirect_apply((func.label:>string), conv_list sb cm args, dbg)

    | Fapply(funct, args, None, dbg, _) ->
      (* the closure parameter of the function is added by cmmgen, but
         it already appears in the list of parameters of the clambda
         function for generic calls. Notice that for direct calls it is
         added here. *)
      Ugeneric_apply(conv sb cm funct, conv_list sb cm args, dbg)

    | Fswitch(arg, sw, _) ->
      (* NB: failaction might get copied, thus it should be some Lstaticraise *)
      let const_index, const_actions =
        conv_switch sb cm sw.fs_consts sw.fs_numconsts sw.fs_failaction
      and block_index, block_actions =
        conv_switch sb cm sw.fs_blocks sw.fs_numblocks sw.fs_failaction in
      Uswitch(conv sb cm arg,
        {us_index_consts = const_index;
         us_actions_consts = const_actions;
         us_index_blocks = block_index;
         us_actions_blocks = block_actions})

    | Fprim(Pgetglobal id, l, dbg, _) ->
      assert(l = []);
      Uprim(Pgetglobal (Ident.create_persistent (Compilenv.symbol_for_global id)),
        [], dbg)

    | Fprim(Pgetglobalfield(id,i), l, dbg, _) ->
      assert false;
      (* assert(l = []); *)
      (* Uprim(Pfield i, *)
      (*       [Uprim(Pgetglobal (Ident.create_persistent *)
      (*                            (Compilenv.symbol_for_global id)), [], dbg)], *)
      (*       dbg) *)

    | Fprim(Psetglobalfield i, [arg], dbg, _) ->
      Uprim(Psetfield (i,false),
            [Uprim(Pgetglobal (Ident.create_persistent
                                 (Compilenv.make_symbol None)), [], dbg);
             conv sb cm arg],
            dbg)

    | Fprim(Pmakeblock(tag, Immutable) as p, args, dbg, _) ->
      let args = conv_list sb cm args in
      begin match constant_list args with
        | None ->
          Uprim(p, args, dbg)
        | Some l ->
          let cst = Uconst_block (tag,l) in
          Uconst(cst, None)
      end

    | Fprim(p, args, dbg, _) ->
      Uprim(p, conv_list sb cm args, dbg)
    | Fstaticfail (i, args, _) ->
      Ustaticfail (i, conv_list sb cm args)
    | Fcatch (i, vars, body, handler, _) ->
      Ucatch (i, vars, conv sb cm body, conv sb cm handler)
    | Ftrywith(body, id, handler, _) ->
      Utrywith(conv sb cm body, id, conv sb cm handler)
    | Fifthenelse(arg, ifso, ifnot, _) ->
      Uifthenelse(conv sb cm arg, conv sb cm ifso, conv sb cm ifnot)
    | Fsequence(lam1, lam2, _) ->
      let ulam1 = conv sb cm lam1 in
      let ulam2 = conv sb cm lam2 in
      Usequence(ulam1, ulam2)
    | Fwhile(cond, body, _) ->
      Uwhile(conv sb cm cond, conv sb cm body)
    | Ffor(id, lo, hi, dir, body, _) ->
      Ufor(id, conv sb cm lo, conv sb cm hi, dir, conv sb cm body)
    | Fassign(id, lam, _) ->
      Uassign(id, conv sb cm lam)
    | Fsend(kind, met, obj, args, dbg, _) ->
      Usend(kind, conv sb cm met, conv sb cm obj, conv_list sb cm args, dbg)

    | Funreachable _ ->
      Printf.printf "\n\n\nUnreachable code\n\n\n%!";
      (* fatal_error "unreachable node" *)
      (* shoudl'nt be executable, maybe build something else *)
      Uprim(Praise, [Uconst (Uconst_pointer 0, None)], Debuginfo.none)

  and make_offset ulam offset =
    match ulam with
    | Uconst(Uconst_label lbl,_)
    | Uconst(_,Some lbl) ->
      Uconst(Uconst_label (offset_label lbl offset),None)
    | _ ->
      if offset = 0
      then ulam
      (* compilation of let rec in cmmgen assumes
         that a closure is not offseted (Cmmgen.expr_size) *)
      else Uoffset(ulam, offset)

  and offset_label lbl offset =
    if offset = 0
    then lbl
    else lbl ^ "_" ^ string_of_int offset

  and conv_switch sb cm cases num_keys default =
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
    let actions = Array.map (conv sb cm) (store.act_get ()) in
    match actions with
    | [| |] -> [| |], [| |] (* May happen when default is None *)
    | _     -> index, actions

  and conv_closure sb cm functs fv =
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

    let fv_pos = FunMap.find functs.ident fv_pos_table in

    let fv_ulam = List.map (fun (id,lam) -> id,conv sb cm lam) fv in

    let conv_function (id,func) =
      (* adds variables from the closure to the substitution environment *)
      let fun_offset = OffsetMap.find (to_offset id) fun_offset_table in

      let env_param =
        if closed
        then
          (* This case not be used: when the function is closed every
             acces to the environment is directly substituted *)
          Uconst(Uconst_label (offset_label closure_lbl fun_offset), None)
        else Uvar env_var in

      (* inside the body of the function, we cannot access variables
         declared outside, so take a clean substitution table. *)
      let sb = IdentMap.empty in

      let sb, cm =
        (* Add to the substitution the value of the free variables *)

        let add_env_variable (sb,cm,pos) (id,lam) =
          let sb, cm =
            match constant_label lam with
            | Not_const ->
              assert(not closed);
              IdentMap.add id (Uprim(Pfield pos, [env_param], Debuginfo.none)) sb,
              cm
            | No_lbl ->
              let sb = IdentMap.add id lam sb in
              sb, cm
            | Lbl lbl ->
              let cm = IdentMap.add id lbl cm in
              sb, cm in
          sb, cm, (pos+1) in

        let (sb, cm, _pos) = List.fold_left add_env_variable
            (sb, cm, fv_pos - fun_offset) fv_ulam in

        (* Add to the substitution the value of the functions defined in
           the current closure:
         * If the function is not closed, this can be retrieved by shifting
           the environment.
         * If the function is closed, we use a global variable named
           'closure_lbl_offset' defined. *)
        let add_offset_subst pos (sb,cm) (id,_) =
          let offset = OffsetMap.find (to_offset id) fun_offset_table in
          if closed
          then
            let lbl = offset_label closure_lbl offset in
            sb, IdentMap.add id lbl cm
          else
            let exp = Uoffset(Uvar env_var, offset - pos) in
            IdentMap.add id exp sb,cm in

        List.fold_left (add_offset_subst fun_offset) (sb,cm) funct in

      { Clambda.label = (func.label:>string);
        arity = if func.kind = Tupled then -func.arity else func.arity;
        params = if closed then func.params else func.params @ [env_var];
        body = conv sb cm func.body;
        dbg = func.dbg } in

    let ufunct = List.map conv_function funct in

    if closed
    then
      match constant_list (List.map snd fv_ulam) with
      | None -> assert false
      | Some fv_const ->
        let cst = Uconst_closure (ufunct, closure_lbl, fv_const) in
        Compilenv.add_structured_constant closure_lbl cst true;
        Uconst(cst,Some closure_lbl)
    else
      (* let fv_ulam = *)
      (*   (\* only not constants are in the closure *\) *)
      (*   List.filter (fun (id, _) -> *)
      (*       IdentSet.mem id not_constants.Constants.not_constant_id) fv_ulam in *)
      Uclosure (ufunct, List.map snd fv_ulam)

  and conv_list sb cm l = List.map (conv sb cm) l

  and conv_const sb cm = function
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
      let lbl = Compilenv.new_structured_constant cst false in
      Lbl lbl
    | Uprim(Pgetglobal id, [], _) ->
      Lbl (Ident.name id)
    | _ -> Not_const

  and set_label set_lbl = function
    | Uconst(
        (Uconst_base(Const_int _ | Const_char _)
        | Uconst_pointer _), _) -> assert false
    | Uconst(Uconst_label lbl, _)
    | Uconst(_, Some lbl) ->
      if lbl <> set_lbl
      then Compilenv.new_symbol_alias ~orig:lbl ~alias:set_lbl false
    | Uconst(cst, None) ->
      Compilenv.add_structured_constant set_lbl cst false
    | _ -> assert false

  let res = conv IdentMap.empty IdentMap.empty P.expr

end

let offset_indexed_closures closures =
  let aux_fun ffunctions off_id _ map =
    OffsetMap.add {off_unit = ffunctions.unit; off_id} ffunctions map in
  let aux _ f map = IdentMap.fold (aux_fun f) f.funs map in
  FunMap.fold aux closures OffsetMap.empty

let convert (type a) (expr:a Flambda.flambda) =
  let export_info = Constants.export_info expr in
  let not_constants = export_info.Constants.export_constant in
  let expr = mark_closed not_constants expr in
  let ex_functions = Flambdautils.exportable_functions expr in
  let closures =
    offset_indexed_closures
      (FunMap.disjoint_union ex_functions
         (Compilenv.approx_env ()).Flambdaexport.ex_functions) in
  let module P1 = struct
    type t = a
    let expr = expr
    let not_constants = not_constants
  end in
  let fun_offset_table, fv_offset_table, fv_pos_table =
    let module O = Offsets(P1) in
    O.res
  in
  let assigned_symbols, ex_id_symbol = make_symbols export_info in
  let ex_offset_fun, ex_offset_fv = exported_offsets export_info
      ~fv_offset_table ~fun_offset_table in
  let module P2 = struct include P1
    let fun_offset_table = fun_offset_table
    let fv_offset_table = fv_offset_table
    let fv_pos_table = fv_pos_table
    let not_constants = not_constants
    let assigned_symbols = assigned_symbols
    let closures = closures
  end in
  let module C = Conv(P2) in
  let current_unit_id = Compilenv.current_unit_id () in
  let module_symbol = current_unit_id,
                      Compilenv.symbol_for_global current_unit_id in
  let open Flambdaexport in
  let ex_id_symbol =
    let tmp = EidMap.map (fun v -> current_unit_id,v) ex_id_symbol in
    match export_info.Constants.export_global with
    | Value_unknown -> assert false
    | Value_id eid -> EidMap.add eid module_symbol tmp
  in
  let exported =
    { ex_functions;
      ex_values = export_info.Constants.export_values;
      ex_globals =
        IdentMap.singleton current_unit_id
          export_info.Constants.export_global;
      ex_id_symbol; ex_offset_fun; ex_offset_fv } in
  C.res, exported
