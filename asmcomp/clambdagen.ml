open Misc
open Asttypes
open Lambda
open Switch
open Clambda
open Flambda

(* The offset table associate a function label to its offset inside a closure *)
let offset_table = ref IdentMap.empty

(* Converts a flambda expression to clambda.
   During the conversion it:
    * substitute variables bound by a closure by a field access inside the closure
    * replace symbolic closure offset by the real integer offset.
    * build the switch tables
    * annotate empty closures as constants
    * emit structured constant and assign labels
    * add closure parameter for direct calls

   For everything else, it is basically the identity.
*)
let rec conv (sb:ulambda IdentMap.t) (cm:string IdentMap.t) = function
  | Fvar (id,_) ->

    (* If the variable is an acces to a constant, it is replaced by
       the constant label *)
    begin
      try
        let lbl = IdentMap.find id cm in
        Uconst(Uconst_label lbl, None)
      with Not_found ->

        (* If the variable is a recursive access to the function currently
           being defined: it is replaced by an offset in the closure.  If
           the variable is bound by the closure, it is replace by a field
           access inside the closure *)
        try IdentMap.find id sb
        with Not_found -> Uvar id
    end
  | Fconst (cst,_) ->
    let cst' = conv_const sb cm cst in
    begin match cst with
      | Fconst_base(Const_int n) -> Uconst (cst',None)
      | Fconst_base(Const_char c) -> Uconst (cst',None)
      | Fconst_pointer n -> Uconst (cst', None)
      | _ -> Uconst (cst', Some (Compilenv.new_structured_constant cst' true))
    end
  | Flet(str, id, lam, body, _) ->
    (* we need to ensure that definitions are converted before the
       body to be able to use the offset of function defined inside
       it. *)
    let lam = conv sb cm lam in
    Ulet(id, lam, conv sb cm body)
  | Fletrec(defs, body, _) ->
    let defs = List.map (fun (id,def) -> id, conv sb cm def) defs in
    Uletrec(defs, conv sb cm body)
  | Fclosure(funct, fv, _) ->
    conv_closure sb cm funct fv
  | Foffset(lam,id, _) ->
    let ulam = conv sb cm lam in
    if not (IdentMap.mem id !offset_table)
    then fatal_error (Printf.sprintf "missing offset %s" (Ident.unique_name id));
    Uoffset(ulam, IdentMap.find id !offset_table)

  | Fenv_field(lam,id, _) ->
    failwith "TODO env_field will appear only when inlining"

  | Fapply(funct, args, Some (direct_func,closed), dbg, _) ->
    let args = match closed with Closed -> args | NotClosed -> args @ [funct] in
    Udirect_apply((direct_func:>string), conv_list sb cm args, dbg)
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
  | Fsend(kind, met, obj, args, dbg, _) ->
    Usend(kind, conv sb cm met, conv sb cm obj, conv_list sb cm args, dbg)
  | Fprim(Pgetglobal id, l, dbg, _) ->
    assert(l = []);
    Uprim(Pgetglobal (Ident.create_persistent (Compilenv.symbol_for_global id)),
      [], dbg)
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
    Usequence(conv sb cm lam1, conv sb cm lam2)
  | Fwhile(cond, body, _) ->
    Uwhile(conv sb cm cond, conv sb cm body)
  | Ffor(id, lo, hi, dir, body, _) ->
    Ufor(id, conv sb cm lo, conv sb cm hi, dir, conv sb cm body)
  | Fassign(id, lam, _) ->
    Uassign(id, conv sb cm lam)

and conv_switch sb cm cases num_keys default =
  let index = Array.create num_keys 0
  and store = mk_store Flambda.same in

  (* First default case *)
  begin match default with
    | Some def when List.length cases < num_keys ->
      ignore (store.act_store def)
    | _ -> ()
  end ;
  (* Then all other cases *)
  List.iter
    (fun (key,lam) ->
      if key >= Array.length index
      then Printf.printf "len: %i key: %i\n%!" (Array.length index) key;
      index.(key) <- store.act_store lam)
    cases ;
  (* Compile action *)
  let actions = Array.map (conv sb cm) (store.act_get ()) in
  match actions with
  | [| |] -> [| |], [| |] (* May happen when default is None *)
  | _     -> index, actions

and conv_closure sb cm funct fv =
  let funct = IdentMap.bindings funct.funs in
  let fv = IdentMap.bindings fv in
  let closed = fv = [] in
  let env_var = Ident.create "env" in
  (* the label used for constant (empty) closures *)
  let closure_lbl = Compilenv.new_const_symbol () in

  let aux_offset (map,env_pos) (id, func) =
    let pos = env_pos + 1 in
    let env_pos = env_pos + 1 +
        (if func.arity <> 1 then 3 else 2) in
    assert(not (IdentMap.mem id map));
    let map = IdentMap.add id pos map in
    (map,env_pos)
  in
  let offset, fv_pos =
    List.fold_left aux_offset (!offset_table, -1) funct in

  offset_table := offset;

  (* When the function is closed, it is compiled as a function without
     closure parameter. So the env_var variable does not exist and
     thus can't be used to retrieve the closure. Since the closure of
     a closed function is a constant, it is compiled as a global
     constant that we can access using the closure_lbl label *)
  let closure_flam =
    if closed
    then Uconst(Uconst_label closure_lbl, None)
    else Uvar env_var in

  (* Add the relative offset of functions in the closure to the
     substitution *)
  let add_offset_subst pos sb (id,_) =
    IdentMap.add id
      (Uoffset (closure_flam, IdentMap.find id !offset_table - pos)) sb in

  let aux_funct (id,func) =
    (* the offset inside the closure of the function currently being
       converted: used to compute relative offset of other functions
       of the closure and the field of variables bounds in the closure.

       If all the functions are closed the closure is statically
       allocated and each function use the same closure (and point to
       its head) so the relative offset in the closure is null.

       Notice that when the closure is statically allocated, we could
       avoid computing the offset when pointing to a function inside
       the closure: we could add intermediate labels. *)
    let fun_offset =
      if closed
      then 0
      else IdentMap.find id offset in
    (* adds variables from the closure to the substitution environment *)
    let sb = build_closure_env env_var (fv_pos - fun_offset) (List.map fst fv) in
    (* adds recursive function variables to the substitution environment *)
    let sb = List.fold_left (add_offset_subst fun_offset) sb funct in
    { Clambda.label = (func.label:>string);
      arity = if func.kind = Tupled then -func.arity else func.arity;
      params = if closed then func.params else func.params @ [env_var];
      body = conv sb cm func.body;
      dbg = func.dbg } in

  let ufunct = List.map aux_funct funct in

  let ulam = conv_list sb cm (List.map snd fv) in
  match ulam with
  | [] -> Uconst_closure (ufunct,closure_lbl)
  | _ -> Uclosure (ufunct,ulam)

and conv_list sb cm l = List.map (conv sb cm) l

and conv_const sb cm cst =
  let (* rec *) aux = function
    | Fconst_base c -> Uconst_base c
    | Fconst_pointer c -> Uconst_pointer c
    (* | Fconst_block (tag,l) -> *)
    (*   Const_block (tag,List.map aux l) *)
    | Fconst_float_array c -> Uconst_float_array c
    | Fconst_immstring c -> Uconst_immstring c
    (* | Fconst_id id -> *)
    (*   failwith "TODO: ident constant" *)
      (* let label = Compilenv.make_symbol (Some (Ident.unique_name id)) in *)
      (* Const_lbl label *)
  in
 aux cst


(* If fun_a is of arity one and fun_b of arity two, and there are
   variables v1 and v2 bound in the closure, its memory representation will be

   closure = [ closure header; fun_a; 1; infix header; fun caml_curry_2; 2; fun_b; v1; v2 ]

   the closure parameter inside fun_a is the whole closure block, from
   inside fun_b it is the closure block shifted from fun_b offset (here 3)

   So access to v1 from fun_a is field( 6, env ) and from fun_b is field( 3, env ) .

   build_closure_env adds substitutions like (Uvar v1) -> Pfield( 6 - fun_offset, env )
*)
and build_closure_env env_param pos = function
    [] -> IdentMap.empty
  | id :: rem ->
    IdentMap.add id
      (Uprim(Pfield pos, [Uvar env_param], Debuginfo.none))
      (build_closure_env env_param (pos+1) rem)


let convert flam =
  offset_table := IdentMap.empty;
  let r = conv IdentMap.empty IdentMap.empty flam in
  offset_table := IdentMap.empty; (* allows garbage collection *)
  r
