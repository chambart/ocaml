open Misc
open Asttypes
open Lambda
open Flambda
open Values

let splitn n l =
  let rec aux n l acc =
    if n = 0
    then List.rev acc, l
    else match l with
      | [] -> assert false
      | t::q -> aux (n-1) q (t::acc) in
  aux n l []

(* mark expressions in tail position. This is used to track the return
   value of functions that could escape the analysis scope, not really
   if a call is effectively a tail call, so it doesn't matter if
   there are too much arguments to do a tail call. *)
let tail_position t =
  let tail_pos = ref ExprSet.empty in
  let rec aux is_tail expr =
    if is_tail then tail_pos := ExprSet.add (data expr) !tail_pos;
    match expr with
    | Fvar _ -> ()
    | Fconst _ -> ()
    | Fapply (func, args, _, _, expr) ->
      aux false func;
      List.iter (aux false) args
    | Fclosure (funs,fv,_) ->
      IdentMap.iter (fun _ n -> aux false n) fv;
      IdentMap.iter (fun _ { body } -> aux true body) funs.funs
    | Foffset (t,_,_) ->
      aux false t
    | Fenv_field (t,_) ->
      aux false t.env
    | Flet (_,_,n,t,_) ->
      aux false n;
      aux is_tail t
    | Fletrec (l,t,_) ->
      List.iter (fun (_,n) -> aux false n) l;
      aux is_tail t
    | Fprim (Psequand, [n; arg2], _, _)
    | Fprim (Psequor, [n; arg2], _, _) ->
      aux false n;
      aux is_tail arg2
    | Fprim (_,l,_,_) ->
      List.iter (aux false) l;
    | Fswitch (n,sw,_) ->
      aux false n;
      List.iter (aux is_tail) (List.map snd sw.fs_consts);
      List.iter (aux is_tail) (List.map snd sw.fs_blocks)
    | Fstaticfail (_,l,_) ->
      List.iter (aux false) l;
    | Fcatch (_,_,t1,t2,_) ->
      aux is_tail t1;
      aux is_tail t2
    | Ftrywith (n,_,t,_) ->
      aux false n;
      aux is_tail t
    | Fifthenelse (n,t1,t2,_) ->
      aux false n;
      aux is_tail t1;
      aux is_tail t2
    | Fsequence (n,t,_) ->
      aux false n;
      aux is_tail t
    | Fwhile (n1,n2,_) ->
      aux false n1;
      aux false n2
    | Ffor (_,n1,n2,_,n3,_) ->
      aux false n1;
      aux false n2;
      aux false n3
    | Fassign (_,n,_) ->
      aux false n
    | Fsend (_,n1,n2,nl,_,_) ->
      aux false n1;
      aux false n2;
      List.iter (aux false) nl
  in
  aux true t;
  !tail_pos

type stack =
  | Escape
  | St of ExprId.t

module StackSet = Set.Make(struct
    type t = stack
    let compare s1 s2 = match s1,s2 with
      | Escape, Escape -> 0
      | Escape, St _ -> -1
      | St _, Escape -> 1
      | St s1, St s2 -> ExprId.compare s1 s2
  end)

let list_functions t =
  let r = ref FunMap.empty in
  let rec aux = function
    | Fclosure (funcs,_,_) ->
      (* Printf.printf "func %a\n%!" FunId.output funcs.ident; *)
      r := FunMap.add funcs.ident funcs !r;
      IdentMap.iter (fun _ ffunc -> iter_tree ffunc.body) funcs.funs
    | _ -> ()
  and iter_tree t =
    Flambdautils.iter_flambda aux t
  in
  iter_tree t;
  !r

(* let list_code_zones t = *)
(*   let r = ref ExprMap.empty in *)
(*   let rec aux t = *)
(*     let eid = Flambda.data t in *)
(*     IdentMap.add *)
(*  function *)
(*     | ... *)

(* TODO: annotate code zones with their equivalent closure to identify
   static fails.  *)

let global_size t =
  let r = ref (-1) in
  let aux = function
    | Fprim(Psetfield(n, _), [Fprim(Pgetglobal id, arg, _, _); lam], _, _) ->
      r := max !r n;
    | _ -> ()
  in
  Flambdautils.iter_flambda aux t;
  !r + 1

type code_informations = {
  functions : ExprId.t ffunctions FunMap.t;
  tail_position : ExprSet.t;
  global_size : int;
}

let prepare_informations t =
  { functions = list_functions t;
    tail_position = tail_position t;
    global_size = global_size t }

type env = {
  bindings : ValId.t IdentMap.t;
  values : values ValMap.t;
}

type ret =
  | New of values
  | Old of ValSet.t
  | NewOld of (values option * ValSet.t)

type analysis_result = {
  info : code_informations;
  (* associate each binding with its potential values *)
  bindings : ValSet.t IdentMap.t;
  (* associate each point with its current stack *)
  stacks : StackSet.t ExprMap.t;

  (* associate each value identifier to its contents *)
  values : Values.values ValMap.t;

  (* set of variables potentialy returned by expressions *)
  expr : ValSet.t ExprMap.t;

  (* when an expression can produce a value it is here *)
  expr_val : ValId.t ExprMap.t;

  (* values escaping outside of the context of the analysis *)
  escaping : ValSet.t;

  (* values potentially returned by static fails *)
  staticfails : ValSet.t list IntMap.t;

  (* the value representing the global module *)
  global_val : ValId.t;
  (* a special value representing values comming from outside the
     analysis *)
  external_val : ValId.t;
}

(* c'est possible d'implementer ça avec une précision réglable:
   par exemple: les constantes peuvent être définies ou pas

   si elles sont disponibles, on peut éliminer des branches de if,
   mais il faut potentiellement plus de pas pour atteindre le point
   fixe. *)

module type Fparam = sig
  val tree : ExprId.t Flambda.flambda
  val call_per_round : int
  val test_if : bool
end

module Call_graph(Param:Fparam) = struct

  let global_val = ValId.create ()
  let external_val = ValId.create ()
  let external_eid = ExprId.create ()
  (* maybe do some identifier special cases... *)

  let () = assert(Param.call_per_round > 0)

  let info = prepare_informations Param.tree

  let bindings : ValSet.t IdentTbl.t = IdentTbl.create 1000
  let rev_bindings : bindings ValTbl.t = ValTbl.create 1000

  let val_tbl : values ValTbl.t = ValTbl.create 1000
  let stacks : StackSet.t ExprTbl.t = ExprTbl.create 1000

  let expr : ValSet.t ExprTbl.t = ExprTbl.create 1000
  (* set of values potentially returned by an expression *)

  let expr_val : ValId.t ExprTbl.t = ExprTbl.create 1000
  (* value constructed by an expression *)

  let escapes : ValSet.t ref = ref ValSet.empty

  let staticfails : ValSet.t list IntTbl.t = IntTbl.create 20

  let fun_call_count : int IdentTbl.t = IdentTbl.create 100

  let root_callid = CallId.create ~name:"root" ()

  let current_callid : CallId.t ref = ref root_callid

  let is_tail eid =
    ExprSet.mem eid info.tail_position ||
    ExprId.equal eid external_eid
    (* external is always considered as calling in tail position *)

  let value v =
    try ValTbl.find val_tbl v with
    | Not_found -> empty_value

  let set_global v =
    ValTbl.replace val_tbl global_val v

  let () =
    let global_id = Ident.create_persistent (Compilenv.current_unit_name ()) in
    ValTbl.replace rev_bindings
      global_val (set_global_binding unknown_binding global_id);
    set_global (empty_block 0 info.global_size);
    ValTbl.replace val_tbl external_val unknown_value

  let mu exp =
    let eid = data exp in
    try ExprTbl.find expr eid with
    | Not_found ->
      fatal_error (Printf.sprintf "return value not available for expr %s"
            (ExprId.to_string eid))

  (* used for function calls: allow recursive functions *)
  let mu' exp =
    let eid = data exp in
    try ExprTbl.find expr eid with
    | Not_found -> ValSet.empty

  let set_union set =
    let l = ValSet.elements set in
    let vl = List.map value l in
    Values.list_union vl

  let val_union exp =
    let ids = mu exp in
    let l = ValSet.elements ids in
    let vl = List.map value l in
    (* pourrait être mémoizé *)
    Values.list_union vl

  let expr_vid exp =
    let eid = data exp in
    let desc = string_desc exp in
    try ExprTbl.find expr_val eid with
    | Not_found ->
      let vid = ValId.create ~name:desc () in
      ExprTbl.add expr_val eid vid;
      vid

  let find_var id =
    try IdentTbl.find bindings id with
    | Not_found ->
      (* Printf.printf "find none %s\n%!" (Ident.unique_name id); *)
      ValSet.empty

  let bind id values =
    let vids = find_var id in
    (* Printf.printf "bind var %s -> %a \\sub? %a\n%!" *)
    (*   (Ident.unique_name id) *)
    (*   ValSet.output values *)
    (*   ValSet.output vids; *)
    if not (ValSet.subset values vids)
    then IdentTbl.replace bindings id (ValSet.union vids values)

  let find_staticfail i =
    try Some (IntTbl.find staticfails i) with
    | Not_found ->
      (* Printf.printf "find staticfail none %i\n%!" i; *)
      None

  let bind_staticfail i values_list =
    let l = match find_staticfail i with
      | None -> values_list
      | Some l ->
        assert(List.length values_list = List.length l);
        List.map2 ValSet.union l values_list in
    IntTbl.replace staticfails i l

  let escape vids =
    if not (ValSet.subset vids !escapes)
    then escapes := ValSet.union vids !escapes

  let find_global id =
    (* TODO: remove *)
    let global_approx id =
      if Ident.is_predef_exn id
      then New unknown_value
      else
        ((* ignore (Compilenv.global_approx id); *)
         New unknown_value) in

    if id.Ident.name = Compilenv.current_unit_name ()
    then Old (ValSet.singleton global_val)
    else global_approx id
  (* TODO change:
     Compilenv.global_approx id *)

  let rec aux st exp =
    let eid = data exp in
    (* let desc = string_desc exp in *)
    (* Printf.printf "aux %s %a\n%!" desc ExprId.output eid; *)

    let st =
      try
        let st' = ExprTbl.find stacks eid in
        if (StackSet.subset st st')
        then st'
        else
          let st = StackSet.union st st' in
          ExprTbl.replace stacks eid st;
          st
      with Not_found ->
        ExprTbl.add stacks eid st;
        st in

    let vids =
      let insert vid values =
        (* Printf.printf "insert %a\n%!" ValId.output vid; *)
        let v = value vid in
        ValTbl.replace val_tbl vid (Values.union v values);
      in
      match aux' st exp with
      | NewOld (None, val_ids)
      | Old val_ids -> val_ids
      | New values ->
        let vid = expr_vid exp in
        insert vid values;
        ValSet.singleton vid
      | NewOld (Some values, val_ids) ->
        let vid = expr_vid exp in
        insert vid values;
        ValSet.add vid val_ids
    in

    (* if the expression is in tail position, i.e. its value is
       returned by a function and that function can be called by a
       scope outside the analysis (can return to a 'stack' Escape)
       then the returned value can also escape the analysis *)
    if StackSet.mem Escape st && ExprSet.mem eid info.tail_position
    then escape vids;

    try
      let old_vids = ExprTbl.find expr eid in
      if not (ValSet.subset vids old_vids)
      then (
        (* Printf.printf "insert plus %s\n%!" (ExprId.to_string eid); *)
        ExprTbl.replace expr eid (ValSet.union old_vids vids))
    with
    | Not_found ->
      ((* Printf.printf "insert new %a -> %a\n%!" *)
       (*   ExprId.output eid *)
       (*   ValSet.output vids; *)
       ExprTbl.replace expr eid vids)

  and aux' st = function
    | Fvar (id,_) -> Old (find_var id)
    | Fconst (cst,_) -> New (Values.const cst)
    | Flet(str, id, lam, body, _) ->
      aux st lam;
      bind id (mu lam);
      aux st body;
      Old (mu body)

    | Fletrec(defs, body, _) ->
      List.iter (fun (id,lam) ->
          aux st lam;
          bind id (mu lam)) defs;
      aux st body;
      Old (mu body)

    | Fprim(Pmakeblock(tag, mut), lams, _, _) ->
      List.iter (aux st) lams;
      (match mut with
        | Mutable ->
          List.iter (fun lam -> escape (mu lam)) lams;
          New value_mutable
        | Immutable ->
          let vlam = List.map mu lams in
          New (value_block tag (Array.of_list vlam)))
    | Fprim(Pfield n, args, _, _) ->
      (match args with
       | [] | _ :: _ :: _ -> assert false
       | [lam] ->
         aux st lam;
         (* let v, set = Values.field n (val_union lam) in *)
         (* Printf.printf "getfield %i clos %i\n%!" n *)
         (*   (FunMap.cardinal (set_union set).v_clos); *)
         NewOld(Values.field n (val_union lam)))

    | Fprim(Pgetglobal id, arg, _, _) ->
      assert(arg = []);
      find_global id

    | Fprim(Psetfield(n, _), [Fprim(Pgetglobal id, arg, _, _); lam], _, _) ->
      assert(id.Ident.name = Compilenv.current_unit_name ());
      assert(arg = []);
      aux st lam;
      set_global (set_field n (value global_val) (mu lam));
      (* Printf.printf "setfield %i clos %i\n%!" n *)
      (*   (FunMap.cardinal (val_union lam).v_clos); *)
      (* TODO: escape only if the value is available in the interface *)
      escape (mu lam);
      New value_unit

      (* En fait on pourrait accepter ces effets de bord tant que la
         reference n'échappe pas. A voir... *)
    | Fprim(Pmakearray kind, args, _, _) ->
      List.iter (aux st) args;
      List.iter (fun lam -> escape (mu lam)) args;
      (match kind with
       | Pgenarray | Paddrarray | Pintarray ->
         New value_mutable
       | Pfloatarray ->
         New value_floatarray)

    | Fprim(Pccall desc, args, _, _) ->
      List.iter (aux st) args;
      List.iter (fun lam -> escape (mu lam)) args;
      if desc.Primitive.prim_native_float
      then New value_float
      else New unknown_value

    | Fprim(Praise, args, _, _) ->
      List.iter (aux st) args;
      List.iter (fun lam -> escape (mu lam)) args;
      New value_bottom

      (* rare enouth: don't bother and consider that like a mutable *)
    | Fprim(Pduprecord _, args, _, _) ->
      List.iter (aux st) args;
      List.iter (fun lam -> escape (mu lam)) args;
      New value_mutable

    | Fprim(Psetfield _ , args, _, _) ->
      List.iter (aux st) args;
      (match args with
       | [] | [_] | _::_::_::_ -> assert false
       | [a;arg] ->
         escape (mu arg);
         New value_unit)

    | Fprim(p, args, _, eid) ->
      (* Printf.printf "prim %a\n%!" ExprId.output eid; *)
      List.iter (aux st) args;
      New (Values.prim p (List.map val_union args))

    | Fsequence(lam1, lam2, _) ->
      aux st lam1;
      aux st lam2;
      Old (mu lam2)

    | Fifthenelse(arg, ifso, ifnot, _) ->
      (* TODO: version plus cher qui test arg et élimine les branches
         non accessibles. Ce serait raisonnablement faisable à coup de
         narrowing aussi: on calcule le point fixe puis on élimine
         toutes les branches jamais prises, puis renarrowing... *)
      aux st arg;
      let varg = val_union arg in
      begin match if_value varg with
        | True ->
          (* Printf.printf "goto true\n%!"; *)
          aux st ifso;
          Old (mu ifso)
        | False ->
          (* Printf.printf "goto false\n%!"; *)
          aux st ifnot;
          Old (mu ifnot)
        | TrueAndFalse ->
          (* Printf.printf "goto both\n%!"; *)
          aux st ifso;
          aux st ifnot;
          Old (ValSet.union (mu ifso) (mu ifnot))
        | Neither ->
          (* Printf.printf "goto neither\n%!"; *)
          Old ValSet.empty
      end
    | Fassign(id, lam, _) ->
      aux st lam;
      escape (mu lam);
      New value_unit

    | Fclosure (functions,fv,eid) ->
      (* Printf.printf "closure %a\n%!" ExprId.output eid; *)
      IdentMap.iter (fun _ lam -> aux st lam) fv;
      New (value_closure
          { fun_id = None;
            closure_funs = functions.ident;
            closure_vars = IdentMap.map mu fv;
            partial_application = Known []})

    | Fapply ( func, args, _, dbg, eid) ->
      aux st func;
      List.iter (aux st) args;
      let fun_value = val_union func in
      (* Printf.printf "apply %a\n%!" ExprId.output eid; *)
      let annotated_args = List.map (fun s -> mu s, ExprSet.singleton eid) args in
      aux_apply1 st fun_value annotated_args eid

    | Foffset(lam, id, eid) ->
      aux st lam;
      (* let lid = data lam in *)
      (* Printf.printf "offset %a -> %a\n%!" *)
      (*   ExprId.output lid *)
      (*   ExprId.output eid; *)
      (* let l = val_union lam in *)
      (* Printf.printf "offset %a clos %i\n%!" ExprId.output eid *)
      (*   (FunMap.cardinal l.v_clos); *)
      let c = Values.set_closure_funid (val_union lam) id info.functions in
      (* Printf.printf "offset %a %i\n%!" ExprId.output eid *)
      (*   (FunMap.cardinal c.v_clos); *)
      New (c)

    | Fenv_field({env; env_var}, _) ->
      aux st env;
      NewOld(env_field env_var (val_union env))

    | Fswitch(arg, sw, _) ->
      aux st arg;
      (* TODO: comme pour if *)
      let branches =
        let l = List.map snd sw.fs_consts
          @ List.map snd sw.fs_blocks in
        match sw.fs_failaction with
        | None -> l
        | Some b -> b :: l in
      List.iter (aux st) branches;
      let l = List.fold_left ValSet.union
          ValSet.empty (List.map mu branches) in
      Old l

    | Fwhile (cond, body, _) ->
      aux st cond;
      aux st body;
      New value_unit

    | Ffor(id, lo, hi, dir, body, _) ->
      aux st lo;
      aux st hi;
      (* BOF *)
      bind id (ValSet.union (mu lo) (mu hi));
      aux st body;
      New value_unit

    | Fstaticfail(i, args, _) ->
      List.iter (aux st) args;
      bind_staticfail i (List.map mu args);
      New value_bottom

    | Fcatch(i, ids, body, handler, _) ->
      aux st body;
      begin match find_staticfail i with
        | None -> Old (mu body)
        | Some l ->
          List.iter2 bind ids l;
          aux st handler;
          Old (ValSet.union (mu body) (mu handler))
      end

    | Ftrywith(body, id, handler, _) ->
      (* change the stack for exceptions ? *)
      aux st body;
      bind id (ValSet.singleton external_val);
      aux st handler;
      Old (ValSet.union (mu body) (mu handler))

    | Fsend (kind ,meth ,obj ,args , dbg, _) ->
      let exprs = meth :: obj :: args in
      List.iter (aux st) exprs;
      List.iter (fun x -> escape (mu x)) exprs;
      New (unknown_value)

  and aux_apply1 st fun_value args eid =

    let v_other = match fun_value.v_other with
      | Value_unknown -> Some unknown_value
      | _ -> None in

    let aux_funmap _ map acc =
      IdentMap.fold (fun _ f acc -> f :: acc) map acc in
    let functions = FunMap.fold aux_funmap fun_value.v_clos [] in

    (* Printf.printf "aux apply 1 %a funs: %i\n%!" ExprId.output eid *)
    (*   (List.length functions); *)

    let may_union v1 v2 = match v1,v2 with
      | None, v | v, None -> v
      | Some v1, Some v2 -> Some (Values.union v1 v2) in

    let orig_callid = !current_callid in

    let aux_ffuncs (res_value,res_set) ffunction =
      match aux_apply st ffunction args eid with
      | New values -> (may_union (Some values) res_value, res_set)
      | Old set -> (res_value, ValSet.union res_set set)
      | NewOld(values, set) ->
        (may_union values res_value, ValSet.union res_set set)
    in
    let (new_value,old_set) = List.fold_left aux_ffuncs
        (v_other, ValSet.empty) functions in

    current_callid := orig_callid;

    NewOld (new_value,old_set)

  and aux_apply st func args eid =

    (* Printf.printf "aux apply %a\n%!" ExprId.output eid; *)

    let ffunctions = FunMap.find func.closure_funs info.functions in
    let fun_id = match func.fun_id with None -> assert false | Some i -> i in
    let ffunction = IdentMap.find fun_id ffunctions.funs in

    current_callid := CallId.create ~name:(Ident.unique_name fun_id) ();

    match func.partial_application with
    | Unknown param_set ->
      escape param_set;
      List.iter (fun id -> bind id (ValSet.singleton external_val))
        ffunction.params;
      let call_stack = StackSet.singleton Escape in
      aux_count call_stack ffunction.body fun_id;
      New unknown_value

    | Known partial_application ->
      (* Check tuppled functions... *)
      let len = List.length args +
          List.length partial_application in
      let all_args = partial_application @ args in
      if ffunction.arity > len
      then
        ((* Printf.printf "partial application %a %i\n%!" ExprId.output eid len; *)
         New (value_closure { func with partial_application = Known all_args }))
      else begin
        IdentMap.iter bind func.closure_vars;
        if ffunction.arity = len
        then begin
          (* Printf.printf "complete application %a\n%!" ExprId.output eid; *)
          List.iter2 (fun id (value,_) -> bind id value)
            ffunction.params all_args;
          let call_stack =
            if is_tail eid then st
            else StackSet.singleton (St eid)
          in
          aux_count call_stack ffunction.body fun_id;
          Old (mu' ffunction.body)
        end
        else (* ffunction.arity < len *)
          begin
            (* Printf.printf "surapplication arity %i l %i\n%!" ffunction.arity *)
            (*   (List.length all_args); *)
            let args, rest_args = splitn ffunction.arity all_args in
            List.iter2 (fun id (value,_) -> bind id value)
              ffunction.params args;
            (* no tail call when applying more parameters
               than what is expected *)
            let call_stack = st in
            aux_count call_stack ffunction.body fun_id;
            let ret_function = val_union ffunction.body in
            aux_apply1 st ret_function rest_args eid
          end
      end

  and aux_count st lam fun_id =
    let count = try IdentTbl.find fun_call_count fun_id with Not_found -> 0 in
    if count < Param.call_per_round
    then
      (IdentTbl.replace fun_call_count fun_id (count+1);
       (* Printf.printf "call fun %s count %i\n%!" *)
       (*   (Ident.unique_name fun_id) (count+1); *)
       aux st lam)

  let value_transitive_closure init_set =
    let done_val = ref ValSet.empty in
    let todo = Queue.create () in
    let add_set s =
      ValSet.iter (fun v ->
        if not (ValSet.mem v !done_val) then Queue.add v todo) s in
    add_set init_set;
    while not (Queue.is_empty todo) do
      let v = Queue.take todo in
      if not (ValSet.mem v !done_val)
      then begin
        done_val := ValSet.add v !done_val;
        add_set (linked_values (value v));
      end
    done;
    !done_val

  (* if a function escapes, it can be called with any parameters (we
     could restrict a bit using type informations) *)
  let handle_escaping_functions escapes =
    let escape_function fun_value =
      let fun_id = match fun_value.fun_id with
        | None ->
          (* non offseted cannot be called ?
             TODO: verify *)
          assert false
        | Some id -> id in
      (* Printf.printf "\n\nescape %s\n\n\n%!" (Ident.unique_name fun_id); *)
      let ffunc =
        IdentMap.find fun_id
          (FunMap.find fun_value.closure_funs info.functions).funs in
      let params = match fun_value.partial_application with
        | Unknown _ -> ffunc.params
        | Known l ->
          (* preaplied parameters does cannot value is fixed *)
          let _dropped_head, tail = splitn (List.length l) ffunc.params in
          tail in
      let args = List.map (fun _ -> ValSet.singleton external_val,
          ExprSet.singleton external_eid)
          params in
      ignore(aux_apply (StackSet.singleton Escape)
          fun_value args external_eid:ret)
    in
    ValSet.iter (fun id ->
      FunMap.iter (fun _ m -> IdentMap.iter (fun _ v -> escape_function v) m)
        (ValTbl.find val_tbl id).v_clos) escapes

  let result () =
    { info = info;
      bindings = IdentTbl.to_map bindings;
      stacks = ExprTbl.to_map stacks;
      values = ValTbl.to_map val_tbl;
      expr = ExprTbl.to_map expr;
      expr_val = ExprTbl.to_map expr_val;
      escaping = !escapes;
      staticfails = IntTbl.to_map staticfails;

      global_val;
      external_val;
    }

  let bindings_map () =
    IdentTbl.fold IdentMap.add bindings IdentMap.empty

  let val_map () =
    ValTbl.fold ValMap.add val_tbl ValMap.empty

  let one_round () =
    IdentTbl.clear fun_call_count;
    aux (StackSet.singleton Escape) Param.tree;
    escapes := value_transitive_closure !escapes;
    handle_escaping_functions !escapes;
    val_map (), !escapes

  let rec fp n (prev_val,prev_escapes) =
    Printf.printf "\nround %i\nescapes: %i\n%!" n
      (ValSet.cardinal prev_escapes);
    (* Format.printf "esc:@[<1>@ %a@]@." ValSet.print prev_escapes; *)
    let curr_val, curr_escapes = one_round () in
    if ValMap.equal Values.equal prev_val curr_val &&
       ValSet.equal curr_escapes prev_escapes
    then begin
      Printf.printf "\nfix point reached %i\n\n%!" n;
      result ()
    end
    else fp (n+1) (curr_val,curr_escapes)

  let res =
    fp 1 (one_round ())

end


let call_graph tree =
  let module A = struct
    let tree = tree
    let call_per_round = 1
    let test_if = true
  end in
  let module CG = Call_graph(A) in
  CG.res

(* TODO: this is still false:
   if a function escape it should also be called with external parameters.
   returns values from function called from escape stack should escape. *)

let analyse tree = call_graph tree
