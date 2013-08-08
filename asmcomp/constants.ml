
(* Detection of not constant values *)

(* This cannot be done in a single recursive pass due to expressions like:

  let ... =
    let v = ... in

    let rec f1 x =
      let f2 y =
        f1 rec_list
      in
      f2 v
    and rec_list = f1 :: rec_list

    ...

  f1, f2 and rec_list are constants iff v is a constant.

  To handle this we implement it as 2 loops populating a 'not constant'
  set NC:

   - the first one collects informations on the expressions to add dependencies
     between variables and mark values directly known as not constant:

      f1 in NC => rec_list in NC
      f2 in NC => f1 in NC
      rec_list in NC => f2 in NC
      v in NC => f1 in NC

     and if for instance if v is:
      let v = if ... then 1 else 2 in
     it adds

      v in NC

   - the second propagates the implications
*)

open Asttypes
open Lambda
open Flambda

type constant_result = {
  not_constant_id : IdentSet.t;
  not_constant_closure : FunSet.t;
}

module type Param = sig
  type t
  val global_var : Ident.t IdentTbl.t
  val expr : t Flambda.flambda
  val for_clambda : bool
end

module NotConstants(P:Param) = struct

  let global_var = P.global_var
  let for_clambda = P.for_clambda

  let global_id id =
    try
      let gid = IdentTbl.find global_var id in
      if gid.Ident.name = Compilenv.current_unit_name ()
      then Some gid
      else None
    with Not_found -> None

  type dep =
    | Closure of FunId.t
    | Var of Ident.t
    | Global of int (* position of the global *)

  (* Sets representing NC *)
  let variables = ref IdentSet.empty
  let closures = ref FunSet.empty
  let globals = ref IntSet.empty

  (* if the table associates [v1;v2;...;vn] to v, it represents
     v in NC => v1 in NC /\ v2 in NC ... /\ vn in NC *)
  let id_dep_table : dep list IdentTbl.t = IdentTbl.create 100
  let fun_dep_table : dep list FunTbl.t = FunTbl.create 100
  let glob_dep_table : dep list IntTbl.t = IntTbl.create 100

  (* adds in the tables 'dep in NC => curr in NC' *)
  let add_depend curr dep =
    List.iter (fun curr ->
      match dep with
      | Var id ->
        let t = try IdentTbl.find id_dep_table id
        with Not_found -> [] in
        IdentTbl.replace id_dep_table id (curr :: t)
      | Closure cl ->
        let t = try FunTbl.find fun_dep_table cl
        with Not_found -> [] in
        FunTbl.replace fun_dep_table cl (curr :: t)
      | Global i ->
        let t = try IntTbl.find glob_dep_table i
        with Not_found -> [] in
        IntTbl.replace glob_dep_table i (curr :: t))
      curr

  (* adds 'curr in NC' *)
  let mark_curr curr =
    List.iter (function
      | Var id ->
        if not (IdentSet.mem id !variables)
        then variables := IdentSet.add id !variables
      | Closure cl ->
        if not (FunSet.mem cl !closures)
        then closures := FunSet.add cl !closures
      | Global i ->
        if not (IntSet.mem i !globals)
        then globals := IntSet.add i !globals)
      curr

  (* First loop: iterates on the tree to mark dependencies.

     curr is the variables or closures to wich we add constraints like
     '... in NC => curr in NC' or 'curr in NC'

     It can be empty when no constraint can be added like in the toplevel
     expression or in the body of a function.
  *)
  let rec mark_loop (curr:dep list) = function

    | Flet(str, id, lam, body, _) ->
      (* No need to match on str: if the variable is assigned it will
         be marked directly as not constant, but if it is not
         assigned, it could be considered constant *)
      mark_loop [Var id] lam;
      (* adds 'id in NC => curr in NC'
         This is not really necessary, but compiling this correctly is
         trickier than eliminating that earlier. *)
      add_depend curr (Var id);
      mark_loop curr body

    | Fletrec(defs, body, _) ->
      List.iter (fun (id,def) ->
          mark_loop [Var id] def;
          (* adds 'id in NC => curr in NC' same remark as let case *)
          add_depend curr (Var id)) defs;
      mark_loop curr body

    | Fvar (id,_) ->
      (* adds 'id in NC => curr in NC' *)
      add_depend curr (Var id)

    | Fclosure (funcs,fv,_) ->
      (* adds 'funcs in NC => curr in NC' *)
      add_depend curr (Closure funcs.ident);
      (* a closure is constant if its free variables are constants. *)
      IdentMap.iter (fun inner_id lam ->
        mark_loop [Closure funcs.ident; Var inner_id] lam) fv;
      IdentMap.iter (fun fun_id ffunc ->
        (* for each function f in a closure c 'c in NC => f' *)
        add_depend [Var fun_id] (Closure funcs.ident);
        (* function parameters are in NC *)
        List.iter (fun id -> mark_curr [Var id]) ffunc.params;
        mark_loop [] ffunc.body) funcs.funs

    | Fconst (cst,_) -> ()

    (* Constant constructors: those expressions are constant if all their parameters are:
       - makeblock is compiled to a constant block
       - offset is compiled to a pointer inside a constant closure.
         See Cmmgen for the details *)

    | Fprim(Pmakeblock(tag, Immutable), args, dbg, _) ->
      List.iter (mark_loop curr) args

    | Foffset (f1, _,_) ->
      mark_loop curr f1

    | Fenv_field ({env = f1},_) ->
      if for_clambda
      then mark_curr curr;
      mark_loop curr f1

    (* predefined exceptions are constants *)
    | Fprim(Pgetglobal id, [], _, _) ->
      if not (Ident.is_predef_exn id)
      then mark_curr curr

    | Fprim(Pfield i, [Fvar(id,_)], _, _) ->
      (* This case Requires an entry in ANF if for_clambda is false
         Notice that if for_clambda is true then it does the same
         thing that the default Fprim case *)
      if for_clambda
      then mark_curr curr
      else
        begin match global_id id with
          | None ->
            (* This is correct only if there is a rebind phase after !
               Clambdagen cannot handle this *)
            (* if some inlining produced some unreachable code like
               {[let a = 0 in
                 if a
                 then a.(0)
                 else ... ]}
               then a.(0) cannot be compiled. There must be a specialisation
               phase after that eliminating the then branch and a dead code
               elimination eliminating potential reference to a.(0) *)
            add_depend curr (Var id)
          | Some gid ->
            add_depend curr (Global i)
        end

    (* | Fprim(Pgetglobalfield(id,i), [], _, _) -> *)
    (*   (\* adds 'id in NC => curr in NC' *\) *)
    (*   if id.Ident.name = Compilenv.current_unit_name () *)
    (*   then add_depend curr (Global i) *)
    (*   else mark_curr curr *)

    | Fprim(Psetfield (i,_), [Fvar(obj_id,_);Fvar(value_id,_)], _, _) ->
      (* This case Requires an entry in ANF *)
      (* reference between global constants should have been eliminated
         when transforming to clambda
         TODO: check that this is effectively the case *)
      add_depend curr (Var obj_id);
      add_depend curr (Var value_id);
      mark_curr curr;
      begin match global_id obj_id with
        | None -> ()
        | Some gid ->
          add_depend [Global i] (Var value_id)
      end

    (* Not constant cases: we mark directly 'curr in NC' and mark
       bound variables as in NC also *)

    | Fassign (id, f1, _) ->
      (* the assigned is also not constant *)
      mark_curr [Var id];
      mark_curr curr;
      mark_loop [] f1

    | Ftrywith (f1,id,f2,_) ->
      mark_curr [Var id];
      mark_curr curr;
      mark_loop [] f1;
      mark_loop [] f2

    | Fcatch (_,ids,f1,f2,_) ->
      List.iter (fun id -> mark_curr [Var id]) ids;
      mark_curr curr;
      mark_loop [] f1;
      mark_loop [] f2

    | Ffor (id,f1,f2,_,f3,_) ->
      mark_curr [Var id];
      mark_curr curr;
      mark_loop [] f1;
      mark_loop [] f2;
      mark_loop [] f3

    | Fsequence (f1,f2,_)
    | Fwhile (f1,f2,_) ->
      mark_curr curr;
      mark_loop [] f1;
      mark_loop [] f2

    | Fifthenelse (f1,f2,f3,_) ->
      mark_curr curr;
      mark_loop [] f1;
      mark_loop [] f2;
      mark_loop [] f3

    | Fstaticfail (_,l,_)
    | Fprim (_,l,_,_) ->
      mark_curr curr;
      List.iter (mark_loop []) l

    | Fapply (f1,fl,_,_,_) ->
      mark_curr curr;
      mark_loop [] f1;
      List.iter (mark_loop []) fl

    | Fswitch (arg,sw,_) ->
      mark_curr curr;
      List.iter (fun (_,l) -> mark_loop [] l) sw.fs_consts;
      List.iter (fun (_,l) -> mark_loop [] l) sw.fs_blocks;
      Misc.may (fun l -> mark_loop [] l) sw.fs_failaction

    | Fsend (_,f1,f2,fl,_,_) ->
      mark_curr curr;
      mark_loop [] f1;
      mark_loop [] f2;
      List.iter (mark_loop []) fl

    | Funreachable _ ->
      mark_curr curr

  (* Second loop: propagates implications *)
  let propagate () =
    (* Set of variables/closures added to NC but not their dependencies *)
    let q = Queue.create () in
    IdentSet.iter (fun v -> Queue.push (Var v) q) !variables;
    FunSet.iter (fun v -> Queue.push (Closure v) q) !closures;
    while not (Queue.is_empty q) do
      let deps = try match Queue.take q with
        | Var e -> IdentTbl.find id_dep_table e
        | Closure cl -> FunTbl.find fun_dep_table cl
        | Global i -> IntTbl.find glob_dep_table i
      with Not_found -> [] in
      List.iter (function
        | Var id as e ->
          if not (IdentSet.mem id !variables)
          then (variables := IdentSet.add id !variables;
            Queue.push e q)
        | Closure cl as e ->
          if not (FunSet.mem cl !closures)
          then (closures := FunSet.add cl !closures;
            Queue.push e q)
        | Global i as e ->
          if not (IntSet.mem i !globals)
          then (globals := IntSet.add i !globals;
            Queue.push e q))
        deps
    done

  let res =
    mark_loop [] P.expr;
    propagate ();
    { not_constant_id = !variables;
      not_constant_closure = !closures; }
end

let not_constants (type a) ~for_clambda (expr:a Flambda.flambda) =
  let module P = struct
    type t = a
    let expr = expr
    let for_clambda = for_clambda
    let global_var = Flambdautils.global_var expr
  end in
  let module A = NotConstants(P) in
  A.res


module type AliasParam = sig
  type t
  val global_var : Ident.t IdentTbl.t
  val expr : t Flambda.flambda
  val const_result : constant_result
end

module ConstantAlias(P:AliasParam) = struct

  let is_constant id = not (IdentSet.mem id P.const_result.not_constant_id)
  let global_var = P.global_var

  let global_id id =
    try
      let gid = IdentTbl.find global_var id in
      if gid.Ident.name = Compilenv.current_unit_name ()
      then Some gid
      else None
    with Not_found -> None

  type abstract_values =
    | Vvar of Ident.t
    | Vblock of abstract_values array
    | Vclosure of (Ident.t option) * (abstract_values IdentMap.t)
    | Vfield of int * Ident.t
    | Vglobal of int
    | Voffset of abstract_offset
    | Venv_field of abstract_env_field
    | Vpredef_exn of Ident.t
    | Vbase_const
    | Vunreachable

  and abstract_offset =
    { offset_field : Ident.t;
      offset_var : offset_var }

  and offset_var =
    | Direct of abstract_values
    (* value inside the closure. Usefull if there is no offset value *)
    | Indirect of Ident.t

  and abstract_env_field =
    { env_offset : Ident.t; env_field_var : Ident.t; env_fun : Ident.t }

  (* Table representing potential aliases *)
  let abstr_table : abstract_values IdentTbl.t = IdentTbl.create 100
  let abstr_globals : Ident.t IntTbl.t = IntTbl.create 100

  let add_abstr id = function
    | None -> ()
    | Some v -> IdentTbl.add abstr_table id v

  let rec mark_alias v =
    ignore (mark_alias_result v:abstract_values option)

  and mark_alias_result = function
    | Flet(str, id, lam, body, _) ->
      let lam_res = mark_alias_result lam in
      add_abstr id lam_res;
      mark_alias_result body

    | Fletrec(defs, body, _) ->
      List.iter (fun (id,def) ->
          let lam_res = mark_alias_result def in
          add_abstr id lam_res) defs;
      mark_alias_result body

    | Fvar (id,_) ->
      Some (Vvar id)

    | Fclosure (funcs,fv,_) ->
      let closure = IdentMap.mapi (fun inner_id lam -> mark_alias_result lam) fv in
      IdentMap.iter add_abstr closure;
      (* build closure approximation *)
      let result =
        try Some (Vclosure (None, IdentMap.fold (fun inner_id v acc -> match v with
            | None -> raise Exit (* Not a constant: no approximation *)
            | Some v -> IdentMap.add inner_id v acc) closure IdentMap.empty))
        with Exit -> None in
      IdentMap.iter (fun fun_id ffunc ->
          Misc.may (fun result -> add_abstr fun_id
                       (Some (Voffset { offset_field = fun_id;
                                        offset_var = Direct result })))
            result;
          mark_alias ffunc.body) funcs.funs;
      result

    | Fconst (cst,_) ->
      Some Vbase_const

    | Fprim(Pmakeblock(tag, Immutable), args, dbg, _) ->
      let r = List.map mark_alias_result args in
      Misc.may_map (fun x -> Vblock x)
        (Misc.lift_option_array (Array.of_list r))

    | Foffset (f1, offset_field,_) ->
      mark_alias f1;
      begin match f1 with
        | Fvar(offset_var,_) ->
          Some (Voffset { offset_field; offset_var = Indirect offset_var })
        | _ -> assert false (* assumes ANF *)
      end

    | Fenv_field ({env = f1; env_var; env_fun_id = env_fun},_) ->
      mark_alias f1;
      begin match f1 with
        | Fvar(env_field_var,_) ->
          Some (Venv_field { env_offset=env_var; env_field_var; env_fun })
        | _ -> assert false (* assumes ANF *)
      end

    (* predefined exceptions are constants *)
    | Fprim(Pgetglobal id, [], _, _) ->
      if Ident.is_predef_exn id
      then Some (Vpredef_exn id)
      else None

    | Fprim(Pfield i, [Fvar(id,_)], _, _) ->
      (* This case Requires an entry in ANF *)
      begin match global_id id with
        | None -> Some (Vfield (i,id))
        | Some gid -> Some (Vglobal i)
      end

    | Fprim(Psetfield (i,_), [Fvar(obj_id,_);Fvar(value_id,_)], _, _) ->
      (* This case Requires an entry in ANF *)
      begin match global_id obj_id with
        | None -> ()
        | Some gid -> IntTbl.add abstr_globals i value_id
      end;
      None

    | Fassign (id, f1, _) ->
      mark_alias f1;
      None

    | Ftrywith (f1,id,f2,_) ->
      mark_alias f1;
      mark_alias f2;
      None

    | Fcatch (_,ids,f1,f2,_) ->
      mark_alias f1;
      mark_alias f2;
      None

    | Ffor (id,f1,f2,_,f3,_) ->
      mark_alias f1;
      mark_alias f2;
      mark_alias f3;
      None

    | Fsequence (f1,f2,_) -> assert false (* assumes ANF *)

    | Fwhile (f1,f2,_) ->
      mark_alias f1;
      mark_alias f2;
      None

    | Fifthenelse (f1,f2,f3,_) ->
      mark_alias f1;
      mark_alias f2;
      mark_alias f3;
      None

    | Fstaticfail (_,l,_)
    | Fprim (_,l,_,_) ->
      List.iter mark_alias l;
      None

    | Fapply (f1,fl,_,_,_) ->
      mark_alias f1;
      List.iter mark_alias fl;
      None

    | Fswitch (arg,sw,_) ->
      List.iter (fun (_,l) -> mark_alias l) sw.fs_consts;
      List.iter (fun (_,l) -> mark_alias l) sw.fs_blocks;
      Misc.may (fun l -> mark_alias l) sw.fs_failaction;
      None

    | Fsend (_,f1,f2,fl,_,_) ->
      mark_alias f1;
      mark_alias f2;
      List.iter mark_alias fl;
      None

    | Funreachable _ ->
      Some Vunreachable

  (* third loop: propagate alias informations of constants *)
  let propagate_alias () =

    let result : (Ident.t option * abstract_values) IdentTbl.t =
      IdentTbl.create 100 in

    let rec resolve id : (Ident.t option * abstract_values) =
      (* Printf.printf "%s\n%!" (Ident.unique_name id); *)
      try IdentTbl.find result id with Not_found ->
        assert (is_constant id); (* only consider constants *)
        assert (IdentTbl.mem abstr_table id);
        let res = resolve_abs (IdentTbl.find abstr_table id) in
        IdentTbl.add result id res;
        res

    and resolve_abs : _ -> (Ident.t option * abstract_values) = function
      | (Vblock _ | Vclosure _ | Vpredef_exn _ |
         Vbase_const) as v ->
        None, v
      | Vvar id ->
        begin match resolve id with
          | None, res -> Some id, res
          | res -> res
        end
      | Vfield (i,id) ->
        (match resolve id with
         | _, Vblock a ->
           if Array.length a <= i
           then None, Vunreachable
           else resolve_abs a.(i)
         | _ ->
           (* This can happen with impossible branch not yet
              eliminated by specialisation *)
           None, Vunreachable)
      | Voffset { offset_field; offset_var } ->
        let res = match offset_var with
          | Direct res -> None, res
          | Indirect offset_var -> resolve offset_var in
        (match res with
         | _, Vclosure (None, map) ->
           None, Vclosure (Some offset_field, map)
         | _ -> assert false)
      | Venv_field { env_offset; env_field_var; env_fun } ->
        (match resolve env_field_var with
         | _, Vclosure (Some fun_id, map) ->
           assert(Ident.same fun_id env_fun);
           assert(IdentMap.mem env_offset map);
           resolve_abs (IdentMap.find env_offset map)
         | _ ->
           assert false)
      | Vglobal i ->
        assert(IntTbl.mem abstr_globals i);
        let var = IntTbl.find abstr_globals i in
        resolve var
      | Vunreachable ->
        None, Vunreachable
    in

    let resolve id _ =
      if is_constant id (* only call on constants *)
      then ignore (resolve id:_ * _) in

    IdentTbl.iter resolve abstr_table;

    IdentTbl.fold (fun key abs map -> match abs with
        | Some aid, _ -> IdentMap.add key aid map
        | _ -> map) result IdentMap.empty

  let res =
    mark_alias P.expr;
    propagate_alias ()

end

type alias_result =
  { constant_result : constant_result;
    constant_alias : Ident.t Flambda.IdentMap.t }

let alias (type a) (expr:a Flambda.flambda) =
  let module P = struct
    type t = a
    let expr = expr
    let global_var = Flambdautils.global_var expr
    let const_result = not_constants ~for_clambda:false expr
  end in
  let module A = ConstantAlias(P) in
  { constant_result = P.const_result;
    constant_alias = A.res }
