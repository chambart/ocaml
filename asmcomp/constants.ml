
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
  val expr : t Flambda.flambda
end

module NotConstants(P:Param) = struct

  type dep =
    | Closure of FunId.t
    | Var of Ident.t

  (* Sets representing NC *)
  let variables = ref IdentSet.empty
  let closures = ref FunSet.empty

  (* if the table associates [v1;v2;...;vn] to v, it represents
     v in NC => v1 in NC /\ v2 in NC ... /\ vn in NC *)
  let id_dep_table : dep list IdentTbl.t = IdentTbl.create 100
  let fun_dep_table : dep list FunTbl.t = FunTbl.create 100

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
        FunTbl.replace fun_dep_table cl (curr :: t))
      curr

  (* adds 'curr in NC' *)
  let mark_curr curr =
    List.iter (function
      | Var id ->
        if not (IdentSet.mem id !variables)
        then variables := IdentSet.add id !variables
      | Closure cl ->
        if not (FunSet.mem cl !closures)
        then closures := FunSet.add cl !closures)
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
      mark_loop curr body

    | Fletrec(defs, body, _) ->
      List.iter (fun (id,def) -> mark_loop [Var id] def) defs;
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
      IdentMap.iter (fun _ ffunc ->
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

    | Fenv_field (f1, _,_) ->
      mark_curr curr;
      mark_loop [] f1

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
      with Not_found -> [] in
      List.iter (function
        | Var id as e ->
          if not (IdentSet.mem id !variables)
          then (variables := IdentSet.add id !variables;
            Queue.push e q)
        | Closure cl as e ->
          if not (FunSet.mem cl !closures)
          then (closures := FunSet.add cl !closures;
            Queue.push e q))
        deps
    done

  let res =
    mark_loop [] P.expr;
    propagate ();
    { not_constant_id = !variables;
      not_constant_closure = !closures; }
end

let not_constants (type a) (expr:a Flambda.flambda) =
  let module P = struct type t = a let expr = expr end in
  let module A = NotConstants(P) in
  A.res
