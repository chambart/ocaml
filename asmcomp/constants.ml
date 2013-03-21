
(* Detection of constant values *)

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

  let variables = ref IdentSet.empty
  let closures = ref FunSet.empty

  let id_dep_table : dep list IdentTbl.t = IdentTbl.create 100
  let fun_dep_table : dep list FunTbl.t = FunTbl.create 100

  let add_dep_id id v =
    let t = try IdentTbl.find id_dep_table id
    with Not_found -> [] in
    IdentTbl.replace id_dep_table id (v :: t)

  let add_dep_closure cl v =
    let t = try FunTbl.find fun_dep_table cl
    with Not_found -> [] in
    FunTbl.replace fun_dep_table cl (v :: t)

  let add_depend curr dep =
    match curr with
    | None -> ()
    | Some curr ->
      match dep with
      | Var id -> add_dep_id id curr
      | Closure cl -> add_dep_closure cl curr

  let mark_curr curr =
    match curr with
    | None -> ()
    | Some (Var id) ->
      if not (IdentSet.mem id !variables)
      then variables := IdentSet.add id !variables
    | Some (Closure cl) ->
      if not (FunSet.mem cl !closures)
      then closures := FunSet.add cl !closures

  let rec mark_loop (curr:dep option) = function
    | Fvar (id,_) ->
      add_depend curr (Var id)
    | Flet(str, id, lam, body, _) ->
      (* No need to match on str: if the variable is assign it will be marked,
         but if it is not assigned, it could be considered *)
      mark_loop (Some (Var id)) lam;
      mark_loop curr body
    | Fletrec(defs, body, _) ->
      List.iter (fun (id,def) -> mark_loop (Some (Var id)) def) defs;
      mark_loop curr body
    | Fconst (cst,_) -> ()

      (* Those are constants if all their arguments are *)
    | Fprim(Pmakeblock(tag, Immutable), args, dbg, _) ->
      List.iter (mark_loop curr) args
    | Foffset (f1, _,_)
    | Fenv_field (f1, _,_) ->
      (* in fact f1 is a constant only if it is closed, so there is no
         way we can acces to its closure like this... ???
         At least I wish it is effectively the case... *)
      mark_loop curr f1

    | Fclosure (funcs,fv,_) ->
      (* a function is considered to be constant if all the free variables
         are aliased to constants. *)
      let aux _ = function
        | Fvar (id,_) ->
          add_depend (Some (Closure funcs.ident)) (Var id)
        | lam ->
          mark_curr curr;
          mark_curr (Some (Closure funcs.ident));
          mark_loop None lam in
      IdentMap.iter aux fv;
      IdentMap.iter (fun _ ffunc -> mark_loop None ffunc.body) funcs.funs

        (* Not constants *)
    | Fassign (id, f1, _) ->
      mark_curr (Some (Var id));
      mark_curr curr;
      mark_loop None f1

    | Ftrywith (f1,_,f2,_)
    | Fsequence (f1,f2,_)
    | Fwhile (f1,f2,_)
    | Fcatch (_,_,f1,f2,_) ->
      mark_curr curr;
      mark_loop None f1;
      mark_loop None f2

    | Ffor (_,f1,f2,_,f3,_)
    | Fifthenelse (f1,f2,f3,_) ->
      mark_curr curr;
      mark_loop None f1;
      mark_loop None f2;
      mark_loop None f3

    | Fstaticfail (_,l,_)
    | Fprim (_,l,_,_) ->
      mark_curr curr;
      List.iter (mark_loop None) l

    | Fapply (f1,fl,_,_,_) ->
      mark_curr curr;
      mark_loop None f1;
      List.iter (mark_loop None) fl

    | Fswitch (arg,sw,_) ->
      mark_curr curr;
      List.iter (fun (_,l) -> mark_loop None l) sw.fs_consts;
      List.iter (fun (_,l) -> mark_loop None l) sw.fs_blocks

    | Fsend (_,f1,f2,fl,_,_) ->
      mark_curr curr;
      mark_loop None f1;
      mark_loop None f2;
      List.iter (mark_loop None) fl

  let propagate () =
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
    mark_loop None P.expr;
    propagate ();
    { not_constant_id = !variables;
      not_constant_closure = !closures; }
end

let not_constants (type a) (expr:a Flambda.flambda) =
  let module P = struct type t = a let expr = expr end in
  let module A = NotConstants(P) in
  A.res
