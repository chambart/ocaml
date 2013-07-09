
(* Detection of unused variables *)

open Flambda

type used_result = {
  used_id : IdentSet.t;
  used_expr : ExprSet.t;
  used_fun : IdentSet.t
}

module type Param = sig
  val expr : ExprId.t Flambda.flambda
  val effectful : Purity.effectful
end

module Used(P:Param) = struct

  type dep =
    (* TODO exceptions/static fail *)
    | Var of Ident.t
    | Eid of ExprId.t
    | Fun of Ident.t (* TODO later... *)

  let is_effectful lam = ExprSet.mem (data lam) P.effectful.Purity.effectful_expr

  (* Set representing reachable variables *)
  let reachable_var = ref IdentSet.empty
  let reachable_eid = ref ExprSet.empty
  let reachable_fun = ref IdentSet.empty

  (* if the table associates [v1;v2;...;vn] to v, it represents
     v in Reach => v1 in Reach /\ v2 in Reach ... /\ vn in Reach *)
  let var_dep_table : dep list IdentTbl.t = IdentTbl.create 100
  let eid_dep_table : dep list ExprTbl.t = ExprTbl.create 100
  let fun_dep_table : dep list IdentTbl.t = IdentTbl.create 100

  (* adds in the tables 'curr in Reach => dep in Reach' *)

  let add_depend_id dep curr =
    (* Printf.printf "add depend id %s -> _\n" (Ident.unique_name curr); *)
    let t = try IdentTbl.find var_dep_table curr
      with Not_found -> [] in
    IdentTbl.replace var_dep_table curr (dep :: t)

  let add_depend_eid dep = function
    | None -> ()
      (* print_endline "add depend eid none" *)
    | Some curr ->
      (* Printf.printf "add depend %a -> _\n" ExprId.output curr; *)
      let t = try ExprTbl.find eid_dep_table curr
        with Not_found -> [] in
      ExprTbl.replace eid_dep_table curr (dep :: t)

  let add_depend_fun dep curr =
    let t = try IdentTbl.find fun_dep_table curr
      with Not_found -> [] in
    IdentTbl.replace fun_dep_table curr (dep :: t)

  let depends v =
    try
      match v with
      | Var var -> IdentTbl.find var_dep_table var
      | Eid eid -> ExprTbl.find eid_dep_table eid
      | Fun fid -> IdentTbl.find fun_dep_table fid
    with Not_found -> []

  let is_in_reachable = function
    | Var var ->
      (* Printf.printf "is in reach %s\n%!" (Ident.unique_name var); *)
      IdentSet.mem var !reachable_var
    | Eid eid -> ExprSet.mem eid !reachable_eid
    | Fun fid -> IdentSet.mem fid !reachable_fun

  (* adds 'var in reachable' *)
  let reachable = function
    | Var var ->
      (* Printf.printf "reachable %s\n%!" (Ident.unique_name var); *)
      reachable_var := IdentSet.add var !reachable_var
    | Eid eid ->
      (* Printf.printf "reachable %a\n%!" ExprId.output eid; *)
      reachable_eid := ExprSet.add eid !reachable_eid
    | Fun fid ->
      reachable_fun := IdentSet.add fid !reachable_fun

  type env =
    { curr_fun : Ident.t option;
      curr_eid : ExprId.t option }

  let mark expr =
    let rec mark iter env expr =
      let eid = data expr in
      add_depend_eid (Eid eid) env.curr_eid;
      match expr with

      (* adds implications:
         variable reachable => definition reachable *)
      | Flet(str, id, lam, body, eid) ->
        add_depend_id (Eid (data lam)) id;
        (* the reachability of the current expression does not imply
           anything on the reachability of lam *)
        mark iter { env with curr_eid = None } lam;
        mark iter { env with curr_eid = Some eid } body

      | Fletrec(defs, body, eid) ->
        List.iter (fun (id,lam) ->
            add_depend_id (Eid (data lam)) id;
            mark iter { env with curr_eid = None } lam) defs;
        mark iter { env with curr_eid = Some eid } body

      | Foffset(expr, fun_id, eid) ->
        add_depend_eid (Fun fun_id) (Some eid);
        mark iter { env with curr_eid = Some eid } expr

      | Fenv_field({ env = expr; env_fun_id = fun_id; env_var = var }, eid) ->
        add_depend_eid (Var var) (Some eid);
        mark iter { env with curr_eid = Some eid } expr

      | Fclosure(funct, fv, eid) ->
        IdentMap.iter (fun fun_id func ->
            let body_eid = data func.body in
            add_depend_fun (Eid body_eid) fun_id;
            (* inside the function recursive call are done with the
               id, without going throug an offset: the function is
               used if its variable is used *)
            add_depend_id (Fun fun_id) fun_id;
            let env = { curr_fun = Some fun_id; curr_eid = Some body_eid } in
            mark iter env func.body)
          funct.funs;
        IdentMap.iter (fun id expr ->
            let eid = data expr in
            add_depend_id (Eid eid) id;
            (* mark iter ou pas ici ? *)
            mark iter { env with curr_eid = Some eid } expr)
          fv

(*
  TODO:
    for and while does not depend on the last expression of their body
*)

      | Fsequence (e1, e2, eid) ->
        mark iter { env with curr_eid = None } e1;
        mark iter { env with curr_eid = Some eid } e2

      | Fvar (id,eid) ->
        add_depend_eid (Var id) (Some eid)

      | expr ->
        begin if is_effectful expr
          then match env.curr_fun with
            | None -> reachable (Eid eid)
            | Some curr_fun -> add_depend_fun (Eid eid) curr_fun end;
        iter { env with curr_eid = Some eid } expr
    in
    let init_env = { curr_fun = None; curr_eid = None } in
    Flambdautils.iter2_flambda mark init_env expr

  (* Second loop: propagates implications *)
  let propagate () =
    (* Set of variables/closures added to Reach but not their dependencies *)
    let q = Queue.create () in
    IdentSet.iter (fun v -> Queue.push (Var v) q) !reachable_var;
    ExprSet.iter (fun v -> Queue.push (Eid v) q) !reachable_eid;
    IdentSet.iter (fun v -> Queue.push (Fun v) q) !reachable_fun;
    while not (Queue.is_empty q) do
      List.iter (fun dep ->
          if not (is_in_reachable dep)
          then (reachable dep;
                Queue.push dep q))
        (depends (Queue.pop q))
    done

  let res =
    mark P.expr;
    propagate ();
    { used_id = !reachable_var;
      used_expr = !reachable_eid;
      used_fun = !reachable_fun }

end

let used effectful (expr:ExprId.t Flambda.flambda) =
  let module P = struct
    let expr = expr
    let effectful = effectful
  end in
  let module A = Used(P) in
  A.res


let dead_code_elimination effectful used expr =
  (* TODO: elimination of unused functions *)
  let can_remove id lam =
    not (IdentSet.mem id used.used_id) &&
    not (ExprSet.mem (data lam) effectful.Purity.effectful_expr) in
  let can_remove_expr expr =
    let eid = data expr in
    not (ExprSet.mem eid used.used_expr) &&
    not (ExprSet.mem eid effectful.Purity.effectful_expr) in
  let mapper expr = match expr with
    | Fvar _ -> expr
    | Fconst _ -> expr
    | Flet(str, id, lam, body, eid) ->
      if can_remove_expr lam
      then body
      else if not (IdentSet.mem id used.used_id)
      then Fsequence(lam, body, eid)
      else expr
    | Fletrec(defs, body, eid) ->
      begin match List.filter (fun (id,lam) -> not (can_remove id lam)) defs with
        | [] -> body
        | defs -> Fletrec(defs, body, eid)
      end
    | Fsequence(lam1, lam2, eid) ->
      if can_remove_expr lam1
      then lam2
      else expr
    | Ffor(id, lo, hi, dir, body, eid) ->
      if can_remove_expr expr
      then Fconst (Fconst_pointer 0, eid)
      else expr
    | Fassign(id, lam, eid) ->
      if not (IdentSet.mem id used.used_id)
      then Fconst (Fconst_pointer 0, eid)
      else expr

    | Fclosure(funct, fv, eid) ->
      let funs = IdentMap.filter
          (fun id _ -> IdentSet.mem id used.used_fun) funct.funs in
      let funs = IdentMap.map
          (fun func ->
             { func with
               closure_params = IdentSet.filter
                   (fun id -> IdentSet.mem id used.used_id)
                   func.closure_params }) funs in
      let fv = IdentMap.filter
          (fun id _ -> IdentSet.mem id used.used_id) fv in
      Fclosure({ funct with funs }, fv, eid)

    (* | Foffset(lam,id,data) -> data *)
    (* | Fenv_field(_,data) -> data *)
    (* | Fapply(funct, args, _, _,data) -> data *)
    (* | Fswitch(arg, sw,data) -> data *)
    (* | Fsend(kind, met, obj, args, _,data) -> data *)
    (* | Fprim(_, args, _,data) -> data *)
    (* | Fstaticfail (i, args,data) -> data *)
    (* | Fcatch (i, vars, body, handler,data) -> data *)
    (* | Ftrywith(body, id, handler,data) -> data *)
    (* | Fifthenelse(arg, ifso, ifnot,data) -> data *)
    (* | Fwhile(cond, body,data) -> data *)
    | _ ->
      if can_remove_expr expr
      then Fconst (Fconst_pointer 42, data expr)
      else expr
  in
  Flambdautils.map mapper expr

let eliminate_dead_code tree =
  let effectful = Purity.effectful Purity.Effectful tree in
  let used = used effectful tree in
  dead_code_elimination effectful used tree
