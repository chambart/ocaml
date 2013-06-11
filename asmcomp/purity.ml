open Lambda
open Flambda
open Flambdautils

let pure_prim = function
    Psetglobal _ | Psetfield _ | Psetfloatfield _ | Pduprecord _ |
    Pccall _ | Praise | Poffsetref _ | Pstringsetu | Pstringsets |
    Parraysetu _ | Parraysets _ | Pbigarrayset _

  | Pstringrefs | Parrayrefs _ | Pbigarrayref (false,_,_,_)

  | Pstring_load_16 false | Pstring_load_32 false | Pstring_load_64 false

  | Pbigstring_load_16 false | Pbigstring_load_32 false
  | Pbigstring_load_64 false

    -> false
  | _ -> true (* TODO: exhaustive list *)

let unpure_expressions t : ExprSet.t =
  let unpure_expr = ref ExprSet.empty in

  let mark e =
    unpure_expr := ExprSet.add e !unpure_expr in

  let module Unpure = struct
    type annot = ExprId.t
    type data = pure
    let merge l data =
      if List.for_all (fun v -> v = Pure) l
      then Pure
      else
        (mark data;
         Unpure)
  end in
  let module UnpureInst = struct
    include Merger(Unpure)

    let apply ~func ~args ~direct ~dbg data =
      (* mark pure functions *)
      mark data;
      Data Unpure

    let prim p args _ data =
      let d =
        if pure_prim p &&
           List.for_all (fun lam -> Flambda.data lam = Pure) args
        then Pure
        else (mark data; Unpure)
      in
      Data d
    let staticfail _ _ data =
      mark data;
      Data Unpure

    let assign _ _ data =
      mark data;
      Data Unpure
    let send _ _ _ _ _ data =
      mark data;
      Data Unpure

    let closure ffunc fv data =
      let unpure = List.map (fun (_,v) -> Flambda.data v) (IdentMap.bindings fv) in
      Data (Unpure.merge unpure data)

  end in
  let module M = Fold(UnpureInst) in
  let _ = M.fold t in
  !unpure_expr

let effectful_node = function
  | Fvar _
  | Fconst _
  | Flet _
  | Fletrec _
  | Fclosure _
  | Foffset _
  | Fenv_field _
  | Fswitch _
  | Fcatch _
  | Ftrywith _
  | Fifthenelse _
  | Fsequence _
  | Fwhile _
  | Ffor _ -> false

  | Fapply(funct, _, _, _,_) ->
    true
  | Fsend(kind, met, obj, _, _,_) ->
    true
  | Fprim(p, _, _,_) ->
    not (pure_prim p)

  | Fassign(id, lam,data) -> true
  | Fstaticfail _ -> true

type effectful =
  { effectful_expr : ExprSet.t;
    effectful_fun : IdentSet.t }

let effectful expr =
  let effectful_expr = ref ExprSet.empty in

  let effectful_fun = ref IdentSet.empty in
  let effectful_fun_dep = IdentTbl.create 10 in

  (* ouch... in fact also need dependency from function to expression
     and conversly ... *)

  let current_effectful = ref false in

  let do_expr iter env expr =
    let prev = !current_effectful in
    current_effectful := false;
    if effectful_node expr
    then current_effectful := true;
    iter env expr;
    if !current_effectful
    then effectful_expr := ExprSet.add (data expr) !effectful_expr;
    current_effectful := !current_effectful || prev in

  let mark iter env = function
    | Fclosure(funct, _, _) as expr ->
      (* TODO funct ! *)
      IdentMap.iter (fun _ func -> iter env func.body) funct.funs;
      do_expr iter env expr (* mark for fv *)

    | Fapply _ as expr ->
      (* "TODO" ...  attention: pour quand je vais le faire pour de
         vrai: do_expr utilise effectful_node qui considere toujours
         apply comme effectful *)
      do_expr iter env expr

    | expr -> do_expr iter env expr
  in

  let depends fid = try IdentTbl.find effectful_fun_dep fid with
      Not_found -> [] in

  let propagate () =
    let q = Queue.create () in
    IdentSet.iter (fun v -> Queue.push v q) !effectful_fun;
    while not (Queue.is_empty q) do
      List.iter (fun dep ->
          if not (IdentSet.mem dep !effectful_fun)
          then (effectful_fun := IdentSet.add dep !effectful_fun;
                Queue.push dep q))
        (depends (Queue.pop q))
    done
  in

  Flambdautils.iter2_flambda mark () expr;
  propagate ();

  { effectful_expr = !effectful_expr;
    effectful_fun = !effectful_fun }
