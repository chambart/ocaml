open Lambda
open Flambda

let pure_prim = function
  | Pidentity
  | Pignore -> true

  | Prevapply _
  | Pdirapply _ -> assert false

  | Pgetglobal _ -> true
  | Psetglobal _ -> false

  | Pmakeblock _ -> true

  | Psetfield _
  | Pfloatfield _
  | Psetfloatfield _ -> false

  (* pure on immutable blocks: find a way to distinguish *)
  | Pfield _ -> false
  | Pduprecord _ -> false
  | Pbittest -> false

  (* maybe this one could be considered pure: assigned only one time *)
  (* | Pgetglobalfield _ -> false *)

  | Plazyforce
  | Pccall _
  | Praise -> false

  | Psequand | Psequor | Pnot -> true
  | Pnegint | Paddint | Psubint | Pmulint | Pdivint | Pmodint
  | Pandint | Porint | Pxorint
  | Plslint | Plsrint | Pasrint
  | Pintcomp _ -> true

  | Poffsetint _ -> true
  | Poffsetref _ -> false

  | Pintoffloat | Pfloatofint
  | Pnegfloat | Pabsfloat
  | Paddfloat | Psubfloat | Pmulfloat | Pdivfloat
  | Pfloatcomp _ -> true

  | Pstringlength -> true (* string length can't change *)

  | Pstringrefu | Pstringsetu | Pstringrefs | Pstringsets -> false

  | Pmakearray _
  | Parraylength _ -> true

  | Parrayrefu _
  | Parraysetu _
  | Parrayrefs _
  | Parraysets _ -> false

  | Pisint -> true
  | Pisout -> true

  | Pbintofint _
  | Pintofbint _
  | Pcvtbint _
  | Pnegbint _
  | Paddbint _
  | Psubbint _
  | Pmulbint _
  | Pdivbint _
  | Pmodbint _
  | Pandbint _
  | Porbint _
  | Pxorbint _
  | Plslbint _
  | Plsrbint _
  | Pasrbint _
  | Pbintcomp _ -> true

  | Pbigarrayref _
  | Pbigarrayset _ -> false
  | Pbigarraydim _ -> false

  | Pstring_load_16 _
  | Pstring_load_32 _
  | Pstring_load_64 _
  | Pstring_set_16 _
  | Pstring_set_32 _
  | Pstring_set_64 _
  | Pbigstring_load_16 _
  | Pbigstring_load_32 _
  | Pbigstring_load_64 _
  | Pbigstring_set_16 _
  | Pbigstring_set_32 _
  | Pbigstring_set_64 _ -> false

  | Pctconst _ -> assert false

  | Pbswap16
  | Pbbswap _ -> true
  | Pphyscomp _ -> true

let effectless_prim = function
    Psetglobal _ | Psetfield _ | Psetfloatfield _ | Pduprecord _ |
    Pccall _ | Praise | Poffsetref _ | Pstringsetu | Pstringsets |
    Parraysetu _ | Parraysets _ | Pbigarrayset _

  | Pstringrefs | Parrayrefs _ | Pbigarrayref (false,_,_,_)

  | Pstring_load_16 false | Pstring_load_32 false | Pstring_load_64 false

  | Pbigstring_load_16 false | Pbigstring_load_32 false
  | Pbigstring_load_64 false

    -> false
  | _ -> true (* TODO: exhaustive list *)

type kind = Pure | Effectful

let effectful_node kind unpure_var = function
  | Fvar (id,_) -> IdentSet.mem id unpure_var
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
    begin match kind with
      | Pure -> not (pure_prim p)
      | Effectful -> not (effectless_prim p)
    end
  | Fassign(id, lam,data) -> true
  | Fstaticfail _ -> true
  | Funreachable _ -> true

type effectful =
  { effectful_expr : ExprSet.t;
    effectful_fun : IdentSet.t }

let effectful kind expr =

  let unpure_var =
    match kind with
    | Pure -> Flambdautils.all_assigned_var expr
    | Effectful ->
      (* reading unpure variable do no effect so we can skip this *)
      IdentSet.empty in

  let effectful_expr = ref ExprSet.empty in

  let effectful_fun = ref IdentSet.empty in
  let effectful_fun_dep = IdentTbl.create 10 in

  (* ouch... in fact also need dependency from function to expression
     and conversly ... *)

  let current_effectful = ref false in

  let do_expr iter env expr =
    let prev = !current_effectful in
    current_effectful := false;
    if effectful_node kind unpure_var expr
    then current_effectful := true;
    iter env expr;
    if !current_effectful
    then effectful_expr := ExprSet.add (data expr) !effectful_expr;
    current_effectful := !current_effectful || prev in

  let rec mark iter env = function
    | Fclosure(funct, _, _) as expr ->
      (* TODO funct ! *)
      IdentMap.iter (fun _ func -> mark iter env func.body) funct.funs;
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
