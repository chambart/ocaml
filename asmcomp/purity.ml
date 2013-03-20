open Lambda
open Flambda
open Flambdautils

let pure_prim = function
    Psetglobal _ | Psetfield _ | Psetfloatfield _ | Pduprecord _ |
    Pccall _ | Praise | Poffsetref _ | Pstringsetu | Pstringsets |
    Parraysetu _ | Parraysets _ | Pbigarrayset _ -> false
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
