
type constant_result = {
  not_constant_id : Flambda.IdentSet.t;
  not_constant_closure : Flambda.FunSet.t;
}

val not_constants : for_clambda:bool -> 'a Flambda.flambda -> constant_result
(** [not_constant ~for_clambda expr] *)

type alias_result =
  { constant_result : constant_result;
    constant_alias : Ident.t Flambda.IdentMap.t }

val alias : 'a Flambda.flambda -> alias_result
(** [alias expr] *)
