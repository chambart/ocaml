
type constant_result = {
  not_constant_id : Flambda.IdentSet.t;
  not_constant_closure : Flambda.FunSet.t;
}

val not_constants : 'a Flambda.flambda -> constant_result
