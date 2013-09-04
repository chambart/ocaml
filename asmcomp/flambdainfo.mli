open Flambda
open Values

type code_informations = {
  functions : ExprId.t ffunctions FunMap.t;
  global_size : int;
}

type analysis_result = {
  info : code_informations;
  (* associate each binding with its potential values *)
  bindings : ValSet.t IdentMap.t;

  (* associate each value identifier to its contents *)
  values : Values.values ValMap.t;

  (* set of variables potentialy returned by expressions *)
  expr : ValSet.t ExprMap.t;

  (* when an expression can produce a value it is here *)
  expr_val : ValId.t ExprMap.t;

  (* values created for identifiers *)
  var_val : ValId.t IdentMap.t;

  (* values potentially returned by static fails *)
  staticfails : ValSet.t list IntMap.t;

  (* the value representing the global module *)
  global_val : ValId.t;
  (* a special value representing values comming from outside the
     analysis *)
  external_val : ValId.t;
}

val analyse : ExprId.t flambda -> analysis_result

