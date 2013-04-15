

(* il faut:

   type call site: expression * nombre de paramêtres appliqués (c'est comme n'autoriser les application que d'un paramètre)

   (call site * fonction) list * expression -> valid

   valid -> term * valid list (qui sert à le creer)

   contrainte : valid

   valid -> (contrainte * valid) list (valeurs qui dépendent de celle là sous quelle contrainte)

   fonction -> call context


*)

open Asttypes
(* open Misc *)
open Flambda

module ValId : Id = Id(Flambda.Empty)
module ValMap = Flambda.ExtMap(ValId)
module ValSet = Flambda.ExtSet(ValId)
module ValTbl = Flambda.ExtHashtbl(ValId)

type v = ValId.t
type funid = Ident.t

type term =
  | Closure of IdentSet.t * v IdentMap.t
  | Offset of Ident.t * v
  | Field of int * v
  | Makeblock_module of v list
  | Makeblock of int * mutable_flag * v list
  | Cmpint of Lambda.comparison * v * v
  | Addint of v * v
  | Mulint of v * v
  | Const of Flambda.const
  | FunParam of ValSet.t ref
  | FunReturn
  | Union of v list
  | Apply of v * v list

  (* | MutableVar of ValSet.t ref (* ssa phi *) *)

type constraint' =
  | Funvalue of funid * int (* function, number of preapplied parameters *)
  | Bool of bool
  | SwitchCst of int
  | SwitchTag of int
  | Exists (* for exceptions, we only need to check that a value is not empty *)

type constraints = (v * constraint') list

type call_site = ExprId.t * int
(* point d'appel, et numéro du paramètre *)

type location = (call_site * funid) list * ExprId.t
(* the call stack and the position in the current function *)

module Location = struct
  type t = location
  let compare = compare
end
module LocMap = Map.Make(Location)

type call_info = {
  parameters : v list;
  parameter_sets : ValSet.t ref list;
  return : v;
}

type graph =
  { mutable locations : v LocMap.t;
    mutable v_location : location ValMap.t;
    mutable term : term ValMap.t;
    flow : ValSet.t ValTbl.t;
    (* which values depends on that one *)
    mutable block : constraints ValMap.t;
    param_dependency : ValSet.t ValTbl.t;
    mutable call_info : call_info IdentMap.t;
    (* TODO: allow specialisation per call site *)
    mutable values : ValSet.t;
    virtual_flow : ValSet.t ValTbl.t;
    (* which values depends on that one *)
    virtual_union : ValSet.t ValTbl.t;
    functions : ExprId.t ffunction IdentTbl.t; }

let empty_graph () =
  { locations = LocMap.empty;
    v_location = ValMap.empty;
    term = ValMap.empty;
    flow = ValTbl.create 10;
    block = ValMap.empty;
    param_dependency = ValTbl.create 10;
    call_info = IdentMap.empty;
    values = ValSet.empty;
    virtual_flow = ValTbl.create 10;
    virtual_union = ValTbl.create 10;
    functions = IdentTbl.create 10 }

let get_flow graph value =
  try ValTbl.find graph.flow value with Not_found -> ValSet.empty

let get_virtual_flow graph value =
  try ValTbl.find graph.virtual_flow value with Not_found -> ValSet.empty

let get_virtual_flow' graph value =
  try Some (ValTbl.find graph.virtual_flow value) with Not_found -> None

let get_virtual_union graph value =
  try ValTbl.find graph.virtual_union value with Not_found -> ValSet.empty

let get_virtual_union' graph value =
  try Some (ValTbl.find graph.virtual_union value) with Not_found -> None

let get_param_dependency graph value =
  try ValTbl.find graph.param_dependency value with
  | Not_found -> ValSet.empty

let get_term graph value =
  ValMap.find value graph.term

let get_block graph value =
  ValMap.find value graph.block

let get_call_info graph f =
  IdentMap.find f graph.call_info

let add_flow graph src dst =
  let flow = get_flow graph src in
  if not (ValSet.mem dst flow)
  then
    let flow = ValSet.add dst flow in
    ValTbl.replace graph.flow src flow

let add_flows graph srcs dst =
  ValSet.iter (fun src -> add_flow graph src dst) srcs

(* list of values on wich depends the term *)
let term_dependencies = function
  | Closure (_, map) -> List.map snd (IdentMap.bindings map)
  | Offset (_,v)
  | Field (_,v) -> [v]
  | Makeblock (_, _, l) -> l
  | Cmpint (_,v1,v2)
  | Addint (v1,v2)
  | Mulint (v1,v2)
    -> [v1;v2]
  | Const _ -> []
  | FunParam set -> ValSet.elements !set
  | FunReturn -> []
  | Makeblock_module l -> l
  | Union l -> l
  | Apply (f, params) -> f :: params

(* values needed to evaluate the term: if one is not present the term
   will not be evaluated *)
type and_or =
  | And of v list
  | Or of v list

let term_needed_dependencies = function
  | Closure (_, map) -> And (List.map snd (IdentMap.bindings map))
  | Offset (_,v)
  | Field (_,v) -> And [v]
  | Makeblock (_, _, l) -> And l
  | Cmpint (_,v1,v2)
  | Addint (v1,v2)
  | Mulint (v1,v2) -> And [v1;v2]
  | Const _ -> And []
  | Apply (f, params) -> And (f :: params)

  | FunReturn -> And []

  | FunParam set -> Or (ValSet.elements !set)
  | Makeblock_module l -> Or l
  | Union l -> Or l

let term_desc = function
  | Closure _ -> "closure"
  | Offset _ -> "offset"
  | Field (i,_) -> Printf.sprintf "field %i" i
  | Makeblock (_, _, l) -> "makeblock"
  | Cmpint _ -> "cmpint"
  | Addint _ -> "addint"
  | Mulint _ -> "mulint"
  | Const _ -> "constant"
  | FunParam _ -> "fun param"
  | FunReturn -> "fun return"
  | Union _ -> "union"
  | Apply _ -> "apply"
  | Makeblock_module _ -> "makeblock module"

let equal_constraint (v1,c1) (v2,c2) = ValId.equal v1 v2 && c1 = c2

(* the difference between two constraints stacks *)
(* let rec added_constraints c1 c2 = match c1, c2 with *)
(*   | _, [] -> c1 *)
(*   | [], _ -> fatal_error "added_constraints c1 shorter than c2" *)
(*   | t1::q1, t2::q2 -> *)
(*     assert(equal_constraint t1 t2); *)
(*     added_constraints q1 q2 *)

let make_value ?name graph constraints_stack =
  let new_value = ValId.create ?name () in
  graph.values <- ValSet.add new_value graph.values;
  graph.block <- ValMap.add new_value constraints_stack graph.block;
  new_value

let associate_term graph value term =
  let dep = term_dependencies term in
  let add_flow dep = add_flow graph dep value in
  List.iter add_flow dep;
  graph.term <- ValMap.add value term graph.term

let new_expr ?name ?(new_value=ValId.create ?name ()) ?location graph
    constraints_stack term =
  graph.values <- ValSet.add new_value graph.values;
  let dep = term_dependencies term in
  let add_flow dep = add_flow graph dep new_value in
  List.iter add_flow dep;
  List.iter (fun (v,_) -> add_flow v) constraints_stack;
  graph.block <- ValMap.add new_value constraints_stack graph.block;
  (match location with
   | None -> ()
   | Some location ->
     graph.locations <- LocMap.add location new_value graph.locations;
     graph.v_location <- ValMap.add new_value location graph.v_location);
  graph.term <- ValMap.add new_value term graph.term;
  new_value

let rec (-->) i j =
  if i > j
  then []
  else i :: ((i+1) --> j)

let new_function graph funid arity =
  let new_param i =
    let name = Printf.sprintf "arg_%i" i in
    let set = ref ValSet.empty in
    let expr = new_expr ~name graph [] (FunParam set) in
    expr, set
  in
  let l = List.map new_param (1 --> arity) in
  let parameters, parameter_sets = List.split l in
  let return = new_expr ~name:"return" graph [] FunReturn in
  let call_info = { parameters; parameter_sets; return } in
  graph.call_info <- IdentMap.add funid call_info graph.call_info;
  call_info

(* let add_call_link graph funid constraints_stack parameters = *)
(*   let fun_param = IdentMap.find funid graph.fun_parameters in *)
(*   let changed = ref false in *)
(*   let add_fun_dep fun_param apply_param = *)
(*     let dep_set = get_param_dependency graph fun_param in *)
(*     if not (ValSet.mem apply_param dep_set) *)
(*     then *)
(*       let dep_set = ValSet.add apply_param dep_set in *)
(*       ValTbl.replace graph.param_dependency fun_param dep_set; *)
(*       changed := true *)
(*   in *)
(*   List.iter2 add_fun_dep fun_param parameters; *)
(*   !changed *)

(* returns true if a dependency has been updated *)
let add_virtual_union graph src dst =
  let flow = get_virtual_flow graph src in
  let union = get_virtual_union graph dst in
  (if not (ValSet.mem dst flow)
   then
     let flow = ValSet.add dst flow in
     ValTbl.replace graph.virtual_flow src flow);
  if not (ValSet.mem src union)
  then
    let union = ValSet.add src union in
    ValTbl.replace graph.virtual_union dst union;
    true
  else
    false

let add_virtual_unions graph srcs dst =
  ValSet.fold (fun src acc -> add_virtual_union graph src dst || acc) srcs false

(* let link_constraint graph block v = *)
(*   let bsrc = ValMap.find v graph.block in *)
(*   added_constraints bsrc block *)

let escape graph v =
  failwith "TODO"
