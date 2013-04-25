

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

(* module UnitName = struct *)
(*   let unit_name () = Compilenv.current_unit_name () *)
(* end *)

module ValId : Id = Flambda.Id(Flambda.Empty)
module ValMap = Flambda.ExtMap(ValId)
module ValSet = Flambda.ExtSet(ValId)
module ValTbl = Flambda.ExtHashtbl(ValId)

type v = ValId.t
type funid = Ident.t

type intbinop =
  | Addint | Mulint | Subint | Divint | Modint | Andint | Orint
  | Xorint | Lslint | Lsrint | Asrint
  | Isout

type term =
  | Closure of IdentSet.t * v IdentMap.t
  | Offset of Ident.t * v
  | Identity of v
  | Stringlength of v
  | Stringget of bool * v * v (* safe * string * index *)
  | Arraylength of v
  | Field of int * v
  | Setfield of int * v * v
  | Makeblock_module of v list
  | Makeblock of int * mutable_flag * v list
  (* Integer operations *)
  | Negint of v
  | Intbinop of intbinop * v * v
  | Cmpphys of Lambda.comparison * v * v
  | Cmpint of Lambda.comparison * v * v
  (* Boolean operations *)
  | Sequand of v * v
  | Sequor of v * v
  | Not of v
  (* constants *)
  | Const of Flambda.const
  | Exception_term
  | FunParam of ValSet.t ref
  | FunReturn
  | Union of v list
  | Interval of v * v
  | Apply of v * v list
  | Unknown_primitive of string * v list
        (* C calls (first field is the description) *)
  | Predef_exn of Ident.t
  | Unknown
  | Other

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

type block_info = {
  block_desc : string;
  block_constraints : constraints;
  mutable block_exception : v option;
  block_parent : block_info option;
}

type call_info = {
  parameters : v list;
  parameter_sets : ValSet.t ref list;
  return : v;
  call_block : block_info;
}

type graph =
  { mutable locations : v LocMap.t;
    mutable v_location : location ValMap.t;
    mutable term : term ValMap.t;
    flow : ValSet.t ValTbl.t;
    (** which values depends on that one *)
    mutable block : block_info ValMap.t;
    param_dependency : ValSet.t ValTbl.t;
    mutable call_info : call_info IdentMap.t;
    (* TODO: allow specialisation per call site *)
    mutable values : ValSet.t;
    virtual_flow : ValSet.t ValTbl.t;
    (** which values depends on that one *)
    virtual_union : ValSet.t ValTbl.t;
    functions : ExprId.t ffunction IdentTbl.t;
    mutable ignored_need : ValSet.t;
    (** which variables are not needed to evaluate their flow:
        used to start the evaluation of recursive loops *)
    toplevel_block : block_info; }

let toplevel_block () =
  { block_desc = "toplevel";
    block_constraints = [];
    block_exception = None;
    block_parent = None }

let get_block_exception graph block =
  match block.block_exception with
  | Some exn -> exn
  | None ->
    let exn = ValId.create ~name:("exception_" ^ block.block_desc) () in
    block.block_exception <- Some exn;
    graph.values <- ValSet.add exn graph.values;
    graph.term <- ValMap.add exn Exception_term graph.term;
    graph.block <- ValMap.add exn block graph.block;
    exn

let empty_graph () =
  let toplevel_block = toplevel_block () in
  let graph =
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
      functions = IdentTbl.create 10;
      ignored_need = ValSet.empty;
      toplevel_block = toplevel_block } in
  graph

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
  let virtual_flow = get_virtual_flow graph src in
  if not (ValSet.mem dst flow || ValSet.mem dst virtual_flow)
  then
    let flow = ValSet.add dst flow in
    ValTbl.replace graph.flow src flow

let add_flows graph srcs dst =
  ValSet.iter (fun src -> add_flow graph src dst) srcs

let is_ignored_need graph v =
  ValSet.mem v graph.ignored_need

let ignore_need graph v =
  graph.ignored_need <- ValSet.add v graph.ignored_need

let get_call_exception graph call =
  get_block_exception graph call.call_block

(* values needed to evaluate the term: if one is not present the term
   will not be evaluated *)
type and_or =
  | And of v list
  | Or of v list

let term_needed_dependencies = function
  | Closure (_, map) -> And (List.map snd (IdentMap.bindings map))
  | Offset (_,v)
  | Identity v
  | Stringlength v
  | Arraylength v
  | Field (_,v) -> And [v]
  | Makeblock (_, _, l) -> And l
  | Not v
  | Negint v -> And [v]
  | Stringget (_,v1,v2)
  | Setfield (_, v1, v2)
  | Sequand (v1, v2)
  | Sequor (v1, v2)
  | Cmpphys (_,v1,v2)
  | Cmpint (_,v1,v2)
  | Intbinop (_,v1,v2) -> And [v1;v2]
  | Predef_exn _
  | Const _ -> And []
  | Apply (f, params) -> And (f :: params)

  | Interval (v1,v2) -> And [v1;v2]

  | Exception_term -> And []
  | FunReturn -> And []
  | Unknown_primitive (_,l) -> And l
  | FunParam set -> Or (ValSet.elements !set)
  | Makeblock_module l -> Or l
  | Union l -> Or l

  | Unknown -> And []
  | Other -> assert false

let term_dependencies term =
  match term_needed_dependencies term with
  | And l
  | Or l -> l

let intbinop_desc = function
  | Addint -> "addint"
  | Mulint -> "mulint"
  | Subint -> "subint"
  | Divint -> "divint"
  | Modint -> "modint"
  | Andint -> "andint"
  | Orint -> "orint"
  | Xorint -> "xorint"
  | Lslint -> "lslint"
  | Lsrint -> "lsrint"
  | Asrint -> "asrint"
  | Isout -> "isout"

let term_desc = function
  | Closure _ -> "closure"
  | Identity _ -> "identity"
  | Offset _ -> "offset"
  | Stringlength _ -> "stringlength"
  | Stringget _ -> "stringget"
  | Arraylength _ -> "arraylength"
  | Setfield _ -> "setfield"
  | Field (i,_) -> Printf.sprintf "field %i" i
  | Makeblock (_, _, l) -> "makeblock"
  | Cmpphys _ -> "cmpphys"
  | Cmpint _ -> "cmpint"
  | Negint _ -> "negint"
  | Intbinop (op,_,_) -> intbinop_desc op
  | Sequand _ -> "sequand"
  | Sequor _ -> "sequor"
  | Not _ -> "not"
  | Predef_exn id -> Printf.sprintf "exn_%s" (Ident.unique_name id)
  | Const _ -> "constant"
  | Exception_term -> "exception"
  | FunParam _ -> "fun param"
  | FunReturn -> "fun return"
  | Interval _ -> "interval"
  | Union _ -> "union"
  | Apply _ -> "apply"
  | Makeblock_module _ -> "makeblock module"
  | Unknown_primitive (desc,_) -> Printf.sprintf "unknown: %s" desc

  | Unknown -> "unknown"
  | Other -> assert false

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
    block term =
  graph.values <- ValSet.add new_value graph.values;
  let dep = term_dependencies term in
  let add_flow dep = add_flow graph dep new_value in
  List.iter add_flow dep;
  List.iter (fun (v,_) -> add_flow v) block.block_constraints;
  graph.block <- ValMap.add new_value block graph.block;
  (match location with
   | None -> ()
   | Some location ->
     graph.locations <- LocMap.add location new_value graph.locations;
     graph.v_location <- ValMap.add new_value location graph.v_location);
  graph.term <- ValMap.add new_value term graph.term;
  new_value

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

let new_block graph parent desc added_constraint =
  let block =
    { block_desc = desc;
      block_constraints = added_constraint @ parent.block_constraints;
      block_exception = None;
      block_parent = Some parent } in
  block

let add_block_exception graph block v =
  ignore(add_virtual_union graph v (get_block_exception graph block):bool)

let rec (-->) i j =
  if i > j
  then []
  else i :: ((i+1) --> j)

let new_function graph funid arity =
  let new_param i =
    let name = Printf.sprintf "arg_%i" i in
    let set = ref ValSet.empty in
    let expr = new_expr ~name graph graph.toplevel_block (FunParam set) in
    expr, set
  in
  let l = List.map new_param (1 --> arity) in
  let parameters, parameter_sets = List.split l in
  let return = new_expr ~name:"return" graph graph.toplevel_block FunReturn in
  let call_block = new_block graph graph.toplevel_block (Ident.unique_name funid) [] in
  let call_info = { parameters; parameter_sets; return; call_block } in
  graph.call_info <- IdentMap.add funid call_info graph.call_info;
  call_info

let escape graph v =
  failwith "TODO"
