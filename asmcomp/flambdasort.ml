open Misc
open Flambda

type 'a bindings = 'a flambda IdentMap.t

type dependencies = IdentSet.t IdentMap.t

let dependencies (bindings:'a bindings) : dependencies =
  IdentMap.map Flambdautils.free_variables bindings

(* ensure that the dependency graph does not have external dependencies *)
let check dependencies =
  IdentMap.iter (fun _ set ->
      IdentSet.iter (fun v ->
          if not (IdentMap.mem v dependencies)
          then fatal_error
              (Printf.sprintf "Flambdasort.check: the graph has external \
                               dependencies (%s)" (Ident.unique_name v)))
        set)
    dependencies

type numbering =
  { back : int IdentMap.t;
    forth : Ident.t array }

let number graph =
  let size = IdentMap.cardinal graph in
  let bindings = IdentMap.bindings graph in
  let a = Array.of_list bindings in
  let forth = Array.map fst a in
  let back =
    let back = ref IdentMap.empty in
    for i = 0 to size - 1 do
      back := IdentMap.add forth.(i) i !back;
    done;
    !back in
  let integer_graph = Array.init size (fun i ->
      let _, dests = a.(i) in
      IdentSet.fold (fun dest acc -> (IdentMap.find dest back) :: acc) dests []) in
  { back; forth }, integer_graph


let transpose graph =
  let size = Array.length graph in
  let transposed = Array.create size [] in
  let add src dst = transposed.(src) <- dst :: transposed.(src) in
  Array.iteri (fun src dsts -> List.iter (fun dst -> add dst src) dsts) graph;
  transposed

let depth_first_order graph =
  let size = Array.length graph in
  let marked = Array.create size false in
  let stack = Array.create size ~-1 in
  let pos = ref 0 in
  let push i =
    stack.(!pos) <- i;
    incr pos
  in
  let rec aux node =
    if not marked.(node)
    then begin
      marked.(node) <- true;
      List.iter aux graph.(node);
      push node
    end
  in
  for i = 0 to size - 1 do
    aux i
  done;
  stack

let mark order graph =
  let size = Array.length graph in
  let graph = transpose graph in

  let marked = Array.create size false in
  let id = Array.create size ~-1 in
  let count = ref 0 in

  let rec aux node =
    if not marked.(node)
    then begin
      marked.(node) <- true;
      id.(node) <- !count;
      List.iter aux graph.(node)
    end
  in

  for i = size - 1 downto 0 do
    let node = order.(i) in
    if not marked.(node)
    then begin
      aux order.(i);
      incr count
    end
  done;
  id, !count

let kosaraju graph =
  let dfo = depth_first_order graph in
  let components, ncomponents = mark dfo graph in
  ncomponents, components

let topological_sort graph =
  let numbering, integer_graph = number graph in
  let ncomponents, scc = kosaraju integer_graph in
  let id_scc = Array.create ncomponents IdentSet.empty in
  Array.iteri (fun node component ->
      id_scc.(component) <-
        IdentSet.add
          numbering.forth.(node)
          id_scc.(component)) scc;
  id_scc

let rebuild_sorted_expr bindings body =
  let deps = dependencies bindings in
  check deps;
  let scc = topological_sort deps in
  let aux body set =
    match IdentSet.elements set with
    | [] -> assert false
    | [id] ->
      (* Printf.printf "%s\n%!" (Ident.unique_name id); *)
      let expr = IdentMap.find id bindings in
      if IdentSet.mem id (IdentMap.find id deps)
      then Fletrec([id,expr],body, ExprId.create ())
      else Flet(Lambda.Strict,id,expr,body, ExprId.create ())
    | elts ->
      let vars = List.map (fun id -> id, IdentMap.find id bindings) elts in
      Fletrec(vars,body, ExprId.create ())
  in
  Array.fold_left aux body scc
