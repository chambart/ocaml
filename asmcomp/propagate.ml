open Data_dependency
open Domains

type work = (constraints * v)

type t = {
  graph : Data_dependency.graph;
  values : ValueDom.t ValTbl.t;
  mutable work_set : ValSet.t;
  work : v Queue.t;
}

let push_work t v =
  if not (ValSet.mem v t.work_set)
  then begin
    t.work_set <- ValSet.add v t.work_set;
    Queue.push v t.work
  end

let push_works t s =
  ValSet.iter (push_work t) s

let take_work t =
  try
    let v = Queue.take t.work in
    t.work_set <- ValSet.remove v t.work_set;
    Some v
  with
  | Queue.Empty -> None

let get t v =
  try ValTbl.find t.values v with
  | Not_found -> ValueDom.empty ()

let get' t v =
  try Some (ValTbl.find t.values v) with
  | Not_found -> None

(* let get_values_dep t v = *)
(*   try ValTbl.find t.values_dep v with *)
(*   | Not_found -> ValSet.empty *)

(* let get_values_dep' t v = *)
(*   try Some (ValTbl.find t.values_dep v) with *)
(*   | Not_found -> None *)

let get_rec_virtual_union t s =
  let added = ref ValSet.empty in
  let todo = Queue.create () in
  let add v =
    if not (ValSet.mem v !added)
    then begin
      added := ValSet.add v !added;
      Queue.push v todo
    end in
  ValSet.iter add s;
  let rec aux acc =
    if not (Queue.is_empty todo)
    then begin
      let v = Queue.take todo in
      (match get_virtual_union' t.graph v with
       | None -> ()
       | Some set -> ValSet.iter add set);
      let acc = ValSet.add v acc in
      aux acc
    end
    else acc
  in
  aux ValSet.empty

let get_values' t v =
  let dep = get_rec_virtual_union t (ValSet.singleton v) in
  ValSet.fold (fun v acc -> match get' t v with
    | None -> acc
    | Some v -> v::acc) dep []

let get_values t v =
  (* simple optimisation *)
  match get_virtual_union' t.graph v with
  | None -> [get t v]
  | Some _ -> get_values' t v

let is_val_empty t v =
  match get' t v, get_virtual_union' t.graph v with
  | None, None ->
    (* Printf.printf "none/none empty\n%!"; *)
    true
  | Some v, None ->
    (* Printf.printf "Some/none is_empty %b\n%!" (ValueDom.is_empty v); *)
    ValueDom.is_empty v
  | _, Some dep ->
    let l = (get_values t v) in
    let b = List.for_all ValueDom.is_empty l in
    (* Printf.printf "rest is_empty %b %i %i\n%!" b (List.length l) (ValSet.cardinal dep); *)
    b

let reachable t block =
  let rec aux = function
    | [] -> true
    | (cstr_val, assumed) :: q ->
      let cstr = get_values t cstr_val in
      match assumed with
      | Bool b ->
        let res = ValueDom.is_bool b cstr in
        Printf.printf "reachable %a = %b: %b\n%!" ValId.output cstr_val b res;
        if res
        then aux q
        else false
      | _ ->
        (* TODO *)
        aux q
  in
  aux block

(* let get_filtered_values t block v = *)
(*   let rec aux = function *)
(*     | [] -> get_values t v *)
(*     | (cstr_val, assumed) :: q -> *)
(*       let cstr = get_values t cstr_val in *)
(*       match assumed with *)
(*       | Bool b -> *)
(*         (\* TODO: add cstr = true in a local environment *)
(*            to retreive it if we get it *\) *)
(*         if ValueDom.is_bool b cstr *)
(*           (\* this should return the new constrainsts to apply. For *)
(*              instance if cstr is 1 = var => we need to add var = 1 as *)
(*              a constraint *\) *)
(*         then aux q *)
(*         else [] *)
(*       | _ -> *)
(*         (\* TODO *\) *)
(*         aux q *)
(*   in *)
(*   aux block *)

type ret =
  | Val of ValueDom.t
  | Set of ValSet.t
  | Both of (ValueDom.t option * ValSet.t)

let rec eval_apply t func param return_val =
  let aux (acc_values, acc_set) = function
    | ValueDom.Unknown_call _ -> failwith "TODO: unknown call"
    | ValueDom.Complete (func,applied_param,over_param) ->
      let call_info = get_call_info t.graph func.ValueDom.function_id in
      let aux_add_virtual (param,param_set) value =
        if not (ValSet.mem value !param_set)
        then begin
          param_set := ValSet.add value !param_set;
          push_work t param;
        end
        (* if add_virtual_union t.graph value param *)
        (* then *)
        (*   (Printf.printf "push work %a\n%!" ValId.output param; *)
        (*    push_work t param) *)
      in
      let l = List.combine call_info.parameters call_info.parameter_sets in
      List.iter2 aux_add_virtual l applied_param;
      (match over_param with
       | [] ->
         Printf.printf "apply complete\n%!";
         acc_values, ValSet.add call_info.return acc_set
       | _ ->
         let returns = get_rec_virtual_union t
             (ValSet.singleton call_info.return) in
         Printf.printf "apply rec %a -> %a (%a)\n%!"
           ValId.output call_info.return ValId.output return_val
           ValSet.output returns;
         add_flows t.graph returns return_val;
         let l, set = eval_apply t (get_values t call_info.return)
             over_param return_val in
         l @ acc_values, ValSet.union set acc_set)
    | ValueDom.Value v -> v :: acc_values, acc_set
  in
  let (l,set) = List.fold_left aux ([], ValSet.empty)
      (ValueDom.apply func param) in
  l, set

let eval_term t block term return_val = match term with

  | Const cst ->
    Val (ValueDom.constant cst)

  | Addint (v1,v2) ->
    let v1 = get_values t v1 in
    let v2 = get_values t v2 in
    Val (ValueDom.addint v1 v2)

  | Mulint (v1,v2) ->
    let v1 = get_values t v1 in
    let v2 = get_values t v2 in
    Val (ValueDom.mulint v1 v2)

  | Makeblock (tag, mut, vl) ->
    Val (ValueDom.makeblock tag mut vl)

  | Makeblock_module vl ->
    Val (ValueDom.makeblock 0 Asttypes.Immutable vl)

  | Field (i, v) ->
    let v = get_values t v in
    Both (ValueDom.field i v)

  | Union l ->
    Set (ValSet.of_list l)

  | Closure (ids, fv) ->
    let funs = Lambda.IdentSet.fold (fun id acc ->
        let open ValueDom in
        let open Flambda in
        let ffunc = IdentTbl.find t.graph.functions id in
        let v = { function_id = id;
                  function_kind = ffunc.kind;
                  function_param = ffunc.params } in
        v::acc)
        ids [] in
    Val (ValueDom.unoffseted_closure_val funs fv)

  | Offset (id, v) ->
    let v = get_values t v in
    Val (ValueDom.offset id v)

  | FunParam set ->
    Set (!set)

  | FunReturn ->
    Set ValSet.empty

  | Apply (func, params) ->
    let func = get_values t func in
    let l, set = eval_apply t func params return_val in
    begin match l with
      | [] -> Set set
      | _ -> Both (Some (ValueDom.union_list l), set)
    end

  | _ ->
    let desc = term_desc term in
    failwith (Printf.sprintf "TODO: eval_term: %s" desc)

let do_term t graph v =
  let prev_val = get t v in
  (* let prev_val_dep = get_virtual_flow t.graph v in *)
  let block = get_block graph v in
  let term = get_term graph v in
  let available_dependencies =
   let needed_dependencies = term_needed_dependencies term in
   match needed_dependencies with
   | And l -> not (List.exists (is_val_empty t) l)
   | Or l -> not (List.for_all (is_val_empty t) l)
  in
  let reach = reachable t block in
  Printf.printf "eval: %a %s\n%!" ValId.output v (term_desc term);
  Printf.printf "\t\t\t\tavailabl %b\t reach:   %b\n%!" available_dependencies reach;
  if available_dependencies && reach
  then begin
    let new_val, new_set = match eval_term t block term v with
      | Val v -> Some v, ValSet.empty
      | Set s -> None, s
      | Both (v,s) -> v, s in

    (* TODO: clean ! *)
    let new_set = ValSet.remove v (get_rec_virtual_union t (ValSet.add v new_set)) in

    (match term with
     | FunParam _ -> Printf.printf "param valset %a\n%!" ValSet.output new_set
     | _ -> ());

    (* let same_val_dep = ValSet.equal prev_val_dep new_set in *)

    (* if not same_val_dep *)
    (* then begin *)
    (*   (\* Printf.printf "add virtual "; *\) *)
    (*   (\* ValSet.iter (fun v -> Printf.printf "%a " ValId.output v) new_set; *\) *)
    (*   (\* Printf.printf "\n%!"; *\) *)
    (*   add_virtual_flows graph new_set v; *)
    (*   ValTbl.replace t.values_dep v (ValSet.union prev_val_dep new_set) *)
    (* end; *)

    let same_val_dep = not (add_virtual_unions graph new_set v) in

    let same_val =
      match new_val with
      | None -> true
      | Some new_val ->
        if not (ValueDom.equal prev_val new_val)
        then begin
          ValTbl.replace t.values v new_val;
          false
        end
        else true in

    Printf.printf "\t\t\t\tsame_val %b\t same_dep %b\n%!" same_val same_val_dep;

    if not (same_val_dep && same_val)
    then begin
      let flow = get_flow graph v in
      let virtual_flow = get_virtual_flow graph v in
      Printf.printf "flow: %i %i\n%!" (ValSet.cardinal flow)
        (ValSet.cardinal virtual_flow);
      push_works t flow;
      push_works t virtual_flow;
    end
  end

let init_work graph =
  let t = {
    graph;
    values = ValTbl.create 10;
    (* values_dep = ValTbl.create 10; *)
    work_set = ValSet.empty;
    work = Queue.create ();
  } in
  push_works t graph.Data_dependency.values;
  t

let run_graph graph =
  let t = init_work graph in
  let rec aux () =
    match take_work t with
    | None -> ()
    | Some v ->
      do_term t graph v;
      aux ()
  in
  aux ();
  t.values
