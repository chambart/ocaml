
let print_flows ppf graph =
  let open Data_dependency in
  let open Format in
  ValSet.iter (fun v ->
    let term = get_term graph v in
    let dep = term_needed_dependencies term in
    let print_dep ppf = function
      | And l -> fprintf ppf "and:@[ %a@]" ValSet.print (ValSet.of_list l)
      | Or l -> fprintf ppf "or:@[ %a@]" ValSet.print (ValSet.of_list l)
    in
    let flow = get_flow graph v in
    let virtual_flow = get_virtual_flow graph v in
    fprintf ppf "%a: %s@ ->@[<2>(@ %a@ )@]@ @[<2>(@ %a@ )@]@ %a@."
      ValId.print v
      (term_desc term)
      ValSet.print flow ValSet.print virtual_flow
      print_dep dep)
    graph.values

let run ppf tree =
  let module_value, base_graph = Make_data_graph.basic_graph tree in
  print_flows ppf base_graph;
  let values = Propagate.run_graph base_graph in
  let getter x = Data_dependency.ValTbl.find values x in
  let getter_dep x = Data_dependency.get_virtual_union' base_graph x in
  Domains.ValueDom.print_value getter getter_dep ppf module_value;
  Domains.ValueDom.print_values getter getter_dep ppf
    base_graph.Data_dependency.values;
  print_flows ppf base_graph;

  ignore base_graph;
  ()
