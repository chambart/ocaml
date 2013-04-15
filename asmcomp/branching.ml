open Asttypes
open Flambda

module type Body = sig
  type body
end

module type BranchName = sig
  type name
end

module type Interp = sig

  type body

  type fixed_value

  type values
  type traces

  type name

  val fixed_value : traces -> values -> fixed_value
  val empty_value : fixed_value

  (* type partial_result *)
  (* type final_result *)
  (* type result = *)
  (*   | Continue of partial_result *)
  (*   | Fix_point of final_result *)

  val start_branch : name -> traces
  (* val restart : partial_result -> traces *)
  (* val result : partial_result -> traces -> result *)

  (* val union : traces list -> traces *)
  val fixed_value_union : fixed_value list -> fixed_value

  type if_return =
    { ifso : traces option;
      ifnot : traces option }

  val if_branch : name -> condition:values -> traces -> if_return
  val if_return : name -> parent:traces -> (values * traces) list -> (values * traces)

  (* type 'a switch = *)
  (*   { numconsts : int; *)
  (*     consts : (int * 'a) list; *)
  (*     numblocks : int; *)
  (*     blocks : (int * 'a) list; *)
  (*     failaction : 'a option; } *)

  (* val switch : name -> condition:values -> unit switch -> traces -> traces switch *)

  type inside =
    { body : body;
      over_parameters : values list;
      bindings : values IdentMap.t }

  type apply =
    | Inside of inside
    | Return of values

  val call : name -> func:values -> values list -> traces -> (apply * traces) list
  val return : name -> parent:traces -> (values * traces) list -> (values * traces)

  (* val send : name -> Lambda.meth_kind -> met:values -> obj:values -> *)
  (*   args:values list -> traces -> (apply * traces) list *)

  (* type loop_return = *)
  (*   { loop : traces option; *)
  (*     terminate : traces option; } *)

  (* val while_branch : name -> values -> traces -> loop_return *)
  (* val for_branch : name -> var:values -> low:values -> high:values -> traces -> *)
  (*   loop_return *)

  (* type catch_return = *)
  (*   { direct : traces; *)
  (*     caught : traces option } *)

  (* val catch_enter : name -> int -> traces -> traces *)
  (* val catch : name -> int -> traces -> catch_return *)

  (* val try_enter : name -> traces -> traces *)
  (* val try_with : name -> traces -> catch_return *)

  type funct =
    { function_id : Ident.t;
      function_kind : Lambda.function_kind;
      function_param: Ident.t list;
      function_body : body }

  type trivial_expr =
    | Closure of funct list * values IdentMap.t
    | Predef_exn of Ident.t
    | Unknown
    | Unit
    | Const of Flambda.const
    | Offset of values * Ident.t
    | Env_field of values * Ident.t
    | Addint of values * values
    | Makeblock of int * mutable_flag * values list
    | Field of int * values
    | Fixed of fixed_value

  val trivial_expr : traces -> trivial_expr -> values * traces

  val print_values : traces -> Format.formatter -> values -> unit

  val same_values : values -> values -> bool

end




module BasicInterp(P:Body)(Name:BranchName) : Interp with type body = P.body and type name = Name.name = struct

  type body = P.body

  type name = Name.name

  module ValId : Id = Id(Flambda.Empty)
  module ValMap = Flambda.ExtMap(ValId)
  module ValSet = Flambda.ExtSet(ValId)
  module ValTbl = Flambda.ExtHashtbl(ValId)

  type values = ValId.t

  let same_values = ValId.equal

  type funct =
    { function_id : Ident.t;
      function_kind : Lambda.function_kind;
      function_param: Ident.t list;
      function_body : body }

  type partial_parameters =
    | Known_param of values list
    | Unknown_param of ValSet.t
        (* we do not take time to merge that: it is only used for escaping *)

  type intern_funct =
    { funct : funct;
      partial_param : partial_parameters;
      closure : values IdentMap.t }

  type intern_block =
    { tag : int;
      fields : values list;
      mut : mutable_flag }

  type other_values =
    | Value_unoffseted_closure of funct list * values IdentMap.t
    | Value_mutable
    | Value_string
    | Value_floatarray
    | Value_integer of int
    | Value_any_integer
    | Value_float
    | Value_boxed_int of Lambda.boxed_integer
    | Value_unknown_not_block (* an unknown value that can't contain any value *)
    | Value_unknown     (* unknown result *)
    | Value_none        (* no other case possible *)

  type v = {
    v_clos : intern_funct IdentMap.t;
    v_cstptr: IntSet.t;
    v_block: intern_block IntMap.t;
    v_other: other_values;
  }

  type fixed_value = v

  type function_analysis = {
    function_funct : funct;
    (* values do not flow througt function interface, only their 'concretisation' *)
    function_closure : v IdentMap.t;
    function_known_param : v IdentMap.t;
    function_unknown_param : v option;
    function_return : v;
    function_escaping : bool;
    (* wether the function can be called in an uncontrolled context *)
  }

  (* common value updated by side effects for all traces *)
  type common = {
    mutable function_map : function_analysis IdentMap.t;
    empty_values : values;
  }

  type branch_kind =
    | Root (* with no parent *)
    | If of bool
    (* | Switch *)
    | Function of funct option

  type traces = {
    name : name;
    branch_kind : branch_kind;
    parent : traces option;
    common : common;
    bindings : v ValMap.t;
    escaping : ValSet.t;
    static_raises: (values list * traces) IntMap.t;
    exceptions : (values*traces) option;
  }

  (* escaping *)

  let add_escapings traces s =
    { traces with escaping = ValSet.union s traces.escaping }
  let add_escaping traces v =
    { traces with escaping = ValSet.add v traces.escaping }

  (* value_building *)

  let empty_val = {
    v_clos = IdentMap.empty;
    v_cstptr = IntSet.empty;
    v_block = IntMap.empty;
    v_other = Value_none;
  }

  let empty_value = empty_val

  let any_int_val = { empty_val with v_other = Value_any_integer }

  let int_val i = { empty_val with v_other = Value_integer i }

  let unknown_val = { empty_val with v_other = Value_unknown }

  let unknown_not_block_val =
    { empty_val with v_other = Value_unknown_not_block }
  let mutable_val = { empty_val with v_other = Value_mutable }
  let floatarray_val = { empty_val with v_other = Value_floatarray }
  let string_val = { empty_val with v_other = Value_string }
  let float_val = { empty_val with v_other = Value_float }
  let boxedint_val bi = { empty_val with v_other = Value_boxed_int bi }
  let unoffseted_closure_val fl cl =
    { empty_val with v_other = Value_unoffseted_closure (fl,cl) }

  let constptr_val i = { empty_val with v_cstptr = IntSet.singleton i }
  let constptrs_val s = { empty_val with v_cstptr = s }

  let block_val tag mut fields =
    let block = { tag; mut; fields } in
    { empty_val with v_block = IntMap.singleton tag block }

  let fun_val ident fl cl =
    let fl = List.filter (fun funct -> Idt.equal ident funct.function_id) fl in
    match fl with
    | []
    | _::_::_ -> assert false
    (* it should not be possible to build an expression doing that *)
    | [funct] ->
      let intern_funct = { funct; partial_param = Known_param []; closure = cl } in
      { empty_val with v_clos = IdentMap.singleton ident intern_funct }

  let fun_val' intern_funct =
    { empty_val with v_clos = IdentMap.singleton intern_funct.funct.function_id intern_funct }

  (* value retrieval *)

  let rec get traces v =
    try ValMap.find v traces.bindings with
    | Not_found ->
      match traces.parent with
      | Some parent -> get parent v
      | None -> failwith "Could not retrieve value"

  let fixed_value = get

  (* v union *)

  let value_union (v1:v) (v2:v) : v =
    let dummy_merge _ v1 v2 = match v1, v2 with
      | None, v | v, None -> v
      | Some v1, Some v2 ->
        ignore (Unknown_param (ValSet.empty));
        failwith "TODO"
    in
    let v_clos = IdentMap.merge dummy_merge v1.v_clos v2.v_clos in
    let v_block = IntMap.merge dummy_merge v1.v_block v2.v_block in
    let v_cstptr = IntSet.union v1.v_cstptr v2.v_cstptr in
    let v_other = match v1.v_other, v2.v_other with
      | Value_none, v | v, Value_none -> v
      | Value_mutable, Value_mutable -> Value_mutable
      | Value_string, Value_string -> Value_string
      | Value_floatarray, Value_floatarray -> Value_floatarray
      | Value_any_integer, Value_any_integer -> Value_any_integer
      | Value_float, Value_float -> Value_float
      | Value_integer i, Value_integer j ->
        if i = j then Value_integer i
        else Value_any_integer
      | Value_unknown_not_block, Value_unknown_not_block ->
        Value_unknown_not_block
      | Value_unknown, Value_unknown -> Value_unknown
      | _, _ -> failwith "TODO"
    in
    { v_clos; v_block; v_cstptr; v_other }

  let value_list_union (vl:v list) : v = match vl with
    | [] -> empty_val
    | [v] -> v
    | t::q -> List.fold_left value_union t q

  (* values union *)

  let values_union traces (v1:values) (v2:values) : values * v =
    failwith "TODO"

  let values_list_union traces (v:values list) : values * v =
    match v with
    | [] -> traces.common.empty_values, empty_val
    | [t] -> t, get traces t
    | _ -> failwith "TODO"

  let fixed_value_union = value_list_union

  (* let is_empty_values traces v = *)
  (*   ValId.equal traces.common.empty_values v *)

  (* let value_union' traces v1 v2 = *)
  (*   if is_empty_values traces v1 *)
  (*   then v2, traces *)
  (*   else if is_empty_values traces v2 *)
  (*   then v1, traces *)
  (*   else *)
  (*     let bindings = traces.bindings in *)
  (*     let v1' = ValMap.find v1 bindings in *)
  (*     let v2' = ValMap.find v2 bindings in *)
  (*     (\* creer une nouvelle valeur union et la mettre dans traces *\) *)
  (*     failwith "TODO" *)

  (* let set_union traces set = *)
  (*   let elts = ValSet.elements set in *)
  (*   match elts with *)
  (*   | [] -> traces.common.empty_values, traces *)
  (*   | [t] -> t, traces *)
  (*   | t::q -> *)
  (*     List.fold_left (fun (v,traces) elt -> value_union' traces v elt) (t,traces) q *)

  (* basic branching *)

  let start_branch name =
    let empty_values = ValId.create ~name:"empty" () in
    let common =
      { function_map = IdentMap.empty;
        empty_values } in
    { name;
      branch_kind = Root;
      parent = None;
      common;
      bindings = ValMap.singleton empty_values empty_val;
      escaping = ValSet.empty;
      static_raises = IntMap.empty;
      exceptions = None }

  let new_branch name branch_kind traces =
    { name;
      branch_kind;
      parent = Some traces;
      common = traces.common;
      bindings = ValMap.empty;
      escaping = ValSet.empty;
      (* exceptions and static exceptions can only 'flow up' *)
      static_raises = IntMap.empty;
      exceptions = None }

  let bind_val traces values v =
    { traces with bindings = ValMap.add values v traces.bindings }

  let bind_new_val traces v =
    let values = ValId.create () in
    values, bind_val traces values v

  (* querying *)

  type 'a answer =
    { known : 'a; (* all known values matching the request *)
      has_unknown : bool; (* does it have unknown values matching the request *)
      has_other : bool; (* are there values not matching the request *) }

  type 'a single_set =
    | Singleton of 'a
    | Multiple
    | Empty

  type boolset = { true_ : bool; false_ : bool }

  type bool_answer = boolset answer

  type int_answer = IntSet.t answer

  type fun_answer = intern_funct list answer

  type block_answer = intern_block IntMap.t answer

  type unoffseted_closure_answer = (funct list * values IdentMap.t) option answer

  let to_bool v : bool_answer =
    let true_branch, false_branch, has_unknown =
      match v.v_other with
      | Value_any_integer
      | Value_unknown_not_block
      | Value_unknown -> false, false, true
      | Value_integer i -> i <> 0, i = 0, false
      | Value_unoffseted_closure _
      | Value_mutable
      | Value_string
      | Value_floatarray
      | Value_float
      | Value_boxed_int _ -> true, false, false
      | Value_none -> false, false, false
    in
    let true_branch = true_branch || not (IdentMap.is_empty v.v_clos) in
    let true_branch = true_branch || not (IntMap.is_empty v.v_block) in
    let true_branch =
      true_branch ||
      (not (IntSet.is_empty v.v_cstptr)
       && ((IntSet.min_elt v.v_cstptr <> 0)
           || (IntSet.max_elt v.v_cstptr <> 0))) in
    let false_branch = false_branch || IntSet.mem 0 v.v_cstptr in
    { known = { true_ = true_branch; false_ = false_branch };
      has_unknown;
      has_other = false } (* everything can be considered a boolean: a block is true *)

  let to_int v : int_answer =
    let has_other =
      (not (IdentMap.is_empty v.v_clos)) || (not (IntMap.is_empty v.v_block)) in
    let known = v.v_cstptr in
    match v.v_other with
    | Value_none ->
      { known = known; has_unknown = false; has_other = has_other }
    | Value_integer i ->
      { known = IntSet.add i known; has_unknown = false; has_other = has_other }
    | Value_any_integer ->
      { known = known; has_unknown = true; has_other = has_other }
    | Value_unknown | Value_unknown_not_block ->
      { known = known; has_unknown = true; has_other = true }
    | Value_unoffseted_closure _ | Value_mutable | Value_string
    | Value_floatarray | Value_float | Value_boxed_int _ ->
      { known = known; has_unknown = false; has_other = true }

  let single_set_int ans =
    if ans.has_unknown
    then Multiple
    else
      let c = IntSet.cardinal ans.known in
      match c with
      | 0 -> Empty
      | 1 -> Singleton (IntSet.choose ans.known)
      | _ -> Multiple

  let to_function v : fun_answer =
    let known = List.map snd (IdentMap.bindings v.v_clos) in
    let has_unknown, has_other = match v.v_other with
      | Value_unknown -> true, true
      | Value_any_integer
      | Value_unknown_not_block
      | Value_integer _
      | Value_unoffseted_closure _
      | Value_mutable
      | Value_string
      | Value_floatarray
      | Value_float
      | Value_boxed_int _ -> false, true
      | Value_none -> false, false in
    let has_other = has_other || not (IntSet.is_empty v.v_cstptr)
                    || not (IntMap.is_empty v.v_block) in
    { known;
      has_unknown;
      has_other }

  let to_block v : block_answer =
    let known = v.v_block in
    let has_unknown, has_other = match v.v_other with
      | Value_unknown -> true, true
      | Value_any_integer
      | Value_unknown_not_block
      | Value_integer _
      | Value_unoffseted_closure _
      | Value_mutable
      | Value_string
      | Value_floatarray
      | Value_float
      | Value_boxed_int _ -> false, true
      | Value_none -> false, false in
    let has_other = has_other || not (IntSet.is_empty v.v_cstptr)
                    || not (IdentMap.is_empty v.v_clos) in
    { known;
      has_unknown;
      has_other }

  let to_unoffseted_closure v : unoffseted_closure_answer =
    let known, has_unknown, has_other = match v.v_other with
      | Value_unoffseted_closure (fl,cl) ->
        Some (fl,cl), false, false
      | Value_unknown -> None, true, true
      | Value_any_integer
      | Value_unknown_not_block
      | Value_integer _
      | Value_mutable
      | Value_string
      | Value_floatarray
      | Value_float
      | Value_boxed_int _ -> None, false, true
      | Value_none -> None, false, false in
    let has_other = has_other
                    || not (IntSet.is_empty v.v_cstptr)
                    || not (IdentMap.is_empty v.v_clos)
                    || not (IntMap.is_empty v.v_block) in
    { known;
      has_unknown;
      has_other }

  (* printing *)

  module Print_values = struct
    open Format

    let ppf = std_formatter

    let rec print_clos ?traces ppf map =
      IdentMap.iter (fun id funct ->
        match funct.partial_param with
        | Known_param [] -> fprintf ppf "%a@ " Ident.print id
        | Known_param l ->
          fprintf ppf "%a@ (%a)@ " Ident.print id (print_list ?traces) l;
        | Unknown_param s ->
          fprintf ppf "%a@ Unknown (%a)@ " Ident.print id (print_list ?traces) (ValSet.elements s);
      ) map

    and print_cstptr ppf set =
      IntSet.iter (fun i -> fprintf ppf "%i@ " i) set

    and print_other ppf = function
      | Value_unoffseted_closure _ -> fprintf ppf "Value_unoffseted_closure"
      | Value_mutable
      | Value_string
      | Value_floatarray -> fprintf ppf "TODO"
      | Value_integer i -> fprintf ppf "Int %i" i
      | Value_any_integer -> fprintf ppf "any int"
      | Value_float -> fprintf ppf "Float"
      | Value_boxed_int _  -> fprintf ppf "TODO"
      | Value_unknown_not_block -> fprintf ppf "Unknown not block"
      | Value_unknown -> fprintf ppf "Unknown"
      | Value_none -> ()

    and print_block ?traces ppf map =
      IntMap.iter (fun _ v ->
        let mut = match v.mut with
          | Mutable -> " (mutable)"
          | Immutable -> "" in
        fprintf ppf "[@[<1>%i%s: " v.tag mut;
        print_list ?traces ppf v.fields;
        fprintf ppf "@]]")
        map

    and print_list ?traces ppf l =
      List.iter (fprintf ppf "%a@ " (print_vid ?traces)) l

    and print_vid ?traces ppf v =
      match traces with
      | None -> ValId.print ppf v
      | Some traces -> print_values traces ppf v

    and print_v ?traces ppf v =
      Format.fprintf ppf "(@[<1>%a@ %a@ %a@ %a@ @])"
        (print_clos ?traces) v.v_clos
        (print_block ?traces) v.v_block
        print_cstptr v.v_cstptr
        print_other v.v_other

    and print_values traces ppf values =
      let v = get traces values in
      print_v ~traces ppf v

  end
  include Print_values

  (* function application *)

  let function_analysis traces funct =
    try IdentMap.find funct.function_id traces.common.function_map
    with Not_found ->
      { function_funct = funct;
        function_closure = IdentMap.empty;
        function_known_param = IdentMap.empty;
        function_unknown_param = None;
        function_return = empty_val;
        function_escaping = false; }

  let add_unknown_param traces funct set =
    let an = function_analysis traces funct in
    let unknown_param = match an.function_unknown_param with
      | None -> empty_val
      | Some unknown_param -> unknown_param in
    let elts = List.map (get traces) (ValSet.elements set) in
    let unknown_param = value_list_union (unknown_param::elts) in
    let an = { an with function_unknown_param = Some unknown_param } in
    let map = IdentMap.add funct.function_id an traces.common.function_map in
    traces.common.function_map <- map

  (* Trivial expression application *)

  type trivial_expr =
    | Closure of funct list * values IdentMap.t
    | Predef_exn of Ident.t
    | Unknown
    | Unit
    | Const of Flambda.const
    | Offset of values * Ident.t
    | Env_field of values * Ident.t
    | Addint of values * values
    | Makeblock of int * mutable_flag * values list
    | Field of int * values
    | Fixed of v

  let depend = function
    | Makeblock (tag, _, vl) -> vl
    | Field (_,v) -> [v]
    | Addint (v1,v2) -> [v1;v2]
    | Closure (_,map) -> List.map snd (IdentMap.bindings map)
    | Fixed _
    | Predef_exn _
    | Unit
    | Unknown
    | Const _ -> []
    | Offset _
    | Env_field _ -> failwith "TODO !"

  let get_single_set_int traces v =
    single_set_int (to_int (get traces v))

  let const_base b = let open Asttypes in match b with
    | Const_int i -> int_val i
    | Const_char c -> int_val (Char.code c)
    | Const_string _ -> string_val
    | Const_float _ -> float_val
    | Const_int32 _ -> boxedint_val Lambda.Pint32
    | Const_int64 _ -> boxedint_val Lambda.Pint64
    | Const_nativeint _ -> boxedint_val Lambda.Pnativeint

  let constant_val = function
    | Fconst_base c -> const_base c
    | Fconst_pointer i -> constptr_val i
    | Fconst_float_array _ -> floatarray_val
    | Fconst_immstring _ -> string_val

  let op traces expr = match expr with
    | Addint (v1,v2) ->
      let v1 = get_single_set_int traces v1 in
      let v2 = get_single_set_int traces v2 in
      begin match v1, v2 with
        | Empty, _ | _, Empty -> empty_val
        | Singleton i1, Singleton i2 -> int_val (i1 + i2)
        | Multiple, _ | _, Multiple -> any_int_val
      end, None
    | Unit -> constptr_val 0, None
    | Const cst -> constant_val cst, None
    | Unknown -> unknown_val, None
    | Makeblock(tag, mut, fields) -> block_val tag mut fields, None
    | Field(n, values) ->
      let v = to_block (get traces values) in
      let get_field i v =
        if List.length v.fields <= i
        then traces.common.empty_values
        else List.nth v.fields i in
      let l = List.map (fun (_,v) -> get_field n v) (IntMap.bindings v.known) in
      let field_values, field_v = values_list_union traces l in
      let field_v =
        if v.has_unknown
        then { field_v with v_other = Value_unknown }
        else field_v
      in
      field_v, Some field_values

    | Closure (fl, cl) ->
      unoffseted_closure_val fl cl, None
    | Offset (values, id) ->
      let uc = to_unoffseted_closure (get traces values) in
      if uc.has_unknown || uc.has_other
      then assert false (* should not be possible to build that expression *)
      else begin match uc.known with
        | None -> assert false
        | Some (fl, cl) -> fun_val id fl cl, None
      end

    | Fixed v -> v, None

    | Predef_exn _
    | Env_field _ -> failwith "TODO !"

  let trivial_expr traces expr =
    let v, values_opt = op traces expr in
    match values_opt with
    | None -> bind_new_val traces v
    | Some values -> values, bind_val traces values v

  (* branching *)

  (* merge val1 and val2 in the context of traces1 and traces2 *)
  let merge_traces name (val1,traces1) (val2,traces2) =
    (* branching is correctly parenthesised *)
    assert(traces1.parent == traces2.parent);
    assert(traces1.name == name);
    assert(traces2.name == name);
    failwith "TODO"

  let append_traces parent traces =
    let merge_values bindings v1 v2 =
      let values, v = values_union traces v1 v2 in
      values, ValMap.add values v bindings
    in
    let merge_values_list bindings l1 l2 =
      List.fold_right2 (fun v1 v2 (bindings,l) ->
        let values, bindings = merge_values bindings v1 v2 in
        bindings, values::l) l1 l2 (bindings, []) in
    let merge_values_list_intmap bindings map1 map2 =
      let m = IntMap.merge (fun _ v1 v2 -> Some (v1,v2)) map1 map2 in
      IntMap.fold (fun id vls (bindings,map) ->
        match vls with
        | None, None -> bindings, map
        | None, Some v | Some v, None ->
          bindings, IntMap.add id v map
        | Some (l1,traces1), Some (l2,traces2) ->
          let _ = merge_values_list in failwith "TODO";
          (* let bindings, values_list = merge_values_list bindings l1 l2 in *)
          (* bindings, IntMap.add id values_list map) *)
      ) m (bindings, IntMap.empty) in
    let bindings = traces.bindings in

    let bindings, static_raises = merge_values_list_intmap
        bindings parent.static_raises traces.static_raises in

    let merge_bindings id b1 b2 =
      match b1,b2 with
      | None, b | b, None -> b
      | Some b1, Some b2 ->
        Format.fprintf ppf "%a@." ValId.print id;
        print_v ppf b1;
        print_v ppf b2;
        failwith "TODO" in

    let bindings = ValMap.merge merge_bindings parent.bindings bindings in

    let exceptions = match parent.exceptions, traces.exceptions with
      | None, None -> None
      | _ -> failwith "TODO" in

    { parent with
      escaping = ValSet.union parent.escaping traces.escaping;
      static_raises = static_raises;
      bindings;
      exceptions; }

  let merge_traces_list name parent l =
    match l with
    | [] -> parent.common.empty_values, parent
    | (values1,traces1)::tail ->
      let values, traces =
        List.fold_left (merge_traces name) (values1,traces1) tail in
      let traces = append_traces parent traces in
      values, traces

  type if_return =
    { ifso : traces option;
      ifnot : traces option }

  let if_branch name ~condition traces =
    let cond = to_bool (get traces condition) in
    let ifso_branch = cond.has_unknown || cond.known.true_ in
    let ifnot_branch = cond.has_unknown || cond.known.false_ in
    Printf.printf "if_branch: ifso %b ifnot %b\n%!" ifso_branch ifnot_branch;
    let branch b =
      if b
      then Some (new_branch name (If b) traces)
      else None in
    { ifso = branch ifso_branch; ifnot = branch ifnot_branch }

  let if_return name ~parent branches =
    merge_traces_list name parent branches

  type inside =
    { body : body;
      over_parameters : values list;
      bindings : values IdentMap.t }

  type apply =
    | Inside of inside
    | Return of values

  let call name ~func args traces =
    let aux f =
      let kind = Function (Some f.funct) in
      match f.partial_param with

      | Unknown_param s ->
        (* here, we don't known the real number of parameters
           so we cannot know for sure wether a function was applied or not *)
        let s = List.fold_right ValSet.add args s in
        add_unknown_param traces f.funct s;
        let values, traces =
          let new_traces = new_branch name kind traces in
          bind_new_val new_traces unknown_val in
        (Return values, traces)

      | Known_param partial_params ->
        let body = f.funct.function_body in
        let params = partial_params @ args in
        let fun_params = f.funct.function_param in

        let rec assoc_cut acc params fun_params = match params, fun_params with
          | [], [] -> [], acc, []
          | _::_, [] -> params, acc, []
          | [], _ :: _ -> [], acc, fun_params
          | ph::pt, fh::ft ->
            assoc_cut ((ph,fh)::acc) pt ft
        in
        let remaining_params, named_params, remaining_fun_params =
          assoc_cut [] params fun_params in

        match remaining_fun_params with
        | [] -> (* the function has been completely applied *)

          let bindings = f.closure in
          let bindings = List.fold_left (fun bindings (values,id) ->
              IdentMap.add id values bindings) bindings named_params in

          let kind = Function (Some f.funct) in
          let new_traces = new_branch name kind traces in

          Inside { body;
                   over_parameters = remaining_params;
                   bindings },
          new_traces

        | _ :: _ ->
          (* the function is partially applied: we only add the given
             parameters to the partial_parameters *)
          let v = fun_val' { f with partial_param = Known_param params } in
          let values, traces =
            let kind = Function (Some f.funct) in
            let new_traces = new_branch name kind traces in
            bind_new_val new_traces v in
          (Return values, traces)

    in
    let functions = to_function (get traces func) in
    let known = List.map aux functions.known in
    if functions.has_unknown
    then
      let kind = Function (None) in
      let traces = new_branch name kind traces in
      let values, traces = bind_new_val traces unknown_val in
      let traces = List.fold_left add_escaping traces args in
      (Return values, traces) :: known
    else known

  let return name ~parent branches =
    merge_traces_list name parent branches

end
