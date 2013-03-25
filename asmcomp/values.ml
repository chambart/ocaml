open Flambda

module ValId : Id = Id(Empty)
module ValMap = ExtMap(ValId)
module ValSet = ExtSet(ValId)
module ValTbl = ExtHashtbl(ValId)

type partial_parameters =
  | Known of (ValSet.t * ExprSet.t) list
  | Unknown of ValSet.t
      (* When there is an union between different lenght of parameters
         we fuse all the parameter as this one *)

type function_description =
  { fun_id: Ident.t option;
    closure_funs: FunId.t;
    (* !!! -> This ident must include current module name to avoid
       conflict with function imported from other modules *)
    closure_vars: ValSet.t IdentMap.t;
    (* already applied parameters: If this list is not empty then
       offset and field access are impossible *)
    partial_application: partial_parameters }

type block_description =
  { tag : int;
    fields : ValSet.t array }

type other_values =
  (* | Value_unoffseted_closure of function_description *)
  (*   (\* closure passed to Foffset: should never be unioned with another *)
  (*      kind of values *\) *)
  | Value_mutable
  | Value_string
  | Value_floatarray
  | Value_integer of int
  | Value_any_integer
  | Value_float
  | Value_boxed_int of Lambda.boxed_integer
  | Value_bottom      (* never terminates *)
  | Value_unknown_not_block
  (* an unknown value that can't contain any value *)
  | Value_unknown     (* unknown result *)
  | Value_external    (* value potentially contains an external *)
  | Value_none        (* no other case possible *)

(* This is the ident used to name closures with None in the fun_id field *)
let unspecified_closure = Ident.create_persistent "unspecified closure"

type values = {
  v_clos : function_description IdentMap.t FunMap.t;
  v_cstptr: IntSet.t;
  v_block: block_description IntMap.t;
  v_other: other_values;
}

let content_fun_desc fd acc =
  let acc = IdentMap.fold
      (fun _ set1 set2 -> ValSet.union set1 set2) fd.closure_vars acc in
  match fd.partial_application with
  | Unknown set -> ValSet.union acc set
  | Known l -> acc
    (* Those values are enclosed by the function: they can't be
       refered from outside the closure *)
    (* List.fold_left (fun acc (set,_) -> ValSet.union acc set) acc l *)

(* all values explicitely mentionned inside the value *)
let linked_values v =
  let acc = FunMap.fold (fun _ -> IdentMap.fold (fun _ -> content_fun_desc))
      v.v_clos ValSet.empty in
  IntMap.fold (fun _ b acc -> Array.fold_right ValSet.union b.fields acc)
    v.v_block acc

let equal_option f v1 v2 = match v1, v2 with
  | None, None -> true
  | Some v1, Some v2 -> f v1 v2
  | _ -> false

let equal_partial_appl v1 v2 = match v1, v2 with
  | Unknown s1, Unknown s2 -> ValSet.equal s1 s2
  | Known l1, Known l2 ->
    List.for_all2 (fun (s1,e1) (s2,e2) ->
        ValSet.equal s1 s2 && ExprSet.equal e1 e2) l1 l2
  | _, _ -> false

let equal_fundesc fd1 fd2 =
  equal_option Ident.same fd1.fun_id fd2.fun_id
  && FunId.equal fd1.closure_funs fd2.closure_funs
  && IdentMap.equal ValSet.equal fd1.closure_vars fd2.closure_vars
  && equal_partial_appl fd1.partial_application fd2.partial_application

let equal_block b1 b2 =
  b1.tag = b2.tag
  && Array.length b1.fields = Array.length b2.fields
  && List.for_all2 ValSet.equal (Array.to_list b1.fields)
    (Array.to_list b2.fields)

let equal v1 v2 =
  FunMap.equal (IdentMap.equal equal_fundesc) v1.v_clos v2.v_clos
  && IntSet.equal v1.v_cstptr v2.v_cstptr
  && IntMap.equal equal_block v1.v_block v2.v_block
  && v1.v_other = v2.v_other

let empty_value = {
  v_clos = FunMap.empty; v_cstptr = IntSet.empty;
  v_block = IntMap.empty;
  v_other = Value_none
}

let fun_desc_id fun_desc = match fun_desc.fun_id with
  | None -> unspecified_closure
  | Some i -> i

let unknown_value = { empty_value with v_other = Value_unknown }
let external_value = { empty_value with v_other = Value_external }

let value_unknown_not_block =
  { empty_value with v_other = Value_unknown_not_block }
let value_bottom = { empty_value with v_other = Value_bottom }
let value_mutable = { empty_value with v_other = Value_mutable }
let value_floatarray = { empty_value with v_other = Value_floatarray }
let value_string = { empty_value with v_other = Value_string }
let value_float = { empty_value with v_other = Value_float }
let value_boxedint bi = { empty_value with v_other = Value_boxed_int bi }

let value_int i = { empty_value with v_other = Value_integer i }
let value_any_int = { empty_value with v_other = Value_any_integer }
let value_constptr i = { empty_value with v_cstptr = IntSet.singleton i }
let value_constptrs s = { empty_value with v_cstptr = s }
let value_block tag fields =
  let block = { tag; fields } in
  { empty_value with v_block = IntMap.singleton tag block }

let value_bool b = let i = if b then 1 else 0 in
  value_constptr i

let empty_block tag length =
  value_block tag (Array.init length (fun _ -> ValSet.empty))

let value_closure clos_info =
  let fun_id = fun_desc_id clos_info in
  { empty_value with
    v_clos = FunMap.singleton clos_info.closure_funs
        (IdentMap.singleton fun_id clos_info) }

(* apply an offset to a closure returning an offseted closure *)
let set_closure_funid value fun_id fun_map =
  let aux _ fun_desc_map =
    if IdentMap.mem unspecified_closure fun_desc_map
    then
      let fun_desc = IdentMap.find unspecified_closure fun_desc_map in
      let ffunctions = FunMap.find fun_desc.closure_funs fun_map in
      assert(fun_desc.fun_id = None);
      assert(fun_desc.partial_application = Known []);
      if IdentMap.mem fun_id ffunctions.funs
      then Some (IdentMap.singleton fun_id
            { fun_desc with fun_id = Some fun_id })
      else None
    else None (* probably an impossible case... *)
  in
  let v_clos = FunMap.map_option aux value.v_clos in
  let v_block = IntMap.empty in
  let v_cstptr = IntSet.empty in
  let v_other = match value.v_other with
    | Value_external -> Value_external
    | Value_unknown -> Value_unknown
    | _ -> Value_none
  in
  { v_clos; v_block; v_other; v_cstptr }

let value_unit = value_constptr 0

let union_fundesc id f1 f2 =
  let id = if Ident.same id unspecified_closure
    then None
    else Some id in
  assert(f1.fun_id = id);
  assert(f2.fun_id = id);
  assert(f1.closure_funs = f2.closure_funs);
  let aux_closure_vars _ s1 s2 = match s1, s2 with
    | None, s | s, None -> s
    | Some s1, Some s2 -> Some (ValSet.union s1 s2) in
  (* When number of arguments preapplied is different, we merge it
     brutally as an Unknown: that way we keep arguments that escape
     when we call the function *)
  let partial_application =
    let merge s l =
      List.fold_left (fun set (arg,_) -> ValSet.union set arg) s l in
    match f1.partial_application, f2.partial_application with
    | Unknown s, Known l | Known l, Unknown s ->
      Unknown (merge s l)
    | Unknown s1, Unknown s2 ->
      Unknown (ValSet.union s1 s2)
    | Known l1, Known l2 ->
      if List.length l1 = List.length l2
      then
        let l = List.map2 (fun (v1,e1) (v2,e2) ->
            ValSet.union v1 v2, ExprSet.union e1 e2) l1 l2 in
        Known l
      else
        let s = merge ValSet.empty l1 in
        Unknown (merge s l2)
  in
  { f1 with
    partial_application;
    closure_vars = IdentMap.merge aux_closure_vars
        f1.closure_vars f2.closure_vars }

let union_closure c1 c2 =
  let aux_idmap id desc1 desc2 = match desc1, desc2 with
    | None, desc | desc, None -> desc
    | Some desc1, Some desc2 -> Some (union_fundesc id desc1 desc2) in
  let aux_funmap _ map1 map2 = match map1, map2 with
    | None, map | map, None -> map
    | Some map1, Some map2 -> Some (IdentMap.merge aux_idmap map1 map2) in
  FunMap.merge aux_funmap c1 c2

let union_cstptr s1 s2 = IntSet.union s1 s2

let union_block b1 b2 =
  let aux_tag tag a1 a2 = match a1, a2 with
    | None, a | a, None -> a
    | Some a1, Some a2 ->
      assert (a1.tag = tag);
      assert (a2.tag = tag);
      let len1 = Array.length a1.fields in
      let len2 = Array.length a2.fields in
      let len = max len1 len2 in
      let fields =
        Array.init len (fun i ->
          let set1 = if i >= len1 then ValSet.empty else a1.fields.(i) in
          let set2 = if i >= len2 then ValSet.empty else a2.fields.(i) in
          let res = ValSet.union set1 set2 in
          (* assert(not (ValSet.is_empty res)); *)
          res) in
      Some { tag; fields } in
  IntMap.merge aux_tag b1 b2

let union_other o1 o2 = match o1, o2 with
  (* | Value_unoffseted_closure f1, Value_unoffseted_closure f2 *)
  (*   -> Value_unoffseted_closure (union_fundesc None f1 f2) *)
  (* | Value_unoffseted_closure _, _ *)
  (* | _, Value_unoffseted_closure _ -> assert false *)
  | Value_mutable, Value_mutable -> Value_mutable
  | Value_string, Value_string -> Value_string
  | Value_floatarray, Value_floatarray -> Value_floatarray
  | Value_integer i, Value_integer j ->
    if i = j then Value_integer i
    else Value_any_integer
  | Value_integer _, Value_any_integer
  | Value_any_integer, Value_integer _
  | Value_any_integer, Value_any_integer -> Value_any_integer
  | Value_float, Value_float -> Value_float
  | Value_boxed_int bi1, Value_boxed_int bi2 when bi1 = bi2 ->
    Value_boxed_int bi1

  | Value_none, o
  | o, Value_none -> o
  | Value_bottom, o
  | o, Value_bottom -> o

  | Value_external, _ | _, Value_external -> Value_external
  | Value_unknown, _ | _, Value_unknown -> Value_unknown
  | (Value_mutable | Value_floatarray) , _
  | _ , (Value_mutable | Value_floatarray) -> Value_unknown
  | ( Value_unknown_not_block
    | Value_string
    | Value_integer _
    | Value_any_integer
    | Value_float
    | Value_boxed_int _),
    ( Value_unknown_not_block
    | Value_string
    | Value_integer _
    | Value_any_integer
    | Value_float
    | Value_boxed_int _) -> Value_unknown_not_block


let union v1 v2 =
  { v_clos = union_closure v1.v_clos v2.v_clos;
    v_cstptr = union_cstptr v1.v_cstptr v2.v_cstptr;
    v_block = union_block v1.v_block v2.v_block;
    v_other = union_other v1.v_other v2.v_other }

let list_union = function
  | [] -> empty_value
  | [t] -> t
  | t::q -> List.fold_left union t q

(* access to fields inside blocks *)
let field i v =
  let v_other = match v.v_other with
    | Value_external -> Some external_value
    | Value_unknown
    | Value_mutable -> Some unknown_value
    | Value_floatarray -> Some value_float
    | _ -> None
  in
  let aux _ block set =
    if i < Array.length block.fields
    then ValSet.union block.fields.(i) set
    else set in
  let values = IntMap.fold aux v.v_block ValSet.empty in
  v_other, values

(* update a block representation assuming the field has been set *)
let set_field i v new_val =
  let aux block =
    if i < Array.length block.fields
    then
      let fields = Array.copy block.fields in
      fields.(i) <- ValSet.union fields.(i) new_val;
      { block with fields }
    else block in
  { v with v_block = IntMap.map aux v.v_block }

(* access to fields inside closures *)
let env_field id v =
  let v_other = match v.v_other with
    | Value_external -> Some external_value
    | Value_unknown -> Some unknown_value
    | _ -> None
  in
  let aux_identmap _ clos set =
    if clos.partial_application = Known []
    then
      try
        let vals = IdentMap.find id clos.closure_vars in
        ValSet.union vals set
      with Not_found -> set
    else set in
  let aux_funmap _ fun_map set = IdentMap.fold aux_identmap fun_map set in
  let values = FunMap.fold aux_funmap v.v_clos ValSet.empty in
  v_other, values

let possible_bool_values v =
  let true_branch, false_branch =
    match v.v_other with
    | Value_any_integer
    | Value_unknown_not_block
    | Value_unknown
    | Value_external -> true, true
    | Value_integer i -> i <> 0, i = 0
    | Value_mutable
    | Value_string
    | Value_floatarray
    | Value_float
    | Value_boxed_int _ -> true, false
    | Value_bottom
    | Value_none -> false, false
  in
  let true_branch = true_branch || not (FunMap.is_empty v.v_clos) in
  let true_branch = true_branch || not (IntMap.is_empty v.v_block) in
  let true_branch =
    true_branch ||
    (not (IntSet.is_empty v.v_cstptr)
     && ((IntSet.min_elt v.v_cstptr <> 0)
         || (IntSet.max_elt v.v_cstptr <> 0))) in
  let false_branch = false_branch || IntSet.mem 0 v.v_cstptr in
  true_branch, false_branch

type if_result = True | False | TrueAndFalse | Neither

(* if evaluate to true if the parameter as a tagged value/pointer is
   not 1 (0 untagged). So the only false value is the integer/const
   pointer 0 *)
let if_value v =
  match possible_bool_values v with
  | false, false -> Neither
  | true, true -> TrueAndFalse
  | true, false -> True
  | false, true -> False

type switch_cases =
  | All_cases
  | Some_cases of IntSet.t

type switch_result = { consts : switch_cases; blocks : switch_cases }

let possible_constptrs v =
  match v.v_other with
  | Value_any_integer
  | Value_unknown_not_block
  | Value_unknown
  | Value_external -> All_cases
  | Value_integer i -> Some_cases  (IntSet.add i v.v_cstptr)
  | Value_mutable
  | Value_string
  | Value_floatarray
  | Value_float
  | Value_boxed_int _
  | Value_bottom
  | Value_none -> Some_cases v.v_cstptr

let possible_block_tags v =
  match v.v_other with
  | Value_mutable
  | Value_string
  | Value_floatarray
  | Value_float
  | Value_boxed_int _
    (* in a match we never check for those cases, it could change in
       the future -> safe choice: we know nothing about them *)
  | Value_unknown
  | Value_external -> All_cases
  | Value_integer _
  | Value_any_integer
  | Value_unknown_not_block
  | Value_bottom
  | Value_none ->
    Some_cases (IntMap.fold (fun i _ set -> IntSet.add i set) v.v_block IntSet.empty)

let switch_value v =
  { consts = possible_constptrs v;
    blocks = possible_block_tags v }

type simple_constant =
  | Cint of int
  | Cptr of int

let simple_constant v =
  match v.v_other with
  | Value_any_integer
  | Value_unknown_not_block
  | Value_unknown
  | Value_external
  | Value_mutable
  | Value_string
  | Value_floatarray
  | Value_float
  | Value_boxed_int _ -> None
  | Value_integer i -> Some (Cint i)
  | Value_bottom
  | Value_none ->
    if (IntMap.is_empty v.v_block) &&
       (FunMap.is_empty v.v_clos)
    then
      let c = IntSet.cardinal v.v_cstptr in
      if c = 1
      then Some (Cptr (IntSet.min_elt v.v_cstptr))
      else None (* what constant is 'empty' ? *)
    else
      None

type possible_closure =
  | No_function
  | One_function of function_description
  | Many_functions

let possible_closure v =
  match v.v_other with
  | Value_unknown
  | Value_external -> Many_functions
  | Value_any_integer
  | Value_unknown_not_block
  | Value_mutable
  | Value_string
  | Value_floatarray
  | Value_float
  | Value_boxed_int _
  | Value_integer _
  | Value_bottom
  | Value_none ->
    match FunMap.cardinal v.v_clos with
    | 0 -> No_function
    | 1 ->
      let _, clos_map = FunMap.choose v.v_clos in
      let functions = List.filter (fun (_,f) -> f.fun_id <> None)
          (IdentMap.bindings clos_map) in
      begin match functions with
        | [] -> No_function
        | [_, f] -> One_function f
        | _ -> Many_functions
      end
    | _ -> Many_functions


type array_kind =
  { int_kind : bool
  ; block_kind : bool
  ; float_kind : bool }

let no_kind =
  { int_kind = false
  ; block_kind = false
  ; float_kind = false }

let array_kind_of_element v =
  let kind = match v.v_other with
    | Value_unknown_not_block
    | Value_unknown
    | Value_external ->
      { int_kind = true; block_kind = true; float_kind = true }
    | Value_integer _
    | Value_any_integer ->
      { no_kind with int_kind = true }
    | Value_string
    | Value_floatarray
    | Value_boxed_int _
    | Value_mutable ->
      { no_kind with block_kind = true }
    | Value_float ->
      { no_kind with float_kind = true }
    | Value_bottom
    | Value_none ->
      no_kind
  in
  let kind =
    if not (FunMap.is_empty v.v_clos) || not (IntMap.is_empty v.v_block)
    then { kind with block_kind = true }
    else kind
  in
  let kind =
    if not (IntSet.is_empty v.v_cstptr)
    then { kind with int_kind = true }
    else kind
  in
  kind

let array_kind_of_array v =
  (* WARNING: !!! if in absint mutable are analysed as blocks this must change !!! *)
  match v.v_other with
  | Value_unknown
  | Value_external ->
    { int_kind = true; block_kind = true; float_kind = true }
  | Value_mutable ->
    { no_kind with int_kind = true; block_kind = true }
  | Value_floatarray ->
    { no_kind with float_kind = true }
  | Value_integer _
  | Value_any_integer
  | Value_string
  | Value_boxed_int _
  | Value_float
  | Value_unknown_not_block
  | Value_bottom
  | Value_none ->
    no_kind

let array_kind_intersection k1 k2 =
  { int_kind = k1.int_kind && k2.int_kind
  ; block_kind = k1.block_kind && k2.block_kind
  ; float_kind = k1.float_kind && k2.float_kind }

(* TODO: handle unreachable also to filter potential bad cases *)

let to_int v =
  if (FunMap.is_empty v.v_clos) && (IntSet.is_empty v.v_cstptr)
     && (IntMap.is_empty v.v_block)
  then match v.v_other with
    | Value_integer i -> Some i
    | _ -> None
  else None

let int_unop f l = match l with
  | [a] ->
    begin match to_int a with
      | Some i -> value_int (f i)
      | None -> value_any_int end
  | _ -> assert false

let int_binop f l = match l with
  | [a1; a2] ->
    begin match to_int a1, to_int a2 with
      | Some i1, Some i2 -> value_int (f i1 i2)
      | _, _ -> value_any_int end
  | _ -> assert false

let value_any_bool = value_constptrs (IntSet.add 1 (IntSet.singleton 0))

let int_bool_binop f l = match l with
  | [a1; a2] ->
    begin match to_int a1, to_int a2 with
      | Some i1, Some i2 -> value_bool (f i1 i2)
      | _, _ -> value_any_bool end
  | _ -> assert false

(* TODO: add possible exceptions *)
let not_zero_int_binop f l = match l with
  | [a1; a2] ->
    begin match to_int a1, to_int a2 with
      | Some i1, Some i2 ->
        if i2 = 0 then value_bottom
        else value_int (f i1 i2)
      | _, _ -> value_any_int end
  | _ -> assert false

let cartesian_product l1 l2 =
  List.fold_left (fun acc v1 ->
    List.fold_left (fun acc v2 -> (v1,v2) :: acc) acc l2)
    [] l1

let bool_binop f l = match l with
  | [a1; a2] ->
    let bool_list (t,f) =
      let l = if t then [true] else [] in
      if f then false::l else l in
    let l1 = bool_list (possible_bool_values a1) in
    let l2 = bool_list (possible_bool_values a2) in
    let l = cartesian_product l1 l2 in
    let set =
      List.fold_left (fun acc (v1,v2) ->
        let r = if f v1 v2 then 1 else 0 in
        IntSet.add r acc) IntSet.empty l in
    value_constptrs set
  | _ -> assert false

let bool_possible (tr,fa) =
  let s = if tr
    then IntSet.singleton 1
    else IntSet.empty in
  let s = if fa
    then IntSet.add 0 s
    else s in
  value_constptrs s

let bool_not l = match l with
  | [a] ->
    let (tr,fa) = possible_bool_values a in
    bool_possible (fa,tr)
  | _ -> assert false

let isint l = match l with
  | [v] ->
    let isint, isnot_int = match v.v_other with
      | Value_any_integer
      | Value_integer _ -> true, false
      | Value_unknown_not_block
      | Value_unknown
      | Value_external -> true, true
      | Value_mutable
      | Value_string
      | Value_floatarray
      | Value_float
      | Value_boxed_int _ -> false, true
      | Value_bottom
      | Value_none -> false, false in
    let isnot_int =
      isnot_int ||
      not (FunMap.is_empty v.v_clos) ||
      not (IntMap.is_empty v.v_block) in
    let isint =
      isint ||
      not (IntSet.is_empty v.v_cstptr)
    in
    bool_possible (isint, isnot_int)

  | _ -> assert false

(* TODO: unreachable cases *)
let prim p l =
  let plen = List.length l in
  let open Lambda in
  match p with
  | Paddint -> int_binop (+) l
  | Psubint -> int_binop (-) l
  | Pmulint -> int_binop ( * ) l
  | Pdivint -> not_zero_int_binop (/) l
  | Pmodint -> not_zero_int_binop (mod) l
  | Pandint -> int_binop (land) l
  | Porint -> int_binop (lor) l
  | Pxorint -> int_binop (lxor) l
  | Plslint -> int_binop (lsl) l
  | Plsrint -> int_binop (lsr) l
  | Pasrint -> int_binop (asr) l
  | Pisout -> int_bool_binop (fun x y -> (not (y >= 0 && y <= x))) l
  | Pintcomp cmp ->
    let f x y = match cmp with
        | Ceq -> x = y
        | Cneq -> x <> y
        | Clt -> x < y
        | Cgt -> x > y
        | Cle -> x <= y
        | Cge -> x >= y in
    int_bool_binop f l

  | Pnegint -> int_unop (fun x -> -x) l
  | Pbswap16 -> int_unop (fun x ->
                    ((x land 0xff) lsl 8) lor
                      ((x land 0xff00) lsr 8)) l
  | Poffsetint y -> int_unop ((+) y) l


  | Pidentity -> begin match l with
      (* cas particulier qui devrait renvoyer dirrectement les id des
         paramêtre, pas leur valeur *)
      | [a] -> a
      | _ -> assert false
    end

  | Pignore -> begin match l with
      | [_] -> value_unit
      | _ -> assert false
    end
  | Pfloatfield _ -> begin match l with
      | [_] -> value_float
      | _ -> assert false
    end
  | Psetfloatfield _ -> begin match l with
      | [_] -> value_unit
      | _ -> assert false
    end

  | Psequand -> bool_binop (&&) l
  | Psequor -> bool_binop (||) l
  | Pnot -> bool_not l

  | Pintoffloat ->
    assert(plen = 1);
    value_any_int
  | Pfloatofint ->
    assert(plen = 1);
    value_float

  | Pnegfloat | Pabsfloat ->
    assert(plen = 1);
    value_float

  | Paddfloat | Psubfloat | Pmulfloat | Pdivfloat ->
    assert(plen = 2);
    value_float

  | Pfloatcomp _ ->
    assert(plen = 2);
    value_any_bool

  | Pstringlength ->
    assert(plen = 1);
    value_any_int

  | Pstringrefu | Pstringrefs ->
    assert(plen = 2);
    value_any_int

  | Pstringsetu | Pstringsets ->
    assert(plen = 3);
    value_unit

  | Parraylength _ ->
    assert(plen = 1);
    value_any_int

  | Parrayrefu kind | Parrayrefs kind ->
    assert(plen = 2);
    (match kind with
     | Pgenarray | Paddrarray -> unknown_value
     | Pintarray -> value_any_int
     | Pfloatarray -> value_float)

  | Parraysets _ | Parraysetu _ ->
    assert(plen = 3);
    value_unit

  | Pisint -> isint l

  | Pbittest ->
    assert(plen = 2);
    value_any_bool

  | Pbintofint bi | Pintofbint bi | Pcvtbint (_, bi) | Pnegbint bi
  | Pbbswap bi ->
    assert(plen = 1);
    value_boxedint bi

  | Paddbint bi | Psubbint bi | Pmulbint bi | Pdivbint bi
  | Pmodbint bi | Pandbint bi | Porbint bi | Pxorbint bi
  | Plslbint bi | Plsrbint bi | Pasrbint bi ->
    assert(plen = 2);
    value_boxedint bi

  | Pbintcomp _ ->
    assert(plen = 2);
    value_any_bool

  | Pbigarrayref (_,_,kind,_) ->
    assert(plen = 2);
    (match kind with
     | Pbigarray_unknown -> value_unknown_not_block
     | Pbigarray_float32 | Pbigarray_float64 -> value_float
     | Pbigarray_sint8 | Pbigarray_uint8
     | Pbigarray_sint16 | Pbigarray_uint16
     | Pbigarray_caml_int -> value_any_int
     | Pbigarray_int32 -> value_boxedint Pint32
     | Pbigarray_int64 -> value_boxedint Pint64
     | Pbigarray_native_int -> value_boxedint Pnativeint
     | Pbigarray_complex32 | Pbigarray_complex64 -> value_floatarray)

  | Pbigarrayset _ ->
    assert(plen = 3);
    value_unit

  | Pbigarraydim _ ->
    assert(plen = 1);
    value_any_int

  | Pctconst c ->
    begin
      match c with
      | Big_endian -> value_bool Arch.big_endian
      | Word_size -> value_int (8*Arch.size_int)
      | Ostype_unix -> value_bool (Sys.os_type = "Unix")
      | Ostype_win32 -> value_bool (Sys.os_type = "Win32")
      | Ostype_cygwin -> value_bool (Sys.os_type = "Cygwin")
    end

  | Pstring_load_16 _ | Pbigstring_load_16 _ ->
    value_any_int

  | Pbigstring_load_32 _ | Pstring_load_32 _ ->
    value_boxedint Pint32

  | Pstring_load_64 _ | Pbigstring_load_64 _ ->
    value_boxedint Pint64

  | Pbigstring_set_16 _ | Pbigstring_set_32 _ | Pbigstring_set_64 _
  | Pstring_set_16 _ | Pstring_set_32 _ | Pstring_set_64 _ ->
    value_unit

  | Poffsetref _ ->
    value_unit

  | Prevapply _ | Pdirapply _ | Pgetglobal _ | Psetglobal _
  | Pmakeblock _ | Pfield _ | Psetfield _ | Pduprecord _
  | Plazyforce | Pccall _ | Praise | Pmakearray _ ->
    assert false (* handled somewhere else *)

let const_base b = let open Asttypes in match b with
  | Const_int i -> value_int i
  | Const_char c -> value_int (Char.code c)
  | Const_string _ -> value_string
  | Const_float _ -> value_float
  | Const_int32 _ -> value_boxedint Lambda.Pint32
  | Const_int64 _ -> value_boxedint Lambda.Pint64
  | Const_nativeint _ -> value_boxedint Lambda.Pnativeint

let const = function
  | Fconst_base c -> const_base c
  | Fconst_pointer i -> value_constptr i
  | Fconst_float_array _ -> value_floatarray
  | Fconst_immstring _ -> value_string
