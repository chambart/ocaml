open Asttypes
open Data_dependency
open Flambda

module type T = sig
  type t
  val equal : t -> t -> bool
end

module type Domain = sig

  (* type v *)

  type t

  val empty : unit -> t

  (* val singleton : v -> t *)

  val union : t -> t -> t

end

module SingletonDom(T:T) = struct
  type v = T.t
  type t =
    | Empty
    | Singleton of T.t
    | Any

  let empty () = Empty
  let singleton v = Singleton v
  let union s1 s2 = match s1, s2 with
    | Empty, s | s, Empty -> s
    | Any, _ | _, Any -> Any
    | Singleton v1, Singleton v2 ->
      if T.equal v1 v2
      then s1
      else Any

end

module IntT = struct
  type t = int
  let equal (i1:int) i2 = i1 = i2
  let compare (i1:int) i2 = Pervasives.compare i1 i2
end
module IntDom = SingletonDom(IntT)
module CstPtrDom = Set.Make(IntT)

module ValueDom = struct

  type values = ValId.t

  type funct =
    { function_id : Ident.t;
      function_kind : Lambda.function_kind;
      function_param: Ident.t list }

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
      fields : ValSet.t list;
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

  type t = {
    v_clos : intern_funct IdentMap.t;
    v_cstptr: IntSet.t;
    v_block: intern_block IntMap.t;
    v_other: other_values;
  }

  let empty_val = {
    v_clos = IdentMap.empty;
    v_cstptr = IntSet.empty;
    v_block = IntMap.empty;
    v_other = Value_none;
  }

  (* equality *)

  (* like List.for_all2 but returns false when the list have different length *)
  let rec equal_forall f l1 l2 = match l1, l2 with
    | [], [] -> true
    | [], _ | _, [] -> false
    | t1::q1, t2::q2 ->
      f t1 t2 &&
      equal_forall f q1 q2

  let equal_partial_param p1 p2 = match p1, p2 with
    | Unknown_param s1, Unknown_param s2 -> ValSet.equal s1 s2
    | Known_param l1, Known_param l2 ->
      List.length l1 = List.length l2 &&
      List.for_all2 ValId.equal l1 l2
    | _ -> false

  let equal_clos m1 m2 =
    let aux (id1,funct1) (id2,funct2) =
      Idt.equal id1 id2 &&
      (* funct1.funct = funct2.funct && *) (* should be the same *)
      equal_partial_param funct1.partial_param funct2.partial_param &&
      IdentMap.equal ValId.equal funct1.closure funct2.closure in
    let b1 = IdentMap.bindings m1 in
    let b2 = IdentMap.bindings m2 in
    equal_forall aux b1 b2

  let equal_block m1 m2 =
    let aux (tag1,l1) (tag2,l2) =
      tag1 = tag2 &&
      l1.mut = l2.mut &&
      equal_forall ValSet.equal l1.fields l2.fields in
    let b1 = IntMap.bindings m1 in
    let b2 = IntMap.bindings m2 in
    equal_forall aux b1 b2

  let equal_cstptr v1 v2 =
    IntSet.equal v1 v2

  let equal_other v1 v2 = match v1, v2 with
    | Value_none, Value_none
    | Value_unknown, Value_unknown
    | Value_any_integer, Value_any_integer
      -> true

    | Value_unoffseted_closure (funs1, fv1),
        Value_unoffseted_closure  (funs2, fv2) ->
      (* List.for_all2 Ident.same funs1 funs2 && *) (* should be the same *)
      IdentMap.equal ValId.equal fv1 fv2

    | Value_integer i, Value_integer j -> i = j

    | Value_none, _| _, Value_none
    | Value_unknown, _| _, Value_unknown
    | Value_integer _, _ | _, Value_integer _
    | Value_any_integer, _| _, Value_any_integer
      -> false

    | _ -> failwith "TODO equal other"

  let equal v1 v2 =
    equal_other v1.v_other v2.v_other &&
    equal_cstptr v1.v_cstptr v2.v_cstptr &&
    equal_clos v1.v_clos v2.v_clos &&
    equal_block v1.v_block v2.v_block

  (* is_empty *)

  let is_empty v =
    IdentMap.is_empty v.v_clos &&
    IntSet.is_empty v.v_cstptr &&
    IntMap.is_empty v.v_block &&
    v.v_other = Value_none

  (* union *)

  let union v1 v2 =
    failwith "TODO union"

  let union_list l = match l with
    | [] -> empty_val
    | [t] -> t
    | t::q -> List.fold_left union t q

  (* value_building *)

  let empty () = empty_val

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

  (* value querying *)

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

  let single_set_int ans =
    if ans.has_unknown
    then Multiple
    else
      let c = IntSet.cardinal ans.known in
      match c with
      | 0 -> Empty
      | 1 -> Singleton (IntSet.choose ans.known)
      | _ -> Multiple

  let addint v1 v2 =
    let v1 = single_set_int (to_int (union_list v1)) in
    let v2 = single_set_int (to_int (union_list v2)) in
    match v1, v2 with
    | Empty, _ | _, Empty -> empty_val
    | Singleton i1, Singleton i2 -> int_val (i1 + i2)
    | Multiple, _ | _, Multiple -> any_int_val

  let mulint v1 v2 =
    let v1 = single_set_int (to_int (union_list v1)) in
    let v2 = single_set_int (to_int (union_list v2)) in
    match v1, v2 with
    | Empty, _ | _, Empty -> empty_val
    | Singleton i1, Singleton i2 -> int_val (i1 * i2)
    | Multiple, _ | _, Multiple -> any_int_val

  let makeblock tag mut fields =
    block_val tag mut (List.map ValSet.singleton fields)

  let const_base b = let open Asttypes in match b with
    | Const_int i -> int_val i
    | Const_char c -> int_val (Char.code c)
    | Const_string _ -> string_val
    | Const_float _ -> float_val
    | Const_int32 _ -> boxedint_val Lambda.Pint32
    | Const_int64 _ -> boxedint_val Lambda.Pint64
    | Const_nativeint _ -> boxedint_val Lambda.Pnativeint

  let constant = function
    | Fconst_base c -> const_base c
    | Fconst_pointer i -> constptr_val i
    | Fconst_float_array _ -> floatarray_val
    | Fconst_immstring _ -> string_val

  let sets_union l = match l with
    | [] -> ValSet.empty
    | [t] -> t
    | t::q -> List.fold_left ValSet.union t q

  let field n values =
    (* TODO: do the union at the end rather than at the beginning *)
    let v = to_block (union_list values) in
    let get_field i v =
      if List.length v.fields <= i
      then ValSet.empty
      else List.nth v.fields i in
    let l = List.map (fun (_,v) -> get_field n v) (IntMap.bindings v.known) in
    let s = sets_union l in
    let field_v =
      if v.has_unknown
      then Some unknown_val
      else None
    in
    field_v, s

  let offset id values =
    let uc = to_unoffseted_closure (union_list values) in
    if uc.has_unknown || uc.has_other
    then assert false (* should not be possible to build that expression *)
    else match uc.known with
      | None -> assert false
      | Some (fl, cl) -> fun_val id fl cl

  let to_fun func = to_function (union_list func)

  type call =
    | Value of t (* partial call and unknown *)
    | Complete of funct * v list * v list
    | Unknown_call of intern_funct * ValSet.t

  let splitn n l =
    let rec aux n l acc =
      if n = 0
      then List.rev acc, l
      else match l with
        | [] -> assert false
        | t::q -> aux (n-1) q (t::acc) in
    aux n l []

  let apply func param =
    let func = to_function (union_list func) in
    let aux_unknown fn set = failwith "TODO: unknown params" in
    let aux fn =
      match fn.partial_param with
      | Unknown_param set -> aux_unknown fn set
      | Known_param preapplied ->
        let arity = List.length fn.funct.function_param in
        let param = preapplied @ param in
        if arity > List.length param
        then
          Value (fun_val' { fn with partial_param = Known_param param })
        else
          let applied, over = splitn arity param in
          Complete (fn.funct,applied,over)
    in
    let ret = List.map aux func.known in
    let ret =
      if func.has_other || func.has_unknown
      then (Value unknown_val) :: ret
      else ret
    in
    ret

  let is_bool b values =
    let cond = to_bool (union_list values) in
    if b
    then cond.has_unknown || cond.known.true_ || cond.has_other
    else cond.has_unknown || cond.known.false_ || cond.has_other

  (* printing *)

  module Print_values = struct
    open Format

    type print_work = {
      getter : ValId.t -> t;
      mutable done_set : ValSet.t;
      todo : ValId.t Queue.t;
    }

    let init_work getter = {
      getter;
      done_set = ValSet.empty;
      todo = Queue.create ();
    }

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
        List.iter (fun set -> print_set ?traces ppf set) v.fields;
        fprintf ppf "@]]")
        map

    and print_set ?traces ppf s =
      ValSet.iter (fprintf ppf "%a@ " (print_vid ?traces)) s

    and print_list ?traces ppf l =
      List.iter (fprintf ppf "%a@ " (print_vid ?traces)) l

    and print_vid ?traces ppf v =
      ValId.print ppf v;
      match traces with
      | None -> ()
      | Some traces -> add_traces traces v

    and print_v ?traces ppf v =
      Format.fprintf ppf "(@[<1>%a@ %a@ %a@ %a@ @])"
        (print_clos ?traces) v.v_clos
        (print_block ?traces) v.v_block
        print_cstptr v.v_cstptr
        print_other v.v_other

    and print_values getter getter_dep ppf values =
      let work = init_work getter in
      ValSet.iter (add_traces work) values;
      while not (Queue.is_empty work.todo) do
        let vid = Queue.take work.todo in
        let v = try Some (getter vid) with Not_found -> None in
        let v_dep = getter_dep vid in
        (match v with
        | Some v -> Format.printf "%a %a@." ValId.print vid (print_v ~traces:work) v
        | None -> ());
        (match v_dep with
         | Some s -> Format.printf "%a -> %a@." ValId.print vid (print_set ~traces:work) s
         | None -> ());
      done

    and print_value getter getter_dep ppf value =
      print_values getter getter_dep ppf (ValSet.singleton value)

    and add_traces work value =
      if not (ValSet.mem value work.done_set)
      then begin
        work.done_set <- ValSet.add value work.done_set;
        Queue.push value work.todo
      end

  end
  include Print_values

end
