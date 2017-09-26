(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2017 OCamlPro SAS                                    *)
(*   Copyright 2014--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module F0 = Flambda0
module Named = F0.Named

module Float = Numbers.Float
module Int32 = Numbers.Int32
module Int64 = Numbers.Int64

include F0.Flambda_type

let var (t : t) =
  match t with
  | Value ty -> ty.var
  | Naked_immediate ty -> ty.var
  | Naked_float ty -> ty.var
  | Naked_int32 ty -> ty.var
  | Naked_int64 ty -> ty.var
  | Naked_nativeint ty -> ty.var

let symbol (t : t) =
  match t with
  | Value ty -> ty.symbol
  | Naked_immediate ty -> ty.symbol
  | Naked_float ty -> ty.symbol
  | Naked_int32 ty -> ty.symbol
  | Naked_int64 ty -> ty.symbol
  | Naked_nativeint ty -> ty.symbol

(*
let ty_value_is_bottom (ty : ty_value) =
  match maybe_import_value_type ~import_type ty with
  | Ok Bottom -> true
  | Ok Unknown | Ok (Ok _) | Treat_as_unknown -> false

(* CR mshinwell: Shouldn't this just use [Flambda_kind].lambda_value_kind? *)
let refine_value_kind t (kind : Lambda.value_kind) : Lambda.value_kind =
  match descr t with
  | Boxed_number (Float, { descr = Unboxed_float _; _ }) -> Pfloatval
  | Boxed_number (Int32, { descr = Unboxed_int32 _; _ }) -> Pboxedintval Pint32
  | Boxed_number (Int64, { descr = Unboxed_int64 _; _ }) -> Pboxedintval Pint64
  | Boxed_number (Nativeint, { descr = Unboxed_nativeint _; _ }) ->
    Pboxedintval Pnativeint
  | Union union ->
    begin match Unionable.flatten union with
    | Ok (Int _) -> Pintval
    | _ -> kind
    end
  | _ -> kind
*)

let rename_variables ~importer t ~f =
  clean ~importer t (fun var -> Available_different_name (f var))

let unresolved_symbol sym =
  (* CR mshinwell: check with Pierre about this comment.  I suspect
     this is irrelevant now closure freshening has been removed *)
  (* CR pchambart: This won't be strictly needed, but I remembered
     using that to track some problems. This might helps generate more
     precise inlining report: "couldn't inline because this cmx file
     was missing" *)
  (* We don't know anything, but we must remember that it comes
     from another compilation unit in case it contains a closure. *)
  any_value Must_scan (Unresolved_value (Symbol sym))

let this_tagged_immediate_named n : Named.t * t =
  Const (Tagged_immediate n), this_tagged_immediate n

let this_tagged_bool_named b : Named.t * t =
  let imm =
    if b then Immediate.bool_true
    else Immediate.bool_false
  in
  Const (Tagged_immediate imm), this_tagged_immediate imm

let this_boxed_float_named f : Named.t * t =
  Allocated_const (Float f), this_boxed_float f

let this_boxed_int32_named n : Named.t * t =
  Allocated_const (Int32 n), this_boxed_int32 n

let this_boxed_int64_named n : Named.t * t =
  Allocated_const (Int64 n), this_boxed_int64 n

let this_boxed_nativeint_named n : Named.t * t =
  Allocated_const (Nativeint n), this_boxed_nativeint n

let this_untagged_immediate_named n : Named.t * t =
  Const (Untagged_immediate n), this_naked_immediate n

let this_naked_float_named f : Named.t * t =
  Const (Naked_float f), this_naked_float f

let this_naked_int32_named n : Named.t * t =
  Const (Naked_int32 n), this_naked_int32 n

let this_naked_int64_named n : Named.t * t =
  Const (Naked_int64 n), this_naked_int64 n

let this_naked_nativeint_named n : Named.t * t =
  Const (Naked_nativeint n), this_naked_nativeint n

let is_bottom ~importer (t : t) =
  let module I = (val importer : Importer) in
  match t with
  | Value ty ->
    let ty = I.import_value_type_as_resolved_ty_value ty in
    begin match ty.descr with
    | Bottom -> true
    | Unknown _ | Ok _ -> false
    end
  | Naked_immediate ty ->
    let ty = I.import_naked_immediate_type_as_resolved_ty_naked_immediate ty in
    begin match ty.descr with
    | Bottom -> true
    | Unknown _ | Ok _ -> false
    end
  | Naked_float ty ->
    let ty = I.import_naked_float_type_as_resolved_ty_naked_float ty in
    begin match ty.descr with
    | Bottom -> true
    | Unknown _ | Ok _ -> false
    end
  | Naked_int32 ty ->
    let ty = I.import_naked_int32_type_as_resolved_ty_naked_int32 ty in
    begin match ty.descr with
    | Bottom -> true
    | Unknown _ | Ok _ -> false
    end
  | Naked_int64 ty ->
    let ty = I.import_naked_int64_type_as_resolved_ty_naked_int64 ty in
    begin match ty.descr with
    | Bottom -> true
    | Unknown _ | Ok _ -> false
    end
  | Naked_nativeint ty ->
    let ty = I.import_naked_nativeint_type_as_resolved_ty_naked_nativeint ty in
    begin match ty.descr with
    | Bottom -> true
    | Unknown _ | Ok _ -> false
    end

let known ~importer (t : t) =
  let module I = (val importer : Importer) in
  match t with
  | Value ty ->
    let ty = I.import_value_type_as_resolved_ty_value ty in
    begin match ty.descr with
    | Bottom | Ok _ -> true
    | Unknown _ -> false
    end
  | Naked_immediate ty ->
    let ty = I.import_naked_immediate_type_as_resolved_ty_naked_immediate ty in
    begin match ty.descr with
    | Bottom | Ok _ -> true
    | Unknown _ -> false
    end
  | Naked_float ty ->
    let ty = I.import_naked_float_type_as_resolved_ty_naked_float ty in
    begin match ty.descr with
    | Bottom | Ok _ -> true
    | Unknown _ -> false
    end
  | Naked_int32 ty ->
    let ty = I.import_naked_int32_type_as_resolved_ty_naked_int32 ty in
    begin match ty.descr with
    | Bottom | Ok _ -> true
    | Unknown _ -> false
    end
  | Naked_int64 ty ->
    let ty = I.import_naked_int64_type_as_resolved_ty_naked_int64 ty in
    begin match ty.descr with
    | Bottom | Ok _ -> true
    | Unknown _ -> false
    end
  | Naked_nativeint ty ->
    let ty = I.import_naked_nativeint_type_as_resolved_ty_naked_nativeint ty in
    begin match ty.descr with
    | Bottom | Ok _ -> true
    | Unknown _ -> false
    end

let useful ~importer (t : t) =
  let module I = (val importer : Importer) in
  match t with
  | Value ty ->
    let ty = I.import_value_type_as_resolved_ty_value ty in
    begin match ty.descr with
    | Ok _ -> true
    | Bottom | Unknown _ -> false
    end
  | Naked_immediate ty ->
    let ty = I.import_naked_immediate_type_as_resolved_ty_naked_immediate ty in
    begin match ty.descr with
    | Ok _ -> true
    | Bottom | Unknown _ -> false
    end
  | Naked_float ty ->
    let ty = I.import_naked_float_type_as_resolved_ty_naked_float ty in
    begin match ty.descr with
    | Ok _ -> true
    | Bottom | Unknown _ -> false
    end
  | Naked_int32 ty ->
    let ty = I.import_naked_int32_type_as_resolved_ty_naked_int32 ty in
    begin match ty.descr with
    | Ok _ -> true
    | Bottom | Unknown _ -> false
    end
  | Naked_int64 ty ->
    let ty = I.import_naked_int64_type_as_resolved_ty_naked_int64 ty in
    begin match ty.descr with
    | Ok _ -> true
    | Bottom | Unknown _ -> false
    end
  | Naked_nativeint ty ->
    let ty = I.import_naked_nativeint_type_as_resolved_ty_naked_nativeint ty in
    begin match ty.descr with
    | Ok _ -> true
    | Bottom | Unknown _ -> false
    end

let all_not_useful ~importer ts =
  List.for_all (fun t -> not (useful ~importer t)) ts


(*
let is_boxed_float t =
  match descr t with
  | Boxed_number (Float, t) ->
    begin match descr t with
    | Unboxed_float _ -> true
    | _ -> false
    end
  | Float_array _ | Unknown _ | Bottom | Union _
  | Immutable_string _ | Mutable_string _
  | Set_of_closures _ | Closure _ | Load_lazily _ | Boxed_number _
  | Unboxed_float _ | Unboxed_int32 _ | Unboxed_int64 _
  | Unboxed_nativeint _ -> false

let is_float_array t =
  match descr t with
  | Float_array _ -> true
  | Unknown _ | Bottom | Union _
  | Immutable_string _ | Mutable_string _
  | Set_of_closures _ | Closure _ | Load_lazily _ | Boxed_number _
  | Unboxed_float _ | Unboxed_int32 _ | Unboxed_int64 _
  | Unboxed_nativeint _ -> false

let invalid_to_mutate t =
  match descr t with
  | Union unionable -> Unionable.invalid_to_mutate unionable
  | Mutable_string _
  (* CR mshinwell: Split Float_array into Immutable and Mutable? *)
  | Float_array { contents = Contents _; size = _; } -> false
  | Unknown _ | Bottom | Immutable_string _
  | Float_array { contents = Unknown_or_mutable; size = _; }
  | Set_of_closures _ | Closure _ | Load_lazily _ | Boxed_number _
  | Unboxed_float _ | Unboxed_int32 _ | Unboxed_int64 _
  | Unboxed_nativeint _ -> true

let type_for_bound_var (set_of_closures : set_of_closures) var =
  try Var_within_closure.Map.find var set_of_closures.bound_vars
  with Not_found ->
    Misc.fatal_errorf "The set-of-closures type %a@ does not \
        bind the variable %a@.%s@."
      print_set_of_closures set_of_closures
      Var_within_closure.print var
      (Printexc.raw_backtrace_to_string (Printexc.get_callstack max_int))

(* Given a set-of-closures type and a closure ID, apply any
   freshening specified in the type to the closure ID, and return
   that new closure ID.  A fatal error is produced if the new closure ID
   does not correspond to a function declaration in the given type. *)
let freshen_and_check_closure_id (set_of_closures : set_of_closures)
      (closure_id : Closure_id.Set.t) =
  let closure_id =
    Freshening.Project_var.apply_closure_ids
      set_of_closures.freshening closure_id
  in
  Closure_id.Set.iter (fun closure_id ->
    try
      ignore (F0.Function_declarations.find closure_id
        set_of_closures.function_decls)
    with Not_found ->
      Misc.fatal_error (Format.asprintf
        "Function %a not found in the set of closures@ %a@.%a@."
        Closure_id.print closure_id
        print_set_of_closures set_of_closures
        F0.Function_declarations.print set_of_closures.function_decls))
    closure_id;
  closure_id

let physically_same_values (types : t list) =
  match types with
  | [] | [_] | _ :: _ :: _ :: _ ->
    Misc.fatal_error "wrong number of arguments for equality"
  | [a1; a2] ->
    (* N.B. The following would be incorrect if the variables are not
       bound in the environment:
       match a1.var, a2.var with
       | Some v1, Some v2 when Variable.equal v1 v2 -> true
       | _ -> ...
    *)
    match a1.symbol, a2.symbol with
    | Some (s1, None), Some (s2, None) -> Symbol.equal s1 s2
    | Some (s1, Some f1), Some (s2, Some f2) -> Symbol.equal s1 s2 && f1 = f2
    | _ -> false

let physically_different_values (types : t list) =
  let rec definitely_different (arg1 : t) (arg2 : t) =
    let module Int = Numbers.Int in
    let module Immediate = Unionable.Immediate in
    let immediates_different s1 s2 =
      (* The frontend isn't precise about "int" and "const pointer", for
         example generating "(!= b/1006 0)" for a match against a bool, which
         is a "const pointer".  The same presumably might happen with "char".
         As such for we treat immediates whose runtime representations are
         the same as equal. *)
      let s1 =
        Immediate.Set.fold (fun imm s1 ->
            Int.Set.add (Immediate.represents imm) s1)
          s1 Int.Set.empty
      in
      let s2 =
        Immediate.Set.fold (fun imm s2 ->
            Int.Set.add (Immediate.represents imm) s2)
          s2 Int.Set.empty
      in
      Int.Set.is_empty (Int.Set.inter s1 s2)
    in
    let blocks_different b1 b2 =
      let tags1 = Tag.Scannable.Map.keys b1 in
      let tags2 = Tag.Scannable.Map.keys b2 in
      let overlapping_tags = Tag.Scannable.Set.inter tags1 tags2 in
      Tag.Scannable.Set.exists (fun tag ->
          let fields1 = Tag.Scannable.Map.find tag b1 in
          let fields2 = Tag.Scannable.Map.find tag b2 in
          Array.length fields1 <> Array.length fields2
            || Misc.Stdlib.Array.exists2 definitely_different fields1 fields2)
        overlapping_tags
    in
    match arg1.descr, arg2.descr with
    | Unknown _, _ | _, Unknown _
    (* CR mshinwell: Should [Load_lazily] be an error here?  What about for the
       reification functions below?  [invalid_to_mutate] above has an
       assertion failure for this. *)
    | Load_lazily _, _ | _, Load_lazily _
    | Bottom, _ | _, Bottom -> false
    | Union (Immediates s1), Union (Immediates s2) ->
      immediates_different s1 s2
    | Union (Blocks b1), Union (Blocks b2) ->
      blocks_different b1 b2
    | Union (Blocks_and_immediates (b1, imms1)),
      Union (Blocks_and_immediates (b2, imms2)) ->
      immediates_different imms1 imms2 || blocks_different b1 b2
    | Union _, Union _ -> false
    | Union _, _ | _, Union _ -> true
    | Unboxed_float fs1, Unboxed_float fs2 ->
      Float.Set.is_empty (Float.Set.inter fs1 fs2)
    | Unboxed_float _, _ | _, Unboxed_float _ -> true
    | Unboxed_int32 ns1, Unboxed_int32 ns2 ->
      Int32.Set.is_empty (Int32.Set.inter ns1 ns2)
    | Unboxed_int32 _, _ | _, Unboxed_int32 _ -> true
    | Unboxed_int64 ns1, Unboxed_int64 ns2 ->
      Int64.Set.is_empty (Int64.Set.inter ns1 ns2)
    | Unboxed_int64 _, _ | _, Unboxed_int64 _ -> true
    | Unboxed_nativeint ns1, Unboxed_nativeint ns2 ->
      Nativeint.Set.is_empty (Nativeint.Set.inter ns1 ns2)
    | Unboxed_nativeint _, _ | _, Unboxed_nativeint _ -> true
    | Boxed_number (kind1, t1), Boxed_number (kind2, t2) ->
      (not (Boxed_number_kind.equal kind1 kind2))
        || definitely_different t1 t2
    | Boxed_number _, _ | _, Boxed_number _ -> true
    | Set_of_closures _, Set_of_closures _ -> false
    | Set_of_closures _, _ | _, Set_of_closures _ -> true
    | Closure _, Closure _ -> false
    | Closure _, _ | _, Closure _ -> true
    | Immutable_string s1, Immutable_string s2 -> String.compare s1 s2 <> 0
    | Immutable_string _, _ | _, Immutable_string _ -> true
    | Mutable_string _, Mutable_string _ -> false
    | Mutable_string _, _ | _, Mutable_string _ -> true
    | Float_array { contents = contents1; size = size1; },
      Float_array { contents = contents2; size = size2; } ->
      size1 <> size2
        || begin match contents1, contents2 with
           | Contents ts1, Contents ts2 ->
             Misc.Stdlib.Array.exists2 definitely_different ts1 ts2
           | Contents _, Unknown_or_mutable
           | Unknown_or_mutable, Contents _
           | Unknown_or_mutable, Unknown_or_mutable -> false
           end
  in
  match types with
  | [] | [_] | _ :: _ :: _ :: _ ->
    Misc.fatal_error "Wrong number of arguments for physical inequality"
  | [a1; a2] -> definitely_different a1 a2

type get_field_result =
  | Ok of t
  | Unreachable

let get_field t ~field_index:i : get_field_result =
  match descr t with
  | Union union ->
    begin match Unionable.flatten union with
    | Ok (Block (_tag, fields)) ->
      if i >= 0 && i < Array.length fields then begin
        Ok fields.(i)
      end else begin
        (* This (unfortunately) cannot be a fatal error; it can happen if a
           .cmx file is missing or with GADT code.  However for debugging the
           compiler this can be a useful point to put a [Misc.fatal_errorf]. *)
        Unreachable
      end
    | Ok (Int _ | Char _ | Constptr _) ->
      (* Something seriously wrong is happening: either the user is doing
         something exceptionally unsafe, or it is an unreachable branch.
         We consider this as unreachable and mark the result accordingly. *)
      Unreachable
    | Bottom -> Unreachable
    | Unknown -> Ok (unknown (Flambda_kind.value ()) Other)
    end
  (* CR-someday mshinwell: This should probably return Unreachable in more
     cases.  I added a couple more. *)
  | Bottom -> Unreachable
  | Float_array _ ->
    (* For the moment we return "unknown" even for immutable arrays, since
       it isn't possible for user code to project from an immutable array. *)
    (* CR-someday mshinwell: If Leo's array's patch lands, then we can
       change this, although it's probably not Pfield that is used to
       do the projection. *)
    Ok (unknown (Flambda_kind.value ()) Other)
  | Unboxed_float _ | Unboxed_int32 _ | Unboxed_int64 _ | Unboxed_nativeint _
  | Immutable_string _ | Mutable_string _ | Boxed_number _ ->
    (* The user is doing something unsafe. *)
    Unreachable
  | Set_of_closures _ | Closure _
    (* These are used by [CamlinternalMod]. *)
  | Load_lazily _ ->
    (* These should have been resolved.
       Note that the contents of blocks must always be of kind [Value]. *)
    Ok (unknown (Flambda_kind.value ()) Other)
  | Unknown (_block_kind, reason) ->
    Ok (unknown (Flambda_kind.value ()) reason)

let length_of_array t =
  match descr t with
  | Union union -> Unionable.size_of_block union
  | Float_array { contents = Contents floats; _ } -> Some (Array.length floats)
  | _ -> None  (* Could be improved later if required. *)

let follow_variable_equality t ~is_present_in_env =
  match var t with
  | Some var when is_present_in_env var -> Some var
  | _ -> None

let reify t : Named.t option =
  let try_symbol () =
    match symbol t with
    | Some (sym, None) -> Some (Named.Symbol sym)
    | Some (sym, Some field) -> Some (Named.Read_symbol_field (sym, field))
    | None -> None
  in
  match descr t with
  | Union union ->
    begin match Unionable.flatten union with
    | Ok (Block _) | Bottom | Unknown -> try_symbol ()
    | Ok (Int n) -> Some (fst (make_const_int_named n))
    | Ok (Char n) -> Some (fst (make_const_char_named n))
    | Ok (Constptr n) -> Some (fst (make_const_ptr_named n))
    end
  | Boxed_number (Float, { descr = Unboxed_float fs; _ }) ->
    begin match Float.Set.get_singleton fs with
    | Some f -> Some (fst (make_const_boxed_float_named f))
    | None -> try_symbol ()
    end
  | Boxed_number (Int32, { descr = Unboxed_int32 ns; _ }) ->
    begin match Int32.Set.get_singleton ns with
    | Some n -> Some (fst (make_const_boxed_int32_named n))
    | None -> try_symbol ()
    end
  | Boxed_number (Int64, { descr = Unboxed_int64 ns; _ }) ->
    begin match Int64.Set.get_singleton ns with
    | Some n -> Some (fst (make_const_boxed_int64_named n))
    | None -> try_symbol ()
    end
  | Boxed_number (Nativeint, { descr = Unboxed_nativeint ns; _ }) ->
    begin match Nativeint.Set.get_singleton ns with
    | Some n -> Some (fst (make_const_boxed_nativeint_named n))
    | None -> try_symbol ()
    end
  | Unboxed_float fs ->
    begin match Float.Set.get_singleton fs with
    | Some f -> Some (fst (make_const_unboxed_float_named f))
    | None -> try_symbol ()
    end
  | Unboxed_int32 fs ->
    begin match Int32.Set.get_singleton fs with
    | Some f -> Some (fst (make_const_unboxed_int32_named f))
    | None -> try_symbol ()
    end
  | Unboxed_int64 fs ->
    begin match Int64.Set.get_singleton fs with
    | Some f -> Some (fst (make_const_unboxed_int64_named f))
    | None -> try_symbol ()
    end
  | Unboxed_nativeint fs ->
    begin match Nativeint.Set.get_singleton fs with
    | Some f -> Some (fst (make_const_unboxed_nativeint_named f))
    | None -> try_symbol ()
    end
  | Boxed_number _ | Immutable_string _ | Mutable_string _ | Float_array _
  | Set_of_closures _ | Closure _ | Unknown _ | Bottom | Load_lazily _ ->
    try_symbol ()

let reify_using_env t ~is_present_in_env =
  let named =
    match var t with
    | Some var when is_present_in_env var -> Some (Named.Var var)
    | _ ->
      match symbol t with
      | Some (sym, None) -> Some (Named.Symbol sym)
      | Some (sym, Some field) -> Some (Named.Read_symbol_field (sym, field))
      | None -> None
  in
  match reify t with
  | None -> named
  | Some named -> Some named

let reify_as_unboxed_float_array (fa : float_array) : float list option =
  match fa.contents with
  | Unknown_or_mutable -> None
  | Contents contents ->
    Array.fold_right (fun elt acc ->
        match acc, descr elt with
        | Some acc, Unboxed_float fs ->
          begin match Float.Set.get_singleton fs with
          | None -> None
          | Some f -> Some (f :: acc)
          end
        | None, _
        | Some _, _ -> None)
      contents (Some [])

let reify_as_string t : string option =
  match descr t with
  | Immutable_string str -> Some str
  | Union _ | Boxed_number _ | Unboxed_float _ | Unboxed_int32 _
  | Unboxed_int64 _ | Unboxed_nativeint _ | Unknown _ | Mutable_string _
  | Float_array _ | Bottom | Set_of_closures _ | Closure _
  | Load_lazily _ -> None
*)

type 'a or_wrong =
  | Ok of 'a
  | Wrong

module Or_not_all_values_known = struct
  type 'a t =
    | Exactly of 'a
    | Not_all_values_known

  let join join_contents t1 t2 : _ t or_wrong =
    match t1, t2 with
    | Exactly e1, Exactly e2 ->
      begin match join_contents e1 e2 with
      | Ok e -> Ok (Exactly e)
      | Wrong -> Wrong
      end
    | Exactly _, Not_all_values_known
    | Not_all_values_known, Exactly _
    | Not_all_values_known, Not_all_values_known -> Ok Not_all_values_known
end

module Blocks = struct
  type t = ty_value array Tag.Scannable.Map.t

  let join ~importer t1 t2 : t or_wrong =
    let exception Same_tag_different_arities in
    try
      let map =
        Tag.Scannable.Map.union (fun _tag fields1 fields2 ->
            if Array.length fields1 <> Array.length fields2 then
              raise Same_tag_different_arities
            else
              let fields =
                Array.map2 (fun ty_value1 ty_value2 ->
                    join_ty_value ~importer ty_value1 ty_value2)
                  fields1 fields2
              in
              Some fields)
          t1 t2
      in
      Ok map
    with Same_tag_different_arities -> Wrong
end

module Set_of_closures = struct
  type t = set_of_closures

  let non_inlinable_function_declaration (f : inlinable_function_declaration)
        : non_inlinable_function_declaration =
    { result = f.result }

  let non_inlinable_function_declarations (t : t)
        : non_inlinable_function_declarations =
    match t.function_decls with
    | Non_inlinable non_inlinable_function_decls ->
      non_inlinable_function_decls
    | Inlinable function_decls ->
      let funs =
        Variable.Map.map
          non_inlinable_function_declaration
          function_decls.funs
      in
      { funs }

  let join_non_inlinable ~importer (t1 : t) (t2 : t) : t =
    let f1 = non_inlinable_function_declarations t1 in
    let f2 = non_inlinable_function_declarations t2 in
    let join_results
        (f1:non_inlinable_function_declaration)
        (f2:non_inlinable_function_declaration) =
      if List.length f1.result <> List.length f2.result then
        Misc.fatal_error "A function appear with 2 different return arities";
      let result = List.map2 (join ~importer) f1.result f2.result in
      { result }
    in
    let funs =
      Variable.Map.union_merge join_results f1.funs f2.funs
    in
    let function_decls = Non_inlinable { funs } in
    let closure_elements =
      Var_within_closure.Map.union_merge (join_ty_value ~importer)
        t1.closure_elements t2.closure_elements
    in
    create_set_of_closures
      ~function_decls
      ~closure_elements

  let join ~importer (t1 : t) (t2 : t) : t =
    match t1.function_decls, t2.function_decls with
    | Non_inlinable _, (Inlinable _ | Non_inlinable _)
    | Inlinable _, Non_inlinable _ ->
      join_non_inlinable ~importer t1 t2
    | Inlinable f1, Inlinable f2 ->
      if Set_of_closures_id.equal
          f1.set_of_closures_id
          f2.set_of_closures_id
      then begin
      (* If the set_of_closures are the same, the result is inlinable.
         The real constraint is that the union of two functions is
         inlinable if any of the two functions can be replaced by the
         other. This is an overapproximation, but might not be too
         restrictive in practice. *)
      assert(
        Set_of_closures_origin.equal
          f1.set_of_closures_origin
          f2.set_of_closures_origin);
      (* CR pchambart: this is too strong, but should hold in general.
         It can be kept for now to help debugging *)
      assert(f1.funs == f2.funs);
      let closure_elements =
        Var_within_closure.Map.union_merge (join_ty_value ~importer)
          t1.closure_elements t2.closure_elements
      in
      create_set_of_closures
        ~function_decls:t1.function_decls
        ~closure_elements
    end
      else
        (* We assume here that functions are different, hence the result
           is not inlinable *)
        join_non_inlinable ~importer t1 t2

end

module Summary = struct
  type t =
    | Wrong
    | Blocks_and_tagged_immediates of
        Blocks.t * (Immediate.Set.t Or_not_all_values_known.t)
    | Boxed_floats of Float.Set.t Or_not_all_values_known.t
    | Boxed_int32s of Int32.Set.t Or_not_all_values_known.t
    | Boxed_int64s of Int64.Set.t Or_not_all_values_known.t
    | Boxed_nativeints of Targetint.Set.t Or_not_all_values_known.t
    | Closures of set_of_closures Closure_id.Map.t Or_not_all_values_known.t
    (* CR mshinwell for pchambart: We need a [Set_of_closures] case here
       as well, I think *)

  let join ~importer t1 t2 =
    let join_immediates =
      Or_not_all_values_known.join (fun imms1 imms2 : _ or_wrong ->
        Ok (Immediate.join_set imms1  imms2))
    in
    match t1, t2 with
    | Wrong, _ | _, Wrong -> Wrong
    | Blocks_and_tagged_immediates (b1, imms1),
        Blocks_and_tagged_immediates (b2, imms2) ->
      let blocks_join = Blocks.join ~importer b1 b2 in
      let imms_join = join_immediates imms1 imms2 in
      begin match blocks_join, imms_join with
      | Ok blocks, Ok imms -> Blocks_and_tagged_immediates (blocks, imms)
      | Wrong, _ | _, Wrong -> Wrong
      end
    | Boxed_floats fs1, Boxed_floats fs2 ->
      begin match
        Or_not_all_values_known.join (fun fs1 fs2 : Float.Set.t or_wrong ->
           Ok (Float.Set.union fs1 fs2))
          fs1 fs2
      with
      | Ok fs -> Boxed_floats fs
      | Wrong -> Wrong
      end
    | Boxed_int32s is1, Boxed_int32s is2 ->
      begin match
        Or_not_all_values_known.join (fun is1 is2 : Int32.Set.t or_wrong ->
            Ok (Int32.Set.union is1 is2))
          is1 is2
      with
      | Ok is -> Boxed_int32s is
      | Wrong -> Wrong
      end
    | Boxed_int64s is1, Boxed_int64s is2 ->
      begin match
        Or_not_all_values_known.join (fun is1 is2 : Int64.Set.t or_wrong ->
            Ok (Int64.Set.union is1 is2))
          is1 is2
      with
      | Ok is -> Boxed_int64s is
      | Wrong -> Wrong
      end
    | Boxed_nativeints is1, Boxed_nativeints is2 ->
      begin match
        Or_not_all_values_known.join (fun is1 is2 : Targetint.Set.t or_wrong ->
            Ok (Targetint.Set.union is1 is2))
          is1 is2
      with
      | Ok is -> Boxed_nativeints is
      | Wrong -> Wrong
      end
    | Closures closures1, Closures closures2 ->
      begin match
        Or_not_all_values_known.join (fun map1 map2 : _ or_wrong ->
          let map =
            Closure_id.Map.union_merge (Set_of_closures.join ~importer)
              map1 map2
          in
          (* CR pchambart: Could this fail and not be a real error *)
          Ok map)
          closures1 closures2
      with
      | Ok closures -> Closures closures
      | Wrong -> Wrong
      end
    | (Blocks_and_tagged_immediates _
      | Boxed_floats _
      | Boxed_int32s _
      | Boxed_int64s _
      | Boxed_nativeints _
      | Closures _), _ -> Wrong
end

type 'a known_unknown_or_wrong =
  | Known of 'a
  | Unknown
  | Wrong

let prove_naked_immediate_from_ty_naked_immediate ~importer
      (ty : ty_naked_immediate) : _ known_unknown_or_wrong =
  let module I = (val importer : Importer) in
  let ty = I.import_naked_immediate_type_as_resolved_ty_naked_immediate ty in
  match ty.descr with
  | Unknown _ -> Unknown
  | Ok (Naked_immediate i) -> Known i
  | Bottom -> Wrong

let prove_naked_float_from_ty_naked_float ~importer
      (ty : ty_naked_float) : _ known_unknown_or_wrong =
  let module I = (val importer : Importer) in
  let ty = I.import_naked_float_type_as_resolved_ty_naked_float ty in
  match ty.descr with
  | Unknown _ -> Unknown
  | Ok (Naked_float f) -> Known f
  | Bottom -> Wrong

let prove_naked_int32_from_ty_naked_int32 ~importer
      (ty : ty_naked_int32) : _ known_unknown_or_wrong =
  let module I = (val importer : Importer) in
  let ty = I.import_naked_int32_type_as_resolved_ty_naked_int32 ty in
  match ty.descr with
  | Unknown _ -> Unknown
  | Ok (Naked_int32 i) -> Known i
  | Bottom -> Wrong

let prove_naked_int64_from_ty_naked_int64 ~importer
      (ty : ty_naked_int64) : _ known_unknown_or_wrong =
  let module I = (val importer : Importer) in
  let ty = I.import_naked_int64_type_as_resolved_ty_naked_int64 ty in
  match ty.descr with
  | Unknown _ -> Unknown
  | Ok (Naked_int64 i) -> Known i
  | Bottom -> Wrong

let prove_naked_nativeint_from_ty_naked_nativeint ~importer
      (ty : ty_naked_nativeint) : _ known_unknown_or_wrong =
  let module I = (val importer : Importer) in
  let ty = I.import_naked_nativeint_type_as_resolved_ty_naked_nativeint ty in
  match ty.descr with
  | Unknown _ -> Unknown
  | Ok (Naked_nativeint i) -> Known i
  | Bottom -> Wrong

let summarize ~importer (t : t) : Summary.t =
  let module I = (val importer : Importer) in
  match t with
  | Value ty ->
    begin match (I.import_value_type_as_resolved_ty_value ty).descr with
    | Unknown _ | Bottom -> Wrong
    | Ok of_kind_value ->
      let for_singleton (s : of_kind_value_singleton) : Summary.t =
        match s with
        | Tagged_immediate ty ->
          begin match
            prove_naked_immediate_from_ty_naked_immediate ~importer ty
          with
          | Wrong -> Wrong
          | Unknown ->
            Blocks_and_tagged_immediates (
              Tag.Scannable.Map.empty, Not_all_values_known)
          | Known i ->
            Blocks_and_tagged_immediates (
              Tag.Scannable.Map.empty, Exactly (Immediate.Set.singleton i))
          end
        | Boxed_float ty ->
          begin match
            prove_naked_float_from_ty_naked_float ~importer ty
          with
          | Wrong -> Wrong
          | Unknown -> Boxed_floats Not_all_values_known
          | Known f -> Boxed_floats (Exactly (Float.Set.singleton f))
          end
        | Boxed_int32 ty ->
          begin match
            prove_naked_int32_from_ty_naked_int32 ~importer ty
          with
          | Wrong -> Wrong
          | Unknown -> Boxed_int32s Not_all_values_known
          | Known i -> Boxed_int32s (Exactly (Int32.Set.singleton i))
          end
        | Boxed_int64 ty ->
          begin match
            prove_naked_int64_from_ty_naked_int64 ~importer ty
          with
          | Wrong -> Wrong
          | Unknown -> Boxed_int64s Not_all_values_known
          | Known i -> Boxed_int64s (Exactly (Int64.Set.singleton i))
          end
        | Boxed_nativeint ty ->
          begin match
            prove_naked_nativeint_from_ty_naked_nativeint ~importer ty
          with
          | Wrong -> Wrong
          | Unknown -> Boxed_nativeints Not_all_values_known
          | Known i -> Boxed_nativeints (Exactly (Targetint.Set.singleton i))
          end
        | Block (tag, fields) ->
          let blocks =
            Tag.Scannable.Map.add tag fields Tag.Scannable.Map.empty
          in
          Blocks_and_tagged_immediates (blocks, Exactly Immediate.Set.empty)
        | Set_of_closures _
        | Closure _
        | String _
        | Float_array _ -> Wrong
      in
      let rec for_of_kind_value (o : of_kind_value) =
        match o with
        | Singleton s -> for_singleton s
        | Union (w1, w2) ->
          let summary1 = for_of_kind_value w1.descr in
          let summary2 = for_of_kind_value w2.descr in
          Summary.join ~importer summary1 summary2
      in
      for_of_kind_value of_kind_value
    end
  | Naked_immediate _
  | Naked_float _
  | Naked_int32 _
  | Naked_int64 _
  | Naked_nativeint _ -> Wrong

(*

type proved_scannable_block =
  | Ok of Tag.Scannable.t * ty_value array
  | Can't_prove

let prove_scannable_block t : proved_scannable_block =
  match prove_unboxable_or_untaggable t with
  | Blocks_and_tagged_immediates (blocks, imms) ->
    if not (Immediate.Set.is_empty imms) then Wrong
    else
      begin match Tag.Scannable.Map.get_singleton blocks with
      | Some (tag, fields) -> Ok (tag, fields)
      | None -> Wrong
      end
  | Wrong
  | Tagged_immediates _
  | Boxed_floats _
  | Boxed_int32s _
  | Boxed_int64s _
  | Boxed_nativeints _ -> Wrong

let reify_as_tagged_immediate t =
  match prove_unboxable_or_untaggable t with
  | Blocks_and_tagged_immediates (blocks, imms) ->
    if not (Immediate.Set.is_empty blocks) then None
    else
      begin match imms with
      | Not_all_values_known -> None
      | Exactly imms -> Immediate.Set.get_singleton imms
      end
  | Boxed_floats _
  | Boxed_int32s _
  | Boxed_int64s _
  | Boxed_nativeints _
  | Wrong -> None

let reify_as_boxed_float t =
  match prove_unboxable_or_untaggable t with
  | Boxed_floats fs -> Float.Set.get_singleton fs
  | Boxed_floats Not_all_values_known
  | Blocks_and_tagged_immediates _
  | Boxed_int32s _
  | Boxed_int64s _
  | Boxed_nativeints _
  | Wrong -> None

type reified_as_set_of_closures =
  | Unknown
  | Ok of Variable.t option * set_of_closures
  | Wrong

let reify_as_set_of_closures t : reified_as_set_of_closures =
  ... this will look like one of the above functions, working on the summary

type strict_reified_as_set_of_closures =
  | Wrong
  | Ok of Variable.t option * set_of_closures

let strict_reify_as_set_of_closures t
      : strict_reified_as_set_of_closures =
  match reify_as_set_of_closures t with
  | Ok (var, value_set_of_closures) -> Ok (var, value_set_of_closures)
  | Wrong | Unresolved _ | Unknown -> Wrong

type reified_as_closure_allowing_unresolved =
  | Wrong
  | Unresolved of unresolved_value
  | Unknown
  | Ok of set_of_closures Closure_id.Map.t * Variable.t option * Symbol.t option

let reify_as_closure_allowing_unresolved t
      : reified_as_closure_allowing_unresolved =
  match descr t with
  | Closure closure -> begin
    match Closure_id.Map.get_singleton closure.potential_closures with
    | None -> begin
      try
        let closures =
          Closure_id.Map.map (fun (set_of_closures : t) ->
            match set_of_closures.descr with
            | Set_of_closures set_of_closures -> set_of_closures
            | Closure _ | Union _ | Boxed_number _ | Unboxed_float _
            | Unboxed_int32 _ | Unboxed_int64 _ | Unboxed_nativeint _
            | Unknown _ | Bottom | Load_lazily _ | Immutable_string _
            | Mutable_string _ | Float_array _  ->
              raise Exit)
            closure.potential_closures
        in
        Ok (closures, None, None)
      with Exit -> Wrong
      end
    | Some (closure_id, set_of_closures) ->
      match set_of_closures.descr with
      | Set_of_closures value_set_of_closures ->
        let symbol = match set_of_closures.symbol with
          | Some (symbol, None) -> Some symbol
          | None | Some (_, Some _) -> None
        in
        Ok (Closure_id.Map.singleton closure_id value_set_of_closures,
            set_of_closures.var, symbol)
      | Closure _ | Boxed_number _ | Unboxed_float _ | Unboxed_int32 _
      | Unboxed_int64 _ | Unboxed_nativeint _ | Unknown _ | Bottom
      | Load_lazily _ | Immutable_string _ | Mutable_string _ | Float_array _
      | Union _ -> Wrong
    end
  | Unknown (_, Unresolved_value value) -> Unresolved value
  | Set_of_closures _ | Union _ | Boxed_number _
  | Unboxed_float _ | Unboxed_int32 _ | Unboxed_int64 _ | Unboxed_nativeint _
  | Bottom | Load_lazily _ | Immutable_string _ | Mutable_string _
  | Float_array _  -> Wrong
  (* CR-soon mshinwell: This should be unwound once the reason for a value
     being unknown can be correctly propagated through the export info. *)
  | Unknown (_, Other) -> Unknown

type strict_reified_as_closure_singleton =
  | Wrong
  | Ok of Closure_id.t * Variable.t option * Symbol.t option * set_of_closures

let strict_reify_as_closure_singleton t
  : strict_reified_as_closure_singleton =
  match reify_as_closure_allowing_unresolved t with
  | Ok (closures, set_of_closures_var, set_of_closures_symbol) -> begin
    match Closure_id.Map.get_singleton closures with
    | None -> Wrong
    | Some (closure_id, value_set_of_closures) ->
      Ok (closure_id, set_of_closures_var, set_of_closures_symbol,
          value_set_of_closures)
    end
  | Wrong | Unknown | Unresolved _ -> Wrong

type strict_reified_as_closure =
  | Wrong
  | Ok of set_of_closures Closure_id.Map.t * Variable.t option * Symbol.t option

let strict_reify_as_closure t : strict_reified_as_closure =
  match reify_as_closure_allowing_unresolved t with
  | Ok (closures, set_of_closures_var, set_of_closures_symbol) ->
    Ok (closures, set_of_closures_var, set_of_closures_symbol)
  | Wrong | Unknown | Unresolved _ -> Wrong

type switch_branch_classification =
  | Cannot_be_taken
  | Can_be_taken
  | Must_be_taken

let classify_switch_branch t branch ~import_type
      : switch_branch_classification =
  match join_boxed_immediates t ~import_type with
  | Unknown Value -> Can_be_taken
  | Unknown _ | Bottom -> Cannot_be_taken
  | Ok all_possible_values ->
    if Targetint.Set.mem branch all_possible_values then Must_be_taken
    else Cannot_be_taken

let as_or_more_precise _t ~than:_ =
  Misc.fatal_error "not yet implemented"

let strictly_more_precise _t ~than:_ =
  Misc.fatal_error "not yet implemented"



let join_boxed_immediates t ~import_type : Immediate.Set.t fold_result =
  match join_unboxable t ~import_type with
  | Ok (Blocks_and_immediates { blocks; immediates; }) ->
    if Targetint.Set.is_empty blocks then Ok immediates
    else Bottom
  | Ok (Boxed_floats _ | Boxed_int32s _ | Boxed_int64s _
      | Boxed_nativeints _) ->
    Bottom
  | (Unknown _ | Bottom) as result -> result

let unique_boxed_immediate_in_join t ~import_type =
  match join_boxed_immediates t ~import_type with
  | Ok all_possible_values -> Immediate.Set.get_singleton all_possible_values
  | Unknown _ | Bottom -> None
*)
