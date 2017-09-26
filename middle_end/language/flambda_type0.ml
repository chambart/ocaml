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

module Float = Numbers.Float
module Int32 = Numbers.Int32
module Int64 = Numbers.Int64

module K = Flambda_kind

module Make (Expr : sig
  type t
  val print : Format.formatter -> t -> unit
end) = struct
  type expr = Expr.t

  type unresolved_value =
    | Set_of_closures_id of Set_of_closures_id.t
    | Export_id of Export_id.t
    | Symbol of Symbol.t

  type unknown_because_of =
    | Unresolved_value of unresolved_value
    | Other

  let join_unknown_because_of u1 u2 =
    match u1, u2 with
    | Unresolved_value (Set_of_closures_id s1),
        Unresolved_value (Set_of_closures_id s2) ->
      if Set_of_closures_id.equal s1 s2 then u1 else Other
    | Unresolved_value (Export_id s1), Unresolved_value (Export_id s2) ->
      if Export_id.equal s1 s2 then u1 else Other
    | Unresolved_value (Symbol s1), Unresolved_value (Symbol s2) ->
      if Symbol.equal s1 s2 then u1 else Other
    | _, _ -> Other

  (** Types from other compilation units are loaded lazily.  There are two
      kinds of cross-compilation unit reference to be resolved: via
      [Export_id.t] values and via [Symbol.t] values. *)
  type load_lazily =
    | Export_id of Export_id.t
    | Symbol of Symbol.t

  let print_load_lazily ppf (ll : load_lazily) =
    match ll with
    | Export_id id -> Format.fprintf ppf "(eid %a)" Export_id.print id
    | Symbol sym -> Format.fprintf ppf "(sym %a)" Symbol.print sym

  (* CR mshinwell: update comment *)
  (* A value of type [T.t] corresponds to an "approximation" of the result of
     a computation in the program being compiled.  That is to say, it
     represents what knowledge we have about such a result at compile time.
     The simplification pass exploits this information to partially evaluate
     computations.

     At a high level, an approximation for a value [v] has three parts:
     - the "description" (for example, "the constant integer 42");
     - an optional variable;
     - an optional symbol or symbol field.
     If the variable (resp. symbol) is present then that variable (resp.
     symbol) may be used to obtain the value [v].

     The exact semantics of the variable and symbol fields follows.

     Approximations are deduced at particular points in an expression tree,
     but may subsequently be propagated to other locations.

     At the point at which an approximation is built for some value [v], we can
     construct a set of variables (call the set [S]) that are known to alias the
     same value [v].  Each member of [S] will have the same or a more precise
     [descr] field in its approximation relative to the approximation for [v].
     (An increase in precision may currently be introduced for pattern
     matches.)  If [S] is non-empty then it is guaranteed that there is a
     unique member of [S] that was declared in a scope further out ("earlier")
     than all other members of [S].  If such a member exists then it is
     recorded in the [var] field.  Otherwise [var] is [None].

     Analogous to the construction of the set [S], we can construct a set [T]
     consisting of all symbols that are known to alias the value whose
     approximation is being constructed.  If [T] is non-empty then the
     [symbol] field is set to some member of [T]; it does not matter which
     one.  (There is no notion of scope for symbols.)

     Note about mutable blocks:

     Mutable blocks are always represented by [Unknown] or
     [Bottom].  Any other approximation could leave the door open to
     a miscompilation.   Such bad scenarios are most likely a user using
     [Obj.magic] or [Obj.set_field] in an inappropriate situation.
     Such a situation might be:
     [let x = (1, 1) in
     Obj.set_field (Obj.repr x) 0 (Obj.repr 2);
     assert(fst x = 2)]
     The user would probably expect the assertion to be true, but the
     compiler could in fact propagate the value of [x] across the
     [Obj.set_field].

     Insisting that mutable blocks have [Unknown] or [bottom]
     approximations certainly won't always prevent this kind of error, but
     should help catch many of them.

     It is possible that there may be some false positives, with correct
     but unreachable code causing this check to fail.  However the likelihood
     of this seems sufficiently low, especially compared to the advantages
     gained by performing the check, that we include it.

     An example of a pattern that might trigger a false positive is:
     [type a = { a : int }
     type b = { mutable b : int }
     type t =
       | A : a t
       | B : b t
     let f (type x) (v:x t) (r:x) =
       match v with
       | A -> r.a
       | B -> r.b <- 2; 3

     let v =
     let r =
       ref A in
       r := A; (* Some pattern that the compiler can't understand *)
       f !r { a = 1 }]
     When inlining [f], the B branch is unreachable, yet the compiler
     cannot prove it and must therefore keep it.
  *)

  type string_contents =
    | Contents of string
    | Unknown_or_mutable

  type string_ty = {
    contents : string_contents;
    size : int;
  }

  type 'a with_var_and_symbol = {
    descr : 'a;
    var : Variable.t option;
    symbol : (Symbol.t * int option) option;
  }

  type t =
    | Value of ty_value
    | Naked_immediate of ty_naked_immediate
    | Naked_float of ty_naked_float
    | Naked_int32 of ty_naked_int32
    | Naked_int64 of ty_naked_int64
    | Naked_nativeint of ty_naked_nativeint

  and ty_value = (of_kind_value, Flambda_kind.scanning) ty
  and ty_naked_immediate = (of_kind_naked_immediate, unit) ty
  and ty_naked_float = (of_kind_naked_float, unit) ty
  and ty_naked_int32 = (of_kind_naked_int32, unit) ty
  and ty_naked_int64 = (of_kind_naked_int64, unit) ty
  and ty_naked_nativeint = (of_kind_naked_nativeint, unit) ty

  and ('a, 'u) ty = ('a, 'u) maybe_unresolved with_var_and_symbol

  and resolved_t =
    | Value of resolved_ty_value
    | Naked_immediate of resolved_ty_naked_immediate
    | Naked_float of resolved_ty_naked_float
    | Naked_int32 of resolved_ty_naked_int32
    | Naked_int64 of resolved_ty_naked_int64
    | Naked_nativeint of resolved_ty_naked_nativeint

  and resolved_ty_value = (of_kind_value, Flambda_kind.scanning) resolved_ty
  and resolved_ty_naked_immediate = (of_kind_naked_immediate, unit) resolved_ty
  and resolved_ty_naked_float = (of_kind_naked_float, unit) resolved_ty
  and resolved_ty_naked_int32 = (of_kind_naked_int32, unit) resolved_ty
  and resolved_ty_naked_int64 = (of_kind_naked_int64, unit) resolved_ty
  and resolved_ty_naked_nativeint = (of_kind_naked_nativeint, unit) resolved_ty
  and resolved_ty_set_of_closures = (set_of_closures, unit) resolved_ty

  and ('a, 'u) resolved_ty = ('a, 'u) or_unknown_or_bottom with_var_and_symbol

  and ('a, 'u) maybe_unresolved =
    | Ok of ('a, 'u) or_unknown_or_bottom
    | Load_lazily of load_lazily

  and ('a, 'u) or_unknown_or_bottom =
    | Unknown of unknown_because_of * 'u
    | Ok of 'a
    | Bottom

  and of_kind_value =
    | Singleton of of_kind_value_singleton
    | Union of of_kind_value with_var_and_symbol
        * of_kind_value with_var_and_symbol

  and of_kind_value_singleton =
    | Tagged_immediate of ty_naked_immediate
    | Boxed_float of ty_naked_float
    | Boxed_int32 of ty_naked_int32
    | Boxed_int64 of ty_naked_int64
    | Boxed_nativeint of ty_naked_nativeint
    | Block of Tag.Scannable.t * (ty_value array)
    | Set_of_closures of set_of_closures
    | Closure of {
        set_of_closures : resolved_ty_set_of_closures;
        closure_id : Closure_id.t
      }
    | String of string_ty
    | Float_array of float_array_ty

  and inlinable_function_declarations = {
    set_of_closures_id : Set_of_closures_id.t;
    set_of_closures_origin : Set_of_closures_origin.t;
    funs : inlinable_function_declaration Variable.Map.t;
    invariant_params : Variable.Set.t Variable.Map.t lazy_t;
    size : int option Variable.Map.t lazy_t;
    direct_call_surrogates : Closure_id.t Closure_id.Map.t;
  }

  and non_inlinable_function_declarations = {
    funs : non_inlinable_function_declaration Variable.Map.t
  }

  and function_declarations =
    | Non_inlinable of non_inlinable_function_declarations
    | Inlinable of inlinable_function_declarations

  and function_body = {
    body : expr;
    free_variables : Variable.Set.t;
    free_symbols : Symbol.Set.t;
  }

  and inlinable_function_declaration = {
    closure_origin : Closure_origin.t;
    params : (Parameter.t * t) list;
    body : function_body;
    result : t list;
    stub : bool;
    dbg : Debuginfo.t;
    inline : Lambda.inline_attribute;
    specialise : Lambda.specialise_attribute;
    is_a_functor : bool;
  }

  and non_inlinable_function_declaration = {
    result : t list;
  }

  and set_of_closures = {
    function_decls : function_declarations;
    closure_elements : ty_value Var_within_closure.Map.t;
  }

  and float_array_contents =
    | Contents of ty_naked_float array
    | Unknown_or_mutable

  and float_array_ty = {
    contents : float_array_contents;
    size : int;
  }

  and of_kind_naked_immediate =
    | Naked_immediate of Immediate.t

  and of_kind_naked_float =
    | Naked_float of float

  and of_kind_naked_int32 =
    | Naked_int32 of Int32.t

  and of_kind_naked_int64 =
    | Naked_int64 of Int64.t

  and of_kind_naked_nativeint =
    | Naked_nativeint of Targetint.t

  let _ = Expr.print  (* temporarily silence compiler warning *)

  (* CR mshinwell: we should probably enhance this for debugging *)
  let print_non_inlinable_funs ppf
        (funs : non_inlinable_function_declaration Variable.Map.t) =
    Format.fprintf ppf "@[(non_inlinable %a)@]"
      Variable.Set.print (Variable.Map.keys funs)

  let print_inlinable_funs ppf funs =
    Format.fprintf ppf "@[(inlinable %a)@]"
      Variable.Set.print (Variable.Map.keys funs)

  let print_non_inlinable_function_declarations ppf
        (function_decls : non_inlinable_function_declarations) =
    print_non_inlinable_funs ppf function_decls.funs

  let print_inlinable_function_declarations ppf
        (function_decls : inlinable_function_declarations) =
    Format.fprintf ppf
      "@[(@[(set_of_closures_id %a)@]@,\
          @[(set_of_closures_origin %a)@]@,\
          @[(invariant_params %a)@]@,\
          @[(funs %a)@]@]"
      Set_of_closures_id.print function_decls.set_of_closures_id
      Set_of_closures_origin.print function_decls.set_of_closures_origin
      (Variable.Map.print Variable.Set.print)
        (Lazy.force function_decls.invariant_params)
      print_inlinable_funs function_decls.funs

  let print_function_declarations ppf function_decls =
    match function_decls with
    | Non_inlinable function_decls ->
      Format.fprintf ppf "@[(non_inlinable %a)@]"
        print_non_inlinable_function_declarations function_decls
    | Inlinable function_decls ->
      Format.fprintf ppf "@[(inlinable %a)@]"
        print_inlinable_function_declarations function_decls

  let print_set_of_closures ppf
        { function_decls; _ } =
    Format.fprintf ppf
      "(function_decls:@ %a)"
      print_function_declarations function_decls

  let print_unresolved_value ppf (unresolved : unresolved_value) =
    match unresolved with
    | Set_of_closures_id set ->
      Format.fprintf ppf "Set_of_closures_id %a" Set_of_closures_id.print set
    | Symbol symbol ->
      Format.fprintf ppf "Symbol %a" Symbol.print symbol
    | Export_id id ->
      Format.fprintf ppf "Export_id %a" Export_id.print id

  let print_with_var_and_symbol print_descr ppf { descr; var; symbol; } =
    let print_symbol ppf = function
      | None -> Symbol.print_opt ppf None
      | Some (sym, None) -> Symbol.print ppf sym
      | Some (sym, Some field) ->
        Format.fprintf ppf "%a.(%i)" Symbol.print sym field
    in
    Format.fprintf ppf "{ descr=%a var=%a symbol=%a }"
      print_descr descr
      Variable.print_opt var
      print_symbol symbol

  let print_maybe_unresolved print_contents ppf (m : _ maybe_unresolved) =
    match m with
    | Ok contents -> print_contents ppf contents
    | Load_lazily ll -> Format.fprintf ppf "lazy(%a)" print_load_lazily ll

  let print_of_kind_naked_immediate ppf (o : of_kind_naked_immediate) =
    match o with
    | Naked_immediate i -> Format.fprintf ppf "%a" Immediate.print i

  let print_of_kind_naked_float ppf (o : of_kind_naked_float) =
    match o with
    | Naked_float f -> Format.fprintf ppf "%f" f

  let print_of_kind_naked_int32 ppf (o : of_kind_naked_int32) =
    match o with
    | Naked_int32 i -> Format.fprintf ppf "%a" Int32.print i

  let print_of_kind_naked_int64 ppf (o : of_kind_naked_int64) =
    match o with
    | Naked_int64 i -> Format.fprintf ppf "%a" Int64.print i

  let print_of_kind_naked_nativeint ppf (o : of_kind_naked_nativeint) =
    match o with
    | Naked_nativeint i -> Format.fprintf ppf "%a" Targetint.print i

  let print_or_unknown_or_bottom print_contents print_unknown_payload ppf
        (o : _ or_unknown_or_bottom) =
    match o with
    | Unknown (reason, payload) ->
      begin match reason with
      | Unresolved_value value ->
        Format.fprintf ppf "?%a(due to unresolved %a)"
          print_unknown_payload payload
          print_unresolved_value value
      | Other -> Format.fprintf ppf "?%a" print_unknown_payload payload
      end;
    | Ok contents -> print_contents ppf contents
    | Bottom -> Format.fprintf ppf "bottom"

  let print_ty_generic print_contents print_unknown_payload ppf ty =
    (print_with_var_and_symbol
      (print_maybe_unresolved
        (print_or_unknown_or_bottom print_contents print_unknown_payload)))
      ppf ty

  let print_resolved_ty_generic print_contents print_unknown_payload ppf ty =
    (print_with_var_and_symbol
       (print_or_unknown_or_bottom print_contents print_unknown_payload))
      ppf ty

  let rec print_of_kind_value ppf (o : of_kind_value) =
    match o with
    | Singleton singleton -> print_of_kind_value_singleton ppf singleton
    | Union (w1, w2) ->
      let print_part ppf w =
        print_with_var_and_symbol print_of_kind_value ppf w
      in
      Format.fprintf ppf "@[(Union (%a)(%a))@]"
        print_part w1 print_part w2

  and print_of_kind_value_singleton ppf (singleton : of_kind_value_singleton) =
    match singleton with
    | Tagged_immediate t ->
      Format.fprintf ppf "(tagged_imm %a)" print_ty_naked_immediate t
    | Boxed_float f ->
      Format.fprintf ppf "(boxed_float %a)" print_ty_naked_float f
    | Boxed_int32 n ->
      Format.fprintf ppf "(boxed_int32 %a)" print_ty_naked_int32 n
    | Boxed_int64 n ->
      Format.fprintf ppf "(boxed_int64 %a)" print_ty_naked_int64 n
    | Boxed_nativeint n ->
      Format.fprintf ppf "(boxed_nativeint %a)" print_ty_naked_nativeint n
    | Block (tag, fields) ->
      Format.fprintf ppf "@[[%a: %a]@]"
        Tag.Scannable.print tag
        (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf "; ")
          print_ty_value) (Array.to_list fields)
    | Set_of_closures set_of_closures ->
      print_set_of_closures ppf set_of_closures
    | Closure { set_of_closures; closure_id; } ->
      Format.fprintf ppf "(closure:@ @[<2>[@ %a @[<2>from@ %a@];@ ]@])"
        Closure_id.print closure_id
        print_resolved_ty_set_of_closures set_of_closures
    | String { contents; size; } ->
      begin match contents with
      | Unknown_or_mutable -> Format.fprintf ppf "string %i" size
      | Contents s ->
        let s =
          if size > 10 then String.sub s 0 8 ^ "..."
          else s
        in
        Format.fprintf ppf "string %i %S" size s
      end
    | Float_array float_array ->
      begin match float_array.contents with
      | Unknown_or_mutable ->
        Format.fprintf ppf "float_array %i" float_array.size
      | Contents _ ->
        Format.fprintf ppf "float_array_imm %i" float_array.size
      end

  and print_ty_value ppf (ty : ty_value) =
    let print_scanning ppf (scanning : K.scanning) =
      match scanning with
      | Must_scan -> Format.fprintf ppf "*"
      | Can_scan -> ()
    in
    print_ty_generic print_of_kind_value print_scanning ppf ty

  and print_ty_naked_immediate ppf (ty : ty_naked_immediate) =
    print_ty_generic print_of_kind_naked_immediate (fun _ () -> ()) ppf ty

  and print_ty_naked_float ppf (ty : ty_naked_float) =
    print_ty_generic print_of_kind_naked_float (fun _ () -> ()) ppf ty

  and print_ty_naked_int32 ppf (ty : ty_naked_int32) =
    print_ty_generic print_of_kind_naked_int32 (fun _ () -> ()) ppf ty

  and print_ty_naked_int64 ppf (ty : ty_naked_int64) =
    print_ty_generic print_of_kind_naked_int64 (fun _ () -> ()) ppf ty

  and print_ty_naked_nativeint ppf (ty : ty_naked_nativeint) =
    print_ty_generic print_of_kind_naked_nativeint (fun _ () -> ()) ppf ty

  and print_resolved_ty_set_of_closures ppf
        (ty : resolved_ty_set_of_closures) =
    print_resolved_ty_generic print_set_of_closures (fun _ () -> ()) ppf ty

  and print ppf (t : t) =
    match t with
    | Value ty ->
      Format.fprintf ppf "(Value (%a))" print_ty_value ty
    | Naked_immediate ty ->
      Format.fprintf ppf "(Naked_immediate (%a))" print_ty_naked_immediate ty
    | Naked_float ty ->
      Format.fprintf ppf "(Naked_float (%a))" print_ty_naked_float ty
    | Naked_int32 ty ->
      Format.fprintf ppf "(Naked_int32 (%a))" print_ty_naked_int32 ty
    | Naked_int64 ty ->
      Format.fprintf ppf "(Naked_int64 (%a))" print_ty_naked_int64 ty
    | Naked_nativeint ty ->
      Format.fprintf ppf "(Naked_nativeint (%a))" print_ty_naked_nativeint ty

  let augment_with_variable (t : t) var : t =
    let var = Some var in
    match t with
    | Value ty -> Value { ty with var; }
    | Naked_immediate ty -> Naked_immediate { ty with var; }
    | Naked_float ty -> Naked_float { ty with var; }
    | Naked_int32 ty -> Naked_int32 { ty with var; }
    | Naked_int64 ty -> Naked_int64 { ty with var; }
    | Naked_nativeint ty -> Naked_nativeint { ty with var; }

  let augment_with_symbol_internal (t : t) symbol field : t =
    let symbol = Some (symbol, field) in
    match t with
    | Value ty -> Value { ty with symbol; }
    | Naked_immediate ty -> Naked_immediate { ty with symbol; }
    | Naked_float ty -> Naked_float { ty with symbol; }
    | Naked_int32 ty -> Naked_int32 { ty with symbol; }
    | Naked_int64 ty -> Naked_int64 { ty with symbol; }
    | Naked_nativeint ty -> Naked_nativeint { ty with symbol; }

  let augment_with_symbol t symbol =
    augment_with_symbol_internal t symbol None

  let augment_with_symbol_field t symbol ~field =
    augment_with_symbol_internal t symbol (Some field)

  let replace_variable (t : t) var : t =
    match t with
    | Value ty -> Value { ty with var; }
    | Naked_immediate ty -> Naked_immediate { ty with var; }
    | Naked_float ty -> Naked_float { ty with var; }
    | Naked_int32 ty -> Naked_int32 { ty with var; }
    | Naked_int64 ty -> Naked_int64 { ty with var; }
    | Naked_nativeint ty -> Naked_nativeint { ty with var; }

  let unknown_as_ty_value reason scanning : ty_value =
    { descr = Ok (Unknown (reason, scanning));
      var = None;
      symbol = None;
    }

  let unknown_as_resolved_ty_value reason scanning : resolved_ty_value =
    { descr = Unknown (reason, scanning);
      var = None;
      symbol = None;
    }

  let unknown_as_resolved_ty_naked_immediate reason scanning
        : resolved_ty_naked_immediate =
    { descr = Unknown (reason, scanning);
      var = None;
      symbol = None;
    }

  let unknown_as_resolved_ty_naked_float reason scanning
        : resolved_ty_naked_float =
    { descr = Unknown (reason, scanning);
      var = None;
      symbol = None;
    }

  let unknown_as_resolved_ty_naked_int32 reason scanning
        : resolved_ty_naked_int32 =
    { descr = Unknown (reason, scanning);
      var = None;
      symbol = None;
    }

  let unknown_as_resolved_ty_naked_int64 reason scanning
        : resolved_ty_naked_int64 =
    { descr = Unknown (reason, scanning);
      var = None;
      symbol = None;
    }

  let unknown_as_resolved_ty_naked_nativeint reason scanning
        : resolved_ty_naked_nativeint =
    { descr = Unknown (reason, scanning);
      var = None;
      symbol = None;
    }

  let bottom (kind : K.t) : t =
    match kind with
    | Value _ ->
      Value {
        descr = Ok Bottom;
        var = None;
        symbol = None;
      }
    | Naked_immediate ->
      Naked_immediate {
        descr = Ok Bottom;
        var = None;
        symbol = None;
      }
    | Naked_float ->
      Naked_float {
        descr = Ok Bottom;
        var = None;
        symbol = None;
      }
    | Naked_int32 ->
      Naked_int32 {
        descr = Ok Bottom;
        var = None;
        symbol = None;
      }
    | Naked_int64 ->
      Naked_int64 {
        descr = Ok Bottom;
        var = None;
        symbol = None;
      }
    | Naked_nativeint ->
      Naked_nativeint {
        descr = Ok Bottom;
        var = None;
        symbol = None;
      }

  let this_naked_immediate (i : Immediate.t) : t =
    let i : of_kind_naked_immediate = Naked_immediate i in
    Naked_immediate {
      descr = Ok (Ok i);
      var = None;
      symbol = None;
    }

  let this_naked_float f : t =
    let f : of_kind_naked_float = Naked_float f in
    Naked_float {
      descr = Ok (Ok f);
      var = None;
      symbol = None;
    }

  let this_naked_int32 n : t =
    let n : of_kind_naked_int32 = Naked_int32 n in
    Naked_int32 {
      descr = Ok (Ok n);
      var = None;
      symbol = None;
    }

  let this_naked_int64 n =
    let n : of_kind_naked_int64 = Naked_int64 n in
    Naked_int64 {
      descr = Ok (Ok n);
      var = None;
      symbol = None;
    }

  let this_naked_nativeint n : t =
    let n : of_kind_naked_nativeint = Naked_nativeint n in
    Naked_nativeint {
      descr = Ok (Ok n);
      var = None;
      symbol = None;
    }

  let tag_immediate (t : t) : t =
    match t with
    | Naked_immediate ty_naked_immediate ->
      Value {
        descr = Ok (Ok (Singleton (Tagged_immediate ty_naked_immediate)));
        var = None;
        symbol = None;
      }
    | Value _
    | Naked_float _
    | Naked_int32 _
    | Naked_int64 _
    | Naked_nativeint _ ->
      Misc.fatal_errorf "Expected type of kind [Naked_immediate] but got %a"
        print t

  let box_float (t : t) : t =
    match t with
    | Naked_float ty_naked_float ->
      Value {
        descr = Ok (Ok (Singleton (Boxed_float ty_naked_float)));
        var = None;
        symbol = None;
      }
    | Value _
    | Naked_immediate _
    | Naked_int32 _
    | Naked_int64 _
    | Naked_nativeint _ ->
      Misc.fatal_errorf "Expected type of kind [Naked_float] but got %a"
        print t

  let box_int32 (t : t) : t =
    match t with
    | Naked_int32 ty_naked_int32 ->
      Value {
        descr = Ok (Ok (Singleton (Boxed_int32 ty_naked_int32)));
        var = None;
        symbol = None;
      }
    | Value _
    | Naked_immediate _
    | Naked_float _
    | Naked_int64 _
    | Naked_nativeint _ ->
      Misc.fatal_errorf "Expected type of kind [Naked_int32] but got %a"
        print t

  let box_int64 (t : t) : t =
    match t with
    | Naked_int64 ty_naked_int64 ->
      Value {
        descr = Ok (Ok (Singleton (Boxed_int64 ty_naked_int64)));
        var = None;
        symbol = None;
      }
    | Value _
    | Naked_immediate _
    | Naked_float _
    | Naked_int32 _
    | Naked_nativeint _ ->
      Misc.fatal_errorf "Expected type of kind [Naked_int64] but got %a"
        print t

  let box_nativeint (t : t) : t =
    match t with
    | Naked_nativeint ty_naked_nativeint ->
      Value {
        descr = Ok (Ok (Singleton (Boxed_nativeint ty_naked_nativeint)));
        var = None;
        symbol = None;
      }
    | Value _
    | Naked_immediate _
    | Naked_float _
    | Naked_int32 _
    | Naked_int64 _ ->
      Misc.fatal_errorf "Expected type of kind [Naked_nativeint] but got %a"
        print t

  let this_tagged_immediate i : t =
    let i : ty_naked_immediate =
      let i : of_kind_naked_immediate = Naked_immediate i in
      { descr = Ok (Ok i);
        var = None;
        symbol = None;
      }
    in
    Value {
      descr = Ok (Ok (Singleton (Tagged_immediate i)));
      var = None;
      symbol = None;
    }

  let this_boxed_float f =
    let f : ty_naked_float =
      let f : of_kind_naked_float = Naked_float f in
      { descr = Ok (Ok f);
        var = None;
        symbol = None;
      }
    in
    Value {
      descr = Ok (Ok (Singleton (Boxed_float f)));
      var = None;
      symbol = None;
    }

  let this_boxed_int32 n =
    let n : ty_naked_int32 =
      let n : of_kind_naked_int32 = Naked_int32 n in
      { descr = Ok (Ok n);
        var = None;
        symbol = None;
      }
    in
    Value {
      descr = Ok (Ok (Singleton (Boxed_int32 n)));
      var = None;
      symbol = None;
    }

  let this_boxed_int64 n =
    let n : ty_naked_int64 =
      let n : of_kind_naked_int64 = Naked_int64 n in
      { descr = Ok (Ok n);
        var = None;
        symbol = None;
      }
    in
    Value {
      descr = Ok (Ok (Singleton (Boxed_int64 n)));
      var = None;
      symbol = None;
    }

  let this_boxed_nativeint n =
    let n : ty_naked_nativeint =
      let n : of_kind_naked_nativeint = Naked_nativeint n in
      { descr = Ok (Ok n);
        var = None;
        symbol = None;
      }
    in
    Value {
      descr = Ok (Ok (Singleton (Boxed_nativeint n)));
      var = None;
      symbol = None;
    }

  let immutable_string_as_ty_value str : ty_value =
    let string_ty : string_ty =
      { contents = Contents str;
        size = String.length str;
      }
    in
    { descr = Ok (Ok (Singleton (String string_ty)));
      var = None;
      symbol = None;
    }

  let this_immutable_string str : t =
    Value (immutable_string_as_ty_value str)

  let mutable_string ~size : t =
    let string_ty : string_ty =
      { contents = Unknown_or_mutable;
        size;
      }
    in
    Value {
      descr = Ok (Ok (Singleton (String string_ty)));
      var = None;
      symbol = None;
    }

  let immutable_float_array fields : t =
    let fields =
      Array.map (fun (field : t) ->
          match field with
          | Naked_float ty_naked_float -> ty_naked_float
          | Value _ | Naked_immediate _ | Naked_int32 _ | Naked_int64 _
          | Naked_nativeint _ ->
            Misc.fatal_errorf "Can only form [Float_array] types with fields \
                of kind [Naked_float].  Wrong field type: %a"
              print field)
        fields
    in
    let float_array : float_array_ty =
      { contents = Contents fields;
        size = Array.length fields;
      }
    in
    Value {
      descr = Ok (Ok (Singleton (Float_array float_array)));
      var = None;
      symbol = None;
    }

  let mutable_float_array ~size : t =
    let float_array : float_array_ty =
      { contents = Unknown_or_mutable;
        size;
      }
    in
    Value {
      descr = Ok (Ok (Singleton (Float_array float_array)));
      var = None;
      symbol = None;
    }

  let block tag fields : t =
    let fields =
      Array.map (fun (field : t) ->
          match field with
          | Value ty_value -> ty_value
          | Naked_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
          | Naked_nativeint _ ->
            Misc.fatal_errorf "Can only form [Block] types with fields of \
                kind [Value].  Wrong field type: %a"
              print field)
        fields
    in
    Value {
      descr = Ok (Ok (Singleton (Block (tag, fields))));
      var = None;
      symbol = None;
    }

  let export_id_loaded_lazily (kind : K.t) export_id : t =
    match kind with
    | Value _ ->
      Value {
        descr = Load_lazily (Export_id export_id);
        var = None;
        symbol = None;
      }
    | Naked_immediate ->
      Naked_immediate {
        descr = Load_lazily (Export_id export_id);
        var = None;
        symbol = None;
      }
    | Naked_float ->
      Naked_float {
        descr = Load_lazily (Export_id export_id);
        var = None;
        symbol = None;
      }
    | Naked_int32 ->
      Naked_int32 {
        descr = Load_lazily (Export_id export_id);
        var = None;
        symbol = None;
      }
    | Naked_int64 ->
      Naked_int64 {
        descr = Load_lazily (Export_id export_id);
        var = None;
        symbol = None;
      }
    | Naked_nativeint ->
      Naked_nativeint {
        descr = Load_lazily (Export_id export_id);
        var = None;
        symbol = None;
      }

  let symbol_loaded_lazily (kind : K.t) sym : t =
    match kind with
    | Value _ ->
      Value {
        descr = Load_lazily (Symbol sym);
        var = None;
        symbol = None;
      }
    | Naked_immediate ->
      Naked_immediate {
        descr = Load_lazily (Symbol sym);
        var = None;
        symbol = None;
      }
    | Naked_float ->
      Naked_float {
        descr = Load_lazily (Symbol sym);
        var = None;
        symbol = None;
      }
    | Naked_int32 ->
      Naked_int32 {
        descr = Load_lazily (Symbol sym);
        var = None;
        symbol = None;
      }
    | Naked_int64 ->
      Naked_int64 {
        descr = Load_lazily (Symbol sym);
        var = None;
        symbol = None;
      }
    | Naked_nativeint ->
      Naked_nativeint {
        descr = Load_lazily (Symbol sym);
        var = None;
        symbol = None;
      }

  let any_naked_immediate () : t =
    Naked_immediate
      { descr = Ok (Unknown (Other, ()));
        var = None;
        symbol = None;
      }

  let any_naked_float () : t =
    Naked_float
      { descr = Ok (Unknown (Other, ()));
        var = None;
        symbol = None;
      }

  let any_naked_int32 () : t =
    Naked_int32
      { descr = Ok (Unknown (Other, ()));
        var = None;
        symbol = None;
      }

  let any_naked_int64 () : t =
    Naked_int64
      { descr = Ok (Unknown (Other, ()));
        var = None;
        symbol = None;
      }

  let any_naked_nativeint () : t =
    Naked_nativeint
      { descr = Ok (Unknown (Other, ()));
        var = None;
        symbol = None;
      }

  let any_value scanning unknown_because_of : t =
    Value
      { descr = Ok (Unknown (unknown_because_of, scanning));
        var = None;
        symbol = None;
      }

  let unknown (kind : K.t) unknown_because_of =
    match kind with
    | Value scanning -> any_value scanning unknown_because_of
    | Naked_immediate -> any_naked_immediate ()
    | Naked_float -> any_naked_float ()
    | Naked_int32 -> any_naked_int32 ()
    | Naked_int64 -> any_naked_int64 ()
    | Naked_nativeint -> any_naked_nativeint ()

  let any_tagged_immediate () : t =
    let i : ty_naked_immediate =
      { descr = Ok (Unknown (Other, ()));
        var = None;
        symbol = None;
      }
    in
    Value {
      descr = Ok (Ok (Singleton (Tagged_immediate i)));
      var = None;
      symbol = None;
    }

  let any_boxed_float () =
    let f : ty_naked_float =
      { descr = Ok (Unknown (Other, ()));
        var = None;
        symbol = None;
      }
    in
    Value {
      descr = Ok (Ok (Singleton (Boxed_float f)));
      var = None;
      symbol = None;
    }

  let any_boxed_int32 () =
    let n : ty_naked_int32 =
      { descr = Ok (Unknown (Other, ()));
        var = None;
        symbol = None;
      }
    in
    Value {
      descr = Ok (Ok (Singleton (Boxed_int32 n)));
      var = None;
      symbol = None;
    }

  let any_boxed_int64 () =
    let n : ty_naked_int64 =
      { descr = Ok (Unknown (Other, ()));
        var = None;
        symbol = None;
      }
    in
    Value {
      descr = Ok (Ok (Singleton (Boxed_int64 n)));
      var = None;
      symbol = None;
    }

  let any_boxed_nativeint () =
    let n : ty_naked_nativeint =
      { descr = Ok (Unknown (Other, ()));
        var = None;
        symbol = None;
      }
    in
    Value {
      descr = Ok (Ok (Singleton (Boxed_nativeint n)));
      var = None;
      symbol = None;
    }

  let resolved_ty_value_for_predefined_exception ~name ~symbol
        : resolved_ty_value =
    let fields =
      [| immutable_string_as_ty_value name;
         unknown_as_ty_value Other Must_scan;
      |]
    in
    { descr = Ok (Singleton (Block (Tag.Scannable.object_tag, fields)));
      var = None;
      symbol = Some (symbol, None);
    }

  module type Importer = sig
    val import_value_type_as_resolved_ty_value
       : ty_value
      -> resolved_ty_value

    val import_naked_immediate_type_as_resolved_ty_naked_immediate
       : ty_naked_immediate
      -> resolved_ty_naked_immediate

    val import_naked_float_type_as_resolved_ty_naked_float
       : ty_naked_float
      -> resolved_ty_naked_float

    val import_naked_int32_type_as_resolved_ty_naked_int32
       : ty_naked_int32
      -> resolved_ty_naked_int32

    val import_naked_int64_type_as_resolved_ty_naked_int64
       : ty_naked_int64
      -> resolved_ty_naked_int64

    val import_naked_nativeint_type_as_resolved_ty_naked_nativeint
       : ty_naked_nativeint
      -> resolved_ty_naked_nativeint

    val import_value_type : ty_value -> resolved_t
    val import_naked_immediate_type : ty_naked_immediate -> resolved_t
    val import_naked_float_type : ty_naked_float -> resolved_t
    val import_naked_int32_type : ty_naked_int32 -> resolved_t
    val import_naked_int64_type : ty_naked_int64 -> resolved_t
    val import_naked_nativeint_type : ty_naked_nativeint -> resolved_t
  end

  module type Importer_intf = sig
    val import_export_id : Export_id.t -> t option
    val import_symbol : Symbol.t -> t option
    val symbol_is_predefined_exception : Symbol.t -> string option
  end

  type 'a with_importer = importer:(module Importer) -> 'a

  type 'a create_resolved_t_result =
    | Ok of 'a
    | Load_lazily_again of load_lazily

  module Make_importer (S : sig
    val import_export_id : Export_id.t -> t option
    val import_symbol : Symbol.t -> t option
    val symbol_is_predefined_exception : Symbol.t -> string option
  end) : Importer = struct
    type 'a import_result =
      | Ok of 'a
      | Treat_as_unknown_must_scan of unknown_because_of

    let import_type (type a) ll
          ~(create_resolved_t : t -> a create_resolved_t_result)
          ~(resolve_predefined_exception : Symbol.t -> a option) =
      let rec import_type (ll : load_lazily) ~export_ids_seen ~symbols_seen
            : a import_result =
        match ll with
        | Export_id id ->
          if Export_id.Set.mem id export_ids_seen then begin
            Misc.fatal_errorf "Circularity whilst resolving export ID %a"
              Export_id.print id
          end;
          begin match S.import_export_id id with
          | None -> Treat_as_unknown_must_scan (Unresolved_value (Export_id id))
          | Some t ->
            match create_resolved_t t with
            | Ok resolved_t -> Ok resolved_t
            | Load_lazily_again ll ->
              let export_ids_seen = Export_id.Set.add id export_ids_seen in
              import_type ll ~export_ids_seen ~symbols_seen
          end
        | Symbol sym ->
          match resolve_predefined_exception sym with
          | Some resolved_t -> Ok resolved_t
          | None ->
            if Symbol.Set.mem sym symbols_seen then begin
              Misc.fatal_errorf "Circularity whilst resolving symbol %a"
                Symbol.print sym
            end;
            begin match S.import_symbol sym with
            | None -> Treat_as_unknown_must_scan (Unresolved_value (Symbol sym))
            | Some t ->
              match create_resolved_t t with
              (* CR mshinwell: We used to [augment_with_symbol] here but maybe
                 we don't need it any more? *)
              | Ok resolved_t -> Ok resolved_t
              | Load_lazily_again ll ->
                let symbols_seen = Symbol.Set.add sym symbols_seen in
                import_type ll ~export_ids_seen ~symbols_seen
            end
      in
      import_type ll ~export_ids_seen:Export_id.Set.empty
        ~symbols_seen:Symbol.Set.empty

    let import_value_type_as_resolved_ty_value (ty : ty_value)
          : resolved_ty_value =
      match ty.descr with
      | Ok descr ->
        { descr;
          var = ty.var;
          symbol = ty.symbol;
        }
      | Load_lazily load_lazily ->
        let resolve_predefined_exception sym =
          match S.symbol_is_predefined_exception sym with
          | None -> None
          | Some name ->
            Some (resolved_ty_value_for_predefined_exception ~name ~symbol:sym)
        in
        let create_resolved_t t : resolved_ty_value create_resolved_t_result =
          match t with
          | Value ty ->
            begin match ty.descr with
            | Ok descr ->
              Ok {
                descr;
                var = ty.var;
                symbol = ty.symbol;
              }
            | Load_lazily ll -> Load_lazily_again ll
            end
          | Naked_immediate _
          | Naked_float _
          | Naked_int32 _
          | Naked_int64 _
          | Naked_nativeint _ ->
            Misc.fatal_errorf "Kind mismatch when importing %a; expected kind \
                [Value]"
              print_load_lazily load_lazily
        in
        let result =
          import_type load_lazily ~create_resolved_t
            ~resolve_predefined_exception
        in
        match result with
        | Ok result -> result
        | Treat_as_unknown_must_scan reason ->
          unknown_as_resolved_ty_value reason Must_scan

    let import_value_type ty : resolved_t =
      Value (import_value_type_as_resolved_ty_value ty)

    let import_naked_immediate_type_as_resolved_ty_naked_immediate
          (ty : ty_naked_immediate) : resolved_ty_naked_immediate =
      match ty.descr with
      | Ok descr ->
        { descr;
          var = ty.var;
          symbol = ty.symbol;
        }
      | Load_lazily load_lazily ->
        let resolve_predefined_exception _sym = None in
        let create_resolved_t t
              : resolved_ty_naked_immediate create_resolved_t_result =
          match t with
          | Naked_immediate ty ->
            begin match ty.descr with
            | Ok descr ->
              Ok {
                descr;
                var = ty.var;
                symbol = ty.symbol;
              }
            | Load_lazily ll -> Load_lazily_again ll
            end
          | Value _
          | Naked_float _
          | Naked_int32 _
          | Naked_int64 _
          | Naked_nativeint _ ->
            Misc.fatal_errorf "Kind mismatch when importing %a; expected kind \
                [Naked_immediate]"
              print_load_lazily load_lazily
        in
        let result =
          import_type load_lazily ~create_resolved_t
            ~resolve_predefined_exception
        in
        match result with
        | Ok result -> result
        | Treat_as_unknown_must_scan reason ->
          unknown_as_resolved_ty_naked_immediate reason ()

    let import_naked_immediate_type ty : resolved_t =
      Naked_immediate (
        import_naked_immediate_type_as_resolved_ty_naked_immediate ty)

    let import_naked_float_type_as_resolved_ty_naked_float
          (ty : ty_naked_float) : resolved_ty_naked_float =
      match ty.descr with
      | Ok descr ->
        { descr;
          var = ty.var;
          symbol = ty.symbol;
        }
      | Load_lazily load_lazily ->
        let resolve_predefined_exception _sym = None in
        let create_resolved_t t
              : resolved_ty_naked_float create_resolved_t_result =
          match t with
          | Naked_float ty ->
            begin match ty.descr with
            | Ok descr ->
              Ok {
                descr;
                var = ty.var;
                symbol = ty.symbol;
              }
            | Load_lazily ll -> Load_lazily_again ll
            end
          | Value _
          | Naked_immediate _
          | Naked_int32 _
          | Naked_int64 _
          | Naked_nativeint _ ->
            Misc.fatal_errorf "Kind mismatch when importing %a; expected kind \
                [Naked_float]"
              print_load_lazily load_lazily
        in
        let result =
          import_type load_lazily ~create_resolved_t
            ~resolve_predefined_exception
        in
        match result with
        | Ok result -> result
        | Treat_as_unknown_must_scan reason ->
          unknown_as_resolved_ty_naked_float reason ()

    let import_naked_float_type ty : resolved_t =
      Naked_float (import_naked_float_type_as_resolved_ty_naked_float ty)

    let import_naked_int32_type_as_resolved_ty_naked_int32
          (ty : ty_naked_int32) : resolved_ty_naked_int32 =
      match ty.descr with
      | Ok descr ->
        { descr;
          var = ty.var;
          symbol = ty.symbol;
        }
      | Load_lazily load_lazily ->
        let resolve_predefined_exception _sym = None in
        let create_resolved_t t
              : resolved_ty_naked_int32 create_resolved_t_result =
          match t with
          | Naked_int32 ty ->
            begin match ty.descr with
            | Ok descr ->
              Ok {
                descr;
                var = ty.var;
                symbol = ty.symbol;
              }
            | Load_lazily ll -> Load_lazily_again ll
            end
          | Value _
          | Naked_immediate _
          | Naked_float _
          | Naked_int64 _
          | Naked_nativeint _ ->
            Misc.fatal_errorf "Kind mismatch when importing %a; expected kind \
                [Naked_int32]"
              print_load_lazily load_lazily
        in
        let result =
          import_type load_lazily ~create_resolved_t
            ~resolve_predefined_exception
        in
        match result with
        | Ok result -> result
        | Treat_as_unknown_must_scan reason ->
          unknown_as_resolved_ty_naked_int32 reason ()

    let import_naked_int32_type ty : resolved_t =
      Naked_int32 (import_naked_int32_type_as_resolved_ty_naked_int32 ty)

    let import_naked_int64_type_as_resolved_ty_naked_int64
          (ty : ty_naked_int64) : resolved_ty_naked_int64 =
      match ty.descr with
      | Ok descr ->
        { descr;
          var = ty.var;
          symbol = ty.symbol;
        }
      | Load_lazily load_lazily ->
        let resolve_predefined_exception _sym = None in
        let create_resolved_t t
              : resolved_ty_naked_int64 create_resolved_t_result =
          match t with
          | Naked_int64 ty ->
            begin match ty.descr with
            | Ok descr ->
              Ok {
                descr;
                var = ty.var;
                symbol = ty.symbol;
              }
            | Load_lazily ll -> Load_lazily_again ll
            end
          | Value _
          | Naked_immediate _
          | Naked_float _
          | Naked_int32 _
          | Naked_nativeint _ ->
            Misc.fatal_errorf "Kind mismatch when importing %a; expected kind \
                [Naked_int64]"
              print_load_lazily load_lazily
        in
        let result =
          import_type load_lazily ~create_resolved_t
            ~resolve_predefined_exception
        in
        match result with
        | Ok result -> result
        | Treat_as_unknown_must_scan reason ->
          unknown_as_resolved_ty_naked_int64 reason ()

    let import_naked_int64_type ty : resolved_t =
      Naked_int64 (import_naked_int64_type_as_resolved_ty_naked_int64 ty)

    let import_naked_nativeint_type_as_resolved_ty_naked_nativeint
          (ty : ty_naked_nativeint) : resolved_ty_naked_nativeint =
      match ty.descr with
      | Ok descr ->
        { descr;
          var = ty.var;
          symbol = ty.symbol;
        }
      | Load_lazily load_lazily ->
        let resolve_predefined_exception _sym = None in
        let create_resolved_t t
              : resolved_ty_naked_nativeint create_resolved_t_result =
          match t with
          | Naked_nativeint ty ->
            begin match ty.descr with
            | Ok descr ->
              Ok {
                descr;
                var = ty.var;
                symbol = ty.symbol;
              }
            | Load_lazily ll -> Load_lazily_again ll
            end
          | Value _
          | Naked_immediate _
          | Naked_float _
          | Naked_int32 _
          | Naked_int64 _ ->
            Misc.fatal_errorf "Kind mismatch when importing %a; expected kind \
                [Naked_nativeint]"
              print_load_lazily load_lazily
        in
        let result =
          import_type load_lazily ~create_resolved_t
            ~resolve_predefined_exception
        in
        match result with
        | Ok result -> result
        | Treat_as_unknown_must_scan reason ->
          unknown_as_resolved_ty_naked_nativeint reason ()

    let import_naked_nativeint_type ty : resolved_t =
      Naked_nativeint (
        import_naked_nativeint_type_as_resolved_ty_naked_nativeint ty)
  end

  (* CR pchambart:  (This was written for the "join" case)
     merging the closure value might loose information in the
     case of one branch having the approximation and the other
     having 'Unknown'. We could imagine such as

     {[if ... then M1.f else M2.f]}

     where M1 is where the function is defined and M2 is

     {[let f = M3.f]}

     and M3 is

     {[let f = M1.f]}

     with the cmx for M3 missing

     Since we know that the approximation comes from the same
     value, we know that both version provide additional
     information on the value. Hence what we really want is an
     approximation intersection, not an union (that this join
     is).
     mshinwell: changed to meet *)

  let must_scan_of_kind_value_or_unknown_or_bottom
        (o : (of_kind_value, _) or_unknown_or_bottom) : K.scanning =
    match o with
    | Unknown _ -> Must_scan
    | Bottom -> Can_scan
    | Ok of_kind_value ->
      let rec must_scan_of_kind_value (o : of_kind_value) : K.scanning =
        match o with
        | Singleton (Tagged_immediate _) -> Can_scan
        | Singleton _ -> Must_scan
        | Union (w1, w2) ->
          K.join_scanning (must_scan_of_kind_value w1.descr)
            (must_scan_of_kind_value w2.descr)
      in
      must_scan_of_kind_value of_kind_value

  let kind ~importer (t : t) =
    match t with
    | Naked_immediate _ -> K.naked_immediate ()
    | Naked_float _ -> K.naked_float ()
    | Naked_int32 _ -> K.naked_int32 ()
    | Naked_int64 _ -> K.naked_int64 ()
    | Naked_nativeint _ -> K.naked_nativeint ()
    | Value ty ->
      let module I = (val importer : Importer) in
      let resolved_ty_value = I.import_value_type_as_resolved_ty_value ty in
      let scanning =
        must_scan_of_kind_value_or_unknown_or_bottom resolved_ty_value.descr
      in
      K.value scanning

(*
  (* CR mshinwell: read carefully *)
  let refine_using_value_kind t (kind : Lambda.value_kind) =
    match kind with
    | Pgenval -> t
    | Pfloatval ->
      begin match t.descr with
      | Boxed_or_encoded_number (Boxed Float,
          { descr = Naked_number (Float _); _ }) ->
        t
      | Unknown ((Unboxed_float | Bottom), reason) ->
        { t with
          descr = Boxed_or_encoded_number (Boxed Float,
            just_descr (Unknown (K.unboxed_float (), reason)));
        }
      | Unknown (
          (Value | Tagged_int | Naked_int | Naked_int32 | Naked_int64
            | Unboxed_nativeint), _) ->
        Misc.fatal_errorf "Wrong type for Pfloatval kind: %a"
          print t
      | Union _
      | Naked_number _
      | Boxed_or_encoded_number _
      | Set_of_closures _
      | Closure _
      | Immutable_string _
      | Mutable_string _
      | Float_array _
      | Bottom ->
        (* Unreachable *)
        { t with descr = Bottom }
      | Load_lazily _ ->
        (* We don't know yet *)
        t
      end
    (* CR mshinwell: Do we need more cases here?  We could add Pintval *)
    | _ -> t
*)

  let closure ?closure_var ?set_of_closures_var ?set_of_closures_symbol
        set_of_closures closure_id : t =
    let set_of_closures : resolved_ty_set_of_closures =
      { descr = Ok set_of_closures;
        var = set_of_closures_var;
        symbol = Misc.may_map (fun s -> s, None) set_of_closures_symbol;
      }
    in
    Value {
      descr = Ok (Ok (Singleton (Closure { set_of_closures; closure_id; })));
      var = closure_var;
      symbol = None;
    }

  (* CR mshinwell: crappy name *)
  let create_set_of_closures ~function_decls ~closure_elements
        : set_of_closures =
    { function_decls;
      closure_elements;
    }

  let set_of_closures ?set_of_closures_var set_of_closures : t =
    Value {
      descr = Ok (Ok (Singleton (Set_of_closures set_of_closures)));
      var = set_of_closures_var;
      symbol = None;
    }

  let rec free_variables t acc =
    match t with
    | Value ty -> free_variables_ty_value ty acc
    | Naked_immediate ty -> free_variables_ty_naked_immediate ty acc
    | Naked_float ty -> free_variables_ty_naked_float ty acc
    | Naked_int32 ty -> free_variables_ty_naked_int32 ty acc
    | Naked_int64 ty -> free_variables_ty_naked_int64 ty acc
    | Naked_nativeint ty -> free_variables_ty_naked_nativeint ty acc

  and free_variables_ty_value ({ descr; var; _ } : ty_value) acc =
    let acc =
      match var with
      | None -> acc
      | Some var -> Variable.Set.add var acc
    in
    match descr with
    | Ok ((Unknown _) | Bottom) -> acc
    | Ok (Ok of_kind_value) ->
      free_variables_of_kind_value of_kind_value acc
    | Load_lazily _load_lazily ->
      (* Types saved in .cmx files cannot contain free variables. *)
      acc

  and free_variables_ty_naked_immediate ({ var; _ } : ty_naked_immediate) acc =
    match var with
    | None -> acc
    | Some var -> Variable.Set.add var acc

  and free_variables_ty_naked_float ({ var; _ } : ty_naked_float) acc =
    match var with
    | None -> acc
    | Some var -> Variable.Set.add var acc

  and free_variables_ty_naked_int32 ({ var; _ } : ty_naked_int32) acc =
    match var with
    | None -> acc
    | Some var -> Variable.Set.add var acc

  and free_variables_ty_naked_int64 ({ var; _ } : ty_naked_int64) acc =
    match var with
    | None -> acc
    | Some var -> Variable.Set.add var acc

  and free_variables_ty_naked_nativeint ({ var; _ } : ty_naked_nativeint) acc =
    match var with
    | None -> acc
    | Some var -> Variable.Set.add var acc

  and free_variables_set_of_closures (set_of_closures : set_of_closures) acc =
    let acc =
      Var_within_closure.Map.fold (fun _var ty_value acc ->
          free_variables_ty_value ty_value acc)
        set_of_closures.closure_elements acc
    in
    begin match set_of_closures.function_decls with
    | Non_inlinable function_decls ->
      Variable.Map.fold
        (fun _fun_var (decl : non_inlinable_function_declaration) acc ->
          List.fold_left (fun acc ty ->
            free_variables ty acc)
            acc
            decl.result)
        function_decls.funs
        acc
    | Inlinable function_decls ->
      Variable.Map.fold
        (fun _fun_var (decl : inlinable_function_declaration) acc ->
          let acc =
            List.fold_left (fun acc ty ->
              free_variables ty acc)
              acc
              decl.result
          in
          List.fold_left (fun acc (_param, ty) ->
              free_variables ty acc)
            acc
            decl.params)
        function_decls.funs
        acc
    end

  and free_variables_resolved_ty_set_of_closures
        ({ descr; var; _ } : resolved_ty_set_of_closures) acc =
    let acc =
      match var with
      | None -> acc
      | Some var -> Variable.Set.add var acc
    in
    match descr with
    | (Unknown _) | Bottom -> acc
    | Ok (set_of_closures) ->
      free_variables_set_of_closures set_of_closures acc

  and free_variables_of_kind_value (o : of_kind_value) acc =
    match o with
    | Singleton singleton ->
      begin match singleton with
      | Tagged_immediate i ->
        free_variables_ty_naked_immediate i acc
      | Boxed_float f ->
        free_variables_ty_naked_float f acc
      | Boxed_int32 n ->
        free_variables_ty_naked_int32 n acc
      | Boxed_int64 n ->
        free_variables_ty_naked_int64 n acc
      | Boxed_nativeint n ->
        free_variables_ty_naked_nativeint n acc
      | Block (_tag, fields) ->
        Array.fold_left (fun acc t -> free_variables_ty_value t acc)
          acc fields
      | Set_of_closures set_of_closures ->
        free_variables_set_of_closures set_of_closures acc
      | Closure { set_of_closures; closure_id = _; } ->
        free_variables_resolved_ty_set_of_closures set_of_closures acc
      | String _ -> acc
      | Float_array { contents; size = _; } ->
        begin match contents with
        | Contents ts ->
          Array.fold_left (fun acc t -> free_variables_ty_naked_float t acc)
            acc ts
        | Unknown_or_mutable -> acc
        end
      end
    | Union (w1, w2) ->
      let acc =
        match w1.var with
        | None -> acc
        | Some var -> Variable.Set.add var acc
      in
      let acc =
        match w2.var with
        | None -> acc
        | Some var -> Variable.Set.add var acc
      in
      free_variables_of_kind_value w2.descr
        (free_variables_of_kind_value w1.descr acc)

  let free_variables t =
    free_variables t Variable.Set.empty

  (* CR mshinwell: We need tests to check that [clean] matches up with
     [free_variables]. *)

  type cleaning_spec =
    | Available
    | Available_different_name of Variable.t
    | Unavailable

  let rec clean ~importer t classify =
    let clean_var var =
      match classify var with
      | Available -> Some var
      | Available_different_name new_var -> Some new_var
      | Unavailable -> None
    in
    let clean_var_opt var_opt =
      match var_opt with
      | None -> None
      | Some var ->
        match clean_var var with
        | None -> None
        | (Some var') as var_opt' ->
          if var == var' then var_opt
          else var_opt'
    in
    clean_t ~importer t clean_var_opt

  and clean_t ~importer (t : t) clean_var_opt : t =
    match t with
    | Value ty ->
      Value (clean_ty_value ~importer ty clean_var_opt)
    | Naked_immediate ty ->
      Naked_immediate (clean_ty_naked_immediate ~importer ty clean_var_opt)
    | Naked_float ty ->
      Naked_float (clean_ty_naked_float ~importer ty clean_var_opt)
    | Naked_int32 ty ->
      Naked_int32 (clean_ty_naked_int32 ~importer ty clean_var_opt)
    | Naked_int64 ty ->
      Naked_int64 (clean_ty_naked_int64 ~importer ty clean_var_opt)
    | Naked_nativeint ty ->
      Naked_nativeint (clean_ty_naked_nativeint ~importer ty clean_var_opt)

  and clean_ty_value ~importer ty_value clean_var_opt : ty_value =
    let module I = (val importer : Importer) in
    let ty_value = I.import_value_type_as_resolved_ty_value ty_value in
    let var = clean_var_opt ty_value.var in
    let descr : (of_kind_value, _) or_unknown_or_bottom =
      match ty_value.descr with
      | (Unknown _) | Bottom -> ty_value.descr
      | Ok of_kind_value ->
        Ok (clean_of_kind_value ~importer of_kind_value clean_var_opt)
    in
    { var;
      symbol = ty_value.symbol;
      descr = Ok descr;
    }

  and clean_resolved_ty_set_of_closures ~importer
        (resolved_ty_set_of_closures : resolved_ty_set_of_closures)
        clean_var_opt
        : resolved_ty_set_of_closures =
    let var = clean_var_opt resolved_ty_set_of_closures.var in
    let descr : (set_of_closures, _) or_unknown_or_bottom =
      match resolved_ty_set_of_closures.descr with
      | (Unknown _) | Bottom -> resolved_ty_set_of_closures.descr
      | Ok set_of_closures ->
        Ok (clean_set_of_closures ~importer set_of_closures clean_var_opt)
    in
    { var;
      symbol = resolved_ty_set_of_closures.symbol;
      descr = descr;
    }

  and clean_ty_naked_immediate ~importer ty_naked_immediate clean_var_opt
        : ty_naked_immediate =
    let module I = (val importer : Importer) in
    let ty_naked_immediate =
      I.import_naked_immediate_type_as_resolved_ty_naked_immediate
        ty_naked_immediate
    in
    let var = clean_var_opt ty_naked_immediate.var in
    { var;
      symbol = ty_naked_immediate.symbol;
      descr = Ok ty_naked_immediate.descr;
    }

  and clean_ty_naked_float ~importer ty_naked_float clean_var_opt
        : ty_naked_float =
    let module I = (val importer : Importer) in
    let ty_naked_float =
      I.import_naked_float_type_as_resolved_ty_naked_float ty_naked_float
    in
    let var = clean_var_opt ty_naked_float.var in
    { var;
      symbol = ty_naked_float.symbol;
      descr = Ok ty_naked_float.descr;
    }

  and clean_ty_naked_int32 ~importer ty_naked_int32 clean_var_opt
        : ty_naked_int32 =
    let module I = (val importer : Importer) in
    let ty_naked_int32 =
      I.import_naked_int32_type_as_resolved_ty_naked_int32 ty_naked_int32
    in
    let var = clean_var_opt ty_naked_int32.var in
    { var;
      symbol = ty_naked_int32.symbol;
      descr = Ok ty_naked_int32.descr;
    }

  and clean_ty_naked_int64 ~importer ty_naked_int64 clean_var_opt
        : ty_naked_int64 =
    let module I = (val importer : Importer) in
    let ty_naked_int64 =
      I.import_naked_int64_type_as_resolved_ty_naked_int64 ty_naked_int64
    in
    let var = clean_var_opt ty_naked_int64.var in
    { var;
      symbol = ty_naked_int64.symbol;
      descr = Ok ty_naked_int64.descr;
    }

  and clean_ty_naked_nativeint ~importer ty_naked_nativeint clean_var_opt
        : ty_naked_nativeint =
    let module I = (val importer : Importer) in
    let ty_naked_nativeint =
      I.import_naked_nativeint_type_as_resolved_ty_naked_nativeint
        ty_naked_nativeint
    in
    let var = clean_var_opt ty_naked_nativeint.var in
    { var;
      symbol = ty_naked_nativeint.symbol;
      descr = Ok ty_naked_nativeint.descr;
    }

  and clean_set_of_closures ~importer set_of_closures clean_var_opt =
    let closure_elements =
      Var_within_closure.Map.map (fun t ->
          clean_ty_value ~importer t clean_var_opt)
        set_of_closures.closure_elements
    in
    let function_decls : function_declarations =
    match set_of_closures.function_decls with
    | Non_inlinable function_decls ->
      let funs =
        Variable.Map.map
          (fun (decl : non_inlinable_function_declaration)
                : non_inlinable_function_declaration ->
            let result =
              List.map (fun ty ->
                clean_t ~importer ty clean_var_opt)
                decl.result
            in
            { result; })
          function_decls.funs
      in
      Non_inlinable { funs }
    | Inlinable function_decls ->
      let funs =
        Variable.Map.map
          (fun (decl : inlinable_function_declaration)
                : inlinable_function_declaration ->
            let params =
              List.map (fun (param, t) ->
                  param, clean_t ~importer t clean_var_opt)
                decl.params
            in
            let result =
              List.map (fun ty ->
                clean_t ~importer ty clean_var_opt)
                decl.result
            in
            { decl with
              params;
              result;
            })
          function_decls.funs
      in
      let function_decls : inlinable_function_declarations =
        { set_of_closures_id = function_decls.set_of_closures_id;
          set_of_closures_origin = function_decls.set_of_closures_origin;
          funs;
          invariant_params = function_decls.invariant_params;
          size = function_decls.size;
          direct_call_surrogates = function_decls.direct_call_surrogates;
        }
      in
      Inlinable function_decls
    in
    { function_decls;
      closure_elements = closure_elements;
    }

  and clean_of_kind_value ~importer (o : of_kind_value) clean_var_opt
        : of_kind_value =
    match o with
    | Singleton singleton ->
      let singleton : of_kind_value_singleton =
        match singleton with
        | Tagged_immediate i ->
          Tagged_immediate (clean_ty_naked_immediate ~importer i clean_var_opt)
        | Boxed_float f ->
          Boxed_float (clean_ty_naked_float ~importer f clean_var_opt)
        | Boxed_int32 n ->
          Boxed_int32 (clean_ty_naked_int32 ~importer n clean_var_opt)
        | Boxed_int64 n ->
          Boxed_int64 (clean_ty_naked_int64 ~importer n clean_var_opt)
        | Boxed_nativeint n ->
          Boxed_nativeint (clean_ty_naked_nativeint ~importer n clean_var_opt)
        | Block (tag, fields) ->
          let fields =
            Array.map (fun t -> clean_ty_value ~importer t clean_var_opt)
              fields
          in
          Block (tag, fields)
        | Set_of_closures set_of_closures ->
          Set_of_closures
            (clean_set_of_closures ~importer set_of_closures clean_var_opt)
        | Closure { set_of_closures; closure_id; } ->
          let set_of_closures =
            clean_resolved_ty_set_of_closures ~importer set_of_closures
              clean_var_opt
          in
          Closure { set_of_closures; closure_id; }
        | String _ -> singleton
        | Float_array { contents; size; } ->
          begin match contents with
          | Contents ts ->
            let ts =
              Array.map (fun t ->
                  clean_ty_naked_float ~importer t clean_var_opt)
                ts
            in
            Float_array { contents = Contents ts; size; }
          | Unknown_or_mutable -> singleton
          end
      in
      Singleton singleton
    | Union (w1, w2) ->
      let w1 =
        { var = clean_var_opt w1.var;
          symbol = w1.symbol;
          descr = clean_of_kind_value ~importer w1.descr clean_var_opt;
        }
      in
      let w2 =
        { var = clean_var_opt w2.var;
          symbol = w2.symbol;
          descr = clean_of_kind_value ~importer w2.descr clean_var_opt;
        }
      in
      Union (w1, w2)

  let join_unknown_payload_for_value _descr1 scanning1 descr2 scanning2_opt =
    let scanning2 : K.scanning =
      match scanning2_opt with
      | Some scanning2 -> scanning2
      | None -> must_scan_of_kind_value_or_unknown_or_bottom descr2
    in
    K.join_scanning scanning1 scanning2

  let join_unknown_payload_for_non_value _ty1 () _ty2 (_ : unit option) = ()

  type 'a or_form_union =
    | Joined of 'a
    | Form_union

  let join_ty (type a) (type u) ~importer ~importer_this_kind
        join_contents join_unknown_payload
        (ty1 : (a, u) ty) (ty2 : (a, u) ty) : (a, u) ty =
    let ty1 : (a, u) resolved_ty = importer_this_kind ty1 in
    let ty2 : (a, u) resolved_ty = importer_this_kind ty2 in
    let var =
      match ty1.var, ty2.var with
      | None, _ | _, None -> None
      | Some v1, Some v2 ->
        if Variable.equal v1 v2 then Some v1
        else None
    in
    let symbol =
      match ty1.symbol, ty2.symbol with
      | None, _ | _, None -> None
      | Some (v1, field1), Some (v2, field2) ->
        if Symbol.equal v1 v2 then
          match field1, field2 with
          | None, None -> ty1.symbol
          | Some f1, Some f2 when f1 = f2 -> ty1.symbol
          | _ -> None
        else None
    in
    let descr =
      match ty1.descr, ty2.descr with
      (* Care: we need to take the lub of the payloads of [Unknown]! *)
      | Unknown (reason1, payload1), Unknown (reason2, payload2) ->
        Unknown (join_unknown_because_of reason1 reason2,
          join_unknown_payload ty1.descr payload1 ty2.descr (Some payload2))
      | Unknown (reason, payload), _ ->
        Unknown (reason, join_unknown_payload ty1.descr payload ty2.descr None)
      | _, Unknown (reason, payload) ->
        Unknown (reason, join_unknown_payload ty2.descr payload ty1.descr None)
      | Bottom, _ -> ty2.descr
      | _, Bottom -> ty1.descr
      | Ok contents1, Ok contents2 ->
        join_contents ~importer ty1 contents1 ty2 contents2
    in
    let descr : (a, u) maybe_unresolved = Ok descr in
    let ty : (a, u) ty =
      { descr;
        var;
        symbol;
      }
    in
    ty

  let rec join_of_kind_value_singleton ~importer
        (t1 : of_kind_value_singleton) t2
        : (of_kind_value, K.scanning) or_unknown_or_bottom or_form_union =
    let singleton s =
      Joined ((Ok (Singleton s)) : _ or_unknown_or_bottom)
    in
    let unknown_must_scan =
      Joined ((Unknown (Other, K.Must_scan)) : _ or_unknown_or_bottom)
    in
    (* For cases where forming unions is fruitless, we just return [Unknown]. *)
    match t1, t2 with
    | Tagged_immediate ty1, Tagged_immediate ty2 ->
      singleton (Tagged_immediate (join_ty_naked_immediate ~importer ty1 ty2))
    | Boxed_float ty1, Boxed_float ty2 ->
      singleton (Boxed_float (join_ty_naked_float ~importer ty1 ty2))
    | Boxed_int32 ty1, Boxed_int32 ty2 ->
      singleton (Boxed_int32 (join_ty_naked_int32 ~importer ty1 ty2))
    | Boxed_int64 ty1, Boxed_int64 ty2 ->
      singleton (Boxed_int64 (join_ty_naked_int64 ~importer ty1 ty2))
    | Boxed_nativeint ty1, Boxed_nativeint ty2 ->
      singleton (Boxed_nativeint (join_ty_naked_nativeint ~importer ty1 ty2))
    | Block (tag1, fields1), Block (tag2, fields2) ->
      if not (Tag.Scannable.equal tag1 tag2) then Form_union
      else if Array.length fields1 <> Array.length fields2 then
        unknown_must_scan
      else
        let fields =
          Array.map2 (fun ty1 ty2 -> join_ty_value ~importer ty1 ty2)
            fields1 fields2
        in
        singleton (Block (tag1, fields))
    | Tagged_immediate _, Block _
    | Block _, Tagged_immediate _ ->
      (* These unions are used for unboxing of values of variant type. *)
      Form_union
    | Set_of_closures _, Set_of_closures _ -> Form_union
    | Closure _, Closure _ -> Form_union
    | String { contents = Contents str1; _ },
        String { contents = Contents str2; _ } ->
      if String.equal str1 str2 then singleton t1
      else unknown_must_scan
    | Float_array { contents = Contents ts1; _ },
        Float_array { contents = Contents ts2; _ } ->
      if Array.length ts1 <> Array.length ts2 then
        unknown_must_scan
      else
        let ts =
          Array.map2 (fun ty1 ty2 -> join_ty_naked_float ~importer ty1 ty2)
            ts1 ts2
        in
        singleton (Float_array {
          contents = Contents ts;
          size = Array.length ts;
        })
    | _, _ ->
      (* The only case which would not require scanning is
         [Tagged_immediate] versus itself, which is covered above. *)
      unknown_must_scan

  and join_of_kind_value ~importer ty1 (t1 : of_kind_value) ty2 t2
        : (of_kind_value, _) or_unknown_or_bottom =
    let form_union () : (of_kind_value, _) or_unknown_or_bottom =
      let w1 : of_kind_value with_var_and_symbol =
        { descr = t1;
          var = ty1.var;
          symbol = ty1.symbol;
        }
      in
      let w2 : of_kind_value with_var_and_symbol =
        { descr = t2;
          var = ty2.var;
          symbol = ty2.symbol;
        }
      in
      Ok (Union (w1, w2))
    in
    match t1, t2 with
    | Singleton s1, Singleton s2 ->
      begin match join_of_kind_value_singleton ~importer s1 s2 with
      | Joined join -> join
      | Form_union -> form_union ()
      end
    | Singleton _, Union _
    | Union _, Singleton _
    | Union _, Union _ -> form_union ()

  and join_of_kind_naked_immediate ~importer:_
        _ty1 (t1 : of_kind_naked_immediate) _ty2 (t2 : of_kind_naked_immediate)
        : (of_kind_naked_immediate, _) or_unknown_or_bottom =
    match t1, t2 with
    | Naked_immediate i1, Naked_immediate i2 ->
      if not (Immediate.equal i1 i2) then Unknown (Other, ())
      else Ok ((Naked_immediate i1) : of_kind_naked_immediate)

  and join_of_kind_naked_float ~importer:_
        _ty1 (t1 : of_kind_naked_float) _ty2 (t2 : of_kind_naked_float)
        : (of_kind_naked_float, _) or_unknown_or_bottom =
    match t1, t2 with
    | Naked_float i1, Naked_float i2 ->
      if not (Float.equal i1 i2) then Unknown (Other, ())
      else Ok ((Naked_float i1) : of_kind_naked_float)

  and join_of_kind_naked_int32 ~importer:_
        _ty1 (t1 : of_kind_naked_int32) _ty2 (t2 : of_kind_naked_int32)
        : (of_kind_naked_int32, _) or_unknown_or_bottom =
    match t1, t2 with
    | Naked_int32 i1, Naked_int32 i2 ->
      if not (Int32.equal i1 i2) then Unknown (Other, ())
      else Ok ((Naked_int32 i1) : of_kind_naked_int32)

  and join_of_kind_naked_int64 ~importer:_
        _ty1 (t1 : of_kind_naked_int64) _ty2 (t2 : of_kind_naked_int64)
        : (of_kind_naked_int64, _) or_unknown_or_bottom =
    match t1, t2 with
    | Naked_int64 i1, Naked_int64 i2 ->
      if not (Int64.equal i1 i2) then Unknown (Other, ())
      else Ok ((Naked_int64 i1) : of_kind_naked_int64)

  and join_of_kind_naked_nativeint ~importer:_
        _ty1 (t1 : of_kind_naked_nativeint) _ty2 (t2 : of_kind_naked_nativeint)
        : (of_kind_naked_nativeint, _) or_unknown_or_bottom =
    match t1, t2 with
    | Naked_nativeint i1, Naked_nativeint i2 ->
      if not (Targetint.equal i1 i2) then Unknown (Other, ())
      else Ok ((Naked_nativeint i1) : of_kind_naked_nativeint)

  and join_ty_value ~importer ty1 ty2 =
    let module I = (val importer : Importer) in
    join_ty ~importer
      ~importer_this_kind:I.import_value_type_as_resolved_ty_value
      join_of_kind_value join_unknown_payload_for_value ty1 ty2

  and join_ty_naked_immediate ~importer ty1 ty2 =
    let module I = (val importer : Importer) in
    join_ty ~importer
      ~importer_this_kind:
        I.import_naked_immediate_type_as_resolved_ty_naked_immediate
      join_of_kind_naked_immediate join_unknown_payload_for_non_value ty1 ty2

  and join_ty_naked_float ~importer ty1 ty2 =
    let module I = (val importer : Importer) in
    join_ty ~importer
      ~importer_this_kind:I.import_naked_float_type_as_resolved_ty_naked_float
      join_of_kind_naked_float join_unknown_payload_for_non_value ty1 ty2

  and join_ty_naked_int32 ~importer ty1 ty2 =
    let module I = (val importer : Importer) in
    join_ty ~importer
      ~importer_this_kind:I.import_naked_int32_type_as_resolved_ty_naked_int32
      join_of_kind_naked_int32 join_unknown_payload_for_non_value ty1 ty2

  and join_ty_naked_int64 ~importer ty1 ty2 =
    let module I = (val importer : Importer) in
    join_ty ~importer
      ~importer_this_kind:I.import_naked_int64_type_as_resolved_ty_naked_int64
      join_of_kind_naked_int64 join_unknown_payload_for_non_value ty1 ty2

  and join_ty_naked_nativeint ~importer ty1 ty2 =
    let module I = (val importer : Importer) in
    join_ty ~importer
      ~importer_this_kind:
        I.import_naked_nativeint_type_as_resolved_ty_naked_nativeint
      join_of_kind_naked_nativeint join_unknown_payload_for_non_value ty1 ty2

  let join ~importer (t1 : t) (t2 : t) : t =
    if t1 == t2 then t1
    else
      match t1, t2 with
      | Value ty1, Value ty2 ->
        Value (join_ty_value ~importer ty1 ty2)
      | Naked_immediate ty1, Naked_immediate ty2 ->
        Naked_immediate (join_ty_naked_immediate ~importer ty1 ty2)
      | Naked_float ty1, Naked_float ty2 ->
        Naked_float (join_ty_naked_float ~importer ty1 ty2)
      | Naked_int32 ty1, Naked_int32 ty2 ->
        Naked_int32 (join_ty_naked_int32 ~importer ty1 ty2)
      | Naked_int64 ty1, Naked_int64 ty2 ->
        Naked_int64 (join_ty_naked_int64 ~importer ty1 ty2)
      | Naked_nativeint ty1, Naked_nativeint ty2 ->
        Naked_nativeint (join_ty_naked_nativeint ~importer ty1 ty2)
      | _, _ ->
        Misc.fatal_errorf "Cannot take the join of two types with different \
            kinds: %a and %a"
          print t1
          print t2
end
