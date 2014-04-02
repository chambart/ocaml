(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(*  bytegen.ml : translation of lambda terms to lists of instructions. *)

open Misc
open Asttypes
open Primitive
open Types
open Lambda
open Flambda
open Switch
open Instruct
open Symbol
open Ext_types

(**** Label generation ****)

let label_counter = ref 0

let new_label () =
  incr label_counter; !label_counter

(**** Operations on compilation environments. ****)

let empty_env =
  { ce_stack = VarMap.empty;
    ce_heap = VarMap.empty;
    ce_rec = VarMap.empty;
    ce_closures = ClosureFunctionMap.empty;
  }

(* Add a stack-allocated variable *)

let add_var id pos env =
  { env with ce_stack = VarMap.add id pos env.ce_stack }

let rec add_vars idlist pos env =
  match idlist with
  | [] -> env
  | id :: rem -> add_vars rem (pos + 1) (add_var id pos env)

let add_recclosure id pos env =
  { env with
    ce_closures =
      ClosureFunctionMap.add (Closure_function.create id) pos env.ce_closures }

let rec add_recclosures idlist pos env =
  match idlist with
  | [] -> env
  | id :: rem -> add_recclosures rem (pos + 1) (add_recclosure id pos env)

(**** Examination of the continuation ****)

(* Return a label to the beginning of the given continuation.
   If the sequence starts with a branch, use the target of that branch
   as the label, thus avoiding a jump to a jump. *)

let label_code = function
    Kbranch lbl :: _ as cont -> (lbl, cont)
  | Klabel lbl :: _ as cont -> (lbl, cont)
  | cont -> let lbl = new_label() in (lbl, Klabel lbl :: cont)

(* Return a branch to the continuation. That is, an instruction that,
   when executed, branches to the continuation or performs what the
   continuation performs. We avoid generating branches to branches and
   branches to returns. *)

let rec make_branch_2 lbl n cont =
  function
    Kreturn m :: _ -> (Kreturn (n + m), cont)
  | Klabel _ :: c  -> make_branch_2 lbl n cont c
  | Kpop m :: c    -> make_branch_2 lbl (n + m) cont c
  | _              ->
      match lbl with
        Some lbl -> (Kbranch lbl, cont)
      | None     -> let lbl = new_label() in (Kbranch lbl, Klabel lbl :: cont)

let make_branch cont =
  match cont with
    (Kbranch _ as branch) :: _ -> (branch, cont)
  | (Kreturn _ as return) :: _ -> (return, cont)
  | Kraise :: _ -> (Kraise, cont)
  | Klabel lbl :: _ -> make_branch_2 (Some lbl) 0 cont cont
  | _ ->  make_branch_2 (None) 0 cont cont

(* Avoid a branch to a label that follows immediately *)

let branch_to label cont = match cont with
| Klabel label0::_ when label = label0 -> cont
| _ -> Kbranch label::cont

(* Discard all instructions up to the next label.
   This function is to be applied to the continuation before adding a
   non-terminating instruction (branch, raise, return) in front of it. *)

let rec discard_dead_code = function
    [] -> []
  | (Klabel _ | Krestart | Ksetglobal _) :: _ as cont -> cont
  | _ :: cont -> discard_dead_code cont

(* Check if we're in tailcall position *)

let rec is_tailcall = function
    Kreturn _ :: _ -> true
  | Klabel _ :: c -> is_tailcall c
  | Kpop _ :: c -> is_tailcall c
  | _ -> false

(* Add a Kpop N instruction in front of a continuation *)

let rec add_pop n cont =
  if n = 0 then cont else
    match cont with
      Kpop m :: cont -> add_pop (n + m) cont
    | Kreturn m :: cont -> Kreturn(n + m) :: cont
    | Kraise :: _ -> cont
    | _ -> Kpop n :: cont

(* Add the constant "unit" in front of a continuation *)

let is_const_unit = function
  | Fconst (Fconst_pointer 0, _) -> true
  | _ -> false

let add_const_unit = function
    (Kacc _ | Kconst _ | Kgetglobal _ | Kpush_retaddr _) :: _ as cont -> cont
  | cont -> Kconst const_unit :: cont

let rec push_dummies n k = match n with
| 0 -> k
| _ -> Kconst const_unit::Kpush::push_dummies (n-1) k


(* *)

let structured = function
  | Fconst_base cst -> Const_base cst
  | Fconst_pointer ptr -> Const_pointer ptr
  | Fconst_float_array l -> Const_float_array l
  | Fconst_immstring s -> Const_immstring s

exception Not_constant

let rec constant = function
  | Fconst (cst, _) -> structured cst
  | Fprim (Pmakeblock (tag, Immutable), args, _, _) ->
      Const_block (tag, List.map constant args)
  | _ -> raise Not_constant

(**** Auxiliary for compiling "let rec" ****)

type rhs_kind =
  | RHS_block of int
  | RHS_floatblock of int
  | RHS_nonrec
;;

let rec check_recordwith_updates id e =
  match e with
  | Fsequence (Fprim ((Psetfield _ | Psetfloatfield _), [Fvar (id2, _); _], _, _),
               cont, _)
      -> id2 = id && check_recordwith_updates id cont
  | Fvar (id2, _) -> id2 = id
  | _ -> false
;;

let rec size_of_lambda = function
  | Ffunction ({ fu_closure = Fclosure (clos, _)}, _) ->
      if VarMap.cardinal clos.cl_fun.funs <> 1 then
        fatal_error ("Bytegen.comp_expr: recclosure");
      RHS_block (1 + VarMap.cardinal clos.cl_free_var)
  | Fclosure _ ->
      fatal_error ("Bytegen.size_of_lambda: unexpected closure")
  | Ffunction _->
      fatal_error ("Bytegen.size_of_lambda: unexpected function")
  | Flet (Not_assigned, id, Fprim (Pduprecord (kind, size), _, _, _), body, _)
    when check_recordwith_updates id body ->
      begin match kind with
      | Record_regular -> RHS_block size
      | Record_float -> RHS_floatblock size
      end
  | Flet(str, id, arg, body, _) -> size_of_lambda body
  | Fletrec(bindings, body, _) -> size_of_lambda body
  | Fprim (Pmakeblock(tag, mut), args, _, _) as exp -> begin
      try ignore (constant exp); RHS_nonrec
      with Not_constant -> RHS_block (List.length args)
    end
  | Fprim (Pmakearray (Paddrarray|Pintarray), args, _, _) ->
      RHS_block (List.length args)
  | Fprim (Pmakearray Pfloatarray, args, _, _) ->
      RHS_floatblock (List.length args)
  | Fprim (Pmakearray Pgenarray, args, _, _) -> assert false
  | Fprim (Pduprecord (Record_regular, size), args, _, _) -> RHS_block size
  | Fprim (Pduprecord (Record_float, size), args, _, _) -> RHS_floatblock size
  | Fsequence (lam, lam', _) -> size_of_lambda lam'
  | Fevent (lam, _, _) -> size_of_lambda lam
  | _ -> RHS_nonrec

(**** Merging consecutive events ****)

let copy_event ev kind info repr =
  { ev_pos = 0;                   (* patched in emitcode *)
    ev_module = ev.ev_module;
    ev_loc = ev.ev_loc;
    ev_kind = kind;
    ev_info = info;
    ev_typenv = ev.ev_typenv;
    ev_typsubst = ev.ev_typsubst;
    ev_compenv = ev.ev_compenv;
    ev_stacksize = ev.ev_stacksize;
    ev_repr = repr }

let merge_infos ev ev' =
  match ev.ev_info, ev'.ev_info with
    Event_other, info -> info
  | info, Event_other -> info
  | _                 -> fatal_error "Bytegen.merge_infos"

let merge_repr ev ev' =
  match ev.ev_repr, ev'.ev_repr with
    Event_none, x -> x
  | x, Event_none -> x
  | Event_parent r, Event_child r' when r == r' && !r = 1 -> Event_none
  | Event_child r, Event_parent r' when r == r' -> Event_parent r
  | _, _          -> fatal_error "Bytegen.merge_repr"

let merge_events ev ev' =
  let (maj, min) =
    match ev.ev_kind, ev'.ev_kind with
    (* Discard pseudo-events *)
      Event_pseudo,  _                              -> ev', ev
    | _,             Event_pseudo                   -> ev,  ev'
    (* Keep following event, supposedly more informative *)
    | Event_before,  (Event_after _ | Event_before) -> ev',  ev
    (* Discard following events, supposedly less informative *)
    | Event_after _, (Event_after _ | Event_before) -> ev, ev'
  in
  copy_event maj maj.ev_kind (merge_infos maj min) (merge_repr maj min)

let weaken_event ev cont =
  match ev.ev_kind with
    Event_after _ ->
      begin match cont with
        Kpush :: Kevent ({ev_repr = Event_none} as ev') :: c ->
          begin match ev.ev_info with
            Event_return _ ->
              (* Weaken event *)
              let repr = ref 1 in
              let ev =
                copy_event ev Event_pseudo ev.ev_info (Event_parent repr)
              and ev' =
                copy_event ev' ev'.ev_kind ev'.ev_info (Event_child repr)
              in
              Kevent ev :: Kpush :: Kevent ev' :: c
          | _ ->
              (* Only keep following event, equivalent *)
              cont
          end
      | _ ->
          Kevent ev :: cont
      end
  | _ ->
      Kevent ev :: cont

let add_event ev =
  function
    Kevent ev' :: cont -> weaken_event (merge_events ev ev') cont
  | cont               -> weaken_event ev cont

(**** Compilation of a lambda expression ****)

(* association staticraise numbers -> (lbl,size of stack *)

let sz_static_raises = ref StaticExceptionMap.empty
let find_raise_label i =
  try
    StaticExceptionMap.find i !sz_static_raises
  with
  | Not_found ->
      Misc.fatal_error
        ("exit("^string_of_int (Static_exception.to_int i)
         ^") outside appropriated catch")

(* Will the translation of l lead to a jump to label ? *)
let code_as_jump l sz = match l with
| Fstaticfail (i, [], _) ->
    let label, size = find_raise_label i in
    if sz = size then
      Some label
    else
      None
| _ -> None

(* Function bodies that remain to be compiled *)

type function_to_compile =
  { params: Variable.t list;            (* function parameters *)
    body: ExprId.t flambda;     (* the function body *)
    label: label;                       (* the label of the function entry *)
    free_vars: Variable.t list;         (* free variables of the function  *)
    num_defs: int;             (* number of mutually recursive definitions *)
    rec_vars: Variable.t list;          (* mutually recursive fn names     *)
    rec_pos: int }                      (* rank in recursive definition    *)

let functions_to_compile  = (Stack.create () : function_to_compile Stack.t)

(* Name of current compilation unit (for debugging events) *)

let compunit_name = ref ""

(* Maximal stack size reached during the current function body *)

let max_stack_used = ref 0

(* Translate a primitive to a bytecode instruction (possibly a call to a C
   function) *)

let comp_bint_primitive bi suff args =
  let pref =
    match bi with Pnativeint -> "caml_nativeint_"
                | Pint32 -> "caml_int32_"
                | Pint64 -> "caml_int64_" in
  Kccall(pref ^ suff, List.length args)

let comp_primitive p args =
  match p with
    Pgetglobal id -> Kgetglobal id
  | Psetglobal id -> Ksetglobal id
  | Pintcomp cmp -> Kintcomp cmp
  | Pmakeblock(tag, mut) -> Kmakeblock(List.length args, tag)
  | Pfield n -> Kgetfield n
  | Psetfield(n, ptr) -> Ksetfield n
  | Pfloatfield n -> Kgetfloatfield n
  | Psetfloatfield n -> Ksetfloatfield n
  | Pduprecord _ -> Kccall("caml_obj_dup", 1)
  | Pccall p -> Kccall(p.prim_name, p.prim_arity)
  | Pnegint -> Knegint
  | Paddint -> Kaddint
  | Psubint -> Ksubint
  | Pmulint -> Kmulint
  | Pdivint -> Kdivint
  | Pmodint -> Kmodint
  | Pandint -> Kandint
  | Porint -> Korint
  | Pxorint -> Kxorint
  | Plslint -> Klslint
  | Plsrint -> Klsrint
  | Pasrint -> Kasrint
  | Poffsetint n -> Koffsetint n
  | Poffsetref n -> Koffsetref n
  | Pintoffloat -> Kccall("caml_int_of_float", 1)
  | Pfloatofint -> Kccall("caml_float_of_int", 1)
  | Pnegfloat -> Kccall("caml_neg_float", 1)
  | Pabsfloat -> Kccall("caml_abs_float", 1)
  | Paddfloat -> Kccall("caml_add_float", 2)
  | Psubfloat -> Kccall("caml_sub_float", 2)
  | Pmulfloat -> Kccall("caml_mul_float", 2)
  | Pdivfloat -> Kccall("caml_div_float", 2)
  | Pfloatcomp Ceq -> Kccall("caml_eq_float", 2)
  | Pfloatcomp Cneq -> Kccall("caml_neq_float", 2)
  | Pfloatcomp Clt -> Kccall("caml_lt_float", 2)
  | Pfloatcomp Cgt -> Kccall("caml_gt_float", 2)
  | Pfloatcomp Cle -> Kccall("caml_le_float", 2)
  | Pfloatcomp Cge -> Kccall("caml_ge_float", 2)
  | Pstringlength -> Kccall("caml_ml_string_length", 1)
  | Pstringrefs -> Kccall("caml_string_get", 2)
  | Pstringsets -> Kccall("caml_string_set", 3)
  | Pstringrefu -> Kgetstringchar
  | Pstringsetu -> Ksetstringchar
  | Pstring_load_16(_) -> Kccall("caml_string_get16", 2)
  | Pstring_load_32(_) -> Kccall("caml_string_get32", 2)
  | Pstring_load_64(_) -> Kccall("caml_string_get64", 2)
  | Pstring_set_16(_) -> Kccall("caml_string_set16", 3)
  | Pstring_set_32(_) -> Kccall("caml_string_set32", 3)
  | Pstring_set_64(_) -> Kccall("caml_string_set64", 3)
  | Parraylength kind -> Kvectlength
  | Parrayrefs Pgenarray -> Kccall("caml_array_get", 2)
  | Parrayrefs Pfloatarray -> Kccall("caml_array_get_float", 2)
  | Parrayrefs _ -> Kccall("caml_array_get_addr", 2)
  | Parraysets Pgenarray -> Kccall("caml_array_set", 3)
  | Parraysets Pfloatarray -> Kccall("caml_array_set_float", 3)
  | Parraysets _ -> Kccall("caml_array_set_addr", 3)
  | Parrayrefu Pgenarray -> Kccall("caml_array_unsafe_get", 2)
  | Parrayrefu Pfloatarray -> Kccall("caml_array_unsafe_get_float", 2)
  | Parrayrefu _ -> Kgetvectitem
  | Parraysetu Pgenarray -> Kccall("caml_array_unsafe_set", 3)
  | Parraysetu Pfloatarray -> Kccall("caml_array_unsafe_set_float", 3)
  | Parraysetu _ -> Ksetvectitem
  | Pctconst c ->
     let const_name = match c with
       | Big_endian -> "big_endian"
       | Word_size -> "word_size"
       | Ostype_unix -> "ostype_unix"
       | Ostype_win32 -> "ostype_win32"
       | Ostype_cygwin -> "ostype_cygwin" in
     Kccall(Printf.sprintf "caml_sys_const_%s" const_name, 1)
  | Pisint -> Kisint
  | Pisout -> Kisout
  | Pbittest -> Kccall("caml_bitvect_test", 2)
  | Pbintofint bi -> comp_bint_primitive bi "of_int" args
  | Pintofbint bi -> comp_bint_primitive bi "to_int" args
  | Pcvtbint(Pint32, Pnativeint) -> Kccall("caml_nativeint_of_int32", 1)
  | Pcvtbint(Pnativeint, Pint32) -> Kccall("caml_nativeint_to_int32", 1)
  | Pcvtbint(Pint32, Pint64) -> Kccall("caml_int64_of_int32", 1)
  | Pcvtbint(Pint64, Pint32) -> Kccall("caml_int64_to_int32", 1)
  | Pcvtbint(Pnativeint, Pint64) -> Kccall("caml_int64_of_nativeint", 1)
  | Pcvtbint(Pint64, Pnativeint) -> Kccall("caml_int64_to_nativeint", 1)
  | Pnegbint bi -> comp_bint_primitive bi "neg" args
  | Paddbint bi -> comp_bint_primitive bi "add" args
  | Psubbint bi -> comp_bint_primitive bi "sub" args
  | Pmulbint bi -> comp_bint_primitive bi "mul" args
  | Pdivbint bi -> comp_bint_primitive bi "div" args
  | Pmodbint bi -> comp_bint_primitive bi "mod" args
  | Pandbint bi -> comp_bint_primitive bi "and" args
  | Porbint bi -> comp_bint_primitive bi "or" args
  | Pxorbint bi -> comp_bint_primitive bi "xor" args
  | Plslbint bi -> comp_bint_primitive bi "shift_left" args
  | Plsrbint bi -> comp_bint_primitive bi "shift_right_unsigned" args
  | Pasrbint bi -> comp_bint_primitive bi "shift_right" args
  | Pbintcomp(bi, Ceq) -> Kccall("caml_equal", 2)
  | Pbintcomp(bi, Cneq) -> Kccall("caml_notequal", 2)
  | Pbintcomp(bi, Clt) -> Kccall("caml_lessthan", 2)
  | Pbintcomp(bi, Cgt) -> Kccall("caml_greaterthan", 2)
  | Pbintcomp(bi, Cle) -> Kccall("caml_lessequal", 2)
  | Pbintcomp(bi, Cge) -> Kccall("caml_greaterequal", 2)
  | Pbigarrayref(_, n, _, _) -> Kccall("caml_ba_get_" ^ string_of_int n, n + 1)
  | Pbigarrayset(_, n, _, _) -> Kccall("caml_ba_set_" ^ string_of_int n, n + 2)
  | Pbigarraydim(n) -> Kccall("caml_ba_dim_" ^ string_of_int n, 1)
  | Pbigstring_load_16(_) -> Kccall("caml_ba_uint8_get16", 2)
  | Pbigstring_load_32(_) -> Kccall("caml_ba_uint8_get32", 2)
  | Pbigstring_load_64(_) -> Kccall("caml_ba_uint8_get64", 2)
  | Pbigstring_set_16(_) -> Kccall("caml_ba_uint8_set16", 3)
  | Pbigstring_set_32(_) -> Kccall("caml_ba_uint8_set32", 3)
  | Pbigstring_set_64(_) -> Kccall("caml_ba_uint8_set64", 3)
  | Pbswap16 -> Kccall("caml_bswap16", 1)
  | Pbbswap(bi) -> comp_bint_primitive bi "bswap" args
  | _ -> fatal_error "Bytegen.comp_primitive"

let is_immed n = immed_min <= n && n <= immed_max


(* Compile an expression.
   The value of the expression is left in the accumulator.
   env = compilation environment
   exp = the lambda expression to compile
   sz = current size of the stack frame
   cont = list of instructions to execute afterwards
   Result = list of instructions that evaluate exp, then perform cont. *)

let rebind_recclosure id funid env =
  try
    let pos = ClosureFunctionMap.find funid env.ce_closures in
    add_var id pos env
  with Not_found ->
    fatal_error ("Bytegen.comp_expr: recclosure " ^ Variable.unique_name id)

(*** GRGR comment *)

let ignore_compunit map =
  (* GRGR FIXME: check compilation_unit... *)
  VarMap.fold
    (fun v pos s -> Ident.add (Variable.to_ident v) pos s)
    map Ident.empty

let debug_compenv env =
  { dce_stack = ignore_compunit env.ce_stack;
    dce_heap = ignore_compunit env.ce_heap;
    dce_rec = ignore_compunit env.ce_rec }

(**** GRGR comment *)

type offset_tables = {
  freevars: int ClosureVariableMap.t;
  funs: int ClosureFunctionMap.t;
}

let compute_offset_tables exp =

  (* The offset table associate a function label to its offset
     inside a closure *)
  let fun_offset_table = ref ClosureFunctionMap.empty in

  (* The offset table associate a free variable to its offset inside a
     closure *)
  let fv_offset_table = ref ClosureVariableMap.empty in

  let rec iter = function
    | Fclosure({cl_fun = funct; cl_free_var = fv}, _) ->
        iter_closure funct fv
    | _ -> ()

  and iter_closure functs fv =

    let funct = VarMap.bindings functs.funs in
    let fv = VarMap.bindings fv in

    (* build the table mapping the function to the offset of its code
       pointer inside the closure value *)
    let aux_fun_offset (map,env_pos) (id, func) =
      let pos = env_pos + 1 in
      let env_pos = env_pos + 2 in
      let map = ClosureFunctionMap.add (Closure_function.create id) pos map in
      (map, env_pos)
    in
    let fun_offset, fv_pos =
      List.fold_left aux_fun_offset (!fun_offset_table, -1) funct in

    (* Adds the mapping of free variables to their offset. It is not
       used inside the body of the function: it is directly
       substituted here. But if the function is inlined, it is
       possible that the closure is accessed from outside its body. *)
    let aux_fv_offset (map,pos) (id, _) =
      let off = Closure_variable.create id in
      assert(not (ClosureVariableMap.mem off map));
      let map = ClosureVariableMap.add off pos map in
      (map,pos + 1)
    in
    let fv_offset, _ = List.fold_left aux_fv_offset
        (!fv_offset_table, fv_pos) fv in

    fun_offset_table := fun_offset;
    fv_offset_table := fv_offset;

    List.iter (fun (_,{Flambda.body}) -> Flambdaiter.iter_toplevel iter body) funct in

  Flambdaiter.iter_toplevel iter exp;

  { freevars = !fv_offset_table;
    funs = !fun_offset_table;
  }

let comp_block offset_tables env exp sz cont =

  (**** Compilation of a code block (with tracking of stack usage) ****)

  let rec comp_expr env exp sz cont =
    if sz > !max_stack_used then max_stack_used := sz;
    match exp with
    | Fvar (var, _) ->
        begin try
          let pos = VarMap.find var env.ce_stack in
          Kacc(sz - pos) :: cont
        with Not_found ->
          try
            let pos = VarMap.find var env.ce_heap in
            Kenvacc pos :: cont
          with Not_found ->
            try
              let pos = VarMap.find var env.ce_rec in
              Koffsetclosure pos :: cont
            with Not_found ->
              Format.eprintf "%a@." Variable.print var;
              fatal_error ("Bytegen.comp_expr: var " ^ Variable.unique_name var)
        end
    | Fvariable_in_closure (var, _) ->
        begin try
          let var_offset = ClosureVariableMap.find var.vc_var offset_tables.freevars in
          let fun_offset = ClosureFunctionMap.find var.vc_fun offset_tables.funs in
          let pos = var_offset - fun_offset in
          comp_expr env var.vc_closure sz (Kgetfield pos :: cont)
        with Not_found ->
          Format.eprintf "%a@." Closure_variable.print var.vc_var;
          fatal_error ("Bytegen.comp_expr: closure_var "
                       ^ Closure_variable.unique_name var.vc_var)
        end
    | Fconst (cst, _) ->
        Kconst (structured cst) :: cont
    | Fapply(ap, _) ->
        let args = ap.ap_arg in
        let func = ap.ap_function in
        let nargs = List.length args in
        if is_tailcall cont then begin
          comp_args env args sz
            (Kpush :: comp_expr env func (sz + nargs)
               (Kappterm(nargs, sz + nargs) :: discard_dead_code cont))
        end else begin
          if nargs < 4 then
            comp_args env args sz
              (Kpush :: comp_expr env func (sz + nargs) (Kapply nargs :: cont))
          else begin
            let (lbl, cont1) = label_code cont in
            Kpush_retaddr lbl ::
            comp_args env args (sz + 3)
              (Kpush :: comp_expr env func (sz + 3 + nargs)
                 (Kapply nargs :: cont1))
          end
        end
    | Fsend(kind, met, obj, args, _, _) ->
        let args = if kind = Cached then List.tl args else args in
        let nargs = List.length args + 1 in
        let getmethod, args' =
          if kind = Self then (Kgetmethod, met::obj::args) else
            match met with
              Fconst(Fconst_base(Const_int n), _) -> (Kgetpubmet n, obj::args)
            | _ -> (Kgetdynmet, met::obj::args)
        in
        if is_tailcall cont then
          comp_args env args' sz
            (getmethod :: Kappterm(nargs, sz + nargs) :: discard_dead_code cont)
        else
        if nargs < 4 then
          comp_args env args' sz
            (getmethod :: Kapply nargs :: cont)
        else begin
          let (lbl, cont1) = label_code cont in
          Kpush_retaddr lbl ::
          comp_args env args' (sz + 3)
            (getmethod :: Kapply nargs :: cont1)
        end
    | Ffunction ({ fu_closure = Fclosure (clos, _); fu_fun;
                   fu_relative_to = None }, _) -> begin
        let functs = VarMap.bindings clos.cl_fun.funs in
        let fv = VarMap.bindings clos.cl_free_var in
        match functs with
        | [id, funct] ->
            assert (Closure_function.equal fu_fun (Closure_function.create id));
            let lbl = new_label() in
            let to_compile =
              { params = funct.params; body = funct.body; label = lbl;
                free_vars = List.map fst fv;
                num_defs = 1; rec_vars = [id]; rec_pos = 0 } in
            Stack.push to_compile functions_to_compile;
            comp_args env (List.map snd fv) sz
              (Kclosure(lbl, List.length fv) :: cont)
        | [] | _ :: _ :: _ ->
            fatal_error ("Bytegen.comp_expr: recclosure arity")
      end
    | Flet(Not_assigned, clos_id, Fclosure (clos, _), body, _) ->
        let functs = VarMap.bindings clos.cl_fun.funs in
        let fv = VarMap.bindings clos.cl_free_var in
        let ndecl = List.length functs in
        let rec_idents = List.map fst functs in
        let rec comp_fun pos = function
          | [] -> []
          | (_, (funct : _ Flambda.function_declaration)) :: rem ->
              let lbl = new_label() in
              let to_compile =
                { params = funct.params; body = funct.body; label = lbl;
                  free_vars = List.map fst fv;
                  num_defs = ndecl; rec_vars = rec_idents; rec_pos = pos} in
              Stack.push to_compile functions_to_compile;
              lbl :: comp_fun (pos + 1) rem in
        let lbls = comp_fun 0 functs in
        comp_args env (List.map snd fv) sz
          (Kclosurerec(lbls, List.length fv) ::
           comp_expr (add_recclosures rec_idents (sz+1) env) body (sz+ndecl)
             (add_pop ndecl cont))
    | Flet(Not_assigned, id,
           Ffunction ({ fu_closure = Fvar (clos_id, _); fu_fun;
                        fu_relative_to = None }, _), body, _) ->
        comp_expr (rebind_recclosure id fu_fun env) body sz cont
    | Fclosure _ ->
        fatal_error ("Bytegen.comp_expr: unexpected closure")
    | Ffunction _->
        fatal_error ("Bytegen.comp_expr: unexpected function")
    | Flet(str, id, arg, body, _) ->
        comp_expr env arg sz
          (Kpush :: comp_expr (add_var id (sz+1) env) body (sz+1)
             (add_pop 1 cont))
    | Fletrec(decl, body, _) ->
        let ndecl = List.length decl in
        let decl_size =
          List.map (fun (id, exp) -> (id, exp, size_of_lambda exp)) decl in
        let rec comp_init new_env sz = function
          | [] -> comp_nonrec new_env sz ndecl decl_size
          | (id, exp, RHS_floatblock blocksize) :: rem ->
              Kconst(Const_base(Const_int blocksize)) ::
              Kccall("caml_alloc_dummy_float", 1) :: Kpush ::
              comp_init (add_var id (sz+1) new_env) (sz+1) rem
          | (id, exp, RHS_block blocksize) :: rem ->
              Kconst(Const_base(Const_int blocksize)) ::
              Kccall("caml_alloc_dummy", 1) :: Kpush ::
              comp_init (add_var id (sz+1) new_env) (sz+1) rem
          | (id, exp, RHS_nonrec) :: rem ->
              Kconst(Const_base(Const_int 0)) :: Kpush ::
              comp_init (add_var id (sz+1) new_env) (sz+1) rem
        and comp_nonrec new_env sz i = function
          | [] -> comp_rec new_env sz ndecl decl_size
          | (id, exp, (RHS_block _ | RHS_floatblock _)) :: rem ->
              comp_nonrec new_env sz (i-1) rem
          | (id, exp, RHS_nonrec) :: rem ->
              comp_expr new_env exp sz
                (Kassign (i-1) :: comp_nonrec new_env sz (i-1) rem)
        and comp_rec new_env sz i = function
          | [] -> comp_expr new_env body sz (add_pop ndecl cont)
          | (id, exp, (RHS_block _ | RHS_floatblock _)) :: rem ->
              comp_expr new_env exp sz
                (Kpush :: Kacc i :: Kccall("caml_update_dummy", 2) ::
                 comp_rec new_env sz (i-1) rem)
          | (id, exp, RHS_nonrec) :: rem ->
              comp_rec new_env sz (i-1) rem
        in
        comp_init env sz decl_size
    | Fprim(Pidentity, [arg], _, _) ->
        comp_expr env arg sz cont
    | Fprim(Pignore, [arg], _, _) ->
        comp_expr env arg sz (add_const_unit cont)
    | Fprim(Pdirapply loc, [func;arg], _, id)
    | Fprim(Prevapply loc, [arg;func], _, id) ->
        let exp = Fapply({ ap_function = func; ap_arg = [arg];
                           ap_kind = Indirect; ap_dbg = Debuginfo.none},
                         id) in
        comp_expr env exp sz cont
    | Fprim(Pnot, [arg], _, _) ->
        let newcont =
          match cont with
            Kbranchif lbl :: cont1 -> Kbranchifnot lbl :: cont1
          | Kbranchifnot lbl :: cont1 -> Kbranchif lbl :: cont1
          | _ -> Kboolnot :: cont in
        comp_expr env arg sz newcont
    | Fprim(Psequand, [exp1; exp2], _, _) ->
        begin match cont with
          Kbranchifnot lbl :: _ ->
            comp_expr env exp1 sz (Kbranchifnot lbl ::
                                   comp_expr env exp2 sz cont)
        | Kbranchif lbl :: cont1 ->
            let (lbl2, cont2) = label_code cont1 in
            comp_expr env exp1 sz (Kbranchifnot lbl2 ::
                                   comp_expr env exp2 sz (Kbranchif lbl :: cont2))
        | _ ->
            let (lbl, cont1) = label_code cont in
            comp_expr env exp1 sz (Kstrictbranchifnot lbl ::
                                   comp_expr env exp2 sz cont1)
        end
    | Fprim(Psequor, [exp1; exp2], _, _) ->
        begin match cont with
          Kbranchif lbl :: _ ->
            comp_expr env exp1 sz (Kbranchif lbl ::
                                   comp_expr env exp2 sz cont)
        | Kbranchifnot lbl :: cont1 ->
            let (lbl2, cont2) = label_code cont1 in
            comp_expr env exp1 sz (Kbranchif lbl2 ::
                                   comp_expr env exp2 sz (Kbranchifnot lbl :: cont2))
        | _ ->
            let (lbl, cont1) = label_code cont in
            comp_expr env exp1 sz (Kstrictbranchif lbl ::
                                   comp_expr env exp2 sz cont1)
        end
    | Fprim(Praise, [arg], _, _) ->
        comp_expr env arg sz (Kraise :: discard_dead_code cont)
    | Fprim(Paddint, [arg; Fconst(Fconst_base(Const_int n), _)], _ ,_)
      when is_immed n ->
        comp_expr env arg sz (Koffsetint n :: cont)
    | Fprim(Psubint, [arg; Fconst(Fconst_base(Const_int n), _)], _ ,_)
      when is_immed (-n) ->
        comp_expr env arg sz (Koffsetint (-n) :: cont)
    | Fprim (Poffsetint n, [arg], _ ,_)
      when not (is_immed n) ->
        comp_expr env arg sz
          (Kpush::
           Kconst (Const_base (Const_int n))::
           Kaddint::cont)
    | Fprim(Pmakearray kind, args, _, _) ->
        begin match kind with
          Pintarray | Paddrarray ->
            comp_args env args sz (Kmakeblock(List.length args, 0) :: cont)
        | Pfloatarray ->
            comp_args env args sz (Kmakefloatblock(List.length args) :: cont)
        | Pgenarray ->
            if args = []
            then Kmakeblock(0, 0) :: cont
            else comp_args env args sz
                (Kmakeblock(List.length args, 0) ::
                 Kccall("caml_make_array", 1) :: cont)
        end
    (* Integer first for enabling futher optimization (cf. emitcode.ml)  *)
    | Fprim (Pintcomp c, [arg ; (Fconst _ as k)], _, _) ->
        let p = Pintcomp (commute_comparison c)
        and args = [k ; arg] in
        comp_args env args sz (comp_primitive p args :: cont)
    | Fprim(p, args, _, _) as exp -> begin
        (* GRGR quadratic .... FIXME ??? *)
        try Kconst (constant exp) :: cont
        with Not_constant ->
          comp_args env args sz (comp_primitive p args :: cont)
      end
    | Fcatch (exn, vars, body, handler, _) ->
        let nvars = List.length vars in
        let branch1, cont1 = make_branch cont in
        let r =
          if nvars <> 1 then begin (* general case *)
            let lbl_handler, cont2 =
              label_code
                (comp_expr
                   (add_vars vars (sz+1) env)
                   handler (sz+nvars) (add_pop nvars cont1)) in
            sz_static_raises :=
              StaticExceptionMap.add exn (lbl_handler, sz+nvars) !sz_static_raises;
            push_dummies nvars
              (comp_expr env body (sz+nvars)
                 (add_pop nvars (branch1 :: cont2)))
          end else begin (* small optimization for nvars = 1 *)
            let var = match vars with [var] -> var | _ -> assert false in
            let lbl_handler, cont2 =
              label_code
                (Kpush::comp_expr
                   (add_var var (sz+1) env)
                   handler (sz+1) (add_pop 1 cont1)) in
            sz_static_raises :=
              StaticExceptionMap.add exn (lbl_handler, sz) !sz_static_raises;
            comp_expr env body sz (branch1 :: cont2)
          end in
        sz_static_raises := StaticExceptionMap.remove exn !sz_static_raises ;
        r
    | Fstaticfail (exn, args, _) ->
        let cont = discard_dead_code cont in
        let label,size = find_raise_label exn in
        begin match args with
        | [arg] -> (* optim, argument passed in accumulator *)
            comp_expr env arg sz
              (add_pop (sz-size) (branch_to label cont))
        | _ ->
            comp_exit_args env args sz size
              (add_pop (sz-size) (branch_to label cont))
        end
    | Ftrywith(body, id, handler, _) ->
        let (branch1, cont1) = make_branch cont in
        let lbl_handler = new_label() in
        Kpushtrap lbl_handler ::
        comp_expr env body (sz+4) (Kpoptrap :: branch1 ::
                                   Klabel lbl_handler :: Kpush ::
                                   comp_expr (add_var id (sz+1) env) handler (sz+1) (add_pop 1 cont1))
    | Fifthenelse(cond, ifso, ifnot, _) ->
        comp_binary_test env cond ifso ifnot sz cont
    | Fsequence(exp1, exp2, _) ->
        comp_expr env exp1 sz (comp_expr env exp2 sz cont)
    | Fwhile(cond, body, _) ->
        let lbl_loop = new_label() in
        let lbl_test = new_label() in
        Kbranch lbl_test :: Klabel lbl_loop :: Kcheck_signals ::
        comp_expr env body sz
          (Klabel lbl_test ::
           comp_expr env cond sz (Kbranchif lbl_loop :: add_const_unit cont))
    | Ffor(param, start, stop, dir, body, _) ->
        let lbl_loop = new_label() in
        let lbl_exit = new_label() in
        let offset = match dir with Upto -> 1 | Downto -> -1 in
        let comp = match dir with Upto -> Cgt | Downto -> Clt in
        comp_expr env start sz
          (Kpush :: comp_expr env stop (sz+1)
             (Kpush :: Kpush :: Kacc 2 :: Kintcomp comp :: Kbranchif lbl_exit ::
              Klabel lbl_loop :: Kcheck_signals ::
              comp_expr (add_var param (sz+1) env) body (sz+2)
                (Kacc 1 :: Kpush :: Koffsetint offset :: Kassign 2 ::
                 Kacc 1 :: Kintcomp Cneq :: Kbranchif lbl_loop ::
                 Klabel lbl_exit :: add_const_unit (add_pop 2 cont))))
    | Fswitch(arg, sw, _) ->
        let (branch, cont1) = make_branch cont in
        let c = ref (discard_dead_code cont1) in
        (* Build indirection vectors *)
        let store = mk_store Flambda.can_be_merged in
        let act_consts = Array.create (IntSet.cardinal sw.fs_numconsts) 0
        and act_blocks = Array.create (IntSet.cardinal sw.fs_numblocks) 0 in
        begin match sw.fs_failaction with (* default is index 0 *)
        | Some fail -> ignore (store.act_store fail)
        | None      -> ()
        end ;
        List.iter
          (fun (n, act) -> act_consts.(n) <- store.act_store act) sw.fs_consts;
        List.iter
          (fun (n, act) -> act_blocks.(n) <- store.act_store act) sw.fs_blocks;

        (* Compile and label actions *)
        let acts = store.act_get () in
        let lbls = Array.create (Array.length acts) 0 in
        for i = Array.length acts-1 downto 0 do
          let lbl,c1 = label_code (comp_expr env acts.(i) sz (branch :: !c)) in
          lbls.(i) <- lbl ;
          c := discard_dead_code c1
        done ;

        (* Build label vectors *)
        let lbl_blocks = Array.create (IntSet.cardinal sw.fs_numblocks) 0 in
        for i = Array.length lbl_blocks - 1 downto 0 do
          lbl_blocks.(i) <- lbls.(act_blocks.(i))
        done;
        let lbl_consts = Array.create (IntSet.cardinal sw.fs_numconsts) 0 in
        for i = Array.length lbl_consts - 1 downto 0 do
          lbl_consts.(i) <- lbls.(act_consts.(i))
        done;
        comp_expr env arg sz (Kswitch(lbl_consts, lbl_blocks) :: !c)
    | Fassign(var, expr, _) ->
        begin try
          let pos = VarMap.find var env.ce_stack in
          comp_expr env expr sz (Kassign(sz - pos) :: cont)
        with Not_found ->
          fatal_error "Bytegen.comp_expr: assign"
        end
    | Funreachable _ ->
        Kstop :: discard_dead_code cont
    | Fsymbol _ -> assert false (* Should not happen in bytecode *)
    | Fevent(lam, lev, _) ->
        let event kind info =
          { ev_pos = 0;                   (* patched in emitcode *)
            ev_module = !compunit_name;
            ev_loc = lev.lev_loc;
            ev_kind = kind;
            ev_info = info;
            ev_typenv = lev.lev_env;
            ev_typsubst = Subst.identity;
            ev_compenv = debug_compenv env;
            ev_stacksize = sz;
            ev_repr =
              begin match lev.lev_repr with
                None ->
                  Event_none
              | Some ({contents = 1} as repr) when lev.lev_kind = Lev_function ->
                  Event_child repr
              | Some ({contents = 1} as repr) ->
                  Event_parent repr
              | Some repr when lev.lev_kind = Lev_function ->
                  Event_parent repr
              | Some repr ->
                  Event_child repr
              end }
        in
        begin match lev.lev_kind with
          Lev_before ->
            let c = comp_expr env lam sz cont in
            let ev = event Event_before Event_other in
            add_event ev c
        | Lev_function ->
            let c = comp_expr env lam sz cont in
            let ev = event Event_pseudo Event_function in
            add_event ev c
        | Lev_after _ when is_tailcall cont -> (* don't destroy tail call opt *)
            comp_expr env lam sz cont
        | Lev_after ty ->
            let info =
              match lam with
                Fapply({ ap_arg }, _)      -> Event_return (List.length ap_arg)
              | Fsend(_, _, _, args, _, _) -> Event_return (List.length args + 1)
              | _                          -> Event_other
            in
            let ev = event (Event_after ty) info in
            let cont1 = add_event ev cont in
            comp_expr env lam sz cont1
        end

  (* Compile a list of arguments [e1; ...; eN] to a primitive operation.
     The values of eN ... e2 are pushed on the stack, e2 at top of stack,
     then e3, then ... The value of e1 is left in the accumulator. *)

  and comp_args env argl sz cont =
    comp_expr_list env (List.rev argl) sz cont

  and comp_expr_list env exprl sz cont = match exprl with
      [] -> cont
    | [exp] -> comp_expr env exp sz cont
    | exp :: rem ->
        comp_expr env exp sz (Kpush :: comp_expr_list env rem (sz+1) cont)

  and comp_exit_args  env argl sz pos cont =
    comp_expr_list_assign env (List.rev argl) sz pos cont

  and comp_expr_list_assign env exprl sz pos cont = match exprl with
    | [] -> cont
    | exp :: rem ->
        comp_expr env exp sz
          (Kassign (sz-pos)::comp_expr_list_assign env rem sz (pos-1) cont)

  (* Compile an if-then-else test. *)

  and comp_binary_test env cond ifso ifnot sz cont =
    let cont_cond =
      if is_const_unit ifnot then begin
        let (lbl_end, cont1) = label_code cont in
        Kstrictbranchifnot lbl_end :: comp_expr env ifso sz cont1
      end else
        match code_as_jump ifso sz with
        | Some label ->
            let cont = comp_expr env ifnot sz cont in
            Kbranchif label :: cont
        | _ ->
            match code_as_jump ifnot sz with
            | Some label ->
                let cont = comp_expr env ifso sz cont in
                Kbranchifnot label :: cont
            | _ ->
                let (branch_end, cont1) = make_branch cont in
                let (lbl_not, cont2) = label_code(comp_expr env ifnot sz cont1) in
                Kbranchifnot lbl_not ::
                comp_expr env ifso sz (branch_end :: cont2) in

    comp_expr env cond sz cont_cond in

  max_stack_used := 0;
  let code = comp_expr env exp sz cont in
  (* +1 because comp_expr may have pushed one more word *)
  if !max_stack_used + 1 > Config.stack_threshold then
    Kconst(Const_base(Const_int(!max_stack_used + 1))) ::
    Kccall("caml_ensure_stack_capacity", 1) ::
    code
  else
    code

(**** Compilation of functions ****)

let comp_function offset_tables tc cont =
  let arity = List.length tc.params in
  let rec positions pos delta = function
      [] -> VarMap.empty
    | id :: rem -> VarMap.add id pos (positions (pos + delta) delta rem) in
  let env =
    { ce_stack = positions arity (-1) tc.params;
      ce_heap = positions (2 * (tc.num_defs - tc.rec_pos) - 1) 1 tc.free_vars;
      ce_rec = positions (- 2 * tc.rec_pos) 2 tc.rec_vars;
      ce_closures = ClosureFunctionMap.empty;
    }
  in
  let cont =
    comp_block offset_tables env tc.body arity (Kreturn arity :: cont) in
  if arity > 1 then
    Krestart :: Klabel tc.label :: Kgrab(arity - 1) :: cont
  else
    Klabel tc.label :: cont

let comp_remainder offset_tables cont =
  let c = ref cont in
  begin try
    while true do
      c := comp_function offset_tables (Stack.pop functions_to_compile) !c
    done
  with Stack.Empty ->
    ()
  end;
  !c

(**** Compilation of a lambda phrase ****)

let compile_implementation modulename expr =
  let compilation_unit =
    Compilation_unit.create (Ident.create_persistent modulename) in
  let flam = Flambdagen.intro ~for_bytecode:true ~compilation_unit expr in
  if !Clflags.dump_flambda
  then Format.fprintf Format.err_formatter "%a@." Printflambda.flambda flam;
  Stack.clear functions_to_compile;
  label_counter := 0;
  sz_static_raises := StaticExceptionMap.empty;
  compunit_name := modulename;
  let offset_tables = compute_offset_tables flam in
  let init_code = comp_block offset_tables empty_env flam 0 [] in
  if Stack.length functions_to_compile > 0 then begin
    let lbl_init = new_label() in
    Kbranch lbl_init :: comp_remainder offset_tables (Klabel lbl_init :: init_code)
  end else
    init_code

let compile_phrase expr =
  let compilation_unit =
    Compilation_unit.create (Ident.create_persistent "") in
  let flam = Flambdagen.intro ~for_bytecode:true ~compilation_unit expr in
  if !Clflags.dump_flambda
  then Format.fprintf Format.err_formatter "%a@." Printflambda.flambda flam;
  Stack.clear functions_to_compile;
  label_counter := 0;
  sz_static_raises := StaticExceptionMap.empty;
  let offset_tables = compute_offset_tables flam in
  let init_code = comp_block offset_tables empty_env flam 1 [Kreturn 1] in
  let fun_code = comp_remainder offset_tables [] in
  (init_code, fun_code)
