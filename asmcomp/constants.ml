
(* Detection of not constant values *)

(* This cannot be done in a single recursive pass due to expressions like:

  let ... =
    let v = ... in

    let rec f1 x =
      let f2 y =
        f1 rec_list
      in
      f2 v
    and rec_list = f1 :: rec_list

    ...

  f1, f2 and rec_list are constants iff v is a constant.

  To handle this we implement it as 2 loops populating a 'not constant'
  set NC:

   - the first one collects informations on the expressions to add dependencies
     between variables and mark values directly known as not constant:

      f1 in NC => rec_list in NC
      f2 in NC => f1 in NC
      rec_list in NC => f2 in NC
      v in NC => f1 in NC

     and if for instance if v is:
      let v = if ... then 1 else 2 in
     it adds

      v in NC

   - the second propagates the implications
*)

open Asttypes
open Lambda
open Flambda

type constant_result = {
  not_constant_id : IdentSet.t;
  not_constant_closure : FunSet.t;
}

module type Param = sig
  type t
  val global_var : Ident.t IdentTbl.t
  val expr : t Flambda.flambda
  val for_clambda : bool
end

module NotConstants(P:Param) = struct

  let global_var = P.global_var
  let for_clambda = P.for_clambda

  type dep =
    | Closure of FunId.t
    | Var of Ident.t
    | Global of int (* position of the global *)

  (* Sets representing NC *)
  let variables = ref IdentSet.empty
  let closures = ref FunSet.empty
  let globals = ref IntSet.empty

  (* if the table associates [v1;v2;...;vn] to v, it represents
     v in NC => v1 in NC /\ v2 in NC ... /\ vn in NC *)
  let id_dep_table : dep list IdentTbl.t = IdentTbl.create 100
  let fun_dep_table : dep list FunTbl.t = FunTbl.create 100
  let glob_dep_table : dep list IntTbl.t = IntTbl.create 100

  (* adds in the tables 'dep in NC => curr in NC' *)
  let add_depend curr dep =
    List.iter (fun curr ->
      match dep with
      | Var id ->
        let t = try IdentTbl.find id_dep_table id
        with Not_found -> [] in
        IdentTbl.replace id_dep_table id (curr :: t)
      | Closure cl ->
        let t = try FunTbl.find fun_dep_table cl
        with Not_found -> [] in
        FunTbl.replace fun_dep_table cl (curr :: t)
      | Global i ->
        let t = try IntTbl.find glob_dep_table i
        with Not_found -> [] in
        IntTbl.replace glob_dep_table i (curr :: t))
      curr

  (* adds 'curr in NC' *)
  let mark_curr curr =
    List.iter (function
      | Var id ->
        if not (IdentSet.mem id !variables)
        then variables := IdentSet.add id !variables
      | Closure cl ->
        if not (FunSet.mem cl !closures)
        then closures := FunSet.add cl !closures
      | Global i ->
        if not (IntSet.mem i !globals)
        then globals := IntSet.add i !globals)
      curr

  (* First loop: iterates on the tree to mark dependencies.

     curr is the variables or closures to wich we add constraints like
     '... in NC => curr in NC' or 'curr in NC'

     It can be empty when no constraint can be added like in the toplevel
     expression or in the body of a function.
  *)
  let rec mark_loop (curr:dep list) = function

    | Flet(str, id, lam, body, _) ->
      (* No need to match on str: if the variable is assigned it will
         be marked directly as not constant, but if it is not
         assigned, it could be considered constant *)
      mark_loop [Var id] lam;
      (* adds 'id in NC => curr in NC'
         This is not really necessary, but compiling this correctly is
         trickier than eliminating that earlier. *)
      add_depend curr (Var id);
      mark_loop curr body

    | Fletrec(defs, body, _) ->
      List.iter (fun (id,def) ->
          mark_loop [Var id] def;
          (* adds 'id in NC => curr in NC' same remark as let case *)
          add_depend curr (Var id)) defs;
      mark_loop curr body

    | Fvar (id,_) ->
      (* adds 'id in NC => curr in NC' *)
      add_depend curr (Var id)

    | Fclosure (funcs,fv,_) ->
      (* adds 'funcs in NC => curr in NC' *)
      add_depend curr (Closure funcs.ident);
      (* a closure is constant if its free variables are constants. *)
      IdentMap.iter (fun inner_id lam ->
        mark_loop [Closure funcs.ident; Var inner_id] lam) fv;
      IdentMap.iter (fun fun_id ffunc ->
        (* for each function f in a closure c 'c in NC => f' *)
        add_depend [Var fun_id] (Closure funcs.ident);
        (* function parameters are in NC *)
        List.iter (fun id -> mark_curr [Var id]) ffunc.params;
        mark_loop [] ffunc.body) funcs.funs

    | Fsymbol _ | Fconst _ -> ()

    (* Constant constructors: those expressions are constant if all their parameters are:
       - makeblock is compiled to a constant block
       - offset is compiled to a pointer inside a constant closure.
         See Cmmgen for the details *)

    | Fprim(Pmakeblock(tag, Immutable), args, dbg, _) ->
      List.iter (mark_loop curr) args

    | Foffset (f1, _,_) ->
      mark_loop curr f1

    | Fenv_field ({env = f1},_) ->
      if for_clambda
      then mark_curr curr;
      mark_loop curr f1

    (* predefined exceptions are constants *)
    | Fprim(Pgetglobal id, [], _, _) ->
      if not (Ident.is_predef_exn id)
      then mark_curr curr

    | Fprim(Pgetglobalfield(id,i), [], _, _) ->
      (* adds 'global i in NC => curr in NC' *)
      if for_clambda
      then mark_curr curr
      else
        (* This is correct only if there is a rebind phase after !
           Clambdagen cannot handle this *)
        (* if some inlining produced some unreachable code like
           {[let a = 0 in
             if a
             then a.(0)
             else ... ]}
           then a.(0) cannot be compiled. There must be a specialisation
           phase after that eliminating the then branch and a dead code
           elimination eliminating potential reference to a.(0) *)
      if id.Ident.name = Compilenv.current_unit_name ()
      then add_depend curr (Global i)
      else mark_curr curr

    | Fprim(Psetglobalfield i, [f], _, _) ->
      mark_curr curr;
      (* adds 'f in NC => global i in NC' *)
      mark_loop [Global i] f

    (* Not constant cases: we mark directly 'curr in NC' and mark
       bound variables as in NC also *)

    | Fassign (id, f1, _) ->
      (* the assigned is also not constant *)
      mark_curr [Var id];
      mark_curr curr;
      mark_loop [] f1

    | Ftrywith (f1,id,f2,_) ->
      mark_curr [Var id];
      mark_curr curr;
      mark_loop [] f1;
      mark_loop [] f2

    | Fcatch (_,ids,f1,f2,_) ->
      List.iter (fun id -> mark_curr [Var id]) ids;
      mark_curr curr;
      mark_loop [] f1;
      mark_loop [] f2

    | Ffor (id,f1,f2,_,f3,_) ->
      mark_curr [Var id];
      mark_curr curr;
      mark_loop [] f1;
      mark_loop [] f2;
      mark_loop [] f3

    | Fsequence (f1,f2,_)
    | Fwhile (f1,f2,_) ->
      mark_curr curr;
      mark_loop [] f1;
      mark_loop [] f2

    | Fifthenelse (f1,f2,f3,_) ->
      mark_curr curr;
      mark_loop [] f1;
      mark_loop [] f2;
      mark_loop [] f3

    | Fstaticfail (_,l,_)
    | Fprim (_,l,_,_) ->
      mark_curr curr;
      List.iter (mark_loop []) l

    | Fapply (f1,fl,_,_,_) ->
      mark_curr curr;
      mark_loop [] f1;
      List.iter (mark_loop []) fl

    | Fswitch (arg,sw,_) ->
      mark_curr curr;
      List.iter (fun (_,l) -> mark_loop [] l) sw.fs_consts;
      List.iter (fun (_,l) -> mark_loop [] l) sw.fs_blocks;
      Misc.may (fun l -> mark_loop [] l) sw.fs_failaction

    | Fsend (_,f1,f2,fl,_,_) ->
      mark_curr curr;
      mark_loop [] f1;
      mark_loop [] f2;
      List.iter (mark_loop []) fl

    | Funreachable _ ->
      mark_curr curr

  (* Second loop: propagates implications *)
  let propagate () =
    (* Set of variables/closures added to NC but not their dependencies *)
    let q = Queue.create () in
    IdentSet.iter (fun v -> Queue.push (Var v) q) !variables;
    FunSet.iter (fun v -> Queue.push (Closure v) q) !closures;
    while not (Queue.is_empty q) do
      let deps = try match Queue.take q with
        | Var e -> IdentTbl.find id_dep_table e
        | Closure cl -> FunTbl.find fun_dep_table cl
        | Global i -> IntTbl.find glob_dep_table i
      with Not_found -> [] in
      List.iter (function
        | Var id as e ->
          if not (IdentSet.mem id !variables)
          then (variables := IdentSet.add id !variables;
            Queue.push e q)
        | Closure cl as e ->
          if not (FunSet.mem cl !closures)
          then (closures := FunSet.add cl !closures;
            Queue.push e q)
        | Global i as e ->
          if not (IntSet.mem i !globals)
          then (globals := IntSet.add i !globals;
            Queue.push e q))
        deps
    done

  let res =
    mark_loop [] P.expr;
    propagate ();
    { not_constant_id = !variables;
      not_constant_closure = !closures; }
end

let not_constants (type a) ~for_clambda (expr:a Flambda.flambda) =
  let module P = struct
    type t = a
    let expr = expr
    let for_clambda = for_clambda
    let global_var = Flambdautils.global_var expr
  end in
  let module A = NotConstants(P) in
  A.res

let offset off_id = {off_id; off_unit = Compilenv.current_unit_id ()}

type tag = int

type abstract_values =
  | Vsymbol of symbol
  | Vvar of Ident.t
  | Vblock of tag * abstract_values array
  | Vclosure of FunId.t * (offset option) * (abstract_values IdentMap.t)
  | Vfield of int * abstract_values
  | Vglobal of int
  | Voffset of abstract_offset
  | Venv_field of abstract_env_field
  | Vpredef_exn of Ident.t
  | Vbase_const of base_constant
  | Vunreachable
  | Vnot_constant

and base_constant =
  | Vconst_int of int
  | Vconst_pointer of int
  | Vconst_other

and abstract_offset =
  { offset_field : offset;
    offset_abstract : abstract_values }

and abstract_env_field =
  { env_offset : offset; env_field_val : abstract_values; env_fun : offset }

let print_abs ppf v = let open Format in match v with
  | Vsymbol sym -> fprintf ppf "Sym %a" Symbol.print sym
  | Vvar id -> fprintf ppf "Var %a" Ident.print id
  | Vblock (tag,_) -> fprintf ppf "Block %i" tag
  | Vclosure _ -> fprintf ppf "Closure"
  | Vfield _ -> fprintf ppf "Field"
  | Vglobal i -> fprintf ppf "Global %i" i
  | Voffset _ -> fprintf ppf "Offset"
  | Venv_field _ -> fprintf ppf "Env_field"
  | Vpredef_exn _ -> fprintf ppf "Predef_exn"
  | Vbase_const _ -> fprintf ppf "Const"
  | Vunreachable -> fprintf ppf "Unreachable"
  | Vnot_constant -> fprintf ppf "Not_const"

type abstract_table = abstract_values IdentTbl.t

module type AliasParam = sig
  type t
  val global_var : Ident.t IdentTbl.t
  val expr : t Flambda.flambda
  val const_result : constant_result
end

module ConstantAlias(P:AliasParam) = struct

  let is_constant id = not (IdentSet.mem id P.const_result.not_constant_id)
  let global_var = P.global_var

  (* Table representing potential aliases *)
  let abstr_table : abstract_values IdentTbl.t = IdentTbl.create 100
  let abstr_globals : abstract_values IntTbl.t = IntTbl.create 100

  let add_abstr id v = IdentTbl.add abstr_table id v

  let add_abstr_global n v = IntTbl.add abstr_globals n v

  let rec mark_alias v =
    ignore (mark_alias_result v:abstract_values)

  and mark_alias_result = function
    | Flet(str, id, lam, body, _) ->
      let lam_res = mark_alias_result lam in
      if not (str = Variable && not (is_constant id))
      then add_abstr id lam_res;
      mark_alias_result body

    | Fletrec(defs, body, _) ->
      List.iter (fun (id,def) ->
          let lam_res = mark_alias_result def in
          add_abstr id lam_res) defs;
      mark_alias_result body

    | Fvar (id,_) ->
      Vvar id

    | Fclosure (funcs,fv,_) ->
      let closure = IdentMap.mapi (fun inner_id lam -> mark_alias_result lam) fv in
      IdentMap.iter add_abstr closure;
      let result = Vclosure (funcs.ident, None, closure) in
      IdentMap.iter (fun fun_id ffunc ->
          add_abstr fun_id
            (Voffset { offset_field = offset fun_id;
                       offset_abstract = result });
          mark_alias ffunc.body) funcs.funs;
      result

    | Fsymbol (sym,_) ->
      Vsymbol sym

    | Fconst (cst,_) ->
      let base_cons = match cst with
        | Fconst_pointer i -> Vconst_pointer i
        | Fconst_base (Const_int i) -> Vconst_int i
        | Fconst_base (Const_char c) -> Vconst_int (Char.code c)
        | _ -> Vconst_other
      in
      Vbase_const base_cons

    | Fprim(Pmakeblock(tag, Immutable), args, dbg, _) ->
      let r = List.map mark_alias_result args in
      Vblock (tag, Array.of_list r)

    | Foffset (f1, offset_field,_) ->
      mark_alias f1;
      Voffset { offset_field; offset_abstract = mark_alias_result f1 }

    | Fenv_field ({env = f1; env_var; env_fun_id = env_fun},_) ->
      mark_alias f1;
      Venv_field { env_offset = env_var;
                   env_field_val = mark_alias_result f1;
                   env_fun }

    (* predefined exceptions are constants *)
    | Fprim(Pgetglobal id, [], _, _) ->
      if Ident.is_predef_exn id
      then Vpredef_exn id
      else Vnot_constant

    | Fprim(Pgetglobalfield(id,i), [], _, _) ->
      if id.Ident.name = Compilenv.current_unit_name ()
      then Vglobal i
      else Vnot_constant

    | Fprim(Pfield i, [f1], _, _) ->
      Vfield (i,mark_alias_result f1)

    | Fprim(Psetglobalfield i, [lam], _, _) ->
      let lam_res = mark_alias_result lam in
      add_abstr_global i lam_res;
      Vnot_constant

    | Fassign (id, f1, _) ->
      mark_alias f1;
      Vnot_constant

    | Ftrywith (f1,id,f2,_) ->
      mark_alias f1;
      mark_alias f2;
      Vnot_constant

    | Fcatch (_,ids,f1,f2,_) ->
      mark_alias f1;
      mark_alias f2;
      Vnot_constant

    | Ffor (id,f1,f2,_,f3,_) ->
      mark_alias f1;
      mark_alias f2;
      mark_alias f3;
      Vnot_constant

    | Fsequence (f1,f2,_) ->
      mark_alias f1;
      mark_alias_result f2

    | Fwhile (f1,f2,_) ->
      mark_alias f1;
      mark_alias f2;
      Vnot_constant

    | Fifthenelse (f1,f2,f3,_) ->
      mark_alias f1;
      mark_alias f2;
      mark_alias f3;
      Vnot_constant

    | Fstaticfail (_,l,_)
    | Fprim (_,l,_,_) ->
      List.iter mark_alias l;
      Vnot_constant

    | Fapply (f1,fl,_,_,_) ->
      mark_alias f1;
      List.iter mark_alias fl;
      Vnot_constant

    | Fswitch (arg,sw,_) ->
      List.iter (fun (_,l) -> mark_alias l) sw.fs_consts;
      List.iter (fun (_,l) -> mark_alias l) sw.fs_blocks;
      Misc.may (fun l -> mark_alias l) sw.fs_failaction;
      Vnot_constant

    | Fsend (_,f1,f2,fl,_,_) ->
      mark_alias f1;
      mark_alias f2;
      List.iter mark_alias fl;
      Vnot_constant

    | Funreachable _ ->
      Vunreachable

  (* alias analysis *)

  (* third loop: propagate alias informations of constants *)

  let result : (Ident.t option * abstract_values) IdentTbl.t =
    IdentTbl.create 100

  let rec resolve id : (Ident.t option * abstract_values) =
    (* Printf.printf "%s\n%!" (Ident.unique_name id); *)
    try IdentTbl.find result id with Not_found ->
      (* assert (is_constant id); (\* only consider constants *\) *)
      (* assert (IdentTbl.mem abstr_table id); *)
      let res =
        if IdentTbl.mem abstr_table id
        then resolve_abs (IdentTbl.find abstr_table id)
        else None, Vnot_constant in
      IdentTbl.add result id res;
      res

  and resolve_abs v =
    (* only for debugging *)
    let (_,r) as res = resolve_abs' v in
    match r with
    | Vvar id ->
      Format.printf "resolve to var %a %a@." print_abs v Ident.print id;
      assert false
    | _ -> res

  and resolve_abs' : _ -> (Ident.t option * abstract_values) = function
    | (Vblock _ | Vclosure _ | Vpredef_exn _ |
       Vbase_const _ | Vsymbol _) as v ->
      None, v
    | Vnot_constant as v ->
      None, v
    | Vvar id ->
      begin match resolve id with
        | _, Vvar r ->
          Format.printf "resolve to var %a %a@." Ident.print id Ident.print r;
          assert false
        | None, res -> Some id, res
        | res -> res
      end
    | Vfield (i,abs) ->
      (match resolve_abs abs with
       | _, Vblock (_,a) ->
         if Array.length a <= i
         then None, Vunreachable
         else resolve_abs a.(i)
       | _, Vsymbol _ ->
         (* TODO *)
         None, Vnot_constant
       | _, Vnot_constant ->
         None, Vnot_constant
       | _, a ->
         Format.printf "make unreachable %a@." print_abs a;
         (* This can happen with impossible branch not yet
            eliminated by specialisation *)
         None, Vunreachable)
    | Voffset { offset_field; offset_abstract } ->
      let res = resolve_abs offset_abstract in
      (match res with
       | _, Vclosure (clos_id, None, map) ->
         None, Vclosure (clos_id, Some offset_field, map)
       | _ -> assert false)
    | Venv_field { env_offset; env_field_val; env_fun } ->
      (match resolve_abs env_field_val with
       | _, Vclosure (_, Some fun_id, map) ->
         assert(Offset.equal fun_id env_fun);
         assert(IdentMap.mem env_offset.off_id map);
         resolve_abs (IdentMap.find env_offset.off_id map)
       | _, Vnot_constant ->
         None, Vnot_constant
       | _, Vsymbol _ ->
         (* TODO *)
         None, Vnot_constant
       | _, abs ->
         Format.printf "%a@." print_abs abs;
         assert false)
    | Vglobal i ->
      assert(IntTbl.mem abstr_globals i);
      resolve_abs (IntTbl.find abstr_globals i)
    | Vunreachable ->
      None, Vunreachable

  let propagate_alias () =
    (* let resolve id _ = *)
    (*   if is_constant id (\* only call on constants *\) *)
    (*   then ignore (resolve id:_ * _) in *)

    let resolve id _ = ignore (resolve id:_ * _) in
    IdentTbl.iter resolve abstr_table

  let () = mark_alias P.expr

  let alias () =
    propagate_alias ();
    IdentTbl.fold (fun key abs map -> match abs with
        | Some aid, _ -> IdentMap.add key aid map
        | _ -> map) result IdentMap.empty

  (* global abstraction *)

  let global_abstract () =
    let global_size =
      let r = ref (-1) in
      IntTbl.iter (fun i _ -> r := max !r i) abstr_globals;
      !r in
    let a = Array.init (global_size+1)
        (fun i -> IntTbl.find abstr_globals i) in
    Vblock(0,a)

end

type alias_result =
  { constant_result : constant_result;
    constant_alias : Ident.t Flambda.IdentMap.t }

let alias (type a) (expr:a Flambda.flambda) =
  let module P = struct
    type t = a
    let expr = expr
    let global_var = Flambdautils.global_var expr
    let const_result = not_constants ~for_clambda:false expr
  end in
  let module A = ConstantAlias(P) in
  { constant_result = P.const_result;
    constant_alias = A.alias () }




(** Export *)

open Flambdaexport

type acc =
  { mapping : approx Flambda.IdentMap.t;
    values : descr Flambdaexport.EidMap.t;
    closures : value_closure Flambda.FunMap.t }

let export_informations
    not_constants
    (root:abstract_values)
    (abstr_table:abstract_table)
    (resolve_abs:abstract_values -> (Ident.t option * abstract_values)) =

  let find_id id =
    try Some (IdentTbl.find abstr_table id) with Not_found -> None in

  let rec add_id
      (acc:acc)
      (id:Ident.t) =
    try IdentMap.find id acc.mapping, acc with
    | Not_found ->
      match find_id id with
      | None ->
        (* value unknown *)
        let approx = Value_unknown in
        let mapping = IdentMap.add id approx acc.mapping in
        approx, { acc with mapping }
      | Some abstr ->
        add_abs acc (Some id) abstr

  and add_abs
      (acc:acc)
      (id:Ident.t option)
      (abstr:abstract_values) =
    let alias_id, resolved = resolve_abs abstr in
    match alias_id, abstr with
    | Some alias_id, _ ->
      add_id acc alias_id
    | None,
      (Vvar _ | Vfield _ | Vnot_constant |
       Vglobal _ | Venv_field _ | Vunreachable |
       Vclosure _ | Vbase_const Vconst_other ) ->
      Value_unknown, acc
    | None, abstr ->
      let export_id = ExportId.create () in
      let approx = Value_id export_id in
      let acc = match id with
        | None -> acc
        | Some id ->
          let mapping = IdentMap.add id approx acc.mapping in
          { acc with mapping } in
      let descr, acc = match abstr with
        | Vvar _ | Vfield _ | Vnot_constant
        | Vglobal _ | Venv_field _ | Vunreachable
        | Vclosure _ | Vbase_const Vconst_other ->
          assert false
        | Vpredef_exn id -> Value_predef_exn id, acc
        | Vsymbol sym ->
          Value_symbol sym, acc
        | Vbase_const (Vconst_int i) ->
          Value_int i, acc
        | Vbase_const (Vconst_pointer i) ->
          Value_constptr i, acc
        | Vblock(tag, fields) ->
          aux_block acc tag fields
        | Voffset _ -> begin match resolved with
            | Vclosure (closure_id, Some fun_id, bound_var) ->
              aux_fun acc closure_id fun_id bound_var
            | _ -> assert false
          end
      in
      approx, { acc with values = EidMap.add export_id descr acc.values }

  and aux_block acc tag fields =
    let fields, acc =
      Array.fold_right (fun abs (list_acc, acc) ->
          let approx, acc = add_abs acc None abs in
          approx :: list_acc, acc) fields
        ([], acc) in
    Value_block (tag, Array.of_list fields), acc

  and aux_fun acc closure_id fun_id bound_var =
    let closure, acc = aux_closure acc closure_id bound_var in
    Value_closure { fun_id; closure }, acc

  and aux_closure acc closure_id bound_var =
    try FunMap.find closure_id acc.closures, acc with
    | Not_found ->
      let f id abs (bound_var, acc) =
        let approx, acc = add_abs acc (Some id) abs in
        OffsetMap.add (offset id) approx bound_var, acc
      in
      let bound_var, acc = IdentMap.fold f bound_var
          (OffsetMap.empty, acc) in
      let closure = { closure_id; bound_var } in
      let closures = FunMap.add closure_id closure acc.closures in
      closure, { acc with closures }
  in

  let acc = { mapping = IdentMap.empty;
              values = EidMap.empty;
              closures = FunMap.empty } in
  let approx, acc = add_abs acc None root in
  approx, acc

type export_result =
  { export_constant : constant_result;
    export_global : approx;
    export_values : descr EidMap.t;
    export_mapping : approx Flambda.IdentMap.t }

let export_info (type a) (expr:a Flambda.flambda) =
  let module P = struct
    type t = a
    let expr = expr
    let global_var = Flambdautils.global_var expr
    let const_result = not_constants ~for_clambda:true expr
  end in
  let module A = ConstantAlias(P) in
  let approx, acc =
    export_informations
      P.const_result
      (A.global_abstract ())
      A.abstr_table
      A.resolve_abs
  in
  { export_constant = P.const_result;
    export_global = approx;
    export_values = acc.values;
    export_mapping = acc.mapping }
