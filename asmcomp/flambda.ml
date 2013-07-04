open Misc
open Asttypes
open Lambda

module type PrintableHashOrdered = sig
  type t
  val compare : t -> t -> int
  val output : out_channel -> t -> unit
  val print : Format.formatter -> t -> unit
  val hash : t -> int
  val equal : t -> t -> bool
end

module ExtMap(M:PrintableHashOrdered) = struct
  include Map.Make(M)
  let map_option f m =
    fold (fun id v map ->
      match f id v with
      | None -> map
      | Some r -> add id r map) m empty
  let of_list l =
    List.fold_left (fun map (id,v) -> add id v map) empty l
end

module ExtSet(M:PrintableHashOrdered) = struct
  include Set.Make(M)
  let output oc s =
    Printf.fprintf oc "( ";
    iter (fun v -> Printf.fprintf oc "%a " M.output v) s;
    Printf.fprintf oc ")"
  let print ppf s =
    let elts ppf s = iter (fun e -> Format.fprintf ppf "@ %a" M.print e) s in
    Format.fprintf ppf "@[<1>{@[%a@ @]}@]" elts s
  let of_list l = match l with
    | [] -> empty
    | [t] -> singleton t
    | t :: q -> List.fold_left (fun acc e -> add e acc) (singleton t) q
end

module ExtHashtbl(M:PrintableHashOrdered) = struct
  include Hashtbl.Make(M)
  module MMap = Map.Make(M)
  let to_map v = fold MMap.add v MMap.empty
end

module type Empty = sig end
module Empty : Empty = struct end

module type BaseId = sig
  type t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val name : t -> string option
  val to_string : t -> string
  val output : out_channel -> t -> unit
  val print : Format.formatter -> t -> unit
end

module type Id = sig
  include BaseId
  val create : ?name:string -> unit -> t
end

module type UnitId = sig
  include BaseId
  val create : ?name:string -> string -> t
end

module Id(E:Empty) : Id = struct
  type t = int * string
  let empty_string = ""
  let create = let r = ref 0 in
    fun  ?(name=empty_string) () -> incr r; !r, name
  let equal (t1,_) (t2,_) = (t1:int) = t2
  let compare (t1,_) (t2,_) = t1 - t2
  let hash (t,_) = t
  let name (_,name) =
    if name == empty_string
    then None
    else Some name
  let to_string (t,name) =
    if name == empty_string
    then string_of_int t
    else Printf.sprintf "%s_%i" name t
  let output fd t = output_string fd (to_string t)
  let print ppf v = Format.pp_print_string ppf (to_string v)
end

module ModuleId(E:Empty) : UnitId = struct
  type t = string * int * string
  let empty_string = ""
  let create = let r = ref 0 in
    fun  ?(name=empty_string) unit_name ->
      incr r; unit_name, !r, name
  let equal (u1,t1,_) (u2,t2,_) = (t1:int) = t2 && (u1:string) = u2
  let compare (u1,t1,_) (u2,t2,_) =
    let diff = t1 - t2 in
    if diff <> 0
    then diff
    else String.compare (u1:string) u2
  let hash (u,t,_) = t lxor (Hashtbl.hash u)
  let name (_,_,name) =
    if name == empty_string
    then None
    else Some name
  let to_string (u,t,name) =
    if name == empty_string
    then string_of_int t
    else Printf.sprintf "%s.%s_%i" u name t
  let output fd t = output_string fd (to_string t)
  let print ppf v = Format.pp_print_string ppf (to_string v)
end

module type UnitName = sig val unit_name : unit -> string end

module UnitId(U:UnitName) : Id = struct
  type t = string * int * string
  let empty_string = ""
  let create = let r = ref 0 in
    fun  ?(name=empty_string) () ->
      incr r; U.unit_name (), !r, name
  let equal (u1,t1,_) (u2,t2,_) = (t1:int) = t2 && (u1:string) = u2
  let compare (u1,t1,_) (u2,t2,_) =
    let diff = t1 - t2 in
    if diff <> 0
    then diff
    else String.compare (u1:string) u2
  let hash (u,t,_) = t lxor (Hashtbl.hash u)
  let name (_,_,name) =
    if name == empty_string
    then None
    else Some name
  let to_string (u,t,name) =
    if name == empty_string
    then string_of_int t
    else Printf.sprintf "%s.%s_%i" u name t
  let output fd t = output_string fd (to_string t)
  let print ppf v = Format.pp_print_string ppf (to_string v)
end

module Int = struct
  type t = int
  let compare x y = x - y
  let output oc x = Printf.fprintf oc "%i" x
  let hash i = i
  let equal (i:int) j = i = j
  let print = Format.pp_print_int
end

module IntSet = ExtSet(Int)
module IntMap = ExtMap(Int)
module IntTbl = ExtHashtbl(Int)

module ExprId : Id = Id(Empty)
module ExprMap = ExtMap(ExprId)
module ExprSet = ExtSet(ExprId)
module ExprTbl = ExtHashtbl(ExprId)

module FunId : Id = Id(Empty)
module FunMap = ExtMap(FunId)
module FunSet = ExtSet(FunId)
module FunTbl = ExtHashtbl(FunId)

module Idt = struct
  type t = Ident.t
  let compare x y = compare x.Ident.stamp y.Ident.stamp
  let output oc id = output_string oc (Ident.unique_name id)
  let print = Ident.print
  let hash i = i.Ident.stamp
  let equal = Ident.same
end

module IdentSet = Lambda.IdentSet
module IdentMap = ExtMap(Idt)
module IdentTbl = ExtHashtbl(Idt)

module M : sig
  type function_label = private string
  val make_function_label : string -> function_label
end = struct
  type function_label = string
  let make_function_label str = str
end

include M

type closed = Closed | NotClosed

(* A data is attached to each node. It is often used to uniquely
   identify an expression *)
type 'a flambda =
  | Fvar of Ident.t * 'a
  | Fconst of const * 'a
  | Fapply of 'a flambda * 'a flambda list *
        (function_label*closed) option * Debuginfo.t * 'a
  | Fclosure of 'a ffunctions * 'a flambda IdentMap.t * 'a
  | Foffset of 'a flambda * Ident.t * 'a
    (* Foffset(closure, id) access to the function 'id' from the closure *)
  | Fenv_field of 'a fenv_field * 'a
  | Flet of let_kind * Ident.t * 'a flambda * 'a flambda * 'a
  | Fletrec of (Ident.t * 'a flambda) list * 'a flambda * 'a
  | Fprim of primitive * 'a flambda list * Debuginfo.t * 'a
  | Fswitch of 'a flambda * 'a flambda_switch * 'a
  | Fstaticfail of int * 'a flambda list * 'a
  | Fcatch of int * Ident.t list * 'a flambda * 'a flambda * 'a
  | Ftrywith of 'a flambda * Ident.t * 'a flambda * 'a
  | Fifthenelse of 'a flambda * 'a flambda * 'a flambda * 'a
  | Fsequence of 'a flambda * 'a flambda * 'a
  | Fwhile of 'a flambda * 'a flambda * 'a
  | Ffor of Ident.t * 'a flambda * 'a flambda * direction_flag * 'a flambda * 'a
  | Fassign of Ident.t * 'a flambda * 'a
  | Fsend of meth_kind * 'a flambda * 'a flambda * 'a flambda list * Debuginfo.t * 'a

and const =
  | Fconst_base of constant
  | Fconst_pointer of int
  | Fconst_float_array of string list
  | Fconst_immstring of string

and 'a flambda_switch =
  { fs_numconsts: IntSet.t;               (* integer cases *)
    fs_consts: (int * 'a flambda) list;    (* Integer cases *)
    fs_numblocks: IntSet.t;               (* Number of tag block cases *)
    fs_blocks: (int * 'a flambda) list;    (* Tag block cases *)
    fs_failaction : 'a flambda option }    (* Action to take if failure *)

and 'a ffunction = {
  label  : function_label;
  kind   : function_kind;
  arity  : int;
  params : Ident.t list;
  closure_params : IdentSet.t;
  body   : 'a flambda;
  dbg    : Debuginfo.t;
}

and 'a ffunctions = {
  ident  : FunId.t;
  funs   : 'a ffunction IdentMap.t;
  recursives : bool;
}

and 'a fenv_field = {
  env : 'a flambda;
  env_fun_id : Ident.t;
  env_var : Ident.t
}

let same f1 f2 =
  (* TODO ! used for switch compiling *)
  false

(* Well formedness checking
   Ensures that:
    * No identifier is bound multiple times
    * every variable used is bound
    * Foffset is applied only on closures or on a variable directly
      bound to a closure. ( this is assumed by Clambdagen )
    * At most one place can catch a static exception
    * Staticfail are correctly enclosed inside a catch
    * no let rec x = y and y = ...
      -> assumed by clambdagen

   TODO: check assumptions of the rest of the code
    * check that there is no env_field on a constant
 *)

module StringSet = Set.Make(String)

type 'a env = {
  bound_variables : IdentSet.t;
  seen_variables : IdentSet.t ref;
  seen_fun_label : StringSet.t ref;
  seen_static_catch : IntSet.t ref;
  seen_env_var : IdentSet.t ref;
  seen_fun_env_var : IdentSet.t IdentMap.t ref;
  need_env_var : IdentSet.t IdentMap.t ref;
  caught_static_exceptions : IntSet.t;
  closure_variables : ('a ffunctions * 'a flambda IdentMap.t) IdentMap.t;
  (* variables bound to a closure *)
}

let add_check_env' env id =
  if IdentSet.mem id !(env.seen_variables)
  then fatal_error (Printf.sprintf "Flambda.check: variable %s bound \
                                    multiple times" (Ident.unique_name id));
  env.seen_variables := IdentSet.add id !(env.seen_variables)

let add_env id env =
  { env with
    bound_variables = IdentSet.add id env.bound_variables }

let add_check_env id env =
  add_check_env' env id;
  add_env id env

let need_env_var fun_id var_id env =
  let set = try IdentMap.find fun_id !(env.need_env_var) with
    | Not_found -> IdentSet.empty in
  env.need_env_var :=
    IdentMap.add fun_id (IdentSet.add var_id set) !(env.need_env_var)

let seen_env_var fun_id var_id env =
  let set = try IdentMap.find fun_id !(env.seen_fun_env_var) with
    | Not_found -> IdentSet.empty in
  env.seen_fun_env_var :=
    IdentMap.add fun_id (IdentSet.add var_id set) !(env.seen_fun_env_var)

let bind_var id lam env =
  let env = add_check_env id env in
  match lam with
  | Fvar (var,_) ->
    if IdentMap.mem var env.closure_variables
    then
      let closure_var = IdentMap.find var env.closure_variables in
      { env with closure_variables =
                   IdentMap.add id closure_var env.closure_variables }
    else env
  | Fclosure (ffun,fv,_) ->
    { env with closure_variables =
                 IdentMap.add id (ffun,fv) env.closure_variables }
  | _ -> env

(* Adds without checks: the variable of the closure was already
   inserted in an environment, but will not be available inside the
   closure if we do not add it here also. *)
let add_rec_closure id env =
  { env with bound_variables = IdentSet.add id env.bound_variables }

let add_check_fun_label lbl env =
  if StringSet.mem lbl !(env.seen_fun_label)
  then fatal_error (Printf.sprintf "Flambda.check: function %s appear \
                                    multiple times" lbl);
  env.seen_fun_label := StringSet.add lbl !(env.seen_fun_label)

let add_check_static_catch n env =
  if IntSet.mem n !(env.seen_static_catch)
  then fatal_error (Printf.sprintf "Flambda.check: static exception %i \
                                    caught at multiple places" n);
  env.seen_static_catch := IntSet.add n !(env.seen_static_catch);
  { env with
    caught_static_exceptions = IntSet.add n env.caught_static_exceptions }

let empty_env env =
  { env with bound_variables = IdentSet.empty }

let rec check env = function
  | Fvar (id,_) ->
    if not (IdentSet.mem id env.bound_variables)
    then fatal_error (Printf.sprintf "Flambda.check: unbound variable %s"
          (Ident.unique_name id))
  | Fconst (cst,_) -> ()
  | Flet(str, id, lam, body,_) ->
    check env lam;
    let env = bind_var id lam env in
    check env body
  | Fletrec(defs, body,_) -> List.iter (function
      (* clambdagen assumes this *)
      | (_,Fvar(var_id,_)) ->
        List.iter (fun (id,_) ->
            if Ident.same var_id id
            then fatal_error (Printf.sprintf "Flambda.check: recursive alias to \
                                              var %s" (Ident.unique_name id)))
          defs
      | _ -> ())
      defs;
    let env = List.fold_left (fun env (id,lam) -> bind_var id lam env) env defs in
    List.iter (fun (_,def) -> check env def) defs;
    check env body
  | Fclosure(funct, fv,_) ->
    List.iter (fun (id, l) ->
        if IdentSet.mem id !(env.seen_env_var)
        then fatal_error (Printf.sprintf "Flambda.check: closure variable %s \
                                          bound in multiple closures"
              (Ident.unique_name id))
        else env.seen_env_var := IdentSet.add id !(env.seen_env_var);
        check env l) (IdentMap.bindings fv);
    check_closure env funct fv
  | Foffset(lam,id,_) ->
    check env lam;
    let rec find_var_offset = function
      | Fclosure (ffun,fv,_) -> ffun,fv
      | Fvar(id,_) ->
        (try IdentMap.find id env.closure_variables
         with Not_found ->
           fatal_error (Printf.sprintf "Flambda.check: Foffset on a variable \
                                        not bound to a closure: %s"
                          (Ident.unique_name id)))
      | Flet(_,_,_,body,_) -> find_var_offset body
      | _ ->
        fatal_error (Printf.sprintf "Flambda.check: Foffset on neither a \
                                     variable nor a closure")
    in
    let (ffun,fv) = find_var_offset lam in

    (* TODO: also check the recursive flag *)
    if not (IdentMap.mem id ffun.funs)
    then fatal_error (Printf.sprintf "Flambda.check: Foffset function %s not \
                                      present in the closure"
          (Ident.unique_name id))
  | Fenv_field({ env = env_lam; env_fun_id; env_var },_) ->
    need_env_var env_fun_id env_var env;
    let closure = match env_lam with
      | Fclosure (ffun,fv,_) -> Some (ffun,fv)
      | Fvar(id,_) ->
        (try Some (IdentMap.find id env.closure_variables)
        with Not_found -> None)
      | _ -> None
    in
    begin match closure with
      | None -> () (* In recursive cases we can't know directly *)
      | Some (ffun,fv) ->
        if not (IdentMap.mem env_var fv)
        then fatal_error (Printf.sprintf "Flambda.check: Fenv_field var %s not \
                                          present in the closure"
              (Ident.unique_name env_var));
        if not (IdentMap.mem env_fun_id ffun.funs)
        then fatal_error (Printf.sprintf "Flambda.check: Fenv_field function %s \
                                          not present in the closure"
              (Ident.unique_name env_fun_id))
    end;
    check env env_lam
  | Fapply(funct, args, _, _,_) ->
    check env funct;
    List.iter (check env) args
  | Fswitch(arg, sw,_) ->
    check env arg;
    List.iter (fun (_,l) -> check env l) sw.fs_consts;
    List.iter (fun (_,l) -> check env l) sw.fs_blocks
  | Fsend(kind, met, obj, args, _,_) ->
    check env met;
    check env obj;
    List.iter (check env) args
  | Fprim(_, args, _,_) ->
    List.iter (check env) args
  | Fstaticfail (i, args,_) ->
    if not (IntSet.mem i env.caught_static_exceptions)
    then fatal_error (Printf.sprintf "Flambda.check: uncaught static \
                                      exception %i" i);
    List.iter (check env) args
  | Fcatch (i, vars, body, handler,_) ->
    let env' = add_check_static_catch i env in
    check env' body;
    let env = List.fold_right add_check_env vars env in
    check env handler
  | Ftrywith(body, id, handler,_) ->
    check env body;
    let env = add_check_env id env in
    check env handler
  | Fifthenelse(arg, ifso, ifnot,_) ->
    check env arg;
    check env ifso;
    check env ifnot
  | Fsequence(lam1, lam2,_) ->
    check env lam1;
    check env lam2
  | Fwhile(cond, body,_) ->
    check env cond;
    check env body
  | Ffor(id, lo, hi, dir, body,_) ->
    check env lo; check env hi;
    let env = add_check_env id env in
    check env body
  | Fassign(id, lam,_) ->
    check env lam

and check_closure orig_env funct fv' =
  let fv = List.map fst (IdentMap.bindings fv') in
  let funs = List.map fst (IdentMap.bindings funct.funs) in
  List.iter (add_check_env' orig_env) fv;
  let env = List.fold_right add_rec_closure funs (empty_env orig_env) in
  IdentMap.iter (fun fun_id func ->
    IdentSet.iter (fun id ->
      seen_env_var fun_id id env;
      if not (IdentMap.mem id fv')
      then
        fatal_error (Printf.sprintf "Flambda.check: variable %s not in \
                                     the closure" (Ident.unique_name id)))
      func.closure_params;
    let env = IdentSet.fold add_env func.closure_params env in
    let env = List.fold_right add_check_env func.params env in
    check env func.body) funct.funs

let check_fun_env_var need seen =
  IdentMap.iter (fun fun_id need_set ->
      let seen_set = try IdentMap.find fun_id seen with
        | Not_found ->
          fatal_error (Printf.sprintf "Flambda.check: closure variable needed \
                                       in function %s but not provided"
                         (Ident.unique_name fun_id))
      in
      let diff = IdentSet.diff need_set seen_set in
      IdentSet.iter (fun id ->
          fatal_error (Printf.sprintf "Flambda.check: var offset %s is needed \
                                       but not provided" (Ident.unique_name id)))
        diff) need

let check flam =
  let env = { bound_variables = IdentSet.empty;
              seen_variables = ref IdentSet.empty;
              seen_fun_label = ref StringSet.empty;
              seen_static_catch = ref IntSet.empty;
              seen_env_var = ref IdentSet.empty;
              seen_fun_env_var = ref IdentMap.empty;
              need_env_var = ref IdentMap.empty;
              caught_static_exceptions = IntSet.empty;
              closure_variables = IdentMap.empty } in
  check env flam;
  check_fun_env_var !(env.need_env_var) !(env.seen_fun_env_var)

let data = function
  | Fvar (id,data) -> data
  | Fconst (cst,data) -> data
  | Flet(str, id, lam, body,data) -> data
  | Fletrec(defs, body,data) -> data
  | Fclosure(funct, fv,data) -> data
  | Foffset(lam,id,data) -> data
  | Fenv_field(_,data) -> data
  | Fapply(funct, args, _, _,data) -> data
  | Fswitch(arg, sw,data) -> data
  | Fsend(kind, met, obj, args, _,data) -> data
  | Fprim(_, args, _,data) -> data
  | Fstaticfail (i, args,data) -> data
  | Fcatch (i, vars, body, handler,data) -> data
  | Ftrywith(body, id, handler,data) -> data
  | Fifthenelse(arg, ifso, ifnot,data) -> data
  | Fsequence(lam1, lam2,data) -> data
  | Fwhile(cond, body,data) -> data
  | Ffor(id, lo, hi, dir, body,data) -> data
  | Fassign(id, lam,data) -> data

let string_desc = function
  | Fvar (id,data) -> Ident.unique_name id
  | Fconst (cst,data) -> "const"
  | Flet(str, id, lam, body,data) ->
    Printf.sprintf "let %s"
      (Ident.unique_name id)
  | Fletrec(defs, body,data) -> "letrec"
  | Fclosure(funct, fv,data) -> "closure"
  | Foffset(lam,id,data) -> "offset"
  | Fenv_field(_,data) -> "env_field"
  | Fapply(funct, args, _, _,data) -> "apply"
  | Fswitch(arg, sw,data) -> "switch"
  | Fsend(kind, met, obj, args, _,data) -> "send"
  | Fprim(_, args, _,data) -> "prim"
  | Fstaticfail (i, args,data) -> "staticfail"
  | Fcatch (i, vars, body, handler,data) -> "catch"
  | Ftrywith(body, id, handler,data) -> "trywith"
  | Fifthenelse(arg, ifso, ifnot,data) -> "if"
  | Fsequence(lam1, lam2,data) -> "seq"
  | Fwhile(cond, body,data) -> "while"
  | Ffor(id, lo, hi, dir, body,data) -> "for"
  | Fassign(id, lam,data) -> "assign"
