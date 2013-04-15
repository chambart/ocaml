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

module type Id = sig
  type t
  val create : ?name:string -> unit -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val to_string : t -> string
  val output : out_channel -> t -> unit
  val print : Format.formatter -> t -> unit
end

module Id(E:Empty) : Id = struct
  type t = int * string
  let empty_string = ""
  let create = let r = ref 0 in
    fun  ?(name=empty_string) () -> incr r; !r, name
  let equal (t1,_) (t2,_) = (t1:int) = t2
  let compare (t1,_) (t2,_) = t1 - t2
  let hash (t,_) = t
  let to_string (t,name) =
    if name == empty_string
    then string_of_int t
    else Printf.sprintf "%s_%i" name t
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
  { fs_numconsts: int;                  (* Number of integer cases *)
    fs_consts: (int * 'a flambda) list;    (* Integer cases *)
    fs_numblocks: int;                  (* Number of tag block cases *)
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

   TODO: check assumptions of the rest of the code
   - no let rec x = y and y = ...
     -> assumed by clambdagen
   - allow access to free variables if they are constants ?
 *)

module StringSet = Set.Make(String)

type env = {
  bound_variables : IdentSet.t;
  seen_variables : IdentSet.t ref;
  seen_fun_label : StringSet.t ref;
  seen_static_catch : IntSet.t ref;
  caught_static_exceptions : IntSet.t;
  closure_variables : IdentSet.t;
}

let add_check_env id env =
  if IdentSet.mem id !(env.seen_variables)
  then fatal_error (Printf.sprintf "Flambda.check: variable %s bound \
                                    multiple times" (Ident.unique_name id));
  env.seen_variables := IdentSet.add id !(env.seen_variables);
  { env with
    bound_variables = IdentSet.add id env.bound_variables }

let bind_var id lam env =
  let env = add_check_env id env in
  match lam with
  | Fclosure _ ->
    { env with closure_variables = IdentSet.add id env.closure_variables }
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
  | Fletrec(defs, body,_) ->
    let env = List.fold_left (fun env (id,lam) -> bind_var id lam env) env defs in
    List.iter (fun (_,def) -> check env def) defs;
    check env body
  | Fclosure(funct, fv,_) ->
    List.iter (fun (_, l) -> check env l) (IdentMap.bindings fv);
    check_closure env funct fv
  | Foffset(lam,id,_) ->
    check env lam;
    begin match lam with
      | Fclosure _ -> ()
      | Fvar(id,_) ->
        if not (IdentSet.mem id env.closure_variables)
        then
          fatal_error (Printf.sprintf "Flambda.check: Foffset on a variable \
                                       not bound to a closure: %s"
              (Ident.unique_name id))
      | _ ->
        fatal_error (Printf.sprintf "Flambda.check: Foffset on neither a \
                                     variable nor a closure")
    end
  | Fenv_field({ env = env_lam; env_fun_id; env_var },_) ->
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

and check_closure orig_env funct fv =
  let fv = List.map fst (IdentMap.bindings fv) in
  let funs = List.map fst (IdentMap.bindings funct.funs) in
  let env = List.fold_right add_check_env fv (empty_env orig_env) in
  let env = List.fold_right add_rec_closure funs env in
  IdentMap.iter (fun _ func ->
    let env = List.fold_right add_check_env func.params env in
    check env func.body) funct.funs

let check flam =
  let env = { bound_variables = IdentSet.empty;
              seen_variables = ref IdentSet.empty;
              seen_fun_label = ref StringSet.empty;
              seen_static_catch = ref IntSet.empty;
              caught_static_exceptions = IntSet.empty;
              closure_variables = IdentSet.empty } in
  check env flam

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
