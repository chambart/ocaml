open Asttypes
open Lambda
open Flambda

(* generic code folders *)

let iter_flambda f t =
  let rec aux t =
    f t;
    match t with
    | Fvar _
    | Fconst _ -> ()

    | Fassign (_,f1,_)
    | Foffset (f1, _,_)
    | Fenv_field ({env = f1},_) ->
      aux f1

    | Flet ( _, _, f1, f2,_)
    | Ftrywith (f1,_,f2,_)
    | Fsequence (f1,f2,_)
    | Fwhile (f1,f2,_)
    | Fcatch (_,_,f1,f2,_) ->
      aux f1; aux f2;

    | Ffor (_,f1,f2,_,f3,_)
    | Fifthenelse (f1,f2,f3,_) ->
      aux f1;aux f2;aux f3

    | Fstaticfail (_,l,_)
    | Fprim (_,l,_,_) ->
      iter_list l

    | Fapply (f1,fl,_,_,_) ->
      iter_list (f1::fl)
    | Fclosure (funcs,fv,_) ->
      IdentMap.iter (fun _ v -> f v) fv
    | Fletrec (defs, body,_) ->
      List.iter (fun (_,l) -> aux l) defs;
      aux body
    | Fswitch (arg,sw,_) ->
      aux arg;
      List.iter (fun (_,l) -> aux l) sw.fs_consts;
      List.iter (fun (_,l) -> aux l) sw.fs_blocks
    | Fsend (_,f1,f2,fl,_,_) ->
      iter_list (f1::f2::fl)

  and iter_list l = List.iter aux l in
  aux t

type 'a return =
  | Data of 'a
  | Node of 'a flambda

module type Folder = sig
  type annot
  type data

  val var : Ident.t -> annot -> data return
  val const : const -> annot -> data return
  val apply : func:data flambda -> args:data flambda list
        -> direct:(function_label*closed) option -> dbg:Debuginfo.t
        -> annot -> data return
  val closure : data ffunctions -> data flambda IdentMap.t -> annot -> data return
  val offset : data flambda -> Ident.t -> annot -> data return
  val env_field : data fenv_field -> annot -> data return
  val let_ : let_kind -> Ident.t -> def:data flambda -> body:data flambda -> annot -> data return
  val letrec : (Ident.t * data flambda) list -> data flambda -> annot -> data return
  val prim : primitive -> data flambda list -> Debuginfo.t -> annot -> data return
  val switch : data flambda -> data flambda_switch -> annot -> data return
  val staticfail : int -> data flambda list -> annot -> data return
  val catch : int -> Ident.t list -> data flambda -> data flambda -> annot -> data return
  val trywith : data flambda -> Ident.t -> data flambda -> annot -> data return
  val ifthenelse : data flambda -> data flambda -> data flambda -> annot -> data return
  val sequence : data flambda -> data flambda -> annot -> data return
  val while_ : data flambda -> data flambda -> annot -> data return
  val for_ : Ident.t -> data flambda -> data flambda -> direction_flag -> data flambda -> annot -> data return
  val assign : Ident.t -> data flambda -> annot -> data return
  val send : meth_kind -> data flambda -> data flambda -> data flambda list -> Debuginfo.t -> annot -> data return

end

module Fold(M:Folder) :
sig
  val fold : M.annot flambda -> M.data flambda
end = struct
  let rec fold = function
    | Fvar (id,annot) ->
      begin match M.var id annot with
        | Data data -> Fvar (id,data)
        | Node n -> n
      end
    | Fconst (cst,annot) ->
      begin match M.const cst annot with
        | Data data -> Fconst (cst,data)
        | Node n -> n
      end
    | Fapply (funct, args, direc, dbg, annot) ->
      let funct = fold funct in
      let args = fold_list args in
      begin match M.apply funct args direc dbg annot with
        | Data data -> Fapply (funct, args, direc, dbg, data)
        | Node n -> n
      end
    | Fclosure (ffuns, fv, annot) ->
      let ffuns =
        { ffuns with
          funs = IdentMap.map
              (fun ffun -> { ffun with body = fold ffun.body }) ffuns.funs } in
      let fv = IdentMap.map fold fv in
      begin match M.closure ffuns fv annot with
        | Data data -> Fclosure (ffuns, fv, data)
        | Node n -> n
      end
    | Foffset (flam, off, annot) ->
      let flam = fold flam in
      begin match M.offset flam off annot with
        | Data data -> Foffset (flam, off, data)
        | Node n -> n
      end
    | Fenv_field (fenv_field, annot) ->
      let flam = fold fenv_field.env in
      let fenv_field = { fenv_field with env = flam } in
      begin match M.env_field fenv_field annot with
        | Data data -> Fenv_field (fenv_field, data)
        | Node n -> n
      end
    | Flet(str, id, lam, body, annot) ->
      let lam = fold lam in
      let body = fold body in
      begin match M.let_ str id lam body annot with
        | Data data -> Flet (str, id, lam, body, data)
        | Node n -> n
      end
    | Fletrec(defs, body, annot) ->
      let defs = List.map (fun (id,lam) -> id,fold lam) defs in
      let body = fold body in
      begin match M.letrec defs body annot with
        | Data data -> Fletrec (defs, body, data)
        | Node n -> n
      end
    | Fprim(p, args, dbg, annot) ->
      let args = fold_list args in
      begin match M.prim p args dbg annot with
        | Data data -> Fprim (p, args, dbg, data)
        | Node n -> n
      end
    | Fstaticfail(i, args, annot) ->
      let args = fold_list args in
      begin match M.staticfail i args annot with
        | Data data -> Fstaticfail (i, args, data)
        | Node n -> n
      end
    | Fcatch (i, vars, body, handler, annot) ->
      let body = fold body in
      let handler = fold handler in
      begin match M.catch i vars body handler annot with
        | Data data -> Fcatch (i, vars, body, handler, data)
        | Node n -> n
      end
    | Ftrywith(body, id, handler, annot) ->
      let body = fold body in
      let handler = fold handler in
      begin match M.trywith body id handler annot with
        | Data data -> Ftrywith(body, id, handler, data)
        | Node n -> n
      end
    | Fifthenelse(arg, ifso, ifnot, annot) ->
      let arg = fold arg in
      let ifso = fold ifso in
      let ifnot = fold ifnot in
      begin match M.ifthenelse arg ifso ifnot annot with
        | Data data -> Fifthenelse(arg, ifso, ifnot, data)
        | Node n -> n
      end
    | Fsequence(lam1, lam2, annot) ->
      let lam1 = fold lam1 in
      let lam2 = fold lam2 in
      begin match M.sequence lam1 lam2 annot with
        | Data data -> Fsequence(lam1, lam2, data)
        | Node n -> n
      end
    | Fwhile(cond, body, annot) ->
      let cond = fold cond in
      let body = fold body in
      begin match M.while_ cond body annot with
        | Data data -> Fwhile(cond, body, data)
        | Node n -> n
      end
    | Fsend(kind, met, obj, args, dbg, annot) ->
      let met = fold met in
      let obj = fold obj in
      let args = fold_list args in
      begin match M.send kind met obj args dbg annot with
        | Data data -> Fsend(kind, met, obj, args, dbg, data)
        | Node n -> n
      end
    | Ffor(id, lo, hi, dir, body, annot) ->
      let lo = fold lo in
      let hi = fold hi in
      let body = fold body in
      begin match M.for_ id lo hi dir body annot with
        | Data data -> Ffor(id, lo, hi, dir, body, data)
        | Node n -> n
      end
    | Fassign(id, lam, annot) ->
      let lam = fold lam in
      begin match M.assign id lam annot with
        | Data data -> Fassign(id, lam, data)
        | Node n -> n
      end
    | Fswitch(arg, sw, annot) ->
      let arg = fold arg in
      let sw =
        { sw with
          fs_failaction = Misc.may_map fold sw.fs_failaction;
          fs_consts = List.map (fun (i,v) -> i, fold v) sw.fs_consts;
          fs_blocks = List.map (fun (i,v) -> i, fold v) sw.fs_blocks; } in
      begin match M.switch arg sw annot with
        | Data data -> Fswitch(arg, sw, data)
        | Node n -> n
      end

  and fold_list l = List.map fold l

end

(* simple instanciation of folders *)

module type Setter = sig
  type annot
  type data
  val default : unit -> data
end
module Setter(M:Setter) : Folder with
  type data = M.data and type annot = M.annot =
struct

  type data = M.data
  type annot = M.annot

  let default () = Data (M.default ())

  let var _ data = default ()
  let const _ data = default ()
  let apply ~func ~args ~direct ~dbg data = default ()
  let closure _ _ data = default ()
  let offset _ _ data = default ()
  let env_field _ data = default ()
  let let_ _ _ ~def ~body data = default ()
  let letrec _ _ data = default ()
  let prim _ _ _ data = default ()
  let switch _ _ data = default ()
  let staticfail _ _ data = default ()
  let catch _ _ _ _ data = default ()
  let trywith _ _ _ data = default ()
  let ifthenelse _ _ _ data = default ()
  let sequence _ _ data = default ()
  let while_ _ _ data = default ()
  let for_ _ _ _ _ _ data = default ()
  let assign _ _ data = default ()
  let send _ _ _ _ _ data = default ()
end

module type Identity = sig type annot type data = annot end
module Identity(M:Identity) : Folder with type annot = M.annot and type data = M.data = struct

  type data = M.data
  type annot = M.annot

  let var _ annot = Data annot
  let const _ annot = Data annot
  let apply ~func ~args ~direct ~dbg annot = Data annot
  let closure _ _ annot = Data annot
  let offset _ _ annot = Data annot
  let env_field _ annot = Data annot
  let let_ _ _ ~def ~body annot = Data annot
  let letrec _ _ annot = Data annot
  let prim _ _ _ annot = Data annot
  let switch _ _ annot = Data annot
  let staticfail _ _ annot = Data annot
  let catch _ _ _ _ annot = Data annot
  let trywith _ _ _ annot = Data annot
  let ifthenelse _ _ _ annot = Data annot
  let sequence _ _ annot = Data annot
  let while_ _ _ annot = Data annot
  let for_ _ _ _ _ _ annot = Data annot
  let assign _ _ annot = Data annot
  let send _ _ _ _ _ annot = Data annot

end

module type Merger = sig
  type annot
  type data
  val merge : data list -> annot -> data
end
module Merger(M:Merger) : Folder with
  type annot = M.annot and type data = M.data =
struct

  type data = M.data
  type annot = M.annot

  let merge l annot =
    Data (M.merge (List.map Flambda.data l) annot)

  let var _ annot = Data (M.merge [] annot)
  let const _ annot = Data (M.merge [] annot)
  let apply ~func ~args ~direct ~dbg annot = merge (func::args) annot
  let closure ffunc fv annot =
    let ffuns = List.map (fun (_,ffun) -> ffun.body)
        (IdentMap.bindings ffunc.funs) in
    let fv = List.map (fun (_,v) -> v)
        (IdentMap.bindings fv) in
    merge (ffuns @ fv) annot
  let offset lam _ annot = merge [lam] annot
  let env_field { env } annot = merge [env] annot
  let let_ _ _ ~def ~body annot = merge [def;body] annot
  let letrec defs body annot =
    let defs = List.map snd defs in
    merge (body::defs) annot
  let prim _ args _ annot = merge args annot
  let switch arg sw annot =
    let fail = match sw.fs_failaction with None -> [] | Some v -> [v] in
    let blocks = List.map snd sw.fs_blocks in
    let consts = List.map snd sw.fs_consts in
    let sw = arg::(fail@blocks@consts) in
    merge sw annot
  let staticfail _ args annot = merge args annot
  let catch _ _ body handler annot = merge [body;handler] annot
  let trywith body _ handler annot = merge [body;handler] annot
  let ifthenelse arg ifso ifnot annot = merge [arg;ifso;ifnot] annot
  let sequence lam1 lam2 annot = merge [lam1;lam2] annot
  let while_ cond body annot = merge [cond;body] annot
  let for_ _ lo hi _ body annot = merge [lo;hi;body] annot
  let assign _ lam annot = merge [lam] annot
  let send _ meth obj args _ annot = merge (meth::obj::args) annot

end

let set_tree (type a) (type d) (tree:a flambda) (d:unit -> d) : d flambda =
  let module MySetter = struct
    type annot = a
    type data = d
    let default = d
  end in
  let module F = Fold(Setter(MySetter)) in
  F.fold tree


(* TODO: try to ensure that if the values are equal they should be
   physically equal ? *)
let map f tree =
  let rec aux tree =
    let exp = match tree with
      | Fvar (id,annot) -> tree
      | Fconst (cst,annot) -> tree
      | Fapply (funct, args, direc, dbg, annot) ->
        Fapply (aux funct, List.map aux args, direc, dbg, annot)
      | Fclosure (ffuns, fv, annot) ->
        let ffuns =
          { ffuns with
            funs = IdentMap.map
                (fun ffun -> { ffun with body = aux ffun.body }) ffuns.funs } in
        let fv = IdentMap.map aux fv in
        Fclosure (ffuns, fv, annot)
      | Foffset (flam, off, annot) ->
        Foffset (aux flam, off, annot)
      | Fenv_field (fenv_field, annot) ->
        Fenv_field ({ fenv_field with env = aux fenv_field.env }, annot)
      | Flet(str, id, lam, body, annot) ->
        let lam = aux lam in
        let body = aux body in
        Flet (str, id, lam, body, annot)
      | Fletrec(defs, body, annot) ->
        let defs = List.map (fun (id,lam) -> id,aux lam) defs in
        let body = aux body in
        Fletrec (defs, body, annot)
      | Fprim(p, args, dbg, annot) ->
        let args = List.map aux args in
        Fprim (p, args, dbg, annot)
      | Fstaticfail(i, args, annot) ->
        let args = List.map aux args in
        Fstaticfail (i, args, annot)
      | Fcatch (i, vars, body, handler, annot) ->
        let body = aux body in
        let handler = aux handler in
        Fcatch (i, vars, body, handler, annot)
      | Ftrywith(body, id, handler, annot) ->
        let body = aux body in
        let handler = aux handler in
        Ftrywith(body, id, handler, annot)
      | Fifthenelse(arg, ifso, ifnot, annot) ->
        let arg = aux arg in
        let ifso = aux ifso in
        let ifnot = aux ifnot in
        Fifthenelse(arg, ifso, ifnot, annot)
      | Fsequence(lam1, lam2, annot) ->
        let lam1 = aux lam1 in
        let lam2 = aux lam2 in
        Fsequence(lam1, lam2, annot)
      | Fwhile(cond, body, annot) ->
        let cond = aux cond in
        let body = aux body in
        Fwhile(cond, body, annot)
      | Fsend(kind, met, obj, args, dbg, annot) ->
        let met = aux met in
        let obj = aux obj in
        let args = List.map aux args in
        Fsend(kind, met, obj, args, dbg, annot)
      | Ffor(id, lo, hi, dir, body, annot) ->
        let lo = aux lo in
        let hi = aux hi in
        let body = aux body in
        Ffor(id, lo, hi, dir, body, annot)
      | Fassign(id, lam, annot) ->
        let lam = aux lam in
        Fassign(id, lam, annot)
      | Fswitch(arg, sw, annot) ->
        let arg = aux arg in
        let sw =
          { sw with
            fs_failaction = Misc.may_map aux sw.fs_failaction;
            fs_consts = List.map (fun (i,v) -> i, aux v) sw.fs_consts;
            fs_blocks = List.map (fun (i,v) -> i, aux v) sw.fs_blocks; } in
        Fswitch(arg, sw, annot)
    in
    f exp
  in
  aux tree

(* dead code elimination *)

type used_var = {
  used : IdentSet.t;
  unpure_used : IdentSet.t;
  depend : IdentSet.t IdentMap.t;
  unpure_depend : IdentSet.t IdentMap.t;
}

let empty_used_var = {
  used = IdentSet.empty;
  (* variables used in the context *)
  unpure_used = IdentSet.empty;
  (* variables defined with side effects in the contect *)
  depend = IdentMap.empty;
  (* dependencies between variables *)
  unpure_depend = IdentMap.empty;
  (* dependencies between variables due to side effects in the
     variable definition *)
}

type pure = Pure | Unpure

module UsageMerger = struct
  type annot = pure
  type data = annot * used_var

  let merge_depend m1 m2 =
    let aux _ s1 s2 = match s1,s2 with
      | None, s
      | s, None -> s
      | Some s1, Some s2 -> Some (IdentSet.union s1 s2) in
    IdentMap.merge aux m1 m2

  let merger uv1 uv2 =
    let used = IdentSet.union uv1.used uv2.used in
    let unpure_used = IdentSet.union uv1.unpure_used uv2.unpure_used in
    let depend = merge_depend uv1.depend uv2.depend in
    let unpure_depend = merge_depend uv1.unpure_depend uv2.unpure_depend in
    {used; unpure_used; depend; unpure_depend }

  let merge l annot =
    let usage =
      match l with
      | [] -> empty_used_var
      | (_,t)::q -> List.fold_left merger t (List.map snd q) in
    annot, usage

end
module UsageMergerInst = struct
  include Merger(UsageMerger)
  let var id annot =
    Data(annot, { empty_used_var with used = IdentSet.singleton id })

  let add id def use =
    let def_pure, def_usage = Flambda.data def in
    let unpure_used =
      (* a variable defined with side effects is considered as needed
         for its side effects, hence added to unpure_used *)
      match def_pure with
      | Pure -> use.unpure_used
      | Unpure -> IdentSet.add id use.unpure_used
    in
    let depend = UsageMerger.merge_depend def_usage.depend use.depend in
    let unpure_depend = UsageMerger.merge_depend def_usage.unpure_depend
        use.unpure_depend in
    { used = use.used;
      unpure_used;
      depend = IdentMap.add id def_usage.used depend;
      unpure_depend =
        IdentMap.add id
          (IdentSet.diff def_usage.unpure_used def_usage.used) unpure_depend }

  let let_ _ id ~def ~body annot =
    let _, body_usage = Flambda.data body in
    let use = add id def body_usage in
    Data(annot, use)

  let letrec defs body annot =
    let _, body_usage = Flambda.data body in
    let use =
      List.fold_left (fun use (id,def) -> add id def use)
        body_usage defs in
    Data(annot, use)

end
module UsageAnnotation = Fold(UsageMergerInst)

let reachable_variables t =
  let t = UsageAnnotation.fold t in
  let (_, use) = Flambda.data t in
  let visit_queue = Queue.create () in
  let push_set s = IdentSet.iter
      (fun v -> Queue.add v visit_queue) s in

  let try_find id m =
    try IdentMap.find id m
    with Not_found ->
      IdentSet.empty
      (* if the variable is not bound by a let (like a function
         parameter or a catch), we do not create a dependency *)
  in

  let rec visit visited reachable unpure_reachable =
    if Queue.is_empty visit_queue
    then (reachable, unpure_reachable)
    else
      let id = Queue.take visit_queue in
      if IdentSet.mem id visited
      then visit visited reachable unpure_reachable
      else
        let visited = IdentSet.add id visited in
        let depend = try_find id use.depend in
        let unpure_depend = try_find id use.unpure_depend in
        let reachable = IdentSet.union reachable depend in
        let unpure_reachable = IdentSet.union unpure_reachable
            unpure_depend in
        push_set (IdentSet.diff reachable visited);
        push_set (IdentSet.diff unpure_reachable visited);
        visit visited reachable unpure_reachable
  in
  let reachable = use.used in
  let unpure_reachable = use.unpure_used in
  push_set use.used;
  push_set use.unpure_used;
  let (reachable, unpure_reachable) =
    visit IdentSet.empty reachable unpure_reachable in
  reachable, unpure_reachable

let dead_code_elimination t =
  let reachable, unpure_reachable = reachable_variables t in
  let module Cleaner = struct
    type annot = pure
    type data = pure
  end in
  let module CleanerInst = struct
    include Identity(Cleaner)

    let assign id arg data =
      if IdentSet.mem id reachable
      then Data data
      else
        let unit = Fconst(Fconst_pointer 0,Pure) in
        match Flambda.data arg with
        | Pure -> Node(unit)
        | Unpure -> Node(Fsequence(arg,unit,data))

    let let_ str id ~def ~body annot =
      if IdentSet.mem id reachable
      then Data annot
      else (if IdentSet.mem id unpure_reachable
        then Node (Fsequence(def,body,annot))
        else Node body)

    let letrec defs body annot =
      let aux (kept,kept_for_effect) (id,def) =
        if IdentSet.mem id reachable
        then ((id,def) :: kept,kept_for_effect)
        else (if IdentSet.mem id unpure_reachable
          then (kept,def :: kept_for_effect)
          else (kept,kept_for_effect)) in
      let kept, kept_for_effect = List.fold_left aux ([],[]) defs in
      let kept_body = match kept with
        | [] -> body
        | _ -> Fletrec(kept,body,annot)
      in
      (* There shouldn't be any code executed in the definitions of a
         letrec so if kept_for_effect is not empty, we are seriously
         doing something wrong... *)
      assert(kept_for_effect = []);
      Node kept_body

  end in
  let module Cleaning = Fold(CleanerInst) in
  Cleaning.fold t


(* TODO: clean: copied from flambdaloop to do this quickly now... *)
let pure_prim = function
    Psetglobal _ | Psetfield _ | Psetfloatfield _ | Pduprecord _ |
    Pccall _ | Praise | Poffsetref _ | Pstringsetu | Pstringsets |
    Parraysetu _ | Parraysets _ | Pbigarrayset _ -> false
  | _ -> true (* TODO: exhaustive list *)

let stupid_purity_annotation (type t) t : pure flambda =
  let module Unpure = struct
    type annot = t
    type data = pure
    let merge l _ =
      if List.for_all (fun v -> v = Pure) l
      then Pure
      else Unpure
  end in
  let module UnpureInst = struct
    include Merger(Unpure)

    let apply ~func ~args ~direct ~dbg data = Data Unpure
    let prim p args _ data =
      let d =
        if pure_prim p &&
           List.for_all (fun lam -> Flambda.data lam = Pure) args
        then Pure
        else Unpure
      in
      Data d
    let staticfail _ _ data = Data Unpure
    let assign _ _ data = Data Unpure
    let send _ _ _ _ _ data = Data Unpure

    let closure ffunc fv data =
      let unpure = List.map (fun (_,v) -> Flambda.data v) (IdentMap.bindings fv) in
      Data (Unpure.merge unpure data)

  end in
  let module M = Fold(UnpureInst) in
  M.fold t

let stupid_clean t =
  dead_code_elimination
    (stupid_purity_annotation t)

let reindex (type t) t =
  let module S = struct
    type annot = t
    type data = ExprId.t
    let default () = ExprId.create ()
  end in
  let module F = Fold(Setter(S)) in
  F.fold t

(* variable assignation tracking *)

let assigned_var tree =
  let assign = ref IdentSet.empty in
  let bound = ref IdentSet.empty in
  let aux = function
    | Fassign (id,_,_) ->
      if not (IdentSet.mem id !assign) then assign := IdentSet.add id !assign
    | Flet ( _, id, _, _,_) ->
      bound := IdentSet.add id !bound
    | Fletrec (l, _,_) ->
      List.iter (fun (id,_) -> bound := IdentSet.add id !bound) l
    | _ -> ()
  in
  iter_flambda aux tree;
  IdentSet.diff !assign !bound


let free_variables tree =
  let free = ref IdentSet.empty in
  let bound = ref IdentSet.empty in
  let aux = function
    | Fvar (id,_) ->
      if not (IdentSet.mem id !free) then free := IdentSet.add id !free
    | Ftrywith(_,id,_,_)
    | Ffor(id, _, _, _, _, _)
    | Flet ( _, id, _, _,_) ->
      bound := IdentSet.add id !bound
    | Fletrec (l, _,_) ->
      List.iter (fun (id,_) -> bound := IdentSet.add id !bound) l
    | Fcatch (_,ids,_,_,_) ->
      List.iter (fun id -> bound := IdentSet.add id !bound) ids
    | _ -> ()
  in
  iter_flambda aux tree;
  IdentSet.diff !free !bound




(* Escape analysis *)

(*
type escape_path_element =
  | PField of int
  | PClosureField of Ident.t
  | PTagCase of int

module EscapeElement = struct
  type t = escape_path_element
  let compare a b =
    match a,b with
    | PField i, PField j ->
      compare i j
    | PField _, (PClosureField _|PTagCase _) ->
      -1
    | (PClosureField _|PTagCase _), PField _ ->
      1
    | PClosureField i, PClosureField j ->
      compare i.Ident.stamp j.Ident.stamp
    | PClosureField _, PTagCase _ ->
      -1
    | PTagCase _, PClosureField _ ->
      1
    | PTagCase i, PTagCase j -> compare i j
end

module EscapeElementMap = Map.Make(EscapeElement)
module EEMap = EscapeElementMap

type 'a escape_path_tree =
  { ept_data : 'a;
    ept_children : 'a escape_path_tree EscapeElementMap.t; }

type escape_path = escape_path_element list

(* for the escape path: If ept_children is empty then the path is
   considered to be fully escaping. If it is not then only the marked
   subtree is considered escapting. i.e; there are no meaningless
   parts in the tree. *)

type escape = {
  mutable escaping_path : unit escape_path_tree;
  mutable escaping_dep : escape escape_path_tree;
}

let rec apply_escape_path default f_leaf f_branch f_continue tree = function
  | [] -> f_leaf tree
  | head::tail ->
    if f_continue tree
    then
      let son =
        try EEMap.find head tree.ept_children with
        | Not_found -> default in
      f_branch tree head tail
        (apply_escape_path default f_leaf f_branch f_continue son tail)
    else f_leaf tree

let empty_tree v =
  { ept_data = v; ept_children = EEMap.empty }

let add_escape_path =
  let full_escape = empty_tree () in
  (* if the tree is empty it means that it is fully escaping. Hence we
     do not mark its sons as escaping *)
  let f_continue tree = not (EEMap.is_empty tree.ept_children) in
  let f_branch tree head tail new_branch =
    (* we could avoid allocating when the tree returned did not change *)
    { tree with
      ept_children = EEMap.add head new_branch tree.ept_children } in
  let f_leaf _ = full_escape in
  (fun tree path -> apply_escape_path full_escape f_leaf f_branch f_continue
      tree path)

let rec mark_escaping esc path =
  let escaping_path = add_escape_path esc.escaping_path path in
  (* we mark things as escapting only if the escaping part of esc
     changed. If it didn't change, then whe know that all dependencies
     where already marked.

     We could improve this test by improving add_escape_path such that
     equality implies physical equality *)
  if not (escaping_path = esc.escaping_path)
  then begin
    esc.escaping_path <- escaping_path;
    let mark_queue = Queue.create () in
    let f_continue tree =  not (EEMap.is_empty tree.ept_children) in

  end
*)


  (* (\* ajoute l'information e1 échappe implique e2 échappe *\) *)
(* let rec set_escape_dep e1 e2 = *)
(*   match !e1 with *)
(*   | Escape -> *)
(*     match !e2 with *)
(*     | Escape -> () *)
(*     | Unresolver l -> *)
(*       List.iter *)
