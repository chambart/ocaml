(* open Asttypes *)
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
      IdentMap.iter (fun _ v -> aux v) fv
    | Fletrec (defs, body,_) ->
      List.iter (fun (_,l) -> aux l) defs;
      aux body
    | Fswitch (arg,sw,_) ->
      aux arg;
      List.iter (fun (_,l) -> aux l) sw.fs_consts;
      List.iter (fun (_,l) -> aux l) sw.fs_blocks;
      (match sw.fs_failaction with
       | None -> ()
       | Some f -> aux f)
    | Fsend (_,f1,f2,fl,_,_) ->
      iter_list (f1::f2::fl)

  and iter_list l = List.iter aux l in
  aux t

let iter2_flambda (f: ('env -> 'a flambda -> unit) -> 'env -> 'a flambda -> unit) init_env t =
  let rec aux2 env = function
    | Fvar _
    | Fconst _ -> ()

    | Fassign (_,f1,_)
    | Foffset (f1, _,_)
    | Fenv_field ({env = f1},_) ->
      aux env f1

    | Flet ( _, _, f1, f2,_)
    | Ftrywith (f1,_,f2,_)
    | Fsequence (f1,f2,_)
    | Fwhile (f1,f2,_)
    | Fcatch (_,_,f1,f2,_) ->
      aux env f1; aux env f2;

    | Ffor (_,f1,f2,_,f3,_)
    | Fifthenelse (f1,f2,f3,_) ->
      aux env f1;aux env f2;aux env f3

    | Fstaticfail (_,l,_)
    | Fprim (_,l,_,_) ->
      iter_list env l

    | Fapply (f1,fl,_,_,_) ->
      iter_list env (f1::fl)
    | Fclosure (funcs,fv,_) ->
      IdentMap.iter (fun _ v -> aux env v) fv
    | Fletrec (defs, body,_) ->
      List.iter (fun (_,l) -> aux env l) defs;
      aux env body
    | Fswitch (arg,sw,_) ->
      aux env arg;
      List.iter (fun (_,l) -> aux env l) sw.fs_consts;
      List.iter (fun (_,l) -> aux env l) sw.fs_blocks;
      (match sw.fs_failaction with
       | None -> ()
       | Some f -> aux env f)
    | Fsend (_,f1,f2,fl,_,_) ->
      iter_list env (f1::f2::fl)

  and iter_list env l = List.iter (aux env) l
  and aux env t = f aux2 env t in
  aux init_env t

(* type 'a return = *)
(*   | Data of 'a *)
(*   | Node of 'a flambda *)

(* module type Folder = sig *)
(*   type annot *)
(*   type data *)

(*   val var : Ident.t -> annot -> data return *)
(*   val const : const -> annot -> data return *)
(*   val apply : func:data flambda -> args:data flambda list *)
(*         -> direct:(function_label*closed) option -> dbg:Debuginfo.t *)
(*         -> annot -> data return *)
(*   val closure : data ffunctions -> data flambda IdentMap.t -> annot -> data return *)
(*   val offset : data flambda -> Ident.t -> annot -> data return *)
(*   val env_field : data fenv_field -> annot -> data return *)
(*   val let_ : let_kind -> Ident.t -> def:data flambda -> body:data flambda -> annot -> data return *)
(*   val letrec : (Ident.t * data flambda) list -> data flambda -> annot -> data return *)
(*   val prim : primitive -> data flambda list -> Debuginfo.t -> annot -> data return *)
(*   val switch : data flambda -> data flambda_switch -> annot -> data return *)
(*   val staticfail : int -> data flambda list -> annot -> data return *)
(*   val catch : int -> Ident.t list -> data flambda -> data flambda -> annot -> data return *)
(*   val trywith : data flambda -> Ident.t -> data flambda -> annot -> data return *)
(*   val ifthenelse : data flambda -> data flambda -> data flambda -> annot -> data return *)
(*   val sequence : data flambda -> data flambda -> annot -> data return *)
(*   val while_ : data flambda -> data flambda -> annot -> data return *)
(*   val for_ : Ident.t -> data flambda -> data flambda -> direction_flag -> data flambda -> annot -> data return *)
(*   val assign : Ident.t -> data flambda -> annot -> data return *)
(*   val send : meth_kind -> data flambda -> data flambda -> data flambda list -> Debuginfo.t -> annot -> data return *)

(* end *)

(* module Fold(M:Folder) : *)
(* sig *)
(*   val fold : M.annot flambda -> M.data flambda *)
(* end = struct *)
(*   let rec fold = function *)
(*     | Fvar (id,annot) -> *)
(*       begin match M.var id annot with *)
(*         | Data data -> Fvar (id,data) *)
(*         | Node n -> n *)
(*       end *)
(*     | Fconst (cst,annot) -> *)
(*       begin match M.const cst annot with *)
(*         | Data data -> Fconst (cst,data) *)
(*         | Node n -> n *)
(*       end *)
(*     | Fapply (funct, args, direc, dbg, annot) -> *)
(*       let funct = fold funct in *)
(*       let args = fold_list args in *)
(*       begin match M.apply funct args direc dbg annot with *)
(*         | Data data -> Fapply (funct, args, direc, dbg, data) *)
(*         | Node n -> n *)
(*       end *)
(*     | Fclosure (ffuns, fv, annot) -> *)
(*       let ffuns = *)
(*         { ffuns with *)
(*           funs = IdentMap.map *)
(*               (fun ffun -> { ffun with body = fold ffun.body }) ffuns.funs } in *)
(*       let fv = IdentMap.map fold fv in *)
(*       begin match M.closure ffuns fv annot with *)
(*         | Data data -> Fclosure (ffuns, fv, data) *)
(*         | Node n -> n *)
(*       end *)
(*     | Foffset (flam, off, annot) -> *)
(*       let flam = fold flam in *)
(*       begin match M.offset flam off annot with *)
(*         | Data data -> Foffset (flam, off, data) *)
(*         | Node n -> n *)
(*       end *)
(*     | Fenv_field (fenv_field, annot) -> *)
(*       let flam = fold fenv_field.env in *)
(*       let fenv_field = { fenv_field with env = flam } in *)
(*       begin match M.env_field fenv_field annot with *)
(*         | Data data -> Fenv_field (fenv_field, data) *)
(*         | Node n -> n *)
(*       end *)
(*     | Flet(str, id, lam, body, annot) -> *)
(*       let lam = fold lam in *)
(*       let body = fold body in *)
(*       begin match M.let_ str id lam body annot with *)
(*         | Data data -> Flet (str, id, lam, body, data) *)
(*         | Node n -> n *)
(*       end *)
(*     | Fletrec(defs, body, annot) -> *)
(*       let defs = List.map (fun (id,lam) -> id,fold lam) defs in *)
(*       let body = fold body in *)
(*       begin match M.letrec defs body annot with *)
(*         | Data data -> Fletrec (defs, body, data) *)
(*         | Node n -> n *)
(*       end *)
(*     | Fprim(p, args, dbg, annot) -> *)
(*       let args = fold_list args in *)
(*       begin match M.prim p args dbg annot with *)
(*         | Data data -> Fprim (p, args, dbg, data) *)
(*         | Node n -> n *)
(*       end *)
(*     | Fstaticfail(i, args, annot) -> *)
(*       let args = fold_list args in *)
(*       begin match M.staticfail i args annot with *)
(*         | Data data -> Fstaticfail (i, args, data) *)
(*         | Node n -> n *)
(*       end *)
(*     | Fcatch (i, vars, body, handler, annot) -> *)
(*       let body = fold body in *)
(*       let handler = fold handler in *)
(*       begin match M.catch i vars body handler annot with *)
(*         | Data data -> Fcatch (i, vars, body, handler, data) *)
(*         | Node n -> n *)
(*       end *)
(*     | Ftrywith(body, id, handler, annot) -> *)
(*       let body = fold body in *)
(*       let handler = fold handler in *)
(*       begin match M.trywith body id handler annot with *)
(*         | Data data -> Ftrywith(body, id, handler, data) *)
(*         | Node n -> n *)
(*       end *)
(*     | Fifthenelse(arg, ifso, ifnot, annot) -> *)
(*       let arg = fold arg in *)
(*       let ifso = fold ifso in *)
(*       let ifnot = fold ifnot in *)
(*       begin match M.ifthenelse arg ifso ifnot annot with *)
(*         | Data data -> Fifthenelse(arg, ifso, ifnot, data) *)
(*         | Node n -> n *)
(*       end *)
(*     | Fsequence(lam1, lam2, annot) -> *)
(*       let lam1 = fold lam1 in *)
(*       let lam2 = fold lam2 in *)
(*       begin match M.sequence lam1 lam2 annot with *)
(*         | Data data -> Fsequence(lam1, lam2, data) *)
(*         | Node n -> n *)
(*       end *)
(*     | Fwhile(cond, body, annot) -> *)
(*       let cond = fold cond in *)
(*       let body = fold body in *)
(*       begin match M.while_ cond body annot with *)
(*         | Data data -> Fwhile(cond, body, data) *)
(*         | Node n -> n *)
(*       end *)
(*     | Fsend(kind, met, obj, args, dbg, annot) -> *)
(*       let met = fold met in *)
(*       let obj = fold obj in *)
(*       let args = fold_list args in *)
(*       begin match M.send kind met obj args dbg annot with *)
(*         | Data data -> Fsend(kind, met, obj, args, dbg, data) *)
(*         | Node n -> n *)
(*       end *)
(*     | Ffor(id, lo, hi, dir, body, annot) -> *)
(*       let lo = fold lo in *)
(*       let hi = fold hi in *)
(*       let body = fold body in *)
(*       begin match M.for_ id lo hi dir body annot with *)
(*         | Data data -> Ffor(id, lo, hi, dir, body, data) *)
(*         | Node n -> n *)
(*       end *)
(*     | Fassign(id, lam, annot) -> *)
(*       let lam = fold lam in *)
(*       begin match M.assign id lam annot with *)
(*         | Data data -> Fassign(id, lam, data) *)
(*         | Node n -> n *)
(*       end *)
(*     | Fswitch(arg, sw, annot) -> *)
(*       let arg = fold arg in *)
(*       let sw = *)
(*         { sw with *)
(*           fs_failaction = Misc.may_map fold sw.fs_failaction; *)
(*           fs_consts = List.map (fun (i,v) -> i, fold v) sw.fs_consts; *)
(*           fs_blocks = List.map (fun (i,v) -> i, fold v) sw.fs_blocks; } in *)
(*       begin match M.switch arg sw annot with *)
(*         | Data data -> Fswitch(arg, sw, data) *)
(*         | Node n -> n *)
(*       end *)

(*   and fold_list l = List.map fold l *)

(* end *)

(* (\* simple instanciation of folders *\) *)

(* module type Setter = sig *)
(*   type annot *)
(*   type data *)
(*   val default : unit -> data *)
(* end *)
(* module Setter(M:Setter) : Folder with *)
(*   type data = M.data and type annot = M.annot = *)
(* struct *)

(*   type data = M.data *)
(*   type annot = M.annot *)

(*   let default () = Data (M.default ()) *)

(*   let var _ data = default () *)
(*   let const _ data = default () *)
(*   let apply ~func ~args ~direct ~dbg data = default () *)
(*   let closure _ _ data = default () *)
(*   let offset _ _ data = default () *)
(*   let env_field _ data = default () *)
(*   let let_ _ _ ~def ~body data = default () *)
(*   let letrec _ _ data = default () *)
(*   let prim _ _ _ data = default () *)
(*   let switch _ _ data = default () *)
(*   let staticfail _ _ data = default () *)
(*   let catch _ _ _ _ data = default () *)
(*   let trywith _ _ _ data = default () *)
(*   let ifthenelse _ _ _ data = default () *)
(*   let sequence _ _ data = default () *)
(*   let while_ _ _ data = default () *)
(*   let for_ _ _ _ _ _ data = default () *)
(*   let assign _ _ data = default () *)
(*   let send _ _ _ _ _ data = default () *)
(* end *)

(* module type Identity = sig type annot type data = annot end *)
(* module Identity(M:Identity) : Folder with type annot = M.annot and type data = M.data = struct *)

(*   type data = M.data *)
(*   type annot = M.annot *)

(*   let var _ annot = Data annot *)
(*   let const _ annot = Data annot *)
(*   let apply ~func ~args ~direct ~dbg annot = Data annot *)
(*   let closure _ _ annot = Data annot *)
(*   let offset _ _ annot = Data annot *)
(*   let env_field _ annot = Data annot *)
(*   let let_ _ _ ~def ~body annot = Data annot *)
(*   let letrec _ _ annot = Data annot *)
(*   let prim _ _ _ annot = Data annot *)
(*   let switch _ _ annot = Data annot *)
(*   let staticfail _ _ annot = Data annot *)
(*   let catch _ _ _ _ annot = Data annot *)
(*   let trywith _ _ _ annot = Data annot *)
(*   let ifthenelse _ _ _ annot = Data annot *)
(*   let sequence _ _ annot = Data annot *)
(*   let while_ _ _ annot = Data annot *)
(*   let for_ _ _ _ _ _ annot = Data annot *)
(*   let assign _ _ annot = Data annot *)
(*   let send _ _ _ _ _ annot = Data annot *)

(* end *)

(* module type Merger = sig *)
(*   type annot *)
(*   type data *)
(*   val merge : data list -> annot -> data *)
(* end *)
(* module Merger(M:Merger) : Folder with *)
(*   type annot = M.annot and type data = M.data = *)
(* struct *)

(*   type data = M.data *)
(*   type annot = M.annot *)

(*   let merge l annot = *)
(*     Data (M.merge (List.map Flambda.data l) annot) *)

(*   let var _ annot = Data (M.merge [] annot) *)
(*   let const _ annot = Data (M.merge [] annot) *)
(*   let apply ~func ~args ~direct ~dbg annot = merge (func::args) annot *)
(*   let closure ffunc fv annot = *)
(*     let ffuns = List.map (fun (_,ffun) -> ffun.body) *)
(*         (IdentMap.bindings ffunc.funs) in *)
(*     let fv = List.map (fun (_,v) -> v) *)
(*         (IdentMap.bindings fv) in *)
(*     merge (ffuns @ fv) annot *)
(*   let offset lam _ annot = merge [lam] annot *)
(*   let env_field { env } annot = merge [env] annot *)
(*   let let_ _ _ ~def ~body annot = merge [def;body] annot *)
(*   let letrec defs body annot = *)
(*     let defs = List.map snd defs in *)
(*     merge (body::defs) annot *)
(*   let prim _ args _ annot = merge args annot *)
(*   let switch arg sw annot = *)
(*     let fail = match sw.fs_failaction with None -> [] | Some v -> [v] in *)
(*     let blocks = List.map snd sw.fs_blocks in *)
(*     let consts = List.map snd sw.fs_consts in *)
(*     let sw = arg::(fail@blocks@consts) in *)
(*     merge sw annot *)
(*   let staticfail _ args annot = merge args annot *)
(*   let catch _ _ body handler annot = merge [body;handler] annot *)
(*   let trywith body _ handler annot = merge [body;handler] annot *)
(*   let ifthenelse arg ifso ifnot annot = merge [arg;ifso;ifnot] annot *)
(*   let sequence lam1 lam2 annot = merge [lam1;lam2] annot *)
(*   let while_ cond body annot = merge [cond;body] annot *)
(*   let for_ _ lo hi _ body annot = merge [lo;hi;body] annot *)
(*   let assign _ lam annot = merge [lam] annot *)
(*   let send _ meth obj args _ annot = merge (meth::obj::args) annot *)

(* end *)

(* let set_tree (type a) (type d) (tree:a flambda) (d:unit -> d) : d flambda = *)
(*   let module MySetter = struct *)
(*     type annot = a *)
(*     type data = d *)
(*     let default = d *)
(*   end in *)
(*   let module F = Fold(Setter(MySetter)) in *)
(*   F.fold tree *)

(* TODO: try to ensure that if the values are equal they should be
   physically equal ? *)
let map_no_closure f tree =
  let rec aux tree =
    let exp = match tree with
      | Fvar (id,annot) -> tree
      | Fconst (cst,annot) -> tree
      | Fapply (funct, args, direc, dbg, annot) ->
        Fapply (aux funct, List.map aux args, direc, dbg, annot)
      | Fclosure (ffuns, fv, annot) ->
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

let map2 :
  (('env -> 'a Flambda.flambda -> 'a Flambda.flambda) ->
   'env -> 'a Flambda.flambda -> 'a Flambda.flambda) ->
  'env -> 'a Flambda.flambda -> 'a Flambda.flambda = fun f env tree ->
  let rec aux1 env tree = match tree with
    | Fvar (id,annot) -> tree
    | Fconst (cst,annot) -> tree
    | Fapply (funct, args, direc, dbg, annot) ->
      Fapply (aux env funct, List.map (aux env) args, direc, dbg, annot)
    | Fclosure (ffuns, fv, annot) ->
      let fv = IdentMap.map (aux env) fv in
      Fclosure (ffuns, fv, annot)
    | Foffset (flam, off, annot) ->
      Foffset (aux env flam, off, annot)
    | Fenv_field (fenv_field, annot) ->
      Fenv_field ({ fenv_field with env = aux env fenv_field.env }, annot)
    | Flet(str, id, lam, body, annot) ->
      let lam = aux env lam in
      let body = aux env body in
      Flet (str, id, lam, body, annot)
    | Fletrec(defs, body, annot) ->
      let defs = List.map (fun (id,lam) -> id,aux env lam) defs in
      let body = aux env body in
      Fletrec (defs, body, annot)
    | Fprim(p, args, dbg, annot) ->
      let args = List.map (aux env) args in
      Fprim (p, args, dbg, annot)
    | Fstaticfail(i, args, annot) ->
      let args = List.map (aux env) args in
      Fstaticfail (i, args, annot)
    | Fcatch (i, vars, body, handler, annot) ->
      let body = aux env body in
      let handler = aux env handler in
      Fcatch (i, vars, body, handler, annot)
    | Ftrywith(body, id, handler, annot) ->
      let body = aux env body in
      let handler = aux env handler in
      Ftrywith(body, id, handler, annot)
    | Fifthenelse(arg, ifso, ifnot, annot) ->
      let arg = aux env arg in
      let ifso = aux env ifso in
      let ifnot = aux env ifnot in
      Fifthenelse(arg, ifso, ifnot, annot)
    | Fsequence(lam1, lam2, annot) ->
      let lam1 = aux env lam1 in
      let lam2 = aux env lam2 in
      Fsequence(lam1, lam2, annot)
    | Fwhile(cond, body, annot) ->
      let cond = aux env cond in
      let body = aux env body in
      Fwhile(cond, body, annot)
    | Fsend(kind, met, obj, args, dbg, annot) ->
      let met = aux env met in
      let obj = aux env obj in
      let args = List.map (aux env) args in
      Fsend(kind, met, obj, args, dbg, annot)
    | Ffor(id, lo, hi, dir, body, annot) ->
      let lo = aux env lo in
      let hi = aux env hi in
      let body = aux env body in
      Ffor(id, lo, hi, dir, body, annot)
    | Fassign(id, lam, annot) ->
      let lam = aux env lam in
      Fassign(id, lam, annot)
    | Fswitch(arg, sw, annot) ->
      let arg = aux env arg in
      let sw =
        { sw with
          fs_failaction = Misc.may_map (aux env) sw.fs_failaction;
          fs_consts = List.map (fun (i,v) -> i, aux env v) sw.fs_consts;
          fs_blocks = List.map (fun (i,v) -> i, aux env v) sw.fs_blocks; } in
      Fswitch(arg, sw, annot)
  and aux env tree =
    f aux1 env tree
  in
  aux env tree

(****** ANF transformation *******)

type 'a binds =
  | Simple of let_kind * Ident.t * 'a flambda
  | Rec of (Ident.t * 'a flambda) list

let rec decompose binds = function
  | Fvar _ as expr -> binds, expr
  | Fconst _ as expr -> binds, expr
  | Flet (kind, id, lam, body, _) ->
    let binds_lam, elam = decompose binds lam in
    let binds = Simple (kind,id,elam) :: binds_lam in
    decompose binds body
  | Fletrec (defs, body, _) ->
    let defs = List.map (fun (id, lam) -> id, decompose_recompose lam) defs in
    let binds = Rec defs :: binds in
    decompose binds body
  | Fprim(Psequand | Psequor as prim, [arg1; arg2], dbg, eid) ->
    (* seqand and seqor have a different evaluation order,
       to keep it correct: convert to ifthenelse *)
    let binds, arg1 = decompose_tovar binds arg1 in
    let ifso, ifnot = match prim with
      | Psequand -> anf arg2, Fconst (Fconst_pointer 0, ExprId.create ())
      | _ -> Fconst (Fconst_pointer 1, ExprId.create ()), anf arg2 in
    binds, Fifthenelse(arg1, ifso, ifnot, eid)
  | Fprim(prim, args, dbg, eid) ->
    let binds, args = decompose_tovar_list binds args in
    binds, Fprim(prim, args, dbg, eid)
  | Fsequence(e1,e2,eid) ->
    let binds = ignore_val binds e1 in
    let binds, ex2 = decompose_tovar binds e2 in
    binds, ex2
  | Fapply (func, args, direct, dbg, eid) ->
    let binds, func = decompose_tovar binds func in
    let binds, args = decompose_tovar_list binds args in
    binds, Fapply (func, args, direct, dbg, eid)
  | Fclosure (ffuns, fv, eid) ->
    let binds, fv = decompose_tovar_idmap binds fv in
    let ffuns =
      { ffuns with
        funs = IdentMap.map
            (fun ffun -> { ffun with body = anf ffun.body }) ffuns.funs } in
    binds, Fclosure (ffuns, fv, eid)
  | Foffset(expr, id, eid) ->
    let binds, expr = decompose_tovar binds expr in
    binds, Foffset(expr, id, eid)
  | Fenv_field ({ env; env_fun_id; env_var }, eid) ->
    let binds, env = decompose_tovar binds env in
    binds, Fenv_field ({ env; env_fun_id; env_var }, eid)
  | Fstaticfail (i, args, eid) ->
    let binds, args = decompose_tovar_list binds args in
    binds, Fstaticfail (i, args, eid)
  | Fifthenelse (econd, eso, enot, eid) ->
    let binds, econd = decompose_tovar binds econd in
    binds, Fifthenelse (econd, anf eso, anf enot, eid)
  | Fswitch (econd, sw, eid) ->
    let binds, econd = decompose_tovar binds econd in
    let sw = { sw with
               fs_consts = List.map (fun (i,e) -> i, anf e) sw.fs_consts;
               fs_blocks = List.map (fun (i,e) -> i, anf e) sw.fs_blocks;
               fs_failaction = Misc.may_map anf sw.fs_failaction } in
    binds, Fswitch (econd, sw, eid)
  | Fcatch (i, ids, body, handler, eid) ->
    binds, Fcatch (i, ids, anf body, anf handler, eid)
  | Ftrywith (body, id, handler, eid) ->
    binds, Ftrywith (anf body, id, anf handler, eid)
  | Fwhile (cond, body, eid) ->
    binds, Fwhile (anf cond, anf body, eid)
  | Ffor (id, lo_expr, hi_expr, dir, body, eid) ->
    let binds, lo_expr = decompose_tovar binds lo_expr in
    let binds, hi_expr = decompose_tovar binds hi_expr in
    binds, Ffor (id, lo_expr, hi_expr, dir, anf body, eid)
  | Fassign (id, expr, eid) ->
    let binds, expr = decompose_tovar binds expr in
    binds, Fassign (id, expr, eid)
  | Fsend (kind, meth, obj, args, dbg, eid) ->
    (* TODO: check evaluation order ! *)
    let binds, meth = decompose_tovar binds meth in
    let binds, obj = decompose_tovar binds obj in
    let binds, args = decompose_tovar_list binds args in
    binds, Fsend (kind, meth, obj, args, dbg, eid)

and tovar ?(id="anf") binds = function
  | (Fvar _ | Fconst _) as expr -> binds, expr
  | expr ->
    let id = Ident.create id in
    Simple(Strict, id, expr) :: binds, Fvar(id, ExprId.create ())

and ignore_val binds expr =
  let binds, _ = decompose_tovar ~id:"ignore" binds expr in
  binds

and simple_expr_tovar ?id binds expr = match expr with
  (* control expression are kept as is, var and const also, everything
     else is converted to var *)
  | Fswitch _ | Fstaticfail _ | Fcatch _ | Ftrywith _
  | Fifthenelse _ | Fwhile _ | Ffor _
  | Fprim (Praise, _,_,_) -> binds, expr
  | _ -> tovar ?id binds expr

and decompose_tovar ?id binds expr =
  let binds, expr = decompose binds expr in
  tovar ?id binds expr

and decompose_tovar_list binds l =
  let aux expr (binds,exprs) =
    let binds, expr = decompose_tovar binds expr in
    binds, expr :: exprs
  in
  List.fold_right aux l (binds, [])

and decompose_tovar_idmap binds m =
  let aux id expr (binds,exprs) =
    let binds, expr = decompose_tovar ~id:(Ident.name id) binds expr in
    binds, IdentMap.add id expr exprs
  in
  IdentMap.fold aux m (binds, IdentMap.empty)

and recompose binds body =
  let aux body = function
    | Simple (kind, id, lam) ->
      Flet (kind, id, lam, body, ExprId.create ())
    | Rec defs ->
      Fletrec (defs, body, ExprId.create ())
  in
  List.fold_left aux body binds

and decompose_recompose expr =
  let binds, body = decompose [] expr in
  recompose binds body

and anf expr =
  let binds, body = decompose [] expr in
  let binds, body = simple_expr_tovar binds body in
  recompose binds body

(* let rec anf expr = *)
(*   let r = ref [] in *)
(*   let add_lets defs body = *)
(*     List.fold_left (fun body (kind,id,lam) -> *)
(*         Flet(kind,id,lam,body,ExprId.create ())) body defs in *)
(*   let mapper iter tree : ExprId.t flambda = match tree with *)
(*     | Fvar (_,_) -> tree *)
(*     | Fsequence(e1,e2,eid) -> *)
(*       Fsequence(anf e1, anf e2, eid) *)
(*     (\* | Fifthenelse(econd, eso, enot, eid) -> *\) *)
(*     (\*   let saved = !r in *\) *)
(*     (\*   r := []; *\) *)
(*     (\*   let cond = iter econd in *\) *)
(*     (\*   let added = !r in *\) *)
(*     (\*   let v = Ident.create "anf" in *\) *)
(*     (\*   let expr = *\) *)
(*     (\*     Fifthenelse(Fvar(v,ExprId.create ()), anf eso, anf enot, eid) in *\) *)
(*     (\*   let ret = add_lets added expr in *\) *)
(*     (\*   r := saved; *\) *)
(*     (\*   ret *\) *)

(*     | Flet (kind,id,lam,body,eid) -> *)
(*       r := (kind,id,anf lam) :: !r; *)
(*       iter body *)
(*     | _ -> *)
(*       let saved = !r in *)
(*       r := []; *)
(*       let tree = iter tree in *)
(*       let added = !r in *)
(*       let ret = add_lets added tree in *)
(*       let v = Ident.create "anf" in *)
(*       r := (Strict,v,ret) :: saved; *)
(*       Fvar (v, ExprId.create ()) *)
(*   in *)
(*   let res = map2 mapper expr in *)
(*   add_lets !r res *)

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

(* type pure = Pure | Unpure *)

(* module UsageMerger = struct *)
(*   type annot = pure *)
(*   type data = annot * used_var *)

(*   let merge_depend m1 m2 = *)
(*     let aux _ s1 s2 = match s1,s2 with *)
(*       | None, s *)
(*       | s, None -> s *)
(*       | Some s1, Some s2 -> Some (IdentSet.union s1 s2) in *)
(*     IdentMap.merge aux m1 m2 *)

(*   let merger uv1 uv2 = *)
(*     let used = IdentSet.union uv1.used uv2.used in *)
(*     let unpure_used = IdentSet.union uv1.unpure_used uv2.unpure_used in *)
(*     let depend = merge_depend uv1.depend uv2.depend in *)
(*     let unpure_depend = merge_depend uv1.unpure_depend uv2.unpure_depend in *)
(*     {used; unpure_used; depend; unpure_depend } *)

(*   let merge l annot = *)
(*     let usage = *)
(*       match l with *)
(*       | [] -> empty_used_var *)
(*       | (_,t)::q -> List.fold_left merger t (List.map snd q) in *)
(*     annot, usage *)

(* end *)
(* module UsageMergerInst = struct *)
(*   include Merger(UsageMerger) *)
(*   let var id annot = *)
(*     Data(annot, { empty_used_var with used = IdentSet.singleton id }) *)

(*   let add id def use = *)
(*     let def_pure, def_usage = Flambda.data def in *)
(*     let unpure_used = *)
(*       (\* a variable defined with side effects is considered as needed *)
(*          for its side effects, hence added to unpure_used *\) *)
(*       match def_pure with *)
(*       | Pure -> use.unpure_used *)
(*       | Unpure -> IdentSet.add id use.unpure_used *)
(*     in *)
(*     let depend = UsageMerger.merge_depend def_usage.depend use.depend in *)
(*     let unpure_depend = UsageMerger.merge_depend def_usage.unpure_depend *)
(*         use.unpure_depend in *)
(*     { used = use.used; *)
(*       unpure_used; *)
(*       depend = IdentMap.add id def_usage.used depend; *)
(*       unpure_depend = *)
(*         IdentMap.add id *)
(*           (IdentSet.diff def_usage.unpure_used def_usage.used) unpure_depend } *)

(*   let let_ _ id ~def ~body annot = *)
(*     let _, body_usage = Flambda.data body in *)
(*     let use = add id def body_usage in *)
(*     Data(annot, use) *)

(*   let letrec defs body annot = *)
(*     let _, body_usage = Flambda.data body in *)
(*     let use = *)
(*       List.fold_left (fun use (id,def) -> add id def use) *)
(*         body_usage defs in *)
(*     Data(annot, use) *)

(* end *)
(* module UsageAnnotation = Fold(UsageMergerInst) *)

(* let reachable_variables t = *)
(*   let t = UsageAnnotation.fold t in *)
(*   let (_, use) = Flambda.data t in *)
(*   let visit_queue = Queue.create () in *)
(*   let push_set s = IdentSet.iter *)
(*       (fun v -> Queue.add v visit_queue) s in *)

(*   let try_find id m = *)
(*     try IdentMap.find id m *)
(*     with Not_found -> *)
(*       IdentSet.empty *)
(*       (\* if the variable is not bound by a let (like a function *)
(*          parameter or a catch), we do not create a dependency *\) *)
(*   in *)

(*   let rec visit visited reachable unpure_reachable = *)
(*     if Queue.is_empty visit_queue *)
(*     then (reachable, unpure_reachable) *)
(*     else *)
(*       let id = Queue.take visit_queue in *)
(*       if IdentSet.mem id visited *)
(*       then visit visited reachable unpure_reachable *)
(*       else *)
(*         let visited = IdentSet.add id visited in *)
(*         let depend = try_find id use.depend in *)
(*         let unpure_depend = try_find id use.unpure_depend in *)
(*         let reachable = IdentSet.union reachable depend in *)
(*         let unpure_reachable = IdentSet.union unpure_reachable *)
(*             unpure_depend in *)
(*         push_set (IdentSet.diff reachable visited); *)
(*         push_set (IdentSet.diff unpure_reachable visited); *)
(*         visit visited reachable unpure_reachable *)
(*   in *)
(*   let reachable = use.used in *)
(*   let unpure_reachable = use.unpure_used in *)
(*   push_set use.used; *)
(*   push_set use.unpure_used; *)
(*   let (reachable, unpure_reachable) = *)
(*     visit IdentSet.empty reachable unpure_reachable in *)
(*   reachable, unpure_reachable *)

(* let dead_code_elimination t = *)
(*   let reachable, unpure_reachable = reachable_variables t in *)
(*   let module Cleaner = struct *)
(*     type annot = pure *)
(*     type data = pure *)
(*   end in *)
(*   let module CleanerInst = struct *)
(*     include Identity(Cleaner) *)

(*     let assign id arg data = *)
(*       if IdentSet.mem id reachable *)
(*       then Data data *)
(*       else *)
(*         let unit = Fconst(Fconst_pointer 0,Pure) in *)
(*         match Flambda.data arg with *)
(*         | Pure -> Node(unit) *)
(*         | Unpure -> Node(Fsequence(arg,unit,data)) *)

(*     let let_ str id ~def ~body annot = *)
(*       if IdentSet.mem id reachable *)
(*       then Data annot *)
(*       else (if IdentSet.mem id unpure_reachable *)
(*         then Node (Fsequence(def,body,annot)) *)
(*         else Node body) *)

(*     let letrec defs body annot = *)
(*       let aux (kept,kept_for_effect) (id,def) = *)
(*         if IdentSet.mem id reachable *)
(*         then ((id,def) :: kept,kept_for_effect) *)
(*         else (if IdentSet.mem id unpure_reachable *)
(*           then (kept,def :: kept_for_effect) *)
(*           else (kept,kept_for_effect)) in *)
(*       let kept, kept_for_effect = List.fold_left aux ([],[]) defs in *)
(*       let kept_body = match kept with *)
(*         | [] -> body *)
(*         | _ -> Fletrec(List.rev kept,body,annot) *)
(*       in *)
(*       (\* There shouldn't be any code executed in the definitions of a *)
(*          letrec so if kept_for_effect is not empty, we are seriously *)
(*          doing something wrong... *\) *)
(*       assert(kept_for_effect = []); *)
(*       Node kept_body *)

(*   end in *)
(*   let module Cleaning = Fold(CleanerInst) in *)
(*   Cleaning.fold t *)


(* TODO: clean: copied from flambdaloop to do this quickly now... *)
(* let pure_prim = function *)
(*     Psetglobal _ | Psetfield _ | Psetfloatfield _ | Pduprecord _ | *)
(*     Pccall _ | Praise | Poffsetref _ | Pstringsetu | Pstringsets | *)
(*     Parraysetu _ | Parraysets _ | Pbigarrayset _ -> false *)
(*   | _ -> true (\* TODO: exhaustive list *\) *)

(* let stupid_purity_annotation (type t) t : pure flambda = *)
(*   let module Unpure = struct *)
(*     type annot = t *)
(*     type data = pure *)
(*     let merge l _ = *)
(*       if List.for_all (fun v -> v = Pure) l *)
(*       then Pure *)
(*       else Unpure *)
(*   end in *)
(*   let module UnpureInst = struct *)
(*     include Merger(Unpure) *)

(*     let apply ~func ~args ~direct ~dbg data = Data Unpure *)
(*     let prim p args _ data = *)
(*       let d = *)
(*         if pure_prim p && *)
(*            List.for_all (fun lam -> Flambda.data lam = Pure) args *)
(*         then Pure *)
(*         else Unpure *)
(*       in *)
(*       Data d *)
(*     let staticfail _ _ data = Data Unpure *)
(*     let assign _ _ data = Data Unpure *)
(*     let send _ _ _ _ _ data = Data Unpure *)

(*     let closure ffunc fv data = *)
(*       let unpure = List.map (fun (_,v) -> Flambda.data v) (IdentMap.bindings fv) in *)
(*       Data (Unpure.merge unpure data) *)

(*   end in *)
(*   let module M = Fold(UnpureInst) in *)
(*   M.fold t *)

(* let stupid_clean t = *)
(*   dead_code_elimination *)
(*     (stupid_purity_annotation t) *)

(* let reindex (type t) (tree:t flambda) = *)
(*   let eid () = ExprId.create () in *)
(*   let mapper (type t) : (t flambda) -> 'a = function *)
(*     | Fvar (id,_) -> Fvar (id,eid ()) *)
(*     | Fconst (cst,_) -> Fconst (cst,eid ()) *)
(*     | Flet(str, id, lam, body,_) -> Flet(str, id, lam, body,eid ()) *)
(*     | Fletrec(defs, body,_) -> Fletrec(defs, body,eid ()) *)
(*     | Fclosure(funct, fv,_) -> Fclosure(funct, fv,eid ()) *)
(*     | Foffset(lam,id,_) -> Foffset(lam,id,eid ()) *)
(*     | Fenv_field(env,_) -> Fenv_field(env,eid ()) *)
(*     | Fapply(funct, args, direct, dbg, _) -> *)
(*       Fapply(funct, args, direct, dbg, eid ()) *)
(*     | Fswitch(arg, sw,_) -> Fswitch(arg, sw,eid ()) *)
(*     | Fsend(kind, met, obj, args, dbg, _) -> *)
(*       Fsend(kind, met, obj, args, dbg,eid ()) *)
(*     | Fprim(prim, args, dbg, _) -> Fprim(prim, args, dbg, eid ()) *)
(*     | Fstaticfail (i, args,_) -> Fstaticfail (i, args,eid ()) *)
(*     | Fcatch (i, vars, body, handler,_) -> Fcatch (i, vars, body, handler,eid ()) *)
(*     | Ftrywith(body, id, handler,_) -> Ftrywith(body, id, handler,eid ()) *)
(*     | Fifthenelse(arg, ifso, ifnot,_) -> Fifthenelse(arg, ifso, ifnot,eid ()) *)
(*     | Fsequence(lam1, lam2,_) -> Fsequence(lam1, lam2,eid ()) *)
(*     | Fwhile(cond, body,_) -> Fwhile(cond, body,eid ()) *)
(*     | Ffor(id, lo, hi, dir, body,_) -> Ffor(id, lo, hi, dir, body,eid ()) *)
(*     | Fassign(id, lam,_) -> Fassign(id, lam,eid ()) *)
(*   in *)
(*   map mapper tree *)

let reindex tree =
  let eid prev_id =
    let name = ExprId.name prev_id in
    ExprId.create ?name () in
  let mapper = function
    | Fvar (id,pid) -> Fvar (id,eid pid)
    | Fconst (cst,pid) -> Fconst (cst,eid pid)
    | Flet(str, id, lam, body,pid) -> Flet(str, id, lam, body,eid pid)
    | Fletrec(defs, body,pid) -> Fletrec(defs, body,eid pid)
    | Fclosure(funct, fv,pid) -> Fclosure(funct, fv,eid pid)
    | Foffset(lam,id,pid) -> Foffset(lam,id,eid pid)
    | Fenv_field(env,pid) -> Fenv_field(env,eid pid)
    | Fapply(funct, args, direct, dbg, pid) -> Fapply(funct, args, direct, dbg, eid pid)
    | Fswitch(arg, sw,pid) -> Fswitch(arg, sw,eid pid)
    | Fsend(kind, met, obj, args, dbg, pid) -> Fsend(kind, met, obj, args, dbg,eid pid)
    | Fprim(prim, args, dbg, pid) -> Fprim(prim, args, dbg, eid pid)
    | Fstaticfail (i, args,pid) -> Fstaticfail (i, args,eid pid)
    | Fcatch (i, vars, body, handler,pid) -> Fcatch (i, vars, body, handler,eid pid)
    | Ftrywith(body, id, handler,pid) -> Ftrywith(body, id, handler,eid pid)
    | Fifthenelse(arg, ifso, ifnot,pid) -> Fifthenelse(arg, ifso, ifnot,eid pid)
    | Fsequence(lam1, lam2,pid) -> Fsequence(lam1, lam2,eid pid)
    | Fwhile(cond, body,pid) -> Fwhile(cond, body,eid pid)
    | Ffor(id, lo, hi, dir, body,pid) -> Ffor(id, lo, hi, dir, body,eid pid)
    | Fassign(id, lam,pid) -> Fassign(id, lam,eid pid)
  in
  map mapper tree

(* let reindex (type t) t = *)
(*   let module S = struct *)
(*     type annot = t *)
(*     type data = ExprId.t *)
(*     let default () = ExprId.create () *)
(*   end in *)
(*   let module F = Fold(Setter(S)) in *)
(*   F.fold t *)

(* let reindex' t = *)
(*   let module S = struct *)
(*     type annot = ExprId.t *)
(*     type data = ExprId.t *)
(*     let merge _ prev_id = *)
(*       let name = ExprId.name prev_id in *)
(*       ExprId.create ?name () *)
(*   end in *)
(*   let module F = Fold(Merger(S)) in *)
(*   F.fold t *)

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

