(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                     Pierre Chambart, OCamlPro                       *)
(*                                                                     *)
(*  Copyright 2013 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Flambda

let iter ~f t =
  let rec aux t =
    f t;
    match t with
    | Fsymbol _
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
      IdentMap.iter (fun _ v -> aux v) fv;
      IdentMap.iter (fun _ ffun -> aux ffun.body) funcs.funs

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
    | Funreachable _ -> ()

  and iter_list l = List.iter aux l in
  aux t

let iter_toplevel ~f t =
  let rec aux t =
    f t;
    match t with
    | Fsymbol _
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
    | Funreachable _ -> ()

  and iter_list l = List.iter aux l in
  aux t

let map f tree =
  let rec aux tree =
    let exp = match tree with
      | Fsymbol _ -> tree
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
      | Funreachable _ -> tree
    in
    f exp
  in
  aux tree

let map_data (type t1) (type t2) (f:t1 -> t2) (tree:t1 flambda) : t2 flambda =
  let rec mapper : t1 flambda -> t2 flambda = function
    | Fsymbol (sym, v) -> Fsymbol (sym, f v)
    | Fvar (id, v) -> Fvar (id, f v)
    | Fconst (cst, v) -> Fconst (cst, f v)
    | Flet(str, id, lam, body, v) ->
      Flet(str, id, mapper lam, mapper body, f v)
    | Fletrec(defs, body, v) ->
      let defs = List.map (fun (id,def) -> (id, mapper def)) defs in
      Fletrec( defs, mapper body, f v)
    | Fclosure(funcs, fv, v) ->
      let funcs =
        { funcs with
          funs = IdentMap.map
              (fun ffunc -> { ffunc with body = mapper ffunc.body })
              funcs.funs } in
      let fv = IdentMap.map mapper fv in
      Fclosure(funcs, fv, f v)
    | Foffset(lam,id, v) -> Foffset(mapper lam,id, f v)
    | Fenv_field(env, v) ->
      let env = { env with env = mapper env.env } in
      Fenv_field(env, f v)
    | Fapply(funct, args, direct, dbg, v) ->
      Fapply(mapper funct, list_mapper args, direct, dbg, f v)
    | Fswitch(arg, sw, v) ->
      let aux l = List.map (fun (i,v) -> i, mapper v) l in
      let sw = { sw with
                 fs_consts = aux sw.fs_consts;
                 fs_blocks = aux sw.fs_blocks;
                 fs_failaction = Misc.may_map mapper sw.fs_failaction } in
      Fswitch(mapper arg, sw, f v)
    | Fsend(kind, met, obj, args, dbg, v) ->
      Fsend(kind, mapper met, mapper obj, list_mapper args, dbg, f v)
    | Fprim(prim, args, dbg, v) ->
      Fprim(prim, list_mapper args, dbg, f v)
    | Fstaticfail (i, args, v) ->
      Fstaticfail (i, list_mapper args, f v)
    | Fcatch (i, vars, body, handler, v) ->
      Fcatch (i, vars, mapper body, mapper handler, f v)
    | Ftrywith(body, id, handler, v) ->
      Ftrywith(mapper body, id, mapper handler, f v)
    | Fifthenelse(arg, ifso, ifnot, v) ->
      Fifthenelse(mapper arg, mapper ifso, mapper ifnot, f v)
    | Fsequence(lam1, lam2, v) ->
      Fsequence(mapper lam1, mapper lam2, f v)
    | Fwhile(cond, body, v) ->
      Fwhile(mapper cond, mapper body, f v)
    | Ffor(id, lo, hi, dir, body, v) ->
      Ffor(id, mapper lo, mapper hi, dir, mapper body, f v)
    | Fassign(id, lam, v) ->
      Fassign(id, mapper lam, f v)
    | Funreachable v -> Funreachable (f v)
  and list_mapper l = List.map mapper l in
  mapper tree

