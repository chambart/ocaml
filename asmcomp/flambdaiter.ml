(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                     Pierre Chambart, OCamlPro                       *)
(*                                                                     *)
(*  Copyright 2014 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Flambda

let apply_on_subexpressions f = function
  | Fsymbol _
  | Fvar _
  | Fconst _
  | Funreachable _ -> ()

  | Fassign (_,f1,_)
  | Ffunction({fu_closure = f1},_)
  | Fvariable_in_closure({vc_closure = f1},_) ->
    f f1

  | Flet ( _, _, f1, f2,_)
  | Ftrywith (f1,_,f2,_)
  | Fsequence (f1,f2,_)
  | Fwhile (f1,f2,_)
  | Fcatch (_,_,f1,f2,_) ->
    f f1; f f2;

  | Ffor (_,f1,f2,_,f3,_)
  | Fifthenelse (f1,f2,f3,_) ->
    f f1;f f2;f f3

  | Fstaticfail (_,l,_)
  | Fprim (_,l,_,_) ->
    List.iter f l

  | Fapply ({ap_function;ap_arg},_) ->
    List.iter f (ap_function::ap_arg)
  | Fclosure ({cl_fun;cl_free_var},_) ->
    Ident.Map.iter (fun _ v -> f v) cl_free_var;
    Ident.Map.iter (fun _ ffun -> f ffun.body) cl_fun.funs
  | Fletrec (defs, body,_) ->
    List.iter (fun (_,l) -> f l) defs;
    f body
  | Fswitch (arg,sw,_) ->
    f arg;
    List.iter (fun (_,l) -> f l) sw.fs_consts;
    List.iter (fun (_,l) -> f l) sw.fs_blocks;
    (match sw.fs_failaction with
     | None -> ()
     | Some f1 -> f f1)
  | Fsend (_,f1,f2,fl,_,_) ->
    List.iter f (f1::f2::fl)
