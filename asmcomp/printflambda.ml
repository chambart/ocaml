(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                     Pierre Chambart, OCamlPro                       *)
(*                                                                     *)
(*  Copyright 2013 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)


open Format
open Flambda

let rec pr_idents ppf = function
  | [] -> ()
  | h::t -> fprintf ppf "%a %a" Ident.print h pr_idents t

let rec lam ppf = function
  | Fvar (id,_) ->
    Ident.print ppf id
  | Fconst (cst,_) ->
    const ppf cst
  | Fapply(lfun, largs, direct_name, _,_) ->
    let lams ppf largs =
      List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
    let direct = match direct_name with | None -> "" | Some _ -> "*" in
    fprintf ppf "@[<2>(apply%s@ %a%a)@]" direct lam lfun lams largs
  | Foffset(l,id,_) -> fprintf ppf "@[<2>(offset@ %a@ %a)@]" Ident.print id lam l
  | Fenv_field(l,id,_) -> fprintf ppf "@[<2>(env@ %a@ %a)@]" Ident.print id lam l
  | Fclosure(clos,fv,_) ->
    let idents ppf =
      List.iter (fprintf ppf "@ %a" Ident.print) in
    let one_fun ppf f =
      fprintf ppf "(fun@ %s@ %d@ @[<2>%a@]@ @[<2>%a@])"
        (f.label:>string) f.arity idents f.params lam f.body in
    let funs ppf =
      IdentMap.iter (fun _ v -> fprintf ppf "@ %a" one_fun v) in
    let lams ppf =
      IdentMap.iter (fun id v -> fprintf ppf "@ %a = %a"
          Ident.print id lam v) in
    fprintf ppf "@[<2>(closure%a %a)@]" funs clos.funs lams fv
  | Flet(str, id, arg, body,_) ->
    let rec letbody ul = match ul with
      | Flet(str, id, arg, body,_) ->
        fprintf ppf "@ @[<2>%a@ %a@]" Ident.print id lam arg;
        letbody body
      | _ -> ul in
    fprintf ppf "@[<2>(let@ @[<hv 1>(@[<2>%a@ %a@]" Ident.print id lam arg;
    let expr = letbody body in
    fprintf ppf ")@]@ %a)@]" lam expr
  | Fletrec(id_arg_list, body,_) ->
    let bindings ppf id_arg_list =
      let spc = ref false in
      List.iter
        (fun (id, l) ->
          if !spc then fprintf ppf "@ " else spc := true;
          fprintf ppf "@[<2>%a@ %a@]" Ident.print id lam l)
        id_arg_list in
    fprintf ppf
      "@[<2>(letrec@ (@[<hv 1>%a@])@ %a)@]" bindings id_arg_list lam body
  | Fprim(prim, largs, _,_) ->
    let lams ppf largs =
      List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
    fprintf ppf "@[<2>(%a%a)@]" Printlambda.primitive prim lams largs
  | Fswitch(larg, sw,_) ->
    let switch ppf sw =
      let spc = ref false in
      List.iter
        (fun (n, l) ->
          if !spc then fprintf ppf "@ " else spc := true;
          fprintf ppf "@[<hv 1>case int %i:@ %a@]" n lam l)
        sw.fs_consts;
      List.iter
        (fun (n, l) ->
          if !spc then fprintf ppf "@ " else spc := true;
          fprintf ppf "@[<hv 1>case tag %i:@ %a@]" n lam l)
        sw.fs_blocks ;
      begin match sw.fs_failaction with
        | None  -> ()
        | Some l ->
          if !spc then fprintf ppf "@ " else spc := true;
          fprintf ppf "@[<hv 1>default:@ %a@]" lam l
      end in
    fprintf ppf
      "@[<1>(%s(%i,%i) %a@ @[<v 0>%a@])@]"
      (match sw.fs_failaction with None -> "switch*" | _ -> "switch")
      sw.fs_numconsts sw.fs_numblocks
      lam larg switch sw
  | Fstaticfail (i, ls,_)  ->
    let lams ppf largs =
      List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
    fprintf ppf "@[<2>(exit@ %d%a)@]" i lams ls;
  | Fcatch(i, vars, lbody, lhandler,_) ->
    fprintf ppf "@[<2>(catch@ %a@;<1 -1>with (%d%a)@ %a)@]"
      lam lbody i
      (fun ppf vars -> match vars with
        | [] -> ()
        | _ ->
          List.iter
            (fun x -> fprintf ppf " %a" Ident.print x)
            vars)
      vars
      lam lhandler
  | Ftrywith(lbody, param, lhandler,_) ->
    fprintf ppf "@[<2>(try@ %a@;<1 -1>with %a@ %a)@]"
      lam lbody Ident.print param lam lhandler
  | Fifthenelse(lcond, lif, lelse,_) ->
    fprintf ppf "@[<2>(if@ %a@ %a@ %a)@]" lam lcond lam lif lam lelse
  | Fsequence(l1, l2,_) ->
    fprintf ppf "@[<2>(seq@ %a@ %a)@]" lam l1 sequence l2
  | Fwhile(lcond, lbody,_) ->
    fprintf ppf "@[<2>(while@ %a@ %a)@]" lam lcond lam lbody
  | Ffor(param, lo, hi, dir, body,_) ->
    fprintf ppf "@[<2>(for %a@ %a@ %s@ %a@ %a)@]"
      Ident.print param lam lo
      (match dir with Asttypes.Upto -> "to" | Asttypes.Downto -> "downto")
      lam hi lam body
  | Fassign(id, expr,_) ->
    fprintf ppf "@[<2>(assign@ %a@ %a)@]" Ident.print id lam expr
  | Fsend (k, met, obj, largs, _,_) ->
    let args ppf largs =
      List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
    let kind =
      if k = Lambda.Self then "self" else if k = Lambda.Cached then "cache" else "" in
    fprintf ppf "@[<2>(send%s@ %a@ %a%a)@]" kind lam obj lam met args largs

and sequence ppf ulam = match ulam with
  | Fsequence(l1, l2,_) ->
    fprintf ppf "%a@ %a" sequence l1 sequence l2
  | _ -> lam ppf ulam

and const ppf c = let open Asttypes in match c with
  | Fconst_base(Const_int n) -> fprintf ppf "%i" n
  | Fconst_base(Const_char c) -> fprintf ppf "%C" c
  | Fconst_base(Const_string s) -> fprintf ppf "%S" s
  | Fconst_immstring s -> fprintf ppf "#%S" s
  | Fconst_base(Const_float f) -> fprintf ppf "%s" f
  | Fconst_base(Const_int32 n) -> fprintf ppf "%lil" n
  | Fconst_base(Const_int64 n) -> fprintf ppf "%LiL" n
  | Fconst_base(Const_nativeint n) -> fprintf ppf "%nin" n
  | Fconst_pointer n -> fprintf ppf "%ia" n
  (* | Fconst_block(tag, []) -> *)
  (*   fprintf ppf "[%i]" tag *)
  (* | Fconst_block(tag, sc1::scl) -> *)
  (*   let sconsts ppf scl = *)
  (*     List.iter (fun sc -> fprintf ppf "@ %a" const sc) scl in *)
  (*   fprintf ppf "@[<1>[%i:@ @[%a%a@]]@]" tag const sc1 sconsts scl *)
  | Fconst_float_array [] ->
    fprintf ppf "[| |]"
  | Fconst_float_array (f1 :: fl) ->
    let floats ppf fl =
      List.iter (fun f -> fprintf ppf "@ %s" f) fl in
    fprintf ppf "@[<1>[|@[%s%a@]|]@]" f1 floats fl
  (* | Fconst_id id -> *)
  (*   Ident.print ppf id *)

let flambda ppf ulam =
  fprintf ppf "%a@." lam ulam
