(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)


open Format
open Asttypes
open Clambda

let mutable_flag = function
  | Mutable-> "[mut]"
  | Immutable -> ""

let value_kind =
  let open Lambda in
  function
  | Pgenval -> ""
  | Pintval -> ":int"
  | Pfloatval -> ":float"
  | Pboxedintval Pnativeint -> ":nativeint"
  | Pboxedintval Pint32 -> ":int32"
  | Pboxedintval Pint64 -> ":int64"

let boxed_integer_name = function
  | Lambda.Pnativeint -> "nativeint"
  | Lambda.Pint32 -> "int32"
  | Lambda.Pint64 -> "int64"

let boxed_integer_mark name = function
  | Lambda.Pnativeint -> Printf.sprintf "Nativeint.%s" name
  | Lambda.Pint32 -> Printf.sprintf "Int32.%s" name
  | Lambda.Pint64 -> Printf.sprintf "Int64.%s" name

let print_boxed_integer name ppf bi =
  fprintf ppf "%s" (boxed_integer_mark name bi);;

let array_kind array_kind =
  let open Lambda in
  match array_kind with
  | Pgenarray -> "gen"
  | Paddrarray -> "addr"
  | Pintarray -> "int"
  | Pfloatarray -> "float"

let access_size size =
  let open Clambda_primitives in
  match size with
  | Sixteen -> "16"
  | Thirty_two -> "32"
  | Sixty_four -> "64"

let access_safety safety =
  let open Lambda in
  match safety with
  | Safe -> ""
  | Unsafe -> "unsafe_"

let primitive ppf (prim:Clambda_primitives.primitive) =
  let open Lambda in
  let open Clambda_primitives in
  match prim with
  | Pread_symbol sym ->
      fprintf ppf "read_symbol %s" sym
  | Pmakeblock(tag, Immutable, shape) ->
      fprintf ppf "makeblock %i%a" tag Printlambda.block_shape shape
  | Pmakeblock(tag, Mutable, shape) ->
      fprintf ppf "makemutable %i%a" tag Printlambda.block_shape shape
  | Pfield n -> fprintf ppf "field %i" n
  | Pfield_computed -> fprintf ppf "field_computed"
  | Psetfield(n, ptr, init) ->
      let instr =
        match ptr with
        | Pointer -> "ptr"
        | Immediate -> "imm"
      in
      let init =
        match init with
        | Heap_initialization -> "(heap-init)"
        | Root_initialization -> "(root-init)"
        | Assignment -> ""
      in
      fprintf ppf "setfield_%s%s %i" instr init n
  | Psetfield_computed (ptr, init) ->
      let instr =
        match ptr with
        | Pointer -> "ptr"
        | Immediate -> "imm"
      in
      let init =
        match init with
        | Heap_initialization -> "(heap-init)"
        | Root_initialization -> "(root-init)"
        | Assignment -> ""
      in
      fprintf ppf "setfield_%s%s_computed" instr init
  | Pfloatfield n -> fprintf ppf "floatfield %i" n
  | Psetfloatfield (n, init) ->
      let init =
        match init with
        | Heap_initialization -> "(heap-init)"
        | Root_initialization -> "(root-init)"
        | Assignment -> ""
      in
      fprintf ppf "setfloatfield%s %i" init n
  | Pduprecord (rep, size) ->
      fprintf ppf "duprecord %a %i" Printlambda.record_rep rep size
  | Plazyforce -> fprintf ppf "force"
  | Pccall p -> fprintf ppf "%s" p.Primitive.prim_name
  | Praise k -> fprintf ppf "%s" (Lambda.raise_kind k)
  | Psequand -> fprintf ppf "&&"
  | Psequor -> fprintf ppf "||"
  | Pnot -> fprintf ppf "not"
  | Pnegint -> fprintf ppf "~"
  | Paddint -> fprintf ppf "+"
  | Psubint -> fprintf ppf "-"
  | Pmulint -> fprintf ppf "*"
  | Pdivint Safe -> fprintf ppf "/"
  | Pdivint Unsafe -> fprintf ppf "/u"
  | Pmodint Safe -> fprintf ppf "mod"
  | Pmodint Unsafe -> fprintf ppf "mod_unsafe"
  | Pandint -> fprintf ppf "and"
  | Porint -> fprintf ppf "or"
  | Pxorint -> fprintf ppf "xor"
  | Plslint -> fprintf ppf "lsl"
  | Plsrint -> fprintf ppf "lsr"
  | Pasrint -> fprintf ppf "asr"
  | Pintcomp(cmp) -> Printlambda.integer_comparison ppf cmp
  | Poffsetint n -> fprintf ppf "%i+" n
  | Poffsetref n -> fprintf ppf "+:=%i"n
  | Pintoffloat -> fprintf ppf "int_of_float"
  | Pfloatofint -> fprintf ppf "float_of_int"
  | Pnegfloat -> fprintf ppf "~."
  | Pabsfloat -> fprintf ppf "abs."
  | Paddfloat -> fprintf ppf "+."
  | Psubfloat -> fprintf ppf "-."
  | Pmulfloat -> fprintf ppf "*."
  | Pdivfloat -> fprintf ppf "/."
  | Pfloatcomp(cmp) -> Printlambda.float_comparison ppf cmp
  | Pstringlength -> fprintf ppf "string.length"
  | Pstringrefu -> fprintf ppf "string.unsafe_get"
  | Pstringrefs -> fprintf ppf "string.get"
  | Pbyteslength -> fprintf ppf "bytes.length"
  | Pbytesrefu -> fprintf ppf "bytes.unsafe_get"
  | Pbytessetu -> fprintf ppf "bytes.unsafe_set"
  | Pbytesrefs -> fprintf ppf "bytes.get"
  | Pbytessets -> fprintf ppf "bytes.set"

  | Parraylength k -> fprintf ppf "array.length[%s]" (array_kind k)
  | Pmakearray (k, Mutable) -> fprintf ppf "makearray[%s]" (array_kind k)
  | Pmakearray (k, Immutable) -> fprintf ppf "makearray_imm[%s]" (array_kind k)
  | Pduparray (k, Mutable) -> fprintf ppf "duparray[%s]" (array_kind k)
  | Pduparray (k, Immutable) -> fprintf ppf "duparray_imm[%s]" (array_kind k)
  | Parrayrefu k -> fprintf ppf "array.unsafe_get[%s]" (array_kind k)
  | Parraysetu k -> fprintf ppf "array.unsafe_set[%s]" (array_kind k)
  | Parrayrefs k -> fprintf ppf "array.get[%s]" (array_kind k)
  | Parraysets k -> fprintf ppf "array.set[%s]" (array_kind k)
  | Pisint -> fprintf ppf "isint"
  | Pisout -> fprintf ppf "isout"
  | Pbintofint bi -> print_boxed_integer "of_int" ppf bi
  | Pintofbint bi -> print_boxed_integer "to_int" ppf bi
  | Pcvtbint (bi1, bi2) ->
      fprintf ppf "%s_of_%s" (boxed_integer_name bi2) (boxed_integer_name bi1)
  | Pnegbint bi -> print_boxed_integer "neg" ppf bi
  | Paddbint bi -> print_boxed_integer "add" ppf bi
  | Psubbint bi -> print_boxed_integer "sub" ppf bi
  | Pmulbint bi -> print_boxed_integer "mul" ppf bi
  | Pdivbint { size = bi; is_safe = Safe } ->
      print_boxed_integer "div" ppf bi
  | Pdivbint { size = bi; is_safe = Unsafe } ->
      print_boxed_integer "div_unsafe" ppf bi
  | Pmodbint { size = bi; is_safe = Safe } ->
      print_boxed_integer "mod" ppf bi
  | Pmodbint { size = bi; is_safe = Unsafe } ->
      print_boxed_integer "mod_unsafe" ppf bi
  | Pandbint bi -> print_boxed_integer "and" ppf bi
  | Porbint bi -> print_boxed_integer "or" ppf bi
  | Pxorbint bi -> print_boxed_integer "xor" ppf bi
  | Plslbint bi -> print_boxed_integer "lsl" ppf bi
  | Plsrbint bi -> print_boxed_integer "lsr" ppf bi
  | Pasrbint bi -> print_boxed_integer "asr" ppf bi
  | Pbintcomp(bi, Ceq) -> print_boxed_integer "==" ppf bi
  | Pbintcomp(bi, Cne) -> print_boxed_integer "!=" ppf bi
  | Pbintcomp(bi, Clt) -> print_boxed_integer "<" ppf bi
  | Pbintcomp(bi, Cgt) -> print_boxed_integer ">" ppf bi
  | Pbintcomp(bi, Cle) -> print_boxed_integer "<=" ppf bi
  | Pbintcomp(bi, Cge) -> print_boxed_integer ">=" ppf bi
  | Pbigarrayref(unsafe, _n, kind, layout) ->
      Printlambda.print_bigarray "get" unsafe kind ppf layout
  | Pbigarrayset(unsafe, _n, kind, layout) ->
      Printlambda.print_bigarray "set" unsafe kind ppf layout
  | Pbigarraydim(n) -> fprintf ppf "Bigarray.dim_%i" n
  | Pstring_load(size, safety) ->
      fprintf ppf "string.%sget%s" (access_safety safety) (access_size size)
  | Pbytes_load(size, safety) ->
      fprintf ppf "bytes.%sget%s" (access_safety safety) (access_size size)
  | Pbytes_set(size, safety) ->
      fprintf ppf "bytes.%sset%s" (access_safety safety) (access_size size)
  | Pbigstring_load(size, safety) ->
      fprintf ppf "bigarray.array1.%sget%s"
        (access_safety safety) (access_size size)
  | Pbigstring_set(size, safety) ->
      fprintf ppf "bigarray.array1.%sset%s"
        (access_safety safety) (access_size size)
  | Pbswap16 -> fprintf ppf "bswap16"
  | Pbbswap(bi) -> print_boxed_integer "bswap" ppf bi
  | Pint_as_pointer -> fprintf ppf "int_as_pointer"
  | Popaque -> fprintf ppf "opaque"

let rec structured_constant ppf = function
  | Uconst_float x -> fprintf ppf "%F" x
  | Uconst_int32 x -> fprintf ppf "%ldl" x
  | Uconst_int64 x -> fprintf ppf "%LdL" x
  | Uconst_nativeint x -> fprintf ppf "%ndn" x
  | Uconst_block (tag, l) ->
      fprintf ppf "block(%i" tag;
      List.iter (fun u -> fprintf ppf ",%a" uconstant u) l;
      fprintf ppf ")"
  | Uconst_float_array [] ->
      fprintf ppf "floatarray()"
  | Uconst_float_array (f1 :: fl) ->
      fprintf ppf "floatarray(%F" f1;
      List.iter (fun f -> fprintf ppf ",%F" f) fl;
      fprintf ppf ")"
  | Uconst_string s -> fprintf ppf "%S" s
  | Uconst_closure(clos, sym, fv) ->
      let idents ppf =
        List.iter (fprintf ppf "@ %a" Ident.print)in
      let one_fun ppf f =
        fprintf ppf "(fun@ %s@ %d@ @[<2>%a@]@ @[<2>%a@])"
          f.label f.arity idents f.params lam f.body in
      let funs ppf =
        List.iter (fprintf ppf "@ %a" one_fun) in
      let sconsts ppf scl =
        List.iter (fun sc -> fprintf ppf "@ %a" uconstant sc) scl in
      fprintf ppf "@[<2>(const_closure%a %s@ %a)@]" funs clos sym sconsts fv


and uconstant ppf = function
  | Uconst_ref (s, Some c) ->
      fprintf ppf "%S=%a" s structured_constant c
  | Uconst_ref (s, None) -> fprintf ppf "%S"s
  | Uconst_int i -> fprintf ppf "%i" i
  | Uconst_ptr i -> fprintf ppf "%ia" i

and lam ppf = function
  | Uvar id ->
      Ident.print ppf id
  | Uconst c -> uconstant ppf c
  | Udirect_apply(f, largs, _) ->
      let lams ppf largs =
        List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
      fprintf ppf "@[<2>(apply*@ %s %a)@]" f lams largs
  | Ugeneric_apply(lfun, largs, _) ->
      let lams ppf largs =
        List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
      fprintf ppf "@[<2>(apply@ %a%a)@]" lam lfun lams largs
  | Uclosure(clos, fv) ->
      let idents ppf =
        List.iter (fprintf ppf "@ %a" Ident.print)in
      let one_fun ppf f =
        fprintf ppf "@[<2>(fun@ %s@ %d @[<2>%a@]@ @[<2>%a@]@])"
          f.label f.arity idents f.params lam f.body in
      let funs ppf =
        List.iter (fprintf ppf "@ %a" one_fun) in
      let lams ppf =
        List.iter (fprintf ppf "@ %a" lam) in
      fprintf ppf "@[<2>(closure@ %a %a)@]" funs clos lams fv
  | Uoffset(l,i) -> fprintf ppf "@[<2>(offset %a %d)@]" lam l i
  | Ulet(mut, kind, id, arg, body) ->
      let rec letbody ul = match ul with
        | Ulet(mut, kind, id, arg, body) ->
            fprintf ppf "@ @[<2>%a%s%s@ %a@]"
              Ident.print id (mutable_flag mut) (value_kind kind) lam arg;
            letbody body
        | _ -> ul in
      fprintf ppf "@[<2>(let@ @[<hv 1>(@[<2>%a%s%s@ %a@]"
        Ident.print id (mutable_flag mut) (value_kind kind) lam arg;
      let expr = letbody body in
      fprintf ppf ")@]@ %a)@]" lam expr
  | Uletrec(id_arg_list, body) ->
      let bindings ppf id_arg_list =
        let spc = ref false in
        List.iter
          (fun (id, l) ->
            if !spc then fprintf ppf "@ " else spc := true;
            fprintf ppf "@[<2>%a@ %a@]" Ident.print id lam l)
          id_arg_list in
      fprintf ppf
        "@[<2>(letrec@ (@[<hv 1>%a@])@ %a)@]" bindings id_arg_list lam body
  | Uprim(prim, largs, _) ->
      let lams ppf largs =
        List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
      fprintf ppf "@[<2>(%a%a)@]" primitive prim lams largs
  | Uswitch(larg, sw, _dbg) ->
      let print_case tag index i ppf =
        for j = 0 to Array.length index - 1 do
          if index.(j) = i then fprintf ppf "case %s %i:" tag j
        done in
      let print_cases tag index cases ppf =
        for i = 0 to Array.length cases - 1 do
          fprintf ppf "@ @[<2>%t@ %a@]"
            (print_case tag index i) sequence cases.(i)
        done in
      let switch ppf sw =
        print_cases "int" sw.us_index_consts sw.us_actions_consts ppf ;
        print_cases "tag" sw.us_index_blocks sw.us_actions_blocks ppf  in
      fprintf ppf
       "@[<v 0>@[<2>(switch@ %a@ @]%a)@]"
        lam larg switch sw
  | Ustringswitch(larg,sw,d) ->
      let switch ppf sw =
        let spc = ref false in
        List.iter
          (fun (s,l) ->
            if !spc then fprintf ppf "@ " else spc := true;
            fprintf ppf "@[<hv 1>case \"%s\":@ %a@]"
              (String.escaped s) lam l)
          sw ;
        begin match d with
        | Some d ->
            if !spc then fprintf ppf "@ " else spc := true;
            fprintf ppf "@[<hv 1>default:@ %a@]" lam d
        | None -> ()
        end in
      fprintf ppf
        "@[<1>(switch %a@ @[<v 0>%a@])@]" lam larg switch sw
  | Ustaticfail (i, ls)  ->
      let lams ppf largs =
        List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
      fprintf ppf "@[<2>(exit@ %d%a)@]" i lams ls;
  | Ucatch(i, vars, lbody, lhandler) ->
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
  | Utrywith(lbody, param, lhandler) ->
      fprintf ppf "@[<2>(try@ %a@;<1 -1>with %a@ %a)@]"
        lam lbody Ident.print param lam lhandler
  | Uifthenelse(lcond, lif, lelse) ->
      fprintf ppf "@[<2>(if@ %a@ %a@ %a)@]" lam lcond lam lif lam lelse
  | Usequence(l1, l2) ->
      fprintf ppf "@[<2>(seq@ %a@ %a)@]" lam l1 sequence l2
  | Uwhile(lcond, lbody) ->
      fprintf ppf "@[<2>(while@ %a@ %a)@]" lam lcond lam lbody
  | Ufor(param, lo, hi, dir, body) ->
      fprintf ppf "@[<2>(for %a@ %a@ %s@ %a@ %a)@]"
       Ident.print param lam lo
       (match dir with Upto -> "to" | Downto -> "downto")
       lam hi lam body
  | Uassign(id, expr) ->
      fprintf ppf "@[<2>(assign@ %a@ %a)@]" Ident.print id lam expr
  | Usend (k, met, obj, largs, _) ->
      let args ppf largs =
        List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
      let kind =
        if k = Lambda.Self then "self"
        else if k = Lambda.Cached then "cache"
        else "" in
      fprintf ppf "@[<2>(send%s@ %a@ %a%a)@]" kind lam obj lam met args largs
  | Uunreachable ->
      fprintf ppf "unreachable"

and sequence ppf ulam = match ulam with
  | Usequence(l1, l2) ->
      fprintf ppf "%a@ %a" sequence l1 sequence l2
  | _ -> lam ppf ulam

let clambda ppf ulam =
  fprintf ppf "%a@." lam ulam


let rec approx ppf = function
    Value_closure(fundesc, a) ->
      Format.fprintf ppf "@[<2>function %s@ arity %i"
        fundesc.fun_label fundesc.fun_arity;
      if fundesc.fun_closed then begin
        Format.fprintf ppf "@ (closed)"
      end;
      if fundesc.fun_inline <> None then begin
        Format.fprintf ppf "@ (inline)"
      end;
      Format.fprintf ppf "@ -> @ %a@]" approx a
  | Value_tuple a ->
      let tuple ppf a =
        for i = 0 to Array.length a - 1 do
          if i > 0 then Format.fprintf ppf ";@ ";
          Format.fprintf ppf "%i: %a" i approx a.(i)
        done in
      Format.fprintf ppf "@[<hov 1>(%a)@]" tuple a
  | Value_unknown ->
      Format.fprintf ppf "_"
  | Value_const c ->
      fprintf ppf "@[const(%a)@]" uconstant c
  | Value_global_field (s, i) ->
      fprintf ppf "@[global(%s,%i)@]" s i
