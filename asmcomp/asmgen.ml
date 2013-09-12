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

(* From lambda to assembly code *)

open Format
open Config
open Clflags
open Misc
open Cmm

type error = Assembler_error of string

let time f x =
  let t1 = Sys.time () in
  let r = f x in
  let t2 = Sys.time () in
  let dt = t2 -. t1 in
  if !dump_flambda && dt >= 0.1
  then Printf.printf "%f\n%!" dt;
  r

exception Error of error

let liveness ppf phrase =
  Liveness.fundecl ppf phrase; phrase

let dump_if ppf flag message phrase =
  if !flag then Printmach.phase message ppf phrase

let pass_dump_if ppf flag message phrase =
  dump_if ppf flag message phrase; phrase

let pass_dump_linear_if ppf flag message phrase =
  if !flag then fprintf ppf "*** %s@.%a@." message Printlinear.fundecl phrase;
  phrase

let clambda_dump_if ppf ulambda =
  if !dump_clambda then
    begin
      Printclambda.clambda ppf ulambda;
      List.iter (fun (lbl,_,cst) ->
        Format.fprintf ppf "%s:@ " lbl;
        Printclambda.structured_constant ppf cst)
        (Compilenv.structured_constants ())
    end;
  ulambda

let flambda_dump_if ppf flambda =
  if !dump_flambda then (Printflambda.flambda ppf flambda; Format.fprintf ppf "@.");
  Flambda.check (Compilenv.current_unit_symbol ()) flambda;
  flambda

let rec regalloc ppf round fd =
  if round > 50 then
    fatal_error(fd.Mach.fun_name ^
                ": function too complex, cannot complete register allocation");
  dump_if ppf dump_live "Liveness analysis" fd;
  Interf.build_graph fd;
  if !dump_interf then Printmach.interferences ppf ();
  if !dump_prefer then Printmach.preferences ppf ();
  Coloring.allocate_registers();
  dump_if ppf dump_regalloc "After register allocation" fd;
  let (newfd, redo_regalloc) = Reload.fundecl fd in
  dump_if ppf dump_reload "After insertion of reloading code" newfd;
  if redo_regalloc then begin
    Reg.reinit(); Liveness.fundecl ppf newfd; regalloc ppf (round + 1) newfd
  end else newfd

let (++) x f = time f x

let text t f =
  if !dump_flambda then print_endline t;
  f

let compile_fundecl (ppf : formatter) fd_cmm =
  Proc.init ();
  Reg.reset();
  fd_cmm
  ++ Selection.fundecl
  ++ pass_dump_if ppf dump_selection "After instruction selection"
  ++ Comballoc.fundecl
  ++ pass_dump_if ppf dump_combine "After allocation combining"
  ++ liveness ppf
  ++ pass_dump_if ppf dump_live "Liveness analysis"
  ++ Spill.fundecl
  ++ liveness ppf
  ++ pass_dump_if ppf dump_spill "After spilling"
  ++ Split.fundecl
  ++ pass_dump_if ppf dump_split "After live range splitting"
  ++ liveness ppf
  ++ regalloc ppf 1
  ++ Linearize.fundecl
  ++ pass_dump_linear_if ppf dump_linear "Linearized code"
  ++ Scheduling.fundecl
  ++ pass_dump_linear_if ppf dump_scheduling "After instruction scheduling"
  ++ Emit.fundecl

let compile_phrase ppf p =
  if !dump_cmm then fprintf ppf "%a@." Printcmm.phrase p;
  match p with
  | Cfunction fd -> compile_fundecl ppf fd
  | Cdata dl -> Emit.data dl


(* For the native toplevel: generates generic functions unless
   they are already available in the process *)
let compile_genfuns ppf f =
  List.iter
    (function
       | (Cfunction {fun_name = name}) as ph when f name ->
           compile_phrase ppf ph
       | _ -> ())
    (Cmmgen.generic_functions true [Compilenv.current_unit_infos ()])

let check flambda =
  Flambda.check (Compilenv.current_unit_symbol ()) flambda;
  flambda

let specialise ppf flambda =
  let val_result = Flambdainfo.analyse flambda in
  let effectful_result = Purity.effectful Purity.Effectful flambda in
  Cleaner.specialise val_result effectful_result flambda

let cleaning ppf flambda =
  let val_result = Flambdainfo.analyse flambda in
  let effectful_result = Purity.effectful Purity.Effectful flambda in
  flambda
  ++ text "specialise"
  ++ Cleaner.specialise val_result effectful_result
  ++ flambda_dump_if ppf
  ++ text "rebind"
  ++ Cleaner.rebind val_result effectful_result
  ++ flambda_dump_if ppf
  ++ text "reindex"
  ++ Flambdautils.reindex
  ++ flambda_dump_if ppf
  ++ text "dead code elimination"
  ++ Flambdautils.anf
  ++ Dead_code.eliminate_dead_code
  ++ flambda_dump_if ppf
  ++ text "reindex"
  ++ Flambdautils.reindex
  ++ check

let inlining ppf flambda =
  let val_result = Flambdainfo.analyse flambda in
  let not_const = Constants.not_constants ~for_clambda:false flambda in
  (* Cleaner.inlining Cleaner.Minimal val_result flambda *)
  let step1 =
    Cleaner.inlining (Cleaner.With_local_functions not_const) val_result flambda
    ++ Flambdautils.reindex
    ++ flambda_dump_if ppf
  in
  let val_result = Flambdainfo.analyse step1 in
  step1
  ++ text "inlining minimal"
  ++ Cleaner.inlining Cleaner.Minimal val_result
  ++ Flambdautils.reindex
  ++ flambda_dump_if ppf

let extract_constant ppf flambda =
  (* assumes ANF *)
  Cleaner.extract_constants (Constants.alias flambda) flambda

let prepare ppf flambda =
  Flambdautils.anf flambda
  ++ flambda_dump_if ppf
  ++ text "extract constant"
  ++ extract_constant ppf

let elim_let ppf flambda =
  let not_const = Constants.not_constants ~for_clambda:false flambda in
  let pure_result = Purity.effectful Purity.Pure flambda in
  flambda
  ++ text "eliminate useless let"
  ++ Cleaner.elim_let not_const pure_result
  ++ flambda_dump_if ppf

let unclose ppf flambda =
  let not_const = Constants.not_constants ~for_clambda:false flambda in
  Cleaner.unclose not_const flambda

let optimise_one ppf flambda =
  if not !Clflags.enable_optim (* true *)
  then
    flambda

    ++ text "unclose"
    ++ unclose ppf
    ++ flambda_dump_if ppf

    ++ text "prepare"
    ++ prepare ppf
    ++ flambda_dump_if ppf
    ++ text "cleaning"
    ++ cleaning ppf
    ++ flambda_dump_if ppf
    ++ text "inlining"
    ++ inlining ppf
    ++ flambda_dump_if ppf
    ++ text "prepare"
    ++ prepare ppf
    ++ flambda_dump_if ppf
    ++ text "cleaning"
    ++ cleaning ppf
    ++ flambda_dump_if ppf
    ++ text "end specialise"
    ++ specialise ppf
    ++ flambda_dump_if ppf
    ++ elim_let ppf
    ++ text "end"

  else flambda

let optimise ppf flambda =
  let rec aux n flambda =
    if n <= 0 then flambda else
      aux (n-1) (optimise_one ppf flambda)
  in
  let flambda = aux 1 flambda in
  flambda

let set_global_approx ppf (clambda, exported) =
  if !dump_clambda
  then begin
    Format.fprintf ppf "%a@." Flambdaexport.print_approx exported;
    Flambdaexport.print_symbols ppf exported;
  end;
  Compilenv.set_global_approx_info exported;
  clambda

let compile_implementation ?toplevel prefixname ppf (size, lam) =
  let asmfile =
    if !keep_asm_file
    then prefixname ^ ext_asm
    else Filename.temp_file "camlasm" ext_asm in
  let oc = open_out asmfile in
  begin try
    Emitaux.output_channel := oc;
    Emit.begin_assembly();
    Flambdagen.intro size lam
    ++ flambda_dump_if ppf
    ++ text "optimse"
    ++ optimise ppf
    ++ text "clambdagen"
    ++ Clambdagen.convert
    ++ set_global_approx ppf
    ++ clambda_dump_if ppf
    ++ text "cmmgen"
    ++ Cmmgen.compunit size
    ++ List.iter (compile_phrase ppf) ++ (fun () -> ());
    (match toplevel with None -> () | Some f -> compile_genfuns ppf f);

    (* We add explicit references to external primitive symbols.  This
       is to ensure that the object files that define these symbols,
       when part of a C library, won't be discarded by the linker.
       This is important if a module that uses such a symbol is later
       dynlinked. *)

    compile_phrase ppf
      (Cmmgen.reference_symbols
         (List.filter (fun s -> s <> "" && s.[0] <> '%')
            (List.map Primitive.native_name !Translmod.primitive_declarations))
      );

    Emit.end_assembly();
    close_out oc
  with x ->
    close_out oc;
    if !keep_asm_file then () else remove_file asmfile;
    raise x
  end;
  if Proc.assemble_file asmfile (prefixname ^ ext_obj) <> 0
  then raise(Error(Assembler_error asmfile));
  if !keep_asm_file then () else remove_file asmfile

(* Error report *)

let report_error ppf = function
  | Assembler_error file ->
      fprintf ppf "Assembler error, input left in file %a"
        Location.print_filename file
