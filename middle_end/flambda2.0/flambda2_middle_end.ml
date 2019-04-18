(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

let check_invariants program =
  try Flambda_static.Program.invariant program
  with exn -> begin
    Format.eprintf "Program which failed invariant check:@ %a\n%!"
      Flambda_static.Program.print program;
    raise exn
  end

let print_prepared_lambda ppf lam =
  if !Clflags.dump_prepared_lambda then begin
    Format.fprintf ppf "After Prepare_lambda:@ %a@." Printlambda.lambda lam
  end

let print_ilambda ppf (ilam : Ilambda.program) =
  if !Clflags.dump_ilambda then begin
    Format.fprintf ppf
      "After CPS conversion (return continuation %a) \
       (exception continuation %a):@ %a@."
      Continuation.print ilam.return_continuation
      Continuation.print ilam.exn_continuation.exn_handler
      Ilambda.print ilam.expr
  end

let print_rawflambda ppf program =
  if !Clflags.dump_rawflambda then begin
    Format.fprintf ppf "After closure conversion:@ %a@."
      Flambda_static.Program.print program
  end

let middle_end0 ppf ~prefixname:_ ~backend ~size ~filename
      ~module_ident ~module_initializer =
  Profile.record_call "flambda2.0" (fun () ->
    let prepared_lambda, recursive_static_catches =
      Profile.record_call ~accumulate:true "prepare_lambda" (fun () ->
        Prepare_lambda.run module_initializer)
    in
    print_prepared_lambda ppf prepared_lambda;
    let ilambda =
      Profile.record_call ~accumulate:true "cps_conversion" (fun () ->
        Cps_conversion.lambda_to_ilambda prepared_lambda
          ~recursive_static_catches)
    in
    print_ilambda ppf ilambda;
    let flambda =
      Profile.record_call ~accumulate:true "closure_conversion" (fun () ->
        Closure_conversion.ilambda_to_flambda ~backend ~module_ident
          ~size ~filename ilambda)
    in
    print_rawflambda ppf flambda;
    check_invariants flambda;
    flambda)

let middle_end ~ppf_dump:ppf ~prefixname ~backend ~size ~filename ~module_ident
      ~module_initializer =
  try
    middle_end0 ppf ~prefixname ~backend ~size ~filename ~module_ident
      ~module_initializer
  with Misc.Fatal_error -> begin
    Format.eprintf "\n%sOriginal backtrace is:%s\n%s\n"
      (Misc.Color.bold_red ())
      (Misc.Color.reset ())
      (Printexc.raw_backtrace_to_string (Misc.fatal_error_callstack ()));
    raise Misc.Fatal_error
  end