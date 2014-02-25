{

  let print_loc ppf loc =
    let open Lexing in
    Format.fprintf ppf "File \"%s\", line %i, character %i"
      loc.pos_fname loc.pos_lnum loc.pos_bol

  let error lexbuf =
    failwith
      (Format.asprintf "illegal character %c %a"
         (Lexing.lexeme lexbuf).[0]
         print_loc (Lexing.lexeme_start_p lexbuf))

  module StringMap = Map.Make(String)

}

let blank = [' ' '\013' '\009' '\012']
let newline = [ '\n' ]
let escape_newline = blank* '\n' blank* newline blank*

let not_blank_or_newline = [^ ' ' '\n' '\013' '\009' '\012']

let var = ['A'-'Z'] ['A'-'Z' '_' '0'-'9'] *

rule start_line result = parse
  | (blank | newline) +
    { start_line result lexbuf }
  | '#' [^'\n']* newline
    { start_line result lexbuf }
  | var
    { let var = Lexing.lexeme lexbuf in
      after_ident result var lexbuf }
  | eof
    { result }
  | _
    { error lexbuf }


and after_ident result var = parse
  | blank + | escape_newline
    { after_ident result var lexbuf }
  | '='
    { contents result var [] lexbuf }
  | _
    { error lexbuf }

and contents result var acc = parse
  | blank + | escape_newline
    { contents result var acc lexbuf }
  | newline | eof
    { start_line ((var, List.rev acc) :: result) lexbuf }
  | not_blank_or_newline +
    { let elt = Lexing.lexeme lexbuf in
      contents result var (elt::acc) lexbuf }

and replace oc subst = parse
  | "!!" blank*
    { let txt = Lexing.lexeme lexbuf in
      replace_mark txt oc subst lexbuf }
  | eof
    { () }
  | _ [^'!'] *
    { let txt = Lexing.lexeme lexbuf in
      output_string oc txt;
      replace oc subst lexbuf }

and replace_mark txt oc subst = parse
  | var
    { let var = Lexing.lexeme lexbuf in
      replace_var txt var oc subst lexbuf }
  | eof
    { output_string oc txt }
  | "!!" | _
    { output_string oc txt;
      output_string oc (Lexing.lexeme lexbuf);
      replace oc subst lexbuf }

and replace_var txt var oc subst = parse
  | blank* "!!"
    { (try output_string oc (StringMap.find var subst)
       with Not_found -> failwith (Format.asprintf "undefined variable %s" var));
      replace oc subst lexbuf }
  | eof
    { output_string oc txt;
      output_string oc var }
  | _
    { output_string oc txt;
      output_string oc var;
      output_string oc (Lexing.lexeme lexbuf);
      replace oc subst lexbuf }

{

  let init_lexbuf filename ic =
    let open Lexing in
    let lexbuf = from_channel ic in
    let pos = { pos_fname = filename;
                pos_lnum = 0;
                pos_bol = 0;
                pos_cnum = 0 } in
    { lexbuf with lex_start_p = pos; lex_curr_p = pos }

  let load_variables filename =
    let ic = open_in filename in
    let lexbuf = init_lexbuf filename ic in
    let res = start_line [] lexbuf in
    close_in ic;
    List.fold_left
      (fun acc (var,lst) ->
         StringMap.add var (String.concat " " lst) acc)
      StringMap.empty res

  let replace_template subst filename oc =
    let ic = open_in filename in
    let lexbuf = init_lexbuf filename ic in
    replace oc subst lexbuf;
    flush oc;
    close_in ic

  let run () =
    let variables_file = Sys.argv.(1) in
    let template_file = Sys.argv.(2) in
    let subst = load_variables variables_file in
    replace_template subst template_file stdout

  let () = run ()

}
