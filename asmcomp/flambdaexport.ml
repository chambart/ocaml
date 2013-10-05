open Ext_types
open Flambda

module ExportId : Id = Id(struct end)
module EidMap = ExtMap(ExportId)
module EidSet = ExtSet(ExportId)
module EidTbl = ExtHashtbl(ExportId)

type tag = int

type descr =
  | Value_block of tag * approx array
  | Value_int of int
  | Value_constptr of int
  | Value_closure of value_offset
  | Value_symbol of symbol
  | Value_predef_exn of Ident.t

and value_offset =
  { fun_id : offset;
    closure : value_closure }

and value_closure =
  { closure_id : FunId.t;
    bound_var : approx OffsetMap.t }

and approx =
  | Value_unknown
  | Value_id of ExportId.t
  (* TODO: Value_symbol should be here, but this is too much changes
     to do that now... *)

type exported = {
  ex_functions : unit ffunctions FunMap.t;
  ex_values : descr EidMap.t;
  ex_globals : approx IdentMap.t;
  ex_id_symbol : symbol EidMap.t;
  ex_offset_fun : int OffsetMap.t;
  ex_offset_fv : int OffsetMap.t;
  ex_constants : SymbolSet.t;
}

let empty_export = {
  ex_functions = FunMap.empty;
  ex_values = EidMap.empty;
  ex_globals = IdentMap.empty;
  ex_id_symbol = EidMap.empty;
  ex_offset_fun = OffsetMap.empty;
  ex_offset_fv = OffsetMap.empty;
  ex_constants = SymbolSet.empty;
}

let print_approx ppf export =
  let values = export.ex_values in
  let open Format in
  let printed = ref EidSet.empty in
  let printed_closure = ref FunSet.empty in
  let rec print_approx ppf = function
    | Value_unknown -> fprintf ppf "?"
    | Value_id id ->
      if EidSet.mem id !printed
      then fprintf ppf "(%a: _)" ExportId.print id
      else
        let descr = EidMap.find id values in
        printed := EidSet.add id !printed;
        fprintf ppf "(%a: %a)"
          ExportId.print id
          print_descr descr
  and print_descr ppf = function
    | Value_int i -> pp_print_int ppf i
    | Value_constptr i -> fprintf ppf "%ip" i
    | Value_block (tag, fields) -> fprintf ppf "[%i:%a]" tag print_fields fields
    | Value_closure {fun_id; closure} ->
      fprintf ppf "(function %a, %a)" Offset.print fun_id print_closure closure
    | Value_symbol (id,sym) -> fprintf ppf "%a - %s" Ident.print id sym
    | Value_predef_exn id -> Ident.print ppf id
  and print_fields ppf fields =
    Array.iter (fun approx -> fprintf ppf "%a@ " print_approx approx) fields
  and print_closure ppf { closure_id; bound_var } =
    if FunSet.mem closure_id !printed_closure
    then fprintf ppf "%a" FunId.print closure_id
    else begin
      printed_closure := FunSet.add closure_id !printed_closure;
      fprintf ppf "{%a: %a}"
        FunId.print closure_id
        print_binding bound_var
    end
  and print_binding ppf bound_var =
    OffsetMap.iter (fun {off_id} approx ->
        fprintf ppf "%a -> %a,@ "
          Ident.print off_id
          print_approx approx) bound_var
  in
  let print_approxs id approx =
    fprintf ppf "%a -> %a;@ " Ident.print id print_approx approx
  in
  IdentMap.iter print_approxs export.ex_globals

let print_symbols ppf export =
  let open Format in
  let print_symbol eid (id,sym) =
    fprintf ppf "%a %s -> %a@." Ident.print id sym ExportId.print eid
  in
  EidMap.iter print_symbol export.ex_id_symbol
