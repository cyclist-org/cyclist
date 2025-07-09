open Lib
open Generic

open Symbols
open MParser

let program_pp fmt cmd =
  Format.fprintf fmt "%a@\n%a"
    While.Program.Field.pp ()
    (While.Program.Cmd.pp 0) cmd

let pp_cmd fmt cmd =
  While.Program.Cmd.pp ~abbr:true 0 fmt cmd


let program_vars = ref Seplog.Term.Set.empty

let set_program p =
  program_vars := While.Program.Cmd.vars p

let vars_of_program () = !program_vars

(* remember prog vars when introducing fresh ones *)
let fresh_fvar s =
  Seplog.Term.fresh_fvar (Seplog.Term.Set.union !program_vars s)
let fresh_fvars s i =
  Seplog.Term.fresh_fvars (Seplog.Term.Set.union !program_vars s) i
let fresh_evar s =
  Seplog.Term.fresh_evar (Seplog.Term.Set.union !program_vars s)
let fresh_evars s i =
  Seplog.Term.fresh_evars (Seplog.Term.Set.union !program_vars s) i

(* again, treat prog vars as special *)
let freshen_case_by_seq seq case =
  Seplog.Indrule.freshen
    (Seplog.Term.Set.union !program_vars (Seq.vars seq))
    case

(* fields: FIELDS; COLON; ils = separated_nonempty_list(COMMA, IDENT); SEMICOLON  *)
(*     { List.iter P.Field.add ils }                                              *)
let parse_fields st =
  ( parse_symb keyw_fields >>
    parse_symb symb_colon >>
    sep_by1 While.Program.Field.parse (parse_symb symb_comma) >>= (fun ils ->
    parse_symb symb_semicolon >>$
    List.iter While.Program.Field.add ils) <?> "Fields") st

(* precondition: PRECONDITION; COLON; f = formula; SEMICOLON { f } *)
let parse_precondition st =
  ( parse_symb keyw_precondition >>
    parse_symb symb_colon >>
    Seplog.Form.parse ~allow_tags:false >>= (fun f ->
    parse_symb symb_semicolon >>$ f) <?> "Precondition") st

(* property: PROPERTY; COLON; tf = formula; SEMICOLON { tf } *)
let parse_property st =
  ( parse_symb keyw_property >>
    parse_symb symb_colon >>
    Form.parse >>= (fun tf ->
    parse_symb symb_semicolon >>$ tf) <?> "Property") st

(* fields; p = precondition; tf = property; cmd = command; EOF { (p, cmd, tf) } *)
let parse st =
  ( parse_fields >>
    parse_precondition >>= (fun p ->
    parse_property >>= (fun tf ->
    While.Program.Cmd.parse >>= (fun cmd ->
    eof >>$
    let p = Seplog.Form.complete_tags Tags.empty p in
    let theta = Tagpairs.mk_free_subst Tags.empty (Seplog.Form.tags p) in
    let p = Seplog.Form.subst_tags theta p in
    let tf = Form.complete_tags (Seplog.Form.tags p) tf in
    (p,cmd,tf)))) <?> "program") st

let of_channel c =
  handle_reply (parse_channel parse c ())
