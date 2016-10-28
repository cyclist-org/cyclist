
open Lib
open Symbols
open MParser

module SH = Sl_heap
module Tl_form = Tl_form.Form

let termination = ref true

module Field = While_program.Field
module Cond = While_program.Cond

exception WrongCmd = While_program.WrongCmd

let is_prog_var v = Sl_term.is_free_var v
let is_prog_term t = Sl_term.is_nil t || is_prog_var t

module Cmd = While_program.Cmd
  
let program_pp fmt cmd =
  Format.fprintf fmt "%a@\n%a" Field.pp () (Cmd.pp 0) cmd

let pp_cmd fmt cmd =
  Cmd.pp ~abbr:true 0 fmt cmd


module Seq =
  struct
    type t = Sl_form.t * Cmd.t * Tl_form.t

    let tagset_one = Tags.singleton Tags.anonymous
    let tagpairs_one = Tagpairs.mk tagset_one
    let tags (sf,cmd,tf) = Tags.union (Sl_form.tags sf) (Tl_form.tags tf)
    let tag_pairs (sf,_,tf) = if !termination then Tagpairs.union (Sl_form.tag_pairs sf) (Tagpairs.mk (Tl_form.outermost_tag tf)) else (Tagpairs.mk (Tl_form.outermost_tag tf))
    let sep_vars (sf,_,_) = Sl_form.vars sf
    let temp_vars (_,_,tf) = Tl_form.vars tf
    let vars (sf,_,tf) = Sl_term.Set.union (Sl_form.vars sf) (Tl_form.vars tf)
    let terms (l,_) = Sl_form.terms l
    let subst theta (sf,cmd,tf) = (Sl_form.subst theta sf, cmd, tf)
    let to_string (sf,cmd,tf) =
      (Sl_form.to_string sf) ^ symb_turnstile.sep ^ (Cmd.to_string cmd) ^
	symb_colon.sep ^ (Tl_form.to_string tf)
    let to_melt (sf,cmd,tf) =
      ltx_mk_math
        (Latex.concat [ Sl_form.to_melt sf; symb_turnstile.melt; Cmd.to_melt cmd;
	symb_colon.melt; Tl_form.to_melt tf])

    let pp fmt (sf,cmd,tf) =
      Format.fprintf fmt "@[%a%s%a%s%a@]"
        Sl_form.pp sf symb_turnstile.sep (Cmd.pp ~abbr:true 0) cmd symb_colon.sep
	Tl_form.pp tf

    let equal (sf,cmd,tf) (sf',cmd',tf') = 
      Cmd.equal cmd cmd' && Sl_form.equal sf sf' && Tl_form.equal tf tf'

    let equal_upto_tags (sf,cmd,tf) (sf',cmd',tf') = 
      Cmd.equal cmd cmd' && 
      Sl_form.equal_upto_tags sf sf' && 
      Tl_form.equal_upto_tags tf tf'
								      
										      
    let subsumed (sf,cmd,tf) (sf',cmd',tf') = 
      Cmd.equal cmd cmd' &&
	(if !termination then Sl_form.subsumed else Sl_form.subsumed_upto_tags) 
          ~total:false  sf' sf
    let subsumed_upto_tags (sf,cmd,tf) (sf',cmd',tf') = 
      Cmd.equal cmd cmd' &&
	Sl_form.subsumed_upto_tags ~total:false sf' sf
	  
    let subst_tags tagpairs (sf,cmd,tf) = (Sl_form.subst_tags tagpairs sf, cmd,tf)
  end

let program_vars = ref Sl_term.Set.empty

let set_program p =
  program_vars := Cmd.vars p

let vars_of_program () = !program_vars

(* remember prog vars when introducing fresh ones *)
let fresh_fvar s = Sl_term.fresh_fvar (Sl_term.Set.union !program_vars s)
let fresh_fvars s i = Sl_term.fresh_fvars (Sl_term.Set.union !program_vars s) i
let fresh_evar s = Sl_term.fresh_evar (Sl_term.Set.union !program_vars s)
let fresh_evars s i = Sl_term.fresh_evars (Sl_term.Set.union !program_vars s) i

(* again, treat prog vars as special *)
let freshen_case_by_seq seq case =
  Sl_indrule.freshen (Sl_term.Set.union !program_vars (Seq.vars seq)) case

(* fields: FIELDS; COLON; ils = separated_nonempty_list(COMMA, IDENT); SEMICOLON  *)
(*     { List.iter P.Field.add ils }                                              *)
let parse_fields st = 
  ( parse_symb keyw_fields >>
    parse_symb symb_colon >>
    sep_by1 Field.parse (parse_symb symb_comma) >>= (fun ils ->
    parse_symb symb_semicolon >>$ List.iter Field.add ils) <?> "Fields") st

(* precondition: PRECONDITION; COLON; f = formula; SEMICOLON { f } *)
let parse_precondition st = 
  ( parse_symb keyw_precondition >>
    parse_symb symb_colon >>
    Sl_form.parse ~allow_tags:false >>= (fun f ->
    parse_symb symb_semicolon >>$ f) <?> "Precondition") st

(* property: PROPERTY; COLON; tf = formula; SEMICOLON { tf } *)
let parse_property st = 
  ( parse_symb keyw_property >>
    parse_symb symb_colon >>
    Tl_form.parse >>= (fun tf ->
    parse_symb symb_semicolon >>$ tf) <?> "Property") st

    (* fields; p = precondition; cmd = command; EOF { (p, cmd) } *)
let parse st = 
  ( parse_fields >>
    parse_precondition >>= (fun p ->
    parse_property >>= (fun tf ->
    Cmd.parse >>= (fun cmd ->
    eof >>$ 
    let p = Sl_form.complete_tags Tags.empty p in
    let theta = Tagpairs.mk_free_subst Tags.empty (Sl_form.tags p) in
    let p = Sl_form.subst_tags theta p in
    let tf = Tl_form.complete_tags (Sl_form.tags p) tf in
    (p,cmd,tf)))) <?> "program") st

let of_channel c =
  handle_reply (parse_channel parse c ())
