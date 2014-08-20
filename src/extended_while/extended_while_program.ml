open Util
open Lib
open Symbols
open MParser
open Parsers

module SH = Sl_heap

let termination = ref false

module Field = While_program.Field
exception WrongCmd = While_program.WrongCmd
module Cond = While_program.Cond
module Cmd = While_program.Cmd

module Proc =
  struct
    
    let add p = ()
    
    (* precondition: PRECONDITION; COLON; f = formula; SEMICOLON { f } *)
    let parse_precondition st = 
      ( parse_symb keyw_precondition >>
        parse_symb symb_colon >>
        Sl_form.parse >>= (fun f ->
        parse_symb symb_semicolon >>$ f) <?> "Precondition") st

    (* postcondition: POSTCONDITION; COLON; f = formula; SEMICOLON { f } *)
    let parse_postcondition st = 
      ( parse_symb keyw_postcondition >>
        parse_symb symb_colon >>
        Sl_form.parse >>= (fun f ->
        parse_symb symb_semicolon >>$ f) <?> "Postcondition") st

    (* let parse_named st =                                                                *)
    (*   let parse_params st =                                                             *)
    (*   let rec parse_params' acc st =                                                    *)
    (*   let tail st =                                                                     *)
    (*   let try_parse_next_param check msg st =                                           *)
    (*     (   look_ahead(Sl_term.parse >>= (fun p ->                                      *)
    (*           if (check p) then zero else return ()))                                   *)
    (*     <|> fail msg) st in                                                             *)
    (*   ((followed_by Sl_term.parse "") >>                                                *)
    (*   (try_parse_next_param (fun p -> Sl_term.is_nil p) "Not a formal parameter") >>    *)
    (*   (try_parse_next_param (fun p -> Sl_term.is_exist_var p)                           *)
    (*     "Not a formal parameter - must not be primed (')") >>                           *)
    (*   (try_parse_next_param (fun p -> List.mem p acc) "Duplicate parameter") >>         *)
    (*   Sl_term.parse >>= (fun p -> parse_params' (p::acc))) st in                        *)
    (*   (   (if (List.length acc == 0) then tail else ((parse_symb symb_comma) >> tail))  *)
    (*   <|> (return acc) ) st in                                                          *)
    (*   parse_params' [] st in                                                            *)
    (*   (parse_symb keyw_proc >>                                                          *)
    (*   parse_ident >>= (fun id ->                                                        *)
    (*   (Tokens.parens parse_params) >>= (fun params ->                                   *)
    (*   parse_precondition >>= (fun pre ->                                                *)
    (*   parse_postcondition >>= (fun post ->                                              *)
    (*   Tokens.braces                                                                     *)
    (*     (expect_before Cmd.parse (parse_symb symb_rb) "Expecting CmdList") |>>          *)
    (*   (fun body ->                                                                      *)
    (*     return (id, params, pre, post, body) <?> "Procedure" )))))) st                  *)
    
    let parse_unnamed st = 
      (parse_precondition >>= (fun pre ->
      parse_postcondition >>= (fun post ->
      Cmd.parse >>= 
      (fun body ->
        return (pre, body, post)))) <?> "CmdList") st
    
  end

let program_pp fmt cmd =
  Format.fprintf fmt "%a@\n%a" Field.pp () (Cmd.pp 0) cmd

let pp_cmd fmt cmd =
  Cmd.pp ~abbr:true 0 fmt cmd

module Seq =
  struct
    type t = Sl_form.t * Cmd.t * Sl_form.t

    let tagset_one = Tags.singleton 1
		let tagpairs_one = TagPairs.mk tagset_one
    let tags (pre,_,_) = if !termination then Sl_form.tags pre else tagset_one
    let tag_pairs f = TagPairs.mk (tags f)

		(* Do we want the vars from the postcondition as well, or not? *)
    (* Yes! (NG) *)
		let vars (pre,_,post) = Sl_term.Set.union (Sl_form.vars pre) (Sl_form.vars post)
		(* let vars (pre,_,_) = Sl_form.vars pre *)
		
		(* Do we want the vars from the postcondition as well, or not? *)
		let terms (pre,_,post) = Sl_term.Set.union (Sl_form.terms pre) (Sl_form.terms post)
		(* let terms (pre,_,_) = Sl_form.terms pre *)

		let subst theta (pre,cmd,post) = 
			(Sl_form.subst theta pre, cmd, Sl_form.subst theta post)
    
		let to_string (pre,cmd,post) =
      symb_turnstile.sep ^ 
			symb_lb.str ^ (Sl_form.to_string pre) ^ symb_rb.str ^ " " ^ 
			(Cmd.to_string cmd) ^
			symb_lb.str ^ (Sl_form.to_string post) ^ symb_rb.str
    
		let to_melt (pre,cmd,post) =
      ltx_mk_math
        (Latex.concat [ symb_turnstile.melt; 
				 								symb_lb.melt; Sl_form.to_melt pre; symb_rb.melt;
												Cmd.to_melt cmd;
												symb_lb.melt; Sl_form.to_melt post; symb_rb.melt ])

    let subsumed (pre,cmd,post) (pre',cmd',post') = 
      Cmd.equal cmd cmd' &&
      Sl_form.subsumed pre' pre &&
      Sl_form.subsumed_upto_tags post post' 
       
    let subsumed_upto_tags (pre,cmd,post) (pre',cmd',post') = 
      Cmd.equal cmd cmd' &&
      Sl_form.subsumed_upto_tags pre' pre &&
      Sl_form.subsumed_upto_tags post post' 
    
    (* let uni_subsumption ((pre,cmd,post) as s) ((pre',cmd',post') as s') = *)
      (* if not (Cmd.equal cmd cmd') then None else                                          *)
      (* let tags = Tags.inter (tags s) (tags s') in                                         *)
      (* let valid_pre theta =                                                               *)
      (*   if Sl_term.Map.exists                                                             *)
      (*     (fun k v -> Sl_term.is_univ_var k && not (Sl_form.equates pre k v)) theta       *)
      (*     then None                                                                       *)
      (*   else                                                                              *)
  		(* 		if not !termination then Some theta else                                        *)
      (*     let s'' = subst (theta, Sl_term.empty_subst) s' in                              *)
      (*     let tags' = Tags.fold                                                           *)
      (*       ( fun t acc ->                                                                *)
      (*         let new_acc = Tags.add t acc in                                             *)
      (*         if subsumed_wrt_tags new_acc s s'' then new_acc else acc                    *)
      (*       ) tags Tags.empty in                                                          *)
      (*     if not (Tags.is_empty tags') then Some theta else None                          *)
      (*   in                                                                                *)
      (* let valid_post theta =                                                              *)
      (*   if Sl_term.Map.exists                                                             *)
      (*     (fun k v -> Sl_term.is_univ_var k && not (Sl_form.equates post k v)) theta      *)
      (*     then None                                                                       *)
      (*   else Some theta                                                                   *)
      (*   in                                                                                *)
      (* let theta  = Sl_form.right_subsumption valid_pre Sl_term.empty_subst pre' pre in    *)
      (* let theta' = Sl_form.right_subsumption valid_post Sl_term.empty_subst post post' in *)
      (* match (theta, theta') with                                                          *)
      (*   | (None, _) -> None                                                               *)
      (*   | (_, None) -> None                                                               *)
      (*   | (Some theta, Some theta') -> Some (theta, theta')                               *)

    let pp fmt (pre,cmd,post) =
      Format.fprintf fmt "@[%s{%a}@ %a@ {%a}@]"
        symb_turnstile.sep
				Sl_form.pp pre 
				(Cmd.pp ~abbr:true 0) cmd
				Sl_form.pp post
        (* Tags.pp (tags seq) *)

    let equal (pre, cmd, post) (pre', cmd', post') = 
			Cmd.equal cmd cmd' && 
      Sl_form.equal pre pre' && 
      Sl_form.equal_upto_tags post post'
      
    let equal_upto_tags (pre, cmd, post) (pre', cmd', post') = 
      Cmd.equal cmd cmd' && 
      Sl_form.equal_upto_tags pre pre' && 
      Sl_form.equal_upto_tags post post'
    
    let subst_tags tagpairs (pre,cmd,post) = 
      (Sl_form.subst_tags tagpairs pre, cmd, post)
  end

let program_vars = ref Sl_term.Set.empty

let set_program p =
  program_vars := Cmd.vars p

let vars_of_program () = !program_vars

(* remember prog vars when introducing fresh ones *)
let fresh_uvar s = Sl_term.fresh_uvar (Sl_term.Set.union !program_vars s)
let fresh_uvars s i = Sl_term.fresh_uvars (Sl_term.Set.union !program_vars s) i
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

(* procedures *)
(* let parse_procs st =                                                                *)
(*   ( many Proc.parse_named >>= (fun procs -> return (List.iter Proc.add procs)) ) st *)
  
let parse_main st =
  ( Proc.parse_unnamed << eof <?> "Main procedure") st

(* fields; procs; p = precondition; q = postcondition; cmd = command; EOF { (p, cmd, q) } *)
let parse st = 
  ( parse_fields >>
    (* parse_procs >> *)
    parse_main <?> "Program") st

let of_channel c =
  handle_reply (parse_channel parse c ())
