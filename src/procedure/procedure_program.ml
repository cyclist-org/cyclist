
open Lib
open Symbols
open MParser
open Parsers

module SH = Sl_heap

let termination = ref false

module Field = While_program.Field
module Cond = While_program.Cond
module Cmd = While_program.Cmd

exception WrongCmd = While_program.WrongCmd

module Proc =
  struct
    
    type t = string * Sl_term.t Blist.t * (Sl_form.t * Sl_form.t) list * Cmd.t
    
    let get_name ((id, _, _, _) : t) = id
    let get_params ((_, params, _, _) : t) = params
    let get_spec_list ((_, _, specs, _) : t) = specs
    (* let get_precondition ((_, _, pre, _, _) : t) = pre    *)
    (* let get_postcondition ((_, _, _, post, _) : t) = post *)
    let get_body ((_, _, _, body) : t) = body
    
    let get_seqs ((id, params, specs, _) : t) = 
      let cmd = Cmd.mk_proc_call id params in
      Blist.map (fun (pre, post) -> (pre, cmd, post)) specs
      
    (* precondition: PRECONDITION; COLON; f = formula; SEMICOLON { f } *)
    let parse_precondition st = While_program.parse_precondition st

    (* postcondition: POSTCONDITION; COLON; f = formula; SEMICOLON { f } *)
    let parse_postcondition st = 
      ( parse_symb keyw_postcondition >>
        parse_symb symb_colon >>
        Sl_form.parse >>= (fun f ->
        parse_symb symb_semicolon >>$ f) <?> "Postcondition") st

    let ensure_tags (pre, post) =
      let tags = Tags.union (Sl_form.tags pre) (Sl_form.tags post) in
      let pre' = Sl_form.complete_tags tags pre in
      let inst_subst = 
        Tagpairs.mk_free_subst 
          tags 
          (Tags.diff (Sl_form.tags pre') tags) in
      let pre' = Sl_form.subst_tags inst_subst pre' in
      let post' = Sl_form.complete_tags tags post in
      (pre', post')

    let check_spec (params, body) (pre, post) =
      (* - parameters mentioned in the postcondition are not assigned to  *)
      (*   in the procedure body;                                         *)
      assert(Sl_term.Set.is_empty (Sl_term.Set.inter (Sl_form.vars post) (Sl_term.Set.inter (Cmd.modifies ~strict:false body) (Sl_term.Set.of_list params))));
      (* - local variables are not mentioned in the pre/post;             *)
      assert(Sl_term.Set.is_empty (Sl_term.Set.inter (Sl_form.vars pre) (Cmd.locals (Sl_term.Set.of_list params) body)));
      assert(Sl_term.Set.is_empty (Sl_term.Set.inter (Sl_form.vars post) (Cmd.locals (Sl_term.Set.of_list params) body)));
      (* - existential variables are disjoint between pre and post;       *)
      (* assert(Sl_term.Set.is_empty (Sl_term.Set.inter (Sl_term.Set.filter Sl_term.is_exist_var (Sl_form.vars pre)) (Sl_term.Set.filter Sl_term.is_exist_var (Sl_form.vars post)))); *)
      (* assert(Tags.is_empty (Tags.inter (Tags.filter Tags.is_exist_var (Sl_form.tags pre)) (Tags.filter Tags.is_exist_var (Sl_form.tags post))));                                   *)
      (* - that the unversal variables of the post are a subset of those  *)
      (*   of the pre *)
      (* assert(Sl_term.Set.subset (Sl_term.Set.filter Sl_term.is_free_var (Sl_form.terms post)) (Sl_term.Set.filter Sl_term.is_free_var (Sl_form.terms pre))); *)
      (* assert(Tags.subset (Tags.filter Tags.is_free_var (Sl_form.tags post)) (Tags.filter Tags.is_free_var (Sl_form.tags pre)));                              *)
      ()

    let parse_named st =
      let parse_params st =
      let rec parse_params' acc st =
      let tail st =
      let try_parse_next_param check msg st =
        (   look_ahead(Sl_term.parse >>= (fun p ->
              if (check p) then zero else return ()))
        <|> fail msg) st in
      ((followed_by Sl_term.parse "") >>
      (try_parse_next_param (fun p -> Sl_term.is_nil p) "Not a formal parameter") >>
      (try_parse_next_param (fun p -> Sl_term.is_exist_var p)
        "Not a formal parameter - must not be primed (')") >>
      (try_parse_next_param (fun p -> List.mem p acc) "Duplicate parameter") >>
      Sl_term.parse >>= (fun p -> parse_params' (p::acc))) st in
      (   (if (List.length acc == 0) then tail else ((parse_symb symb_comma) >> tail))
      <|> (return (Blist.rev acc)) ) st in
      parse_params' [] st in
      (parse_symb keyw_proc >>
      parse_ident >>= (fun id ->
      (Tokens.parens parse_params) >>= (fun params ->
      let spec_parser st =
        ( parse_precondition >>= (fun pre ->
          parse_postcondition >>= (fun post -> return (ensure_tags (pre, post))))) st in
      many1 spec_parser >>= (fun specs ->
      Tokens.braces
        (expect_before Cmd.parse (parse_symb symb_rb) "Expecting CmdList") >>=
      (fun body ->
      let () = Blist.iter (check_spec (params, body)) specs in
      (* let specs =                                                        *)
      (*   Blist.bind                                                       *)
      (*   (fun (pre, post) ->                                              *)
      (*     Blist.map (Fun.swap Pair.mk post) (Sl_form.all_symheaps pre))  *)
      (*   specs in                                                         *)
      return (id, params, specs, body) <?> "Procedure" ))))) st
    
    let parse_unnamed st = 
      (parse_precondition >>= (fun pre ->
      parse_postcondition >>= (fun post ->
      (* let f v = Sl_term.is_exist_var v in *)
      (* assert(Sl_term.Set.is_empty (Sl_term.Set.inter (Sl_term.Set.filter f (Sl_form.vars pre)) (Sl_term.Set.filter f (Sl_form.vars post)))); *)
      Cmd.parse >>= 
      (fun body ->
        let (pre, post) = ensure_tags (pre, post) in
        return (pre, body, post)))) <?> "CmdList") st
    
  end

let program_pp fmt cmd =
  Format.fprintf fmt "%a@\n%a" Field.pp () (Cmd.pp 0) cmd

let pp_cmd fmt cmd =
  Cmd.pp ~abbr:true 0 fmt cmd

module Seq =
  struct
    type t = Sl_form.t * Cmd.t * Sl_form.t

    let tagset_one = Tags.singleton Tags.anonymous
		let tagpairs_one = Tagpairs.mk tagset_one
    let form_tags f = if !termination then Sl_form.tags f else tagset_one
    let tags (pre, _, _) = form_tags pre
    let all_tags (pre, _, post) = Tags.union (Sl_form.tags pre) (Sl_form.tags post)
    let tag_pairs (pre, _, _) = Tagpairs.mk (form_tags pre)

		let vars (pre,_,post) = 
      Sl_term.Set.union (Sl_form.vars pre) (Sl_form.vars post)
      
    let all_vars (pre, cmd, post) =
      Sl_term.Set.union_of_list [
          Sl_form.vars pre ;
          Cmd.vars cmd ;
          Sl_form.vars post ;
        ]
		
		let terms (pre,_,post) = 
      Sl_term.Set.union (Sl_form.terms pre) (Sl_form.terms post)

		let subst theta (pre,cmd,post) = 
			(Sl_form.subst theta pre, cmd, Sl_form.subst theta post)
      
    let subst_tags tps (pre, cmd, post) =
      ((Sl_form.subst_tags tps pre), cmd, (Sl_form.subst_tags tps post))
      
    let param_subst theta (pre, cmd, post) = 
      (Sl_form.subst theta pre, Cmd.subst theta cmd, Sl_form.subst theta post)
      
    let with_pre (_, cmd, post) pre = (pre, cmd, post)
    let with_post (pre, cmd, _) post = (pre, cmd, post)
    let with_cmd (pre, _, post) cmd = (pre, cmd, post)
    
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
      Sl_form.subsumed post post' 
       
    let subsumed_upto_tags (pre,cmd,post) (pre',cmd',post') = 
      Cmd.equal cmd cmd' &&
      Sl_form.subsumed_upto_tags pre' pre &&
      Sl_form.subsumed_upto_tags post post' 
    
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
      Sl_form.equal post post'
      
    let equal_upto_tags (pre, cmd, post) (pre', cmd', post') = 
      Cmd.equal cmd cmd' && 
      Sl_form.equal_upto_tags pre pre' && 
      Sl_form.equal_upto_tags post post'
      
    let dest (pre, cmd, post) = (Sl_form.dest pre, cmd, Sl_form.dest post)
    
    let get_tracepairs (pre, _, _) (pre', _, _) =
      let tps = Sl_form.get_tracepairs pre pre' in
      Pair.map (Tagpairs.filter (fun (t, _) -> Tags.is_free_var t)) tps
    
    let frame f (pre, cmd, post) = 
      ( Sl_form.star ~augment_deqs:false pre f,
        cmd,
        Sl_form.star ~augment_deqs:false post f)
    
  end

let program_vars = ref Sl_term.Set.empty

let set_program ((_, cmd, _), procs) =
  program_vars := Blist.foldl 
    (fun vars (_, _, _, body) -> Sl_term.Set.union vars (Cmd.vars body)) 
    (Cmd.vars cmd) 
    (procs)

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

(* procedures *)
let parse_procs st =
  let sigs_equiv (id, params, _,_) (id', params', _,_) = id=id' && (Blist.length params) = (Blist.length params') in
  ( many Proc.parse_named |>> (fun procs ->
    Blist.foldr
      (fun p ps ->
        assert(not (Blist.exists (sigs_equiv p) ps));
        p::ps)
      procs []) ) st
  
(* main: p = precondition; q = postcondition; cmd = command; EOF { (p, cmd, q) } *)
let parse_main st =
  ( Proc.parse_unnamed << eof <?> "Main procedure") st

(* fields; procs = procedures; main = main  { (main, procs) } *)
let parse st = 
  ( parse_fields >>
    parse_procs >>= (fun procs ->
    parse_main |>> (fun main -> (main, procs))) <?> "Program") st

let of_channel c =
  handle_reply (parse_channel parse c ())
