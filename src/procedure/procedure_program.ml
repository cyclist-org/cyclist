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

let main = "main"

module Proc =
  struct

    module K =
      struct
        type t = string * Sl_term.t Blist.t * (Sl_form.t * Sl_form.t) list * Cmd.t
        (** The type of procedures: a tuple of
              - Procedure name
              - Formal parameters
              - A list of pre/post specifications
              - The body of the procedure
        *)

        let compare (id,_,_,_) (id',_,_,_) = Strng.compare id id'
        let equal (id,_,_,_) (id',_,_,_) = Strng.equal id id'
        let hash (id,_,_,_) = Strng.hash id
        let pp_decl fmt (id, params, _,_) =
          Format.fprintf fmt "%s(%a)" id (Blist.pp pp_commasp Sl_term.pp) params
        let pp_specs fmt (_,_,specs,_) =
          let pp_spec fmt (pre, post) =
            Format.fprintf fmt "%s: %a@\n%s: %a"
              keyw_precondition.str Sl_form.pp pre keyw_postcondition.str Sl_form.pp post in
          Blist.pp Format.pp_print_newline pp_spec fmt specs
        let pp_head fmt ((id, params, specs, _) as proc) =
          if Blist.is_empty specs then
            pp_decl fmt proc
          else
            Format.fprintf fmt "%a@\n@[<1>%a@]" pp_decl proc pp_specs proc
        let pp fmt ((_,_,_, body) as proc) =
          Format.fprintf fmt "%a@\n{@\n@[<2>%a@]@\n}" pp_head proc (Cmd.pp ~abbr:true 0) body
        let to_string = mk_to_string pp
      end
    (** The BasicType kernel (N.B. Procs are equal if they have the same name) *)

    include K

    module Set = Treeset.Make(K)
    (* module Map = Treemap.Make(K) *)
    module SigMap = Treemap.Make(Pair.Make(Pair.Make(Strng)(Sl_term.FList))(Pair.Make(Sl_form)(Sl_form)))
    module Graph = Graph.Imperative.Digraph.ConcreteBidirectional(K)

    let get_name ((id, _, _, _) : t) = id
    let get_params ((_, params, _, _) : t) = params
    let get_spec_list ((_, _, specs, _) : t) = specs
    let get_body ((_, _, _, body) : t) = body

    let get_seqs ((id, params, specs, _) : t) =
      let cmd = Cmd.mk_proc_call id params in
      Blist.map (fun (pre, post) -> (pre, cmd, post)) specs

    let number_cmds ((id, params, specs, body) : t) = (id, params, specs, Cmd.number body)

    let get_dependencies ((id, _, _, body) : t) = Cmd.get_dependencies body

    (* precondition: PRECONDITION; COLON; f = formula; SEMICOLON { f } *)
    let parse_precondition st = While_program.parse_precondition ~allow_tags:true st

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

let pp_prog fmt (fields, procs) =
  Format.fprintf fmt "%a@\n@\n%a" Field.pp_fields fields (Blist.pp pp_dbl_nl Proc.pp) procs

let program_vars = ref Sl_term.Set.empty

(* Store a list as well as a map so the procedures can be printed in the order they were provided *)
let proc_list = ref []
let proc_map = ref Strng.Map.empty

module Operations = Graph.Oper.I(Proc.Graph)

let dependencies = ref (Proc.Graph.create ())
let reachability = ref (Proc.Graph.create ())

let pp fmt () = pp_prog fmt (Field.get_fields (), !proc_list)

let get_proc p = Strng.Map.find p !proc_map

let set_program (fields, procs) =
  Field.reset () ;
  Blist.iter Field.add fields ;
  proc_list := procs ;
  program_vars := Blist.foldl
    (fun vars (_, params, _, body) ->
      Sl_term.Set.union_of_list [ vars; (Sl_term.Set.of_list params); (Cmd.vars body); ])
    (Sl_term.Set.empty)
    (procs) ;
  proc_map :=
    Blist.fold_left
      (fun procs ((id,_,_,_) as p) -> Strng.Map.add id p procs)
      Strng.Map.empty
      procs ;
  Proc.Graph.clear !dependencies ;
  Blist.iter
    (fun p ->
      Proc.Graph.add_vertex !dependencies p ;
      Strng.Set.iter
        (fun p' -> Proc.Graph.add_edge !dependencies p (get_proc p'))
        (Proc.get_dependencies p))
    procs ;
  reachability := Operations.transitive_closure ~reflexive:true !dependencies

let get_reachable ps =
  let graph = Proc.Graph.copy !dependencies in
  let () =
    Strng.Map.iter
      (fun id p ->
        if not
          (Blist.exists
            (fun p' -> Proc.Graph.mem_edge !reachability (Strng.Map.find p' !proc_map) p)
            ps)
        then Proc.Graph.remove_vertex graph p)
      !proc_map in
  graph

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
    sep_by1 Field.parse (parse_symb symb_comma)
    <?> "Fields") st

(* procedures *)
let parse_procs st =
  ( many Proc.parse_named |>> (fun procs ->
    Blist.foldr
      (fun p ps ->
        assert(not (Blist.exists (Proc.equal p) ps));
        p::ps)
      procs []) ) st

(* main: p = precondition; q = postcondition; cmd = command; EOF { (p, cmd, q) } *)
let parse_main st =
  ( Proc.parse_unnamed << eof <?> "Main procedure") st

(* fields; procs = procedures; { (main, procs) } *)
let parse st =
  ( (parse_fields << (parse_symb symb_semicolon)) >>= (fun fields ->
    (parse_procs << eof) |>> (fun procs ->
    (fields, procs))) <?> "Program") st

let of_channel c =
  handle_reply (parse_channel parse c ())
