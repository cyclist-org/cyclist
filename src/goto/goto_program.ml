open Util
open Lib
open Symheap
open Symbols
open MParser

type field_t = string
type label_t = int

exception WrongCmd
module Cmd =
  struct
    type cond =
      | Eq of Term.t * Term.t
      | Deq of Term.t * Term.t
      | Non_det
    
    let mk_eq e1 e2 = Eq(e1,e2)
    let mk_deq e1 e2 = Deq(e1,e2)
    let mk_non_det () = Non_det

    let is_deq = function
      | Deq(_, _) -> true
      | Eq _ | Non_det -> false
    let is_non_det = function
      | Non_det -> true
      | Eq _ | Deq _ -> false
    
    let dest_cond = function
      | Eq(e1, e2)
      | Deq(e1, e2) -> (e1,e2)
      | Non_det -> raise WrongCmd
    
    let parse_cond st =
      ( attempt (parse_symb symb_star >>$ mk_non_det ()) <|>
        attempt (UF.parse |>> Fun.uncurry mk_eq) <|>
        attempt (Deqs.parse |>> Fun.uncurry mk_deq) <?> "Cond") st
      
    type t =
      | Assign of Term.t * Term.t
      | Load of Term.t * Term.t * field_t
      | Store of Term.t * field_t * Term.t
      | New of Term.t
      | Free of Term.t
      | Goto of label_t
      | If of cond * label_t
      | Stop
      | Skip

    let mk_assign x e = Assign(x,e)
    let mk_load x e s = Load(x,e,s)
    let mk_store e1 s e2 = Store(e1,s,e2)
    let mk_new x = New(x)
    let mk_free e = Free(e)
    let mk_goto l = Goto(l)
    let mk_if c l = If(c,l)
    let mk_stop = Stop
    let mk_skip = Skip
    
    let parse st = 
  (*   | STOP { P.Cmd.mk_stop }                                                     *)
      ( attempt (parse_symb keyw_stop >>$ mk_stop)
        <|>
  (* | SKIP { P.Cmd.mk_skip }                                                       *)
        attempt (parse_symb keyw_skip >>$ mk_skip)
        <|>
    (* | v1 = var; ASSIGN; v2 = var; POINTS_TO; id = IDENT { P.Cmd.mk_load v1 v2 id } *)
        attempt (Term.parse >>= (fun v1 ->
        parse_symb symb_assign >>
        Term.parse >>= (fun v2 ->
        parse_symb symb_pointsto >>
        parse_ident >>= (fun id ->
        return (assert (Term.is_var v1 && Term.is_var v2) ; mk_load v1 v2 id)))))
        <|>
  (*   | v = var; POINTS_TO; id = IDENT; ASSIGN; t = term { P.Cmd.mk_store v id t } *)
        attempt (Term.parse >>= (fun v ->
        parse_symb symb_pointsto >>
        parse_ident >>= (fun id ->
        parse_symb symb_assign >>
        Term.parse >>= (fun t ->
        return (assert (Term.is_var v) ; mk_store v id t)))))
        <|>
  (*   | FREE; ts = paren_terms                                                     *)
  (*   { assert (List.length ts = 1) ; P.Cmd.mk_free (List.hd ts) }                 *)
        attempt (parse_symb keyw_free >>
        Tokens.parens Term.parse >>= (fun v ->
        return (assert (Term.is_var v) ; mk_free v)))
        <|>
  (*   | v = var; ASSIGN; NEW; LP; RP { P.Cmd.mk_new v }                            *)
        attempt (Term.parse >>= (fun v ->
        parse_symb keyw_new >>
        parse_symb symb_lp >>
        parse_symb symb_rp >>
        return (assert (Term.is_var v) ; mk_new v)))
        <|>
  (*   | GOTO; n = NUM { P.Cmd.mk_goto n }                                          *)
        attempt (parse_symb keyw_goto >>
        Tokens.integer >>= (fun n ->
        return (mk_goto n)))
        <|>
  (*   | IF; c = condition; GOTO; n = NUM { P.Cmd.mk_if c n }                       *)
        attempt (parse_symb keyw_if >> 
        parse_cond >>= (fun c -> 
        parse_symb keyw_goto >>
        Tokens.integer >>= (fun n ->
        return (mk_if c n))))
        <|>
    (* | v = var; ASSIGN; t = term { P.Cmd.mk_assign v t } *)
        attempt (Term.parse >>= (fun v -> 
        parse_symb symb_assign >> 
        Term.parse >>= (fun t -> 
        return (assert (Term.is_var v) ; mk_assign v t)))) 
      <?> "Cmd") st


    let is_assign = function
      | Assign _ -> true
      | _ -> false
    let is_load _ = function
      | Load _ -> true
      | _ -> false
    let is_store = function
      | Store _ -> true
      | _ -> false
    let is_new = function
      | New _ -> true
      | _ -> false
    let is_free = function
      | Free _ -> true
      | _ -> false
    let is_goto = function
      | Goto _ -> true
      | _ -> false
    let is_if = function
      | If _ -> true
      | _ -> false
    let is_stop = function
      | Stop -> true
      | _ -> false
    let is_skip = function
      | Skip -> true
      | _ -> false


    let dest_assign = function
      | Assign(x,e) -> (x,e)
      | _ -> raise WrongCmd
    let dest_load = function
      | Load(x,e,s) -> (x,e,s)
      | _ -> raise WrongCmd
    let dest_store = function
      | Store(e1,s,e2) -> (e1,s,e2)
      | _ -> raise WrongCmd
    let dest_new = function
      | New(x) -> x
      | _ -> raise WrongCmd
    let dest_free = function
      | Free(e) -> e
      | _ -> raise WrongCmd
    let dest_goto = function
      | Goto(l) -> l
      | _ -> raise WrongCmd
    let dest_if = function
      | If(c,l) -> (c,l)
      | _ -> raise WrongCmd
    let dest_stop = function
      | Stop -> ()
      | _ -> raise WrongCmd
    let dest_skip = function
      | Skip -> ()
      | _ -> raise WrongCmd
    let dest_deref = function
      | Load(x,e,s) -> e
      | Store(e1,s,e2) -> e1
      | Free(e) -> e
      | _ -> raise WrongCmd

    let vars = function
      | Assign(x,e) | Load(x,e,_) | Store(x,_,e) ->
        Term.filter_vars (Term.Set.of_list [x; e])
      | New(x) | Free(x) ->
        Term.Set.singleton x
      | If(c,_) ->
        if is_non_det c then Term.Set.empty else
        let (e1,e2) = dest_cond c in
        Term.filter_vars (Term.Set.of_list [e1; e2])
      | Goto _ | Stop | Skip -> Term.Set.empty
  end

type lab_cmds = (label_t * Cmd.t) list
type fields = (string * int) list
type program_t = fields * lab_cmds


module Seq =
  struct
    type t = Form.t * int
    let tags (f,_) = Form.tags f
    let vars (l,_) = Form.vars l
    let terms (l,_) = Form.terms l
    let subst theta (l,i) = (Form.subst theta l, i)
    let to_string (f,i) =
      (Form.to_string f) ^ " |-_" ^ (string_of_int i) ^ " !"
    let to_melt (f,i) =
      Latex.concat
      [ Form.to_melt f;
        Latex.index symb_turnstile.melt (Latex.text (string_of_int i)) ]

    let subsumed_wrt_tags tags (l,i) (l',i') =
      i = i' && Form.spw_subsumed_wrt_tags tags l' l
    (*  s' *)
    (* ___ *)
    (*  s  *)
    let uni_subsumption ((l,i) as s) ((l',i') as s') =
      if i<>i' then None else
      let tags = Tags.inter (tags s) (tags s') in
      let valid theta' =
        if Term.Map.exists
          (fun k v -> Term.is_univ_var k && not (Form.equates l k v)) theta'
          then None else
        let s'' = subst theta' s' in
        let tags' = Tags.fold
          ( fun t acc ->
            let new_acc = Tags.add t acc in
            if subsumed_wrt_tags new_acc s s'' then new_acc else acc
          ) tags Tags.empty in
        if not (Tags.is_empty tags') then Some theta' else None in
      Form.spw_left_subsumption valid Term.empty_subst l' l

    let pp fmt (f,i) =
      Format.fprintf fmt "@[%a |-_%i@]" Symheap.Form.pp f i

    let equal (f,i) (f',i') = (i=i') && Symheap.Form.equal f f'
    
    let parse st = 
      ( Form.parse >>= (fun f ->
        parse_symb symb_turnstile_underscore >>
        Tokens.integer >>= (fun i ->
        parse_symb symb_bang >>$ (f,i))) <?> "GotoSeq") st
  end

let max_prog_var = ref Term.nil
let program_vars = ref Term.Set.empty
let local_vars = ref Term.Set.empty
let set_local_vars seq = local_vars := Term.Set.diff !program_vars (Seq.vars seq)

let program = ref (([], []):program_t)

let get_sel_index s =
  require (fun () -> Blist.mem_assoc s (fst !program)) ;
  Blist.assoc s (fst !program)

let get_no_fields () = Blist.length (fst !program)

let get_no_lines () = Blist.length (snd !program)

let set_program p =
  program := p ;
  let cmds = Blist.map snd (snd !program) in
  max_prog_var :=
    Term.fresh_uvar (Term.Set.union_of_list (Blist.map Cmd.vars cmds)) ;
  let cmds = Blist.map snd (snd !program) in
  program_vars := Term.Set.union_of_list (Blist.map Cmd.vars cmds)

let get_cmd i = snd (Blist.nth (snd !program) i)

let vars_of_program () = !program_vars

let is_local_var v = Term.Set.mem v !local_vars


(* remember prog vars when introducing fresh ones *)
let fresh_uvar s = Term.fresh_uvar (Term.Set.add !max_prog_var s)
let fresh_uvars s i = Term.fresh_uvars (Term.Set.add !max_prog_var s) i
let fresh_evar s = Term.fresh_evar (Term.Set.add !max_prog_var s)
let fresh_evars s i = Term.fresh_evars (Term.Set.add !max_prog_var s) i

(* again, treat prog vars as special *)
let freshen_case_by_seq seq case =
  Case.freshen (Term.Set.union (vars_of_program ()) (Seq.vars seq)) case

let parse_lcmd st = 
  ( Tokens.integer >>= (fun i ->
    parse_symb symb_colon >>
    Cmd.parse >>= (fun c -> return (i,c))) <?> "GotoLcmd" ) st
    
let parse_lcmds st =
  ( sep_by1 parse_lcmd (parse_symb symb_semicolon) <?> "GotoLcmds" ) st

let parse_fields st = 
  ( parse_symb keyw_fields >> 
    parse_symb symb_colon >>
    sep_by1 parse_ident (parse_symb symb_comma) >>= (fun ils ->
    parse_symb symb_semicolon >>
    return (Blist.rev (Blist.combine ils (Blist.range 0 ils)))) <?> "GotoFields") st

(* judgement: JUDGEMENT; COLON; f = formula_; TURNSTILE_; n = NUM; BANG { (f, n) }   *)

let parse_judgment st =
  ( parse_symb keyw_judgement >>
    parse_symb symb_colon >>
    Form.parse >>= (fun f ->
    parse_symb symb_turnstile_underscore >>
    Tokens.integer >>= (fun n ->
    parse_symb symb_bang >>$ (f,n))) <?> "GotoJudgement" ) st

    (* f = fields; j = judgement; l = lcommands; EOF { (j, (f, l)) } *)

let parse st = 
  ( parse_fields >>= (fun f ->
    parse_judgment >>= (fun j ->
    parse_lcmds >>= (fun l -> 
    eof >> return (j, (f,l))))) <?> "GotoProgram") st

let of_channel c =
  handle_reply (MParser.parse_channel parse c ())
