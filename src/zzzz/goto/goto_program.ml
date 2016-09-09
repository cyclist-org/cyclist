open Util
open Lib
open Symbols
open MParser

type field_t = string
type label_t = int

exception WrongCmd
module Cmd =
  struct
    type cond =
      | Eq of Sl_term.t * Sl_term.t
      | Deq of Sl_term.t * Sl_term.t
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
        attempt (Sl_uf.parse |>> Fun.uncurry mk_eq) <|>
        attempt (Sl_deqs.parse |>> Fun.uncurry mk_deq) <?> "Cond") st
      
    type t =
      | Assign of Sl_term.t * Sl_term.t
      | Load of Sl_term.t * Sl_term.t * field_t
      | Store of Sl_term.t * field_t * Sl_term.t
      | New of Sl_term.t
      | Free of Sl_term.t
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
        attempt (Sl_term.parse >>= (fun v1 ->
        parse_symb symb_assign >>
        Sl_term.parse >>= (fun v2 ->
        parse_symb symb_pointsto >>
        parse_ident >>= (fun id ->
        return (assert (Sl_term.is_var v1 && Sl_term.is_var v2) ; mk_load v1 v2 id)))))
        <|>
  (*   | v = var; POINTS_TO; id = IDENT; ASSIGN; t = term { P.Cmd.mk_store v id t } *)
        attempt (Sl_term.parse >>= (fun v ->
        parse_symb symb_pointsto >>
        parse_ident >>= (fun id ->
        parse_symb symb_assign >>
        Sl_term.parse >>= (fun t ->
        return (assert (Sl_term.is_var v) ; mk_store v id t)))))
        <|>
  (*   | FREE; ts = paren_terms                                                     *)
  (*   { assert (List.length ts = 1) ; P.Cmd.mk_free (List.hd ts) }                 *)
        attempt (parse_symb keyw_free >>
        Tokens.parens Sl_term.parse >>= (fun v ->
        return (assert (Sl_term.is_var v) ; mk_free v)))
        <|>
  (*   | v = var; ASSIGN; NEW; LP; RP { P.Cmd.mk_new v }                            *)
        attempt (Sl_term.parse >>= (fun v ->
        parse_symb keyw_new >>
        parse_symb symb_lp >>
        parse_symb symb_rp >>
        return (assert (Sl_term.is_var v) ; mk_new v)))
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
        attempt (Sl_term.parse >>= (fun v -> 
        parse_symb symb_assign >> 
        Sl_term.parse >>= (fun t -> 
        return (assert (Sl_term.is_var v) ; mk_assign v t)))) 
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
        Sl_term.filter_vars (Sl_term.Set.of_list [x; e])
      | New(x) | Free(x) ->
        Sl_term.Set.singleton x
      | If(c,_) ->
        if is_non_det c then Sl_term.Set.empty else
        let (e1,e2) = dest_cond c in
        Sl_term.filter_vars (Sl_term.Set.of_list [e1; e2])
      | Goto _ | Stop | Skip -> Sl_term.Set.empty
  end

type lab_cmds = (label_t * Cmd.t) list
type fields = (string * int) list
type program_t = fields * lab_cmds


module Seq =
  struct
    include Pair.Make(Sl_form)(Int.T)
    
    let tags (f,_) = Sl_form.tags f
    let vars (l,_) = Sl_form.vars l
    let terms (l,_) = Sl_form.terms l
    let subst theta (l,i) = (Sl_form.subst theta l, i)
    let to_string (f,i) =
      (Sl_form.to_string f) ^ " |-_" ^ (string_of_int i) ^ " !"
    let to_melt (f,i) =
      Latex.concat
      [ Sl_form.to_melt f;
        Latex.index symb_turnstile.melt (Latex.text (string_of_int i)) ]

    let pp fmt (f,i) =
      Format.fprintf fmt "@[%a |-_%i@]" Sl_form.pp f i

    let equal (f,i) (f',i') = (i=i') && Sl_form.equal f f'
    
    let equal_upto_tags (f,i) (f',i') = (i=i') && Sl_form.equal_upto_tags f f'
    
    let parse st = 
      ( Sl_form.parse >>= (fun f ->
        parse_symb symb_turnstile_underscore >>
        Tokens.integer >>= (fun i ->
        parse_symb symb_bang >>$ (f,i))) <?> "GotoSeq") st
        
  end

let max_prog_var = ref Sl_term.nil
let program_vars = ref Sl_term.Set.empty
let local_vars = ref Sl_term.Set.empty
let set_local_vars seq = local_vars := Sl_term.Set.diff !program_vars (Seq.vars seq)

let program = ref (([], []):program_t)

let get_sel_index s =
  assert (Blist.mem_assoc s (fst !program)) ;
  Blist.assoc s (fst !program)

let get_no_fields () = Blist.length (fst !program)

let get_no_lines () = Blist.length (snd !program)

let set_program p =
  program := p ;
  let cmds = Blist.map snd (snd !program) in
  max_prog_var :=
    Sl_term.fresh_fvar (Sl_term.Set.union_of_list (Blist.map Cmd.vars cmds)) ;
  let cmds = Blist.map snd (snd !program) in
  program_vars := Sl_term.Set.union_of_list (Blist.map Cmd.vars cmds)

let get_cmd i = snd (Blist.nth (snd !program) i)

let vars_of_program () = !program_vars

let is_local_var v = Sl_term.Set.mem v !local_vars


(* remember prog vars when introducing fresh ones *)
let fresh_fvar s = Sl_term.fresh_fvar (Sl_term.Set.add !max_prog_var s)
let fresh_fvars s i = Sl_term.fresh_fvars (Sl_term.Set.add !max_prog_var s) i
let fresh_evar s = Sl_term.fresh_evar (Sl_term.Set.add !max_prog_var s)
let fresh_evars s i = Sl_term.fresh_evars (Sl_term.Set.add !max_prog_var s) i

(* again, treat prog vars as special *)
let freshen_case_by_seq seq case =
  Sl_indrule.freshen (Sl_term.Set.union (vars_of_program ()) (Seq.vars seq)) case

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
    Sl_form.parse >>= (fun f ->
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
