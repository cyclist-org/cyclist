open Lib
open Symbols
open MParser

type state =
  {lhs: Sl_heap.t option; rhs: Sl_heap.t option; defs: Sl_preddef.t list}

let initial = {lhs= None; rhs= None; defs= []}

let skip_until ch =
  skip_many_satisfy (fun x -> not (Char.equal x ch)) << spaces <?> "skip_until"

let skip_comment =
  Tokens.semi
  >> skip_many_chars_until any_char newline
  >> spaces |>> ignore <?> "Comment"

let space = space |>> ignore <|> skip_comment

let spaces = many space

let spaces1 = many1 space

let parens p = Tokens.parens (p << spaces) <?> "parens"

let skip_label label =
  Tokens.parens (Tokens.skip_symbol label >> skip_until ')') <?> "skip_label"

let skip_brexp =
  char '(' >> skip_until ')' >> char ')' >> spaces |>> ignore <?> "skip_brexp"

let skip_ident =
  let rexp = MParser_PCRE.make_regexp "[a-zA-Z][_0-9a-zA-Z]*" in
  MParser_PCRE.regexp rexp |>> ignore <?> "skip_ident"

let skip_types =
  let skip_ctr = parens (skip_ident >> spaces1 >> skip_ident) in
  let skip_ctr_decl = parens (skip_ident >> spaces >> many skip_ctr) in
  let skip_ctr_decls = parens (many1 skip_ctr_decl) in
  parens
    ( Tokens.skip_symbol "declare-datatypes"
    >> spaces
    >> parens (many skip_brexp)
    >> spaces
    >> parens (many skip_ctr_decls)
    |>> ignore )
  <?> "skip_types"

let skip_heap =
  parens
    (Tokens.skip_symbol "declare-heap" >> spaces >> many skip_brexp |>> ignore)
  <?> "skip_heap"

let parse_exp =
  attempt Sl_term.parse
  <|> parens (Tokens.skip_symbol "as" >> spaces >> Sl_term.parse << skip_ident)
  <?> "parse_exp"

let parse_pred =
  parens
    ( Sl_predsym.parse
    >>= fun predsym ->
    many parse_exp
    >>= fun params ->
    let tpred = (Tags.anonymous, (predsym, params)) in
    return (Sl_heap.mk_ind tpred) )
  <?> "parse_pred"

let parse_emp =
  parens
    ( Tokens.skip_symbol "_" >> spaces >> Tokens.skip_symbol "emp" >> spaces
    >> skip_ident >> spaces >> skip_ident >>$ Sl_heap.empty )
  <?> "parse_emp"

let parse_eq =
  parens
    ( Tokens.skip_symbol "=" >> spaces >> parse_exp
    >>= fun l -> spaces >> parse_exp >>= fun r -> return (Sl_heap.mk_eq (l, r))
    )
  <?> "parse_eq"

let parse_pto =
  parens
    ( Tokens.skip_symbol "pto" >> spaces >> parse_exp
    >>= fun lval ->
    parens (skip_ident >> spaces >> many1 parse_exp)
    >>= fun rvals -> return (Sl_heap.mk_pto (lval, rvals)) )
  <?> "parse_pto"

let parse_deq =
  parens
    ( Tokens.skip_symbol "distinct"
    >> spaces >> parse_exp
    >>= fun lhs -> parse_exp >>= fun rhs -> return (Sl_heap.mk_deq (lhs, rhs))
    )
  <?> "parse_deq"

let rec parse_binop op_str op st =
  parens
    ( Tokens.skip_symbol op_str >> spaces >> parse_heap
    >>= fun lhs ->
    spaces >> parse_heap
    >>= fun rhs1 ->
    many parse_heap
    >>= fun rhss ->
    let final_rhs = List.fold_left op rhs1 rhss in
    return (op lhs final_rhs) <?> "parse_binop" )
    st

and parse_and st = parse_binop "and" Sl_heap.star st

and parse_sep st = parse_binop "sep" Sl_heap.star st

and parse_exists st =
  parens
    ( Tokens.skip_symbol "exists"
    >> spaces
    >> parens (many1 (parens (Sl_term.parse << spaces << skip_ident)))
    >>= fun existentials ->
    spaces >> parse_heap
    >>= fun heap ->
    let allvars = Sl_heap.vars heap in
    let theta =
      Sl_subst.mk_ex_subst allvars (Sl_term.Set.of_list existentials)
    in
    return (Sl_heap.subst theta heap) <?> "parse_exists" )
    st

and parse_heap st =
  ( attempt parse_emp <|> attempt parse_eq <|> attempt parse_deq
  <|> attempt parse_pto <|> attempt parse_and <|> attempt parse_sep
  <|> attempt parse_exists <|> parse_pred <?> "parse_heap" )
    st

let parse_or =
  parens (Tokens.skip_symbol "or" >> spaces >> many1 parse_heap) <?> "parse_or"

let parse_body = attempt parse_heap >>= (fun h -> return [h]) <|> parse_or

let parse_def =
  parens
    ( Tokens.skip_symbol "define-fun-rec"
    >> spaces >> Sl_predsym.parse << spaces
    >>= fun predsym ->
    parens (many (parens (Sl_term.parse << spaces << skip_ident)))
    >>= fun params ->
    let pred = (predsym, params) in
    Tokens.skip_symbol "Bool" >> spaces >> parse_body
    >>= fun heaps ->
    let rules = List.map (fun h -> Sl_indrule.mk h pred) heaps in
    let preddef = Sl_preddef.mk (rules, predsym) in
    update_user_state (fun ({defs} as state) ->
        {state with defs= preddef :: defs} ) )
  <?> "parse_def"

let parse_def_preds =
  let parse_def_pred =
    parens
      ( Sl_predsym.parse << spaces
      >>= fun predsym ->
      parens (many (parens (Sl_term.parse << spaces << skip_ident)))
      << spaces << Tokens.skip_symbol "Bool"
      >>= fun params -> return (predsym, params) )
  in
  parens (many1 parse_def_pred) <?> "parse_def_preds"

let parse_bodies = parens (many1 (parse_body << spaces)) <?> "parse_bodies"

let parse_defs =
  parens
    ( Tokens.skip_symbol "define-funs-rec"
    >> spaces >> parse_def_preds
    >>= fun preds ->
    spaces >> parse_bodies
    >>= fun bodies ->
    let preddefs =
      List.map2
        (fun ((predsym, _) as pred) heaps ->
          let rules = List.map (fun h -> Sl_indrule.mk h pred) heaps in
          Sl_preddef.mk (rules, predsym) )
        preds bodies
    in
    update_user_state (fun ({defs} as state) ->
        {state with defs= preddefs @ defs} ) )
  <?> "parse_defs"

let parse_lhs =
  parens
    ( Tokens.skip_symbol "assert"
    >> spaces >> parse_heap
    >>= fun h -> update_user_state (fun state -> {state with lhs= Some h}) )

let parse_rhs =
  parens
    ( Tokens.skip_symbol "assert"
    >> spaces
    >> parens
         ( Tokens.skip_symbol "not" >> spaces >> parse_heap
         >>= fun h -> update_user_state (fun state -> {state with rhs= Some h})
         ) )

let skip_item =
  attempt (skip_label "set-logic")
  <|> attempt (skip_label "set-info")
  <|> attempt (skip_label "declare-sort")
  <|> attempt skip_types <|> attempt skip_heap
  <|> attempt (skip_label "check-sat")
  <|> attempt (skip_label "declare-const")

let parse_item =
  attempt parse_def <|> attempt parse_defs <|> skip_item << spaces

let parse_seq_part =
  attempt parse_lhs <|> attempt parse_rhs <|> skip_item << spaces

let parse_entl =
  many parse_item >> many parse_seq_part >> eof >> get_user_state

let of_channel c =
  match handle_reply (MParser.parse_channel parse_entl c initial) with
  | {defs; lhs= Some l; rhs= Some r} ->
      let l, r =
        ( Sl_form.with_heaps Sl_form.empty [l]
        , Sl_form.with_heaps Sl_form.empty [r] )
      in
      let tags = Tags.union (Sl_form.tags l) (Sl_form.tags r) in
      let l' = Sl_form.complete_tags tags l in
      let inst_subst =
        Tagpairs.mk_free_subst tags (Tags.diff (Sl_form.tags l') tags)
      in
      let l' = Sl_form.subst_tags inst_subst l' in
      let r' = Sl_form.complete_tags tags r in
      ((l', r'), Sl_defs.of_list defs)
  | _ -> assert false

let parse_sat =
  many parse_item >> parse_seq_part >> many parse_item >> eof >> get_user_state

let defs_of_channel c =
  match handle_reply (MParser.parse_channel parse_sat c initial) with
  | {defs; lhs= Some h} ->
      (Sl_defs.of_list defs, Sl_form.with_heaps Sl_form.empty [h])
  | _ -> assert false
