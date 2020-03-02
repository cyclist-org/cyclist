open Lib
open   Symbols

open Generic

open MParser

type eq_atom = Term.t * Term.t
type pred_atom = string * Term.t list
type ind_pred_atom = Tags.Elt.t * pred_atom

module AtomT =
  struct
    type t =
      | Eq of eq_atom
      | Deq of eq_atom
      | IndPred of ind_pred_atom
    
    let compare ?(match_tags=false) a a' =
      match (a,a') with
      | (Eq(x,y), Eq(x',y')) | (Deq(x,y), Deq(x',y')) ->
        (match Term.compare x x' with
          | 0 -> Term.compare y y'
          | n -> n)
      | (IndPred(tag,(id,terms)), IndPred(tag',(id',terms'))) ->
        let match_rest (id, terms) (id', terms') =
          match Strng.compare id id' with
            | 0 -> Term.FList.compare terms terms'
            | n -> n in
        (match Tags.Elt.compare tag tag' with
          | 0 -> match_rest (id, terms) (id', terms')
          | m -> m)
      | (Eq _ ,_) | (_, IndPred _) -> -1
      | (_, Eq _) | (IndPred _, _) -> 1

    let equal_upto_tags a a' = Int.equal (compare ~match_tags:true a a') 0
    let compare a a' = compare a a'
    
    let equal a a' = Int.equal (compare a a') 0

    let hash = Hashtbl.hash

    let to_string = function
      | Eq eq -> let (sx,sy) = Pair.map Term.to_string eq in sx ^ "=" ^ sy
      | Deq eq -> let (sx,sy) = Pair.map Term.to_string eq in sx ^ "!=" ^ sy
      | IndPred(t,(ident,tl)) ->
        ident ^ "_" ^ (Tags.Elt.to_string t) ^ "(" ^ (Term.FList.to_string tl) ^ ")"

    let pp fmt = function
      | Eq(x,y) -> Format.fprintf fmt "@[%a=%a@]" Term.pp x Term.pp y
      | Deq(x,y) -> Format.fprintf fmt "@[%a!=%a@]" Term.pp x Term.pp y
      | IndPred(t,(ident,args)) ->
        Format.fprintf fmt "@[%s_%a(%a)@]"
          ident Tags.Elt.pp t (Blist.pp pp_comma Term.pp) args

  end

include Containers.Make(AtomT)
include AtomT

let mk_eq x y = Eq(x,y)
let mk_deq x y = Deq(x,y)
let mk_ipred t ident tl = IndPred(t,(ident, tl))

let used_tags = ref Tags.empty

let parse_indpred st =
  (parse_ident >>= (fun pred ->
  option Tags.Elt.parse >>= (fun opt_tag ->
  Tokens.parens (Tokens.comma_sep1 Term.parse) << spaces >>= (fun arg_list ->
  let tag = match opt_tag with
  | Some tag ->
    used_tags := Tags.add tag !used_tags;
    tag
  | None ->
    let tag = Tags.fresh_fvar !used_tags in
    used_tags := Tags.add tag !used_tags;
    tag in
  return (mk_ipred tag pred arg_list)))) <?> "ind") st

let parse_eq st =
  ( Term.parse >>= (fun x ->
    parse_symb symb_eq >>
    Term.parse >>= (fun y ->
    return (mk_eq x y))) <?> "Eq") st

let parse_deq st =
  ( Term.parse >>= (fun x ->
    parse_symb symb_deq >>
    Term.parse >>= (fun y ->
    return (mk_eq x y))) <?> "Deq") st

let parse st =
  ( attempt parse_indpred
    <|>
    attempt parse_deq
    <|>
    parse_eq
    <?> "Atom"
  ) st

let dest_eq = function
  | Eq(x,y) -> (x,y)
  | _ -> invalid_arg "Atom.dest_eq"
let dest_deq = function
  | Deq(x,y) -> (x,y)
  | _ -> invalid_arg "Atom.dest_deq"
let dest_pred = function
  | IndPred(t,(ident,tl)) -> (t, ident, tl)
  | _ -> invalid_arg "Atom.dest_pred"

let is_eq = function
  | Eq _ -> true
  | _ -> false
let is_deq = function
  | Deq _ -> true
  | _ -> false
let is_ipred = function
  | IndPred _ -> true
  | _ -> false

let ipred_eq_mod_tags a a' = match (a,a') with
  | (IndPred(_,(ident,tl)), IndPred(_,(ident',tl'))) ->
    Strng.equal ident ident' && Term.FList.equal tl tl'
  | (_,_) -> false

let terms = function
  | Eq(sx,sy) | Deq(sx,sy) -> Term.Set.union (Term.terms sx) (Term.terms sy)
  | IndPred(_,(_,tl)) -> Term.Set.of_list tl

let vars a = Term.filter_vars (terms a)

let tag = function
  | Eq _ | Deq _ -> None
  | IndPred(i, _) -> Some i

let repl_tags t = function
  | IndPred(t',(ident,tl)) -> IndPred(t,(ident, tl))
  | any -> any

let strip_tags = function
  | IndPred(_,pred) -> pred
  | _ -> invalid_arg "Atom.strip_tags"

let subst theta = function
  | Eq p -> Eq (Pair.map (fun t -> Term.subst theta t) p)
  | Deq p -> Deq (Pair.map (fun t -> Term.subst theta t) p)
  | IndPred(t, (ident, tl)) -> IndPred(t, (ident, Term.subst_list theta tl))

let unify_ipreds theta a a' =
  let ((_, ident, args),(_, ident', args')) = Pair.map dest_pred (a,a') in
  if
    not (Strng.equal ident ident') ||
    not (Int.equal (Blist.length args) (Blist.length args'))
  then
    None
  else
    Term.unify_list theta args args'

let unify_deqs theta a a' =
  let (p,p') = Pair.map dest_deq (a,a') in Term.unify_pairs theta p p'

let unify_eqs theta a a' =
  let (p,p') = Pair.map dest_eq (a,a') in Term.unify_pairs theta p p'
