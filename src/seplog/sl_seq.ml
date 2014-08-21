open Lib
open Util
open Symbols
open MParser

include PairTypes(Sl_form)(Sl_form)

let equal (l,r) (l',r') =
  Sl_form.equal l l' 
  &&
  Sl_form.equal_upto_tags r r'

let equal_upto_tags (l,r) (l',r') =
  Sl_form.equal_upto_tags l l' 
  &&
  Sl_form.equal_upto_tags r r'


let dest seq = Pair.map Sl_form.dest seq

let parse st =
  ( Sl_form.parse >>= (fun l ->
          parse_symb symb_turnstile >> Sl_form.parse >>= (fun r ->
                return (l, r))) <?> "Sequent") st

let of_string s =
  handle_reply (MParser.parse_string parse s ())

let to_string (l, r) =
  (Sl_form.to_string l) ^ symb_turnstile.sep ^ (Sl_form.to_string r)
let to_melt (l, r) =
  ltx_mk_math
    (Latex.concat [Sl_form.to_melt l; symb_turnstile.melt; Sl_form.to_melt r])

let pp fmt (l, r) =
  Format.fprintf fmt "@[%a %s@ %a@]" Sl_form.pp l symb_turnstile.str Sl_form.pp r

let terms (l, r) = Sl_term.Set.union (Sl_form.terms l) (Sl_form.terms r)
let vars seq = Sl_term.filter_vars (terms seq)

let tags seq = Sl_form.tags (fst seq)
let tag_pairs f = TagPairs.mk (tags f)

let subst theta seq = Pair.map (Sl_form.subst theta) seq

let subst_tags tagpairs (l,r) = (Sl_form.subst_tags tagpairs l, r)

(* (l',r') *)
(* ------- *)
(* (l,r)   *)
(* meaning l  |- l' *)
(* and     r' |- r  *)

let subsumed (l,r) (l',r') = 
  Sl_form.subsumed l' l && Sl_form.subsumed_upto_tags r r'

let subsumed_upto_tags (l,r) (l',r') = 
  Sl_form.subsumed_upto_tags l' l && Sl_form.subsumed_upto_tags r r'


let partitions trm_list pi = 
  let pairs = Blist.cartesian_hemi_square trm_list in
  let pairs = 
    Blist.filter 
      (fun (x,y) -> not (Sl_heap.equates pi x y || Sl_heap.disequates pi x y))
      pairs in
  let rec aux = function
    | [] -> [pi]
    | q::qs -> 
      let sub_partitions = aux qs in
      (Blist.map (fun pi' -> Sl_heap.add_eq pi' q) sub_partitions)
      @
      (Blist.map (fun pi' -> Sl_heap.add_deq pi' q) sub_partitions) in
  aux pairs

let invalid defs seq =
  let trm_list = 
    Sl_term.Set.to_list 
      (Sl_term.Set.add Sl_term.nil 
        (Sl_term.Set.filter Sl_term.is_univ_var (vars seq))) in
  let (lbps, rbps) = Pair.map (Sl_basepair.pairs_of_form defs) seq in
  let res = 
    Sl_basepair.Set.exists
    (fun (v,pi) -> 
      Blist.exists 
        (fun sigma -> 
          Sl_basepair.Set.for_all 
            (Fun.neg (fun (v',pi') -> 
              Sl_heap.subsumed pi' sigma
              &&
              Sl_term.Set.for_all
                (fun z -> Sl_term.Set.exists (Sl_heap.equates sigma z) v)
                v'))  
            rbps)
        (partitions trm_list pi))
    lbps in
  if res then Format.eprintf "INVALID: %a@." pp seq ;
  res
    