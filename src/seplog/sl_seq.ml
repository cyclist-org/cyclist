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

module HSet = MakeTreeSet(Sl_heap)

let partitions trm_list pi = 
  let pairs = Blist.cartesian_hemi_square trm_list in
  let pairs = 
    Blist.filter 
      (fun (x,y) -> not (Sl_heap.equates pi x y || Sl_heap.disequates pi x y))
      pairs in
  let aux acc pair = 
    HSet.fold
      (fun pi' acc' ->
        let pi'' = Sl_heap.add_eq pi' pair in
        if Sl_heap.inconsistent pi'' then 
          acc' 
        else
          HSet.add pi'' acc'
      )
      acc
      acc in
  Blist.foldl aux (HSet.singleton pi) pairs

let _invalid defs seq =
  (* Format.eprintf "INVALIDITY CHECK: %a@." pp seq ; *)
  let trm_list = 
    Sl_term.Set.to_list 
      (Sl_term.Set.add Sl_term.nil 
        (Sl_term.Set.filter Sl_term.is_univ_var (vars seq))) in
  (* Format.eprintf "INVALIDITY CHECK: # of terms in T = %d@."  *)
  (*   (Blist.length trm_list) ;                                *)
  let (lbps, rbps) = Pair.map (Sl_basepair.pairs_of_form defs) seq in
  (* Format.eprintf "INVALIDITY CHECK: LHS base pairs = @.%a@."  *)
  (*   Sl_basepair.Set.pp lbps;                                  *)
  (* Format.eprintf "INVALIDITY CHECK: RHS base pairs = @.%a@."  *)
  (*   Sl_basepair.Set.pp rbps;                                  *)
  (* let all_partitions = partitions trm_list Sl_heap.empty in  *)
  (* Format.eprintf "INVALIDITY CHECK: # of partitions = %d@."  *)
    (* (Blist.length all_partitions) ; *)
  let map_through sigma v =
    Sl_term.Set.endomap (fun x -> Sl_uf.find x sigma.Sl_heap.eqs) v in
  let b_move sigma (v,_) (v',pi') =
    Sl_heap.subsumed pi' sigma
    && 
    let (v, v') = Pair.map (map_through sigma) (v, v') in
    Sl_term.Set.subset v' v in     
  let a_partition ((v, pi) as bp) sigma =
    not (Sl_basepair.Set.exists (fun bp' -> b_move sigma bp bp') rbps) in    
  let a_move ((v,pi) as bp) = 
    HSet.exists (fun sigma -> a_partition bp sigma) (partitions trm_list pi) in    
  let res = Sl_basepair.Set.exists a_move lbps in
  (* Format.eprintf "INVALIDITY CHECK: %s@." (string_of_bool res) ; *)
  res

    
let invalid =
  let cache = Hashtbl.create 1001 in
  fun defs seq ->
    let key = (defs, seq) in
    try
      Hashtbl.find cache key
    with Not_found ->
      let v = _invalid defs seq in
      Hashtbl.add cache key v ;
      v

let norm s = Pair.map Sl_form.norm s               