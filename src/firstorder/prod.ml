open Lib
open   Symbols
open Generic

open MParser

module ProdT = Atom.Set

include Containers.Make(ProdT)
include ProdT

let equal_upto_tags atms atms' =
  let subsumed s s' =
    for_all
      (fun a -> exists (fun a' -> Atom.equal_upto_tags a a') s') s in
  (subsumed atms atms') && (subsumed atms' atms)

let tags p =
  let add_tag a s = match Atom.tag a with
    | None -> s
    | Some i -> Tags.add i s in
  fold add_tag p Tags.empty

let to_string f =
  if is_empty f then "T" else
    Blist.to_string " & " Atom.to_string (elements f)

let parse st =
  ( attempt (Tokens.skip_symbol "true" >>$ empty)
    <|>
    (sep_by1 Atom.parse (parse_symb symb_ampersand) |>> of_list)
    <?> "Prod"
  ) st

let pp_conj fmt () =
  Format.pp_print_string fmt " /\\" ; Format.pp_print_space fmt ()

let pp fmt p =
  if is_empty p then
    Format.fprintf fmt "@[T@]"
  else
    Format.fprintf fmt "@[%a@]" (Blist.pp pp_conj Atom.pp) (elements p)

let terms f = map_to Term.Set.union Term.Set.empty Atom.terms f
let vars p = Term.filter_vars (terms p)
let repl_tags t f = map (Atom.repl_tags t) f
let get_eqs p = map_to_list Atom.dest_eq (filter Atom.is_eq p)
let get_deqs p = map_to_list Atom.dest_deq (filter Atom.is_deq p)

let tag_of_ipred ip = Option.get (Atom.tag ip)

module IndSubf = Containers.Make(Pair.Make(Strng)(Term.FList))
module IndSubfs = IndSubf.Set
let subsumed_wrt_tags tags p1 p2 =
  let ((p1_ips, p1_nips), (p2_ips, p2_nips)) =
    Pair.map (partition Atom.is_ipred) (p1,p2) in
  let ((p1_tips, p1_ips),(p2_tips, p2_ips)) =
    Pair.map (partition (fun p -> Tags.mem (tag_of_ipred p) tags))
    (p1_ips, p2_ips) in
  let (p1_ips, p2_ips) =
    Pair.map
      (fun p ->
        map_to IndSubfs.add IndSubfs.empty Atom.strip_tags p) (p1_ips, p2_ips) in
  subset p1_nips p2_nips &&
  subset p1_tips p2_tips &&
  IndSubfs.subset p1_ips p2_ips

let hash = Hashtbl.hash
let tag_pairs f = Tagpairs.mk (tags f)

let filter_by_kind a p =
  let f = 
    if      Atom.is_eq a then Atom.is_eq
    else if Atom.is_deq a then Atom.is_deq
    else if Atom.is_ipred a then Atom.is_ipred
    else invalid_arg "Prod.filter_by_kind" in
  filter f p

(* finds an extension of theta, theta' such that *)
(* p[theta'] is a subset of p' if left=true and  *)
(* p is a subset of p'[theta'] if left=false *)
(* p' |- p[theta] if left=true *)
(* p'[theta] |- p if left=false *)
let rec uni_subsumption left hook theta p p' =
  if is_empty p then hook theta else
  let direct f theta' a a' = if left then f theta' a a' else f theta' a' a in
  let a = choose p in
  let p = remove a p in
  let to_match = elements (filter_by_kind a p') in
  let g theta' = uni_subsumption left hook theta' p p' in
  let f a' = 
    if      Atom.is_eq a
              then Blist.find_map g (direct Atom.unify_eqs theta a a')
    else if Atom.is_deq a
              then Blist.find_map g (direct Atom.unify_deqs theta a a')
    else Option.(flatten (map g (direct Atom.unify_ipreds theta a a'))) in
  Blist.find_map f to_match

let left_subsumption hook theta p p' = uni_subsumption true hook theta p p'

let subst theta p = map (Atom.subst theta) p

let univ s f =
  let vs = vars f in
  let evs = Term.Set.filter Term.is_exist_var vs in
  let n = Term.Set.cardinal evs in
  if Int.equal n 0 then f else
  let uvs = Term.fresh_fvars (Term.Set.union s vs) n in
  let theta = Term.Map.of_list (Blist.combine (Term.Set.elements evs) uvs) in
  subst theta f

exception Not_product
