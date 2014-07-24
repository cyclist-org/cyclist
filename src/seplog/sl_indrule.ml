open Lib
open Util
open Symbols
open Symheap
open MParser

include PairTypes(Sl_heap)(IndSubf)
let mk f i =
  let (_, args) = i in
  let v_args = Sl_term.Set.of_list args in
  let v_h = Sl_heap.vars f in
  let (uv_h, ev_h) = Sl_term.Set.partition Sl_term.is_univ_var v_h in
  assert (Blist.for_all Sl_term.is_univ_var args) ;
  assert (Sl_term.Set.cardinal v_args = Blist.length args) ;
  assert (Sl_term.Set.subset uv_h v_args) ;
  assert (Sl_term.Set.for_all Sl_term.is_exist_var ev_h) ;   
  (f, i)
  
let dest c = c

let vars (f, (_, vs)) =
  Sl_term.Set.union (Sl_term.Set.of_list vs) (Sl_heap.vars f)
  
let subst theta (f, (ident, vs)) =
  mk (Sl_heap.subst theta f) (ident, Blist.map (Sl_term.subst theta) vs)
  
let freshen varset case =
  let casevars = vars case in
  let theta = Sl_term.avoid_theta varset casevars in
  subst theta case

let pp fmt (f, (ident, vs)) =
  Format.fprintf fmt "@[%a%s%s%s%s%s@]"
    Sl_heap.pp f
    symb_ind_implies.sep
    ident
    symb_lp.str
    (Blist.to_string "," Sl_term.to_string vs)
    symb_rp.str

let to_string c = mk_to_string pp c

let parse st =
  ( Sl_heap.parse >>= (fun h ->
          parse_symb symb_ind_implies >>
          Inds.parse << spaces >>=
          (fun (_, head) -> return (mk h head))) <?> "case") st

let unfold vars h (tag, (ident, args)) case =
  let (f, (ident', formals)) = dest (freshen vars case) in
  assert (Strng.equal ident ident') ;
  let f = Sl_heap.freshen_tags h f in 
  let tagpairs = 
    Tags.map_to 
      TagPairs.add 
      TagPairs.empty 
      (fun tag' -> (tag,tag')) 
      (Sl_heap.tags f) in 
  let theta = Sl_term.Map.of_list (Blist.combine formals args) in
  Sl_heap.subst theta f, tagpairs
 
let fold (f, (predsym, args)) h =
  let results : Sl_term.substitution list ref = ref [] in
  let hook theta = results := theta :: !results ; None in 
  let _ = Sl_heap.spw_left_subsumption hook Sl_term.empty_subst f h in
  !results
