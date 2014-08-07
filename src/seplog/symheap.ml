open Lib
open Util
open Symbols
open MParser

let split_heaps = ref true

type ind_identifier = Strng.t

module IndSubf = MakeComplexType(PairTypes(Strng)(Sl_term.FList))
type ind_subf = IndSubf.t

module IndSubfs = IndSubf.MSet

module Ind = MakeComplexType(PairTypes(Int.T)(IndSubf))
type ind_pred = Ind.t

module Inds =
  struct
    include Ind.MSet
    let elt_subst theta (tag, (ident, vs)) : Ind.t =
      (tag, (ident, Blist.map (Sl_term.subst theta) vs))
    let subst theta elts = endomap (fun e -> elt_subst theta e) elts

    let unify_inds theta ((_, (ident, vs)):Ind.t) ((_, (ident', vs')):Ind.t) =
      if not (Strng.equal ident ident') then None else
      Sl_term.FList.unify theta vs vs'

    (* finds an extension of theta, theta' such that *)
    (* p[theta'] is a subset of p' if left=true and  *)
    (* p is a subset of p'[theta'] if left=false *)
    (* p' |- p[theta] if left=true *)
    (* p'[theta] |- p if left=false *)
    (* if spw=true then the above become p' |- p * true *)
    let rec aux_subsumption left spw hook theta p p' =
      if is_empty p then
        if spw || is_empty p' then hook theta else None
      else
      let direct f theta' a a' =
        if left then f theta' a a' else f theta' a' a in
      let a = choose p in
      let p = remove a p in
      let to_match = elements p' in
      let f a' = match direct unify_inds theta a a' with
        | None -> None
        | Some theta' ->
          let p' = remove a' p' in
          aux_subsumption left spw hook theta' p p' in
      Blist.find_some f to_match

    let uni_subsumption left hook theta p p' =
      aux_subsumption left false hook theta p p'

    let spw_left_subsumption hook theta p p' =
      aux_subsumption true true hook theta p p'


    let aux_subsumed_wrt_tags spw tags h h' =
      let (inside, outside) = partition (fun (t, _) -> Tags.mem t tags) h in
      subset inside h' &&
      let h' = diff h' inside in
      let strip s = fold (fun (_,i) s' -> IndSubfs.add i s') s IndSubfs.empty in
      let (untagged_outside, untagged_h') = Pair.map strip (outside, h') in
      if spw then
        IndSubfs.subset untagged_outside untagged_h'
      else
        IndSubfs.equal untagged_outside untagged_h'

    let equal_wrt_tags tags h h' =
      aux_subsumed_wrt_tags false tags h h'

    let subsumed_wrt_tags tags h h' =
      aux_subsumed_wrt_tags true tags h h'

    let elem_to_string (t,(ident,args)) =
      ident ^ symb_caret.str ^
      (string_of_int t) ^ symb_lp.str ^ 
      (Blist.to_string symb_comma.sep Sl_term.to_string args) ^ 
      symb_rp.str
    let elem_to_melt (t,(ident,args)) =
      Latex.concat
        ([ Latex.index
            (Latex.mathit (Latex.text ident))
            (Latex.text (string_of_int t));
          symb_lp.melt; ltx_comma (Blist.map Sl_term.to_melt args); symb_rp.melt ])

    let to_string_list v = Blist.map elem_to_string (elements v)
    let to_string v =
      Blist.to_string symb_star.sep elem_to_string (elements v)
    let to_melt v =
      ltx_star (Blist.map elem_to_melt (elements v))

    let terms inds =
			let l = to_list inds in
			let unfold acc ((_,(_,ys)):Ind.t) = Blist.rev_append ys acc in
			let r = Blist.fold_left unfold [] l in
			Sl_term.Set.of_list r

	  let vars i = Sl_term.filter_vars (terms i)

    let parse st =
      (parse_ident >>= (fun pred ->
      option parse_tag >>= (fun opt_tag ->
      Tokens.parens (Tokens.comma_sep1 Sl_term.parse) << spaces >>= (fun arg_list ->
      let tag = match opt_tag with
      | Some tag -> upd_tag tag 
      | None -> next_tag () in
      return (tag, (pred, arg_list))))) <?> "ind") st

    let tags inds = map_to Tags.add Tags.empty fst inds

    let freshen_tags inds' inds =
      if is_empty inds || is_empty inds' then inds else
      let maxtag = Tags.max_elt (tags inds') in
      let mintag = fold (fun (tag,_) a -> min tag a) inds max_int in
      let delta = 1 + maxtag - mintag in
      endomap (fun (tag, head) -> (tag+delta, head)) inds

    let subsumed eqs inds inds' =
      let theta = Sl_uf.to_subst eqs in
      equal (subst theta inds) (subst theta inds') 

	end


exception Not_symheap

let has_ident ident ((_,(ident',_)):Ind.t) = ident=ident'

