open Lib
open Util
open Symbols
open MParser

let split_heaps = ref true

module Ind = MakeComplexType(PairTypes(Int.T)(Sl_pred))
type ind_pred = Ind.t

module Inds =
  struct
    include Ind.MSet
    let subst theta elts = endomap (Sl_tpred.subst theta) elts



    let aux_subsumed_wrt_tags spw tags h h' =
      let (inside, outside) = partition (fun (t, _) -> Tags.mem t tags) h in
      subset inside h' &&
      let h' = diff h' inside in
      let strip s = fold (fun (_,i) s' -> Sl_pred.MSet.add i s') s Sl_pred.MSet.empty in
      let (untagged_outside, untagged_h') = Pair.map strip (outside, h') in
      if spw then
        Sl_pred.MSet.subset untagged_outside untagged_h'
      else
        Sl_pred.MSet.equal untagged_outside untagged_h'

    let equal_wrt_tags tags h h' =
      aux_subsumed_wrt_tags false tags h h'

    let subsumed_wrt_tags tags h h' =
      aux_subsumed_wrt_tags true tags h h'

    let to_string_list v = Blist.map Sl_tpred.to_string (elements v)
    let to_string v =
      Blist.to_string symb_star.sep Sl_tpred.to_string (elements v)
    let to_melt v =
      ltx_star (Blist.map Sl_tpred.to_melt (elements v))

    let terms inds =
			let l = to_list inds in
			let unfold acc ((_,(_,ys)):Ind.t) = Blist.rev_append ys acc in
			let r = Blist.fold_left unfold [] l in
			Sl_term.Set.of_list r

	  let vars i = Sl_term.filter_vars (terms i)

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
      let f a' = match direct Sl_tpred.unify theta a a' with
        | None -> None
        | Some theta' ->
          let p' = remove a' p' in
          aux_subsumption left spw hook theta' p p' in
      Blist.find_some f to_match

    let uni_subsumption left hook theta p p' =
      aux_subsumption left false hook theta p p'

    let spw_left_subsumption hook theta p p' =
      aux_subsumption true true hook theta p p'
	end


exception Not_symheap

let has_ident ident ((_,(ident',_)):Ind.t) = ident=ident'

