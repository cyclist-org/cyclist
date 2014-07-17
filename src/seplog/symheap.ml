open Lib
open Util
open Symbols
open MParser

let split_heaps = ref true

(* we sort the terms in a pair according to Sl_term.compare *)
let norm_pair ((x,y) as pair) =
  if Sl_term.compare x y <= 0 then pair else (y,x)

let pair_to_string sep p =
  let (x,y) = Pair.map Sl_term.to_string p in x ^ sep ^ y
let pair_to_melt sep p =
  let (x,y) = Pair.map Sl_term.to_melt p in Latex.concat [x; sep; y]

module UF =
  struct
    type t = Sl_term.t Sl_term.Map.t
    let empty : t = Sl_term.Map.empty
    let is_empty : t -> bool = Sl_term.Map.is_empty

    (* if a pair contains an exist. variable then *)
    (* the first comp of the pair is an exist. var *)
    let bindings (m : t) = Sl_term.Map.bindings m

    let rec find x m =
      if Sl_term.Map.mem x m then
        find (Sl_term.Map.find x m) m
      else
        x

    let add (x,y) m =
      let (x,y) = norm_pair (find x m, find y m) in
      (* do not add trivial identities *)
      if Sl_term.equal x y then m else
      (* first split into the pairs that do not/do map to x *)
      let (to_x, rest) =
        Sl_term.Map.partition (fun _ z -> Sl_term.equal z x) m in
      (* now change all the values of keys that map to x to y *)
      let to_y = Sl_term.Map.map (fun _ -> y) to_x in
      (* now union and add *)
      Sl_term.Map.add x y (Sl_term.Map.union rest to_y)

    let union m m' =
      Sl_term.Map.fold (fun x y m'' -> add (x,y) m'') m' m
    let of_list ls =
      Blist.fold_left (fun m pair -> add pair m) empty ls
    let map f (m:t) =
      Sl_term.Map.fold (fun x y m' -> add (f (x,y)) m') m empty
    let equates m x y = Sl_term.equal (find x m) (find y m)
    let is_subsumed m m' =
      Sl_term.Map.for_all (fun x y -> equates m' x y) m

    let equal u u' = Sl_term.Map.equal Sl_term.equal u u'

    let subst theta m =
      map (fun (x,y) -> (Sl_term.subst theta x, Sl_term.subst theta y)) m

    (* finds an extension of theta, theta' such that *)
    (* p[theta'] is a subset of p' if left=true and  *)
    (* p is a subset of p'[theta'] if left=false *)
    (* p' |- p[theta] if left=true *)
    (* p'[theta] |- p if left=false *)
    let rec uni_subsumption left hook theta p p' =
      (* let return s o =                                                           *)
      (*   debug (fun () -> s ^ ": " ^ (string_of_bool (Option.is_some o)) ) ; o in *)
      if is_empty p then hook theta else
      let direct f theta' a a' =
        if left then f theta' a a' else f theta' a' a in
      let (k,_) as a = Sl_term.Map.choose p in
      let p = Sl_term.Map.remove k p in
      let to_match = Sl_term.Map.bindings p' in
      let g theta' = uni_subsumption left hook theta' p p' in
      let f a' = Blist.find_some g (direct Sl_term.unify_pairs theta a a') in
      Blist.find_some f to_match

    let to_string_list v = Blist.map (pair_to_string symb_eq.str) (bindings v)
    let to_string v =
      Blist.to_string symb_star.sep (pair_to_string symb_eq.str) (bindings v)

    let to_melt v =
      ltx_star (Blist.map (pair_to_melt symb_eq.melt) (bindings v))

    let compare m m' = Sl_term.Map.compare Sl_term.compare m m'

    let terms (m:t) =
			let l = bindings m in
			let unfold acc (x,y) : Sl_term.t list = x::y::acc in
			let r = Blist.fold_left unfold [] l in
			Sl_term.Set.of_list r

	  let vars m = Sl_term.filter_vars (terms m)

		let remove x m =
			let xs = Sl_term.Set.filter (equates m x) (vars m) in
			let rest =
				Sl_term.Map.filter
				  (fun y z -> not (Sl_term.Set.mem y xs || Sl_term.Set.mem z xs))
					m in
		  let xs' = Sl_term.Set.to_list (Sl_term.Set.remove x xs) in
			Blist.fold_left (fun m p -> add p m) rest (Blist.pairs xs')

    let parse st =
      (Sl_term.parse >>= (fun x ->
      parse_symb symb_eq >>
      Sl_term.parse << spaces |>> (fun y -> (x,y))) <?> "eq") st
     
    let to_subst m = m    
  end


module Deqs =
  struct
    include MakeListSet(PairTypes(Sl_term)(Sl_term))
    (* if a pair contains an exist. variable then *)
    (* the first comp of the pair is an exist. var *)
    let add p deqs = add (norm_pair p) deqs
    let singleton p = singleton (norm_pair p)
    let mem p deqs = mem (norm_pair p) deqs
    let endomap f s = endomap (fun e -> norm_pair (f e)) s
    let subst theta m =
			endomap (fun (x,y) -> (Sl_term.subst theta x, Sl_term.subst theta y)) m

    (* finds an extension of theta, theta' such that *)
    (* p[theta'] is a subset of p' if left=true and  *)
    (* p is a subset of p'[theta'] if left=false *)
    (* p' |- p[theta] if left=true *)
    (* p'[theta] |- p if left=false *)
    let rec uni_subsumption left hook theta p p' =
      if is_empty p then hook theta else
      let direct f theta' a a' =
        if left then f theta' a a' else f theta' a' a in
      let a = choose p in
      let p = remove a p in
      let to_match = elements p' in
      let g theta' = uni_subsumption left hook theta' p p' in
      let f a' = Blist.find_some g (direct Sl_term.unify_pairs theta a a') in
      Blist.find_some f to_match

    let to_string_list v = Blist.map (pair_to_string symb_deq.str) (elements v)
    let to_string v =
      Blist.to_string symb_star.sep (pair_to_string symb_deq.str) (elements v)
    let to_melt v =
      ltx_star (Blist.map (pair_to_melt symb_deq.melt) (elements v))

    let terms d =
			let l = to_list d in
			let unfold acc (x,y) : Sl_term.t list = x::y::acc in
			let r = Blist.fold_left unfold [] l in
			Sl_term.Set.of_list r

	  let vars d = Sl_term.filter_vars (terms d)
		
    let parse st =
      (Sl_term.parse >>= (fun x ->
      parse_symb symb_deq >>
      Sl_term.parse << spaces |>> (fun y -> (x,y))) <?> "deq") st
  end

module Pto = MakeComplexType(PairTypes(Sl_term)(Sl_term.FList))
module Ptos =
  struct
    include Pto.MSet

    let elt_subst theta (lv, rvs) =
      (Sl_term.subst theta lv, Blist.map (Sl_term.subst theta) rvs)
    let subst theta elts = endomap (fun e -> elt_subst theta e) elts

    let unify_ptos theta (x, ys) (x', ys') =
      Sl_term.unify_list theta (x::ys) (x'::ys')

    (* finds an extension of theta, theta' such that *)
    (* p[theta'] is a (multi-)subset of p' if left=true and  *)
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
      let f a' = match direct unify_ptos theta a a' with
        | None -> None
        | Some theta' ->
          let p' = remove a' p' in
          aux_subsumption left spw hook theta' p p' in
      Blist.find_some f to_match

    let uni_subsumption left hook theta p p' =
      aux_subsumption left false hook theta p p'

    let spw_left_subsumption hook theta p p' =
      aux_subsumption true true hook theta p p'

    let elem_to_string (x,args) =
      (Sl_term.to_string x) ^ symb_pointsto.str ^ (Sl_term.list_to_string args)
    let elem_to_melt (x,args) =
      Latex.concat
        [ Sl_term.to_melt x; symb_pointsto.melt;
        ltx_comma (Blist.map Sl_term.to_melt args) ]

    let to_string_list v = Blist.map elem_to_string (elements v)
    let to_string v =
      Blist.to_string symb_star.sep elem_to_string (elements v)
    let to_melt v =
      ltx_star (Blist.map elem_to_melt (elements v))

    let terms ptos =
			let l = to_list ptos in
			let unfold acc (x,ys) : Sl_term.t list = x::(Blist.rev_append ys acc) in
			let r = Blist.fold_left unfold [] l in
			Sl_term.Set.of_list r

	  let vars p = Sl_term.filter_vars (terms p)

    let parse st = 
      (Sl_term.parse >>= (fun x ->
      parse_symb symb_pointsto >>
      Tokens.comma_sep1 Sl_term.parse << spaces |>> 
      (fun l -> (x,l))) <?> "pto") st
      
	end

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
      Sl_term.unify_list theta vs vs'

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
      (string_of_int t) ^ symb_lp.str ^ (Sl_term.list_to_string args) ^ symb_rp.str
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
		
	end


exception Not_symheap

let has_ident ident ((_,(ident',_)):Ind.t) = ident=ident'

