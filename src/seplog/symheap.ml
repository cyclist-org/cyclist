open Lib
open Util
open Symbols
open MParser

let split_heaps = ref true

module Term =
  struct
    include Int
    include Var

    let nil = 0

    let list_equal = FList.equal

		let is_nil v = v=nil
    let is_var v = v<>nil && Var.is_var v
    let filter_vars s = Set.filter is_var s

    type substitution = t Map.t
    let empty_subst : substitution = Map.empty
    let singleton_subst x y = Map.add x y empty_subst
    let subst theta v =
      if not (equal v nil) && Map.mem v theta then Map.find v theta else v
      (* above is significantly faster than exception handling *)
    let subst_list theta l = Blist.map (fun v -> subst theta v) l

    let unify theta t t' =
      if Map.mem t theta then
        if equal (Map.find t theta) t' then Some theta else None
      else if equal t nil then
        if equal t' nil then Some theta else None
      else if
        is_exist_var t &&
        is_exist_var t' &&
        Map.exists (fun _ t'' -> equal t' t'') theta then
          (* avoid capture *)
          None
      else Some (Map.add t t' theta)

    let rec unify_list theta args args' = match (args,args') with
      | ([], []) -> Some theta
      | (_,  []) | ([], _) -> None
      | (hd::tl, hd'::tl') ->
        match unify theta hd hd' with
          | None -> None
          | Some theta' -> unify_list theta' tl tl'

    let unify_ordered_pairs theta (x,y) (x', y') =
			match unify theta x x' with
				| None -> None
				| Some theta' -> unify theta' y y'

    let unify_pairs theta p p' =
      Option.list_get [
        unify_ordered_pairs theta p p';
        unify_ordered_pairs theta p (Pair.swap p')
      ]

    let to_string v = if v=nil then keyw_nil.str else Var.to_string v
    let list_to_string l = Blist.to_string symb_comma.str to_string l
    let to_melt v =
      ltx_mk_math (if v=nil then keyw_nil.melt else Latex.text (Var.to_string v))
    let pp fmt trm = Format.fprintf fmt "@[%s@]" (to_string trm)
    let pp_list fmt l = Blist.pp pp_comma pp fmt l
    let pp_subst = Map.pp pp

    (* return a substitution that takes all vars in subvars to new variables *)
    (* that are outside vars U subvars, respecting exist/univ *)
    let avoid_theta vars subvars =
      let allvars = Set.union vars subvars in
      let (exist_vars, univ_vars) =
        Pair.map Set.elements (Set.partition is_exist_var subvars) in
      let fresh_u_vars = fresh_uvars allvars (Blist.length univ_vars) in
      let fresh_e_vars = fresh_evars allvars (Blist.length exist_vars) in
      let theta = Map.of_list
          (Blist.rev_append (Blist.combine univ_vars fresh_u_vars)
         (Blist.combine exist_vars fresh_e_vars)) in
      theta
    
    let parse_nil st =
      (parse_symb keyw_nil >>$ 0 <?> "nil") st
    let parse st = 
      (attempt parse_nil <|> Var.parse <?> "Term") st 
  end

(* we sort the terms in a pair according to Term.compare *)
let norm_pair ((x,y) as pair) =
  if Term.compare x y <= 0 then pair else (y,x)

let pair_to_string sep p =
  let (x,y) = Pair.map Term.to_string p in x ^ sep ^ y
let pair_to_melt sep p =
  let (x,y) = Pair.map Term.to_melt p in Latex.concat [x; sep; y]

module UF =
  struct
    type t = Term.t Term.Map.t
    let empty : t = Term.Map.empty
    let is_empty : t -> bool = Term.Map.is_empty

    (* if a pair contains an exist. variable then *)
    (* the first comp of the pair is an exist. var *)
    let bindings (m : t) = Term.Map.bindings m

    let rec find x m =
      if Term.Map.mem x m then
        find (Term.Map.find x m) m
      else
        x

    let add (x,y) m =
      let (x,y) = norm_pair (find x m, find y m) in
      (* do not add trivial identities *)
      if Term.equal x y then m else
      (* first split into the pairs that do not/do map to x *)
      let (to_x, rest) =
        Term.Map.partition (fun _ z -> Term.equal z x) m in
      (* now change all the values of keys that map to x to y *)
      let to_y = Term.Map.map (fun _ -> y) to_x in
      (* now union and add *)
      Term.Map.add x y (Term.Map.union rest to_y)

    let union m m' =
      Term.Map.fold (fun x y m'' -> add (x,y) m'') m' m
    let of_list ls =
      Blist.fold_left (fun m pair -> add pair m) empty ls
    let map f (m:t) =
      Term.Map.fold (fun x y m' -> add (f (x,y)) m') m empty
    let equates m x y = Term.equal (find x m) (find y m)
    let is_subsumed m m' =
      Term.Map.for_all (fun x y -> equates m' x y) m

    let equal u u' = Term.Map.equal Term.equal u u'

    let subst theta m =
      map (fun (x,y) -> (Term.subst theta x, Term.subst theta y)) m

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
      let (k,_) as a = Term.Map.choose p in
      let p = Term.Map.remove k p in
      let to_match = Term.Map.bindings p' in
      let g theta' = uni_subsumption left hook theta' p p' in
      let f a' = Blist.find_some g (direct Term.unify_pairs theta a a') in
      Blist.find_some f to_match

    let to_string_list v = Blist.map (pair_to_string symb_eq.str) (bindings v)
    let to_string v =
      Blist.to_string symb_star.sep (pair_to_string symb_eq.str) (bindings v)

    let to_melt v =
      ltx_star (Blist.map (pair_to_melt symb_eq.melt) (bindings v))

    let compare m m' = Term.Map.compare Term.compare m m'

    let terms (m:t) =
			let l = bindings m in
			let unfold acc (x,y) : Term.t list = x::y::acc in
			let r = Blist.fold_left unfold [] l in
			Term.Set.of_list r

	  let vars m = Term.filter_vars (terms m)

		let remove x m =
			let xs = Term.Set.filter (equates m x) (vars m) in
			let rest =
				Term.Map.filter
				  (fun y z -> not (Term.Set.mem y xs || Term.Set.mem z xs))
					m in
		  let xs' = Term.Set.to_list (Term.Set.remove x xs) in
			Blist.fold_left (fun m p -> add p m) rest (Blist.pairs xs')

    let parse st =
      (Term.parse >>= (fun x ->
      parse_symb symb_eq >>
      Term.parse << spaces |>> (fun y -> (x,y))) <?> "eq") st
     
    let to_subst m = m    
  end


module Deqs =
  struct
    include Term.Pairing.Set
    (* if a pair contains an exist. variable then *)
    (* the first comp of the pair is an exist. var *)
    let add p deqs = add (norm_pair p) deqs
    let singleton p = singleton (norm_pair p)
    let mem p deqs = mem (norm_pair p) deqs
    let endomap f s = endomap (fun e -> norm_pair (f e)) s
    let subst theta m =
			endomap (fun (x,y) -> (Term.subst theta x, Term.subst theta y)) m

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
      let f a' = Blist.find_some g (direct Term.unify_pairs theta a a') in
      Blist.find_some f to_match

    let to_string_list v = Blist.map (pair_to_string symb_deq.str) (elements v)
    let to_string v =
      Blist.to_string symb_star.sep (pair_to_string symb_deq.str) (elements v)
    let to_melt v =
      ltx_star (Blist.map (pair_to_melt symb_deq.melt) (elements v))

    let terms d =
			let l = to_list d in
			let unfold acc (x,y) : Term.t list = x::y::acc in
			let r = Blist.fold_left unfold [] l in
			Term.Set.of_list r

	  let vars d = Term.filter_vars (terms d)
		
    let parse st =
      (Term.parse >>= (fun x ->
      parse_symb symb_deq >>
      Term.parse << spaces |>> (fun y -> (x,y))) <?> "deq") st
  end

module Pto = MakeComplexType(PairTypes(Term)(Term.FList))
module Ptos =
  struct
    include Pto.MSet

    let elt_subst theta (lv, rvs) =
      (Term.subst theta lv, Term.subst_list theta rvs)
    let subst theta elts = endomap (fun e -> elt_subst theta e) elts

    let unify_ptos theta (x, ys) (x', ys') =
      Term.unify_list theta (x::ys) (x'::ys')

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
      (Term.to_string x) ^ symb_pointsto.str ^ (Term.list_to_string args)
    let elem_to_melt (x,args) =
      Latex.concat
        [ Term.to_melt x; symb_pointsto.melt;
        ltx_comma (Blist.map Term.to_melt args) ]

    let to_string_list v = Blist.map elem_to_string (elements v)
    let to_string v =
      Blist.to_string symb_star.sep elem_to_string (elements v)
    let to_melt v =
      ltx_star (Blist.map elem_to_melt (elements v))

    let terms ptos =
			let l = to_list ptos in
			let unfold acc (x,ys) : Term.t list = x::(Blist.rev_append ys acc) in
			let r = Blist.fold_left unfold [] l in
			Term.Set.of_list r

	  let vars p = Term.filter_vars (terms p)

    let parse st = 
      (Term.parse >>= (fun x ->
      parse_symb symb_pointsto >>
      Tokens.comma_sep1 Term.parse << spaces |>> 
      (fun l -> (x,l))) <?> "pto") st
      
	end

type ind_identifier = Strng.t

module IndSubf = MakeComplexType(PairTypes(Strng)(Term.FList))
type ind_subf = IndSubf.t

module IndSubfs = IndSubf.MSet

module Ind = MakeComplexType(PairTypes(Int.T)(IndSubf))
type ind_pred = Ind.t

module Inds =
  struct
    include Ind.MSet
    let elt_subst theta (tag, (ident, vs)) : Ind.t =
      (tag, (ident, Term.subst_list theta vs))
    let subst theta elts = endomap (fun e -> elt_subst theta e) elts

    let unify_inds theta ((_, (ident, vs)):Ind.t) ((_, (ident', vs')):Ind.t) =
      if not (Strng.equal ident ident') then None else
      Term.unify_list theta vs vs'

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
      (string_of_int t) ^ symb_lp.str ^ (Term.list_to_string args) ^ symb_rp.str
    let elem_to_melt (t,(ident,args)) =
      Latex.concat
        ([ Latex.index
            (Latex.mathit (Latex.text ident))
            (Latex.text (string_of_int t));
          symb_lp.melt; ltx_comma (Blist.map Term.to_melt args); symb_rp.melt ])

    let to_string_list v = Blist.map elem_to_string (elements v)
    let to_string v =
      Blist.to_string symb_star.sep elem_to_string (elements v)
    let to_melt v =
      ltx_star (Blist.map elem_to_melt (elements v))

    let terms inds =
			let l = to_list inds in
			let unfold acc ((_,(_,ys)):Ind.t) = Blist.rev_append ys acc in
			let r = Blist.fold_left unfold [] l in
			Term.Set.of_list r

	  let vars i = Term.filter_vars (terms i)

    let parse st =
      (parse_ident >>= (fun pred ->
      option parse_tag >>= (fun opt_tag ->
      Tokens.parens (Tokens.comma_sep1 Term.parse) << spaces >>= (fun arg_list ->
      let tag = match opt_tag with
      | Some tag -> upd_tag tag 
      | None -> next_tag () in
      return (tag, (pred, arg_list))))) <?> "ind") st
		
	end


exception Not_symheap

let has_ident ident ((_,(ident',_)):Ind.t) = ident=ident'

