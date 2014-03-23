open Lib
open Util
open Symbols

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
      ident ^ symb_underscore.str ^
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
	end


type symheap =
  {
    eqs  : UF.t;
    deqs : Deqs.t;
    ptos : Ptos.t;
    inds : Inds.t
  }

module Heap =
  struct
    type t = symheap

    let empty =
      { eqs=UF.empty; deqs=Deqs.empty; ptos=Ptos.empty; inds=Inds.empty }

    let norm h =
      { h with
        deqs=Deqs.subst h.eqs h.deqs;
        ptos=Ptos.subst h.eqs h.ptos;
        inds=Inds.subst h.eqs h.inds
			}

    let subst theta h =
      { eqs= UF.subst theta h.eqs;
			  deqs=Deqs.subst theta h.deqs;
        ptos=Ptos.subst theta h.ptos;
        inds=Inds.subst theta h.inds
			}

    let get_idents p =
      (* Inds.map_to Strng.MSet.add Strng.MSet.empty (fun (_,(id,_)) -> id) p.inds *)
			Strng.MSet.of_list (Blist.rev_map (fun (_,(id,_)) -> id) (Inds.to_list p.inds))

    let terms f =
      Term.Set.union_of_list
			  [UF.vars f.eqs; Deqs.vars f.deqs; Ptos.vars f.ptos; Inds.vars f.inds]

    let vars f = Term.filter_vars (terms f)

    let tags l =
      Inds.map_to Tags.add Tags.empty (fun i -> fst i) l.inds
			(* Tags.of_list (Blist.map (fun i -> fst i) (Inds.to_list l.inds)) *)

    let to_string f =
      let res = String.concat symb_star.sep
        ((UF.to_string_list f.eqs) @ (Deqs.to_string_list f.deqs) @
        (Ptos.to_string_list f.ptos) @ (Inds.to_string_list f.inds)) in
      if res = "" then keyw_emp.str else res

    let to_melt f =
      let sep = if !split_heaps then Latex.text " \\\\ \n" else symb_star.melt in
      let content = Latex.concat (Latex.list_insert sep
        (Blist.filter (fun l -> not (Latex.is_empty l))
          [UF.to_melt f.eqs; Deqs.to_melt f.deqs;
          Ptos.to_melt f.ptos; Inds.to_melt f.inds])) in
      let content = if !split_heaps then
        Latex.concat
          [
            ltx_newl;
            Latex.environment
              ~opt:(Latex.A, Latex.text "b")
              ~args:[(Latex.A, Latex.text "l")]
              "array" (Latex.M, content) Latex.M;
            ltx_newl
          ]
      else
        content in
      ltx_mk_math content

    let pp fmt h =
      let l =
        ((UF.to_string_list h.eqs) @ (Deqs.to_string_list h.deqs) @
        (Ptos.to_string_list h.ptos) @ (Inds.to_string_list h.inds)) in
      if l<>[] then
        Format.fprintf fmt "@[%a@]" (Blist.pp pp_star Format.pp_print_string) l
      else
        Format.fprintf fmt "@[true@]"

    (* star two formulae together *)
    let star f g =
      (* computes all deqs due to a list of ptos - no normalization *)
      let explode_deqs (ptos : Pto.t list) =
        let cp = Blist.cartesian_hemi_square ptos in
        let s1 =
	        (Blist.fold_left (fun s p -> Deqs.add (fst p, Term.nil) s) Deqs.empty ptos) in
 					(Blist.fold_left (fun s (p,q) -> Deqs.add (fst p, fst q) s) s1 cp) in
      let newptos = Ptos.union f.ptos g.ptos in
      (* norm *)
      {
        eqs=UF.union f.eqs g.eqs;
        deqs=Deqs.union_of_list
          [f.deqs; g.deqs; explode_deqs (Ptos.elements newptos)];
        ptos=newptos;
        inds=Inds.union f.inds g.inds
      }

    let univ s f =
      let vs = vars f in
      let evs = Term.Set.filter Term.is_exist_var vs in
			let n = Term.Set.cardinal evs in
      if n=0 then f else
      let uvs = Term.fresh_uvars (Term.Set.union s vs) n in
      let theta = Term.Map.of_list (Blist.combine (Term.Set.elements evs) uvs) in
      subst theta f

    let repl_tags t f =
      { f with inds=Inds.endomap (fun (_, p) -> (t, p)) f.inds }

    let tag_pairs f = TagPairs.mk (tags f)

    let mk_pto v1 v2 = { empty with ptos=Ptos.singleton (v1,v2) }
    let mk_eq v1 v2 = { empty with eqs=UF.add (v1,v2) UF.empty }
    let mk_deq v1 v2 = { empty with deqs=Deqs.singleton (v1,v2) }
    let mk_ind tag ident vs =
      { empty with inds=Inds.singleton (tag, (ident,vs)) }

    let equates h x y = UF.equates h.eqs x y
    let disequates h x y =
      Deqs.exists
        (fun (w,z) ->
          (equates h x w && equates h y z)
          || (equates h x z && equates h y w) ) h.deqs

		let eq_class h x = Term.Set.filter (equates h x) (terms h)

    let aux_subsumption left spw hook theta h h' =
      let f1 theta' = UF.uni_subsumption left hook theta' h.eqs h'.eqs in
      let f2 theta' = Deqs.uni_subsumption left f1 theta' h.deqs h'.deqs in
      let f3 theta' = Ptos.aux_subsumption left spw f2 theta' h.ptos h'.ptos in
      Inds.aux_subsumption left spw f3 theta h.inds h'.inds

    let spw_left_subsumption hook theta h h' =
      aux_subsumption true true hook theta h h'


    let equal h h' =
      UF.equal h.eqs h'.eqs &&
      Deqs.equal h.deqs h'.deqs &&
      Ptos.equal h.ptos h'.ptos &&
      Inds.equal h.inds h'.inds

    include Fixpoint(struct type t = symheap let equal = equal end)
    
    let compare f g =
      match UF.compare f.eqs g.eqs with
        | n when n<>0 -> n
        | _ -> match Deqs.compare f.deqs g.deqs with
          | n when n<>0 -> n
          | _ -> match Ptos.compare f.ptos g.ptos with
            | n when n<>0 -> n
            | _ -> Inds.compare f.inds g.inds

    (* h' |- h  if spw=false *)
    (* h' |- h * true  if spw=true *)
    let aux_subsumed_wrt_tags spw tags h h' =
      let h = subst h'.eqs h in
      UF.is_subsumed h.eqs h'.eqs &&
      Deqs.subset h.deqs h'.deqs &&
      if spw then
        Ptos.subset h.ptos h'.ptos &&
        Inds.subsumed_wrt_tags tags h.inds h'.inds
      else
        Ptos.equal h.ptos h'.ptos &&
        Inds.equal_wrt_tags tags h.inds h'.inds

    let find_lval x h =
      try
        Some (Ptos.find (fun (y,_) -> equates h x y) h.ptos)
      with Not_found -> None

    let inconsistent h =
      (* let lvalues = Ptos.map_to Term.Set.add Term.Set.empty fst h.ptos in *)
      (* Term.Set.cardinal lvalues <> Ptos.cardinal h.ptos ||                *)
      Deqs.exists (fun (x,y) -> equates h x y) h.deqs
      (* Term.Set.mem Term.nil lvalues *)

    let subst_existentials h =
      let aux h' =
        let (ex_eqs, non_ex_eqs) =
          Blist.partition
            (fun (x,_) -> Term.is_exist_var x) (UF.bindings h'.eqs) in
        if ex_eqs=[] then h' else
        (* NB order of subst is reversed so that *)
        (* the greater variable replaces the lesser *)
        (* this maintains universal vars *)
        let h'' = { h' with eqs=UF.of_list non_ex_eqs } in
        subst (Term.Map.of_list ex_eqs) h'' in
      fixpoint aux h

    let is_fresh_in x h = not (Term.Set.mem x (vars h))

    let hash (h:t) = Hashtbl.hash h

    let project f xs =
      (* let () = assert (Inds.is_empty f.inds && Ptos.is_empty f.ptos) in *)
      let trm_nin_lst x =
				not (Term.is_nil x) &&
				not (Blist.exists (fun y -> Term.equal x y) xs) in
      let pair_nin_lst (x,y) = trm_nin_lst x || trm_nin_lst y in
      let rec proj_eqs h =
        let orig_eqs = UF.bindings h.eqs in
				let p = Blist.find_first pair_nin_lst orig_eqs in
				if Option.is_none p then h else
        let (x,y) = Option.get p in
        (* let new_eqs =                                                        *)
				(* 	List.filter                                                        *)
				(* 	  (fun (x',y') -> not (Term.equal x x') || not (Term.equal y y'))  *)
				(* 		orig_eqs in                                                      *)
        let (x',y') = if trm_nin_lst x then (y,x) else (x,y) in
        let theta = Term.singleton_subst y' x' in
        proj_eqs (subst theta h) in
      let proj_deqs g =
        { g with deqs=Deqs.filter (fun p -> not (pair_nin_lst p)) g.deqs } in
      proj_deqs (proj_eqs f)

  end

exception Not_symheap

module Form =
  struct
    include MakeFList(Heap)

    let empty = [ Heap.empty ]

    let dest (f:t)  = match f with
      | [s] -> s
      | _ -> raise Not_symheap

    let star f g =
      Blist.map (fun (f',g') -> Heap.star f' g') (Blist.cartesian_product f g)

    let disj f g : t = f @ g
    let terms d = Term.Set.union_of_list (Blist.map Heap.terms d)
    let vars d = Term.filter_vars (terms d)
    let to_string d = Blist.to_string symb_or.sep Heap.to_string d
    let to_melt d =
      ltx_mk_math
        (if d=[] then symb_false.melt else
          Latex.concat
            (Latex.list_insert symb_or.melt (Blist.map Heap.to_melt d)))

    let norm l = Blist.map Heap.norm l
    let tags d = Tags.union_of_list (Blist.map Heap.tags d)
    let tag_pairs f = TagPairs.mk (tags f)
    let equates f x y = Blist.for_all (fun h -> Heap.equates h x y) f
    let inconsistent f = Blist.for_all Heap.inconsistent f

    let pp fmt f =
      let pp_or fmt () =
        Format.fprintf fmt " %s@ " symb_or.str in
      if f<>[] then
        Format.fprintf fmt "@[%a@]" (Blist.pp pp_or Heap.pp) f
      else
        Format.fprintf fmt "@[F@]"

    let subst theta f = Blist.map (fun h -> Heap.subst theta h) f

    let aux_subsumed_wrt_tags spw t f1 f2 =
      Blist.for_all (fun d2 ->
        Blist.exists (fun d1 ->
          Heap.aux_subsumed_wrt_tags spw t d1 d2) f1) f2

    (* f2 |- f1 *)
    let subsumed_wrt_tags t f1 f2 =
      aux_subsumed_wrt_tags false t f1 f2

    (* f2 |- f1 * true *)
    let spw_subsumed_wrt_tags t f1 f2 =
      aux_subsumed_wrt_tags true t f1 f2

    let rec aux_subsumption left spw fhook theta f f' =
      if []=f' then fhook theta else
      let p' = Blist.hd f' in
      let f' = Blist.tl f' in
      let hook' theta' =
        aux_subsumption left spw fhook theta' f f' in
      let g p = Heap.aux_subsumption left spw hook' theta p p' in
      Blist.find_some g f

    let uni_subsumption left fhook theta f f' =
      aux_subsumption left false fhook theta f f'

    let rec spw_left_subsumption fhook theta f f' =
      aux_subsumption true true fhook theta f f'

    let left_subsumption fhook theta f f' =
      uni_subsumption true fhook theta f f'
    let right_subsumption fhook theta f f' =
      uni_subsumption false fhook theta f f'

    let subst_existentials f = Blist.map Heap.subst_existentials f

    let is_fresh_in x f = Blist.for_all (Heap.is_fresh_in x) f
  end

module Seq =
  struct
    include PairTypes(Form)(Form)

    let dest seq = Pair.map Form.dest seq

    let to_string (l,r) =
      (Form.to_string l) ^ symb_turnstile.sep ^ (Form.to_string r)
    let to_melt (l,r) =
      ltx_mk_math
        (Latex.concat [Form.to_melt l; symb_turnstile.melt; Form.to_melt r])

    let pp fmt (l,r) =
      Format.fprintf fmt "@[%a %s@ %a@]" Form.pp l symb_turnstile.str Form.pp r

    let vars (l,r) =
			Term.filter_vars (Term.Set.union (Form.terms l) (Form.terms r))
    let tags (seq:t) = Form.tags (fst seq)
    let tag_pairs f = TagPairs.mk (tags f)

    let subst theta seq = Pair.map (fun f -> Form.subst theta f) seq

    (* s2 entails s1 *)
    let subsumed_wrt_tags t s1 s2 =
      Form.subsumed_wrt_tags t (fst s2) (fst s1) &&
      Form.subsumed_wrt_tags Tags.empty (snd s1) (snd s2)
    (*  s' *)
    (* ___ *)
    (*  s  *)
    let uni_subsumption s s' =
      let ((l,r),(l',r')) = (s,s') in
      let tags = Tags.inter (tags s) (tags s') in
      let valid theta' =
        let s'' = subst theta' s' in
        let tags' = Tags.fold
          ( fun t acc ->
            let new_acc = Tags.add t acc in
            if subsumed_wrt_tags new_acc s s'' then new_acc else acc
          ) tags Tags.empty in
        if not (Tags.is_empty tags') then
          Some theta' else None in
      let hook theta' = Form.right_subsumption valid theta' r r' in
      Form.left_subsumption hook Term.Map.empty l' l

    let norm s = Pair.map Form.norm s
  end

module Case =
  struct
    include PairTypes(Heap)(IndSubf)
    let mk f i : t = (f, i)
    let dest (c : t) = c
    let vars ((f,(_,vs)):t) =
      Term.filter_vars (Term.Set.union (Term.Set.of_list vs) (Heap.terms f))
    let subst theta ((f, (ident, vs)):t) =
      (Heap.subst theta f, (ident, Term.subst_list theta vs))
    let freshen varset case =
      let casevars = vars case in
      let theta = Term.avoid_theta varset casevars in
      subst theta case

    let to_string (f,(ident,vs)) =
      (Heap.to_string f) ^ symb_ind_implies.sep ^
      ident ^ symb_lp.str ^ (Blist.to_string symb_comma.str Term.to_string vs) ^
      symb_rp.str

    let pp fmt (f,(ident,vs)) =
      Format.fprintf fmt "@[%a%s%s%s%s%s@]"
        Heap.pp f
        symb_ind_implies.sep
        ident
        symb_lp.str
        (Blist.to_string "," Term.to_string vs)
        symb_rp.str

  end

module Defs =
  struct
		module CaseList = MakeFList(Case)
		module DefPair = PairTypes(CaseList)(Strng)
    include MakeFList(DefPair)
    include Fixpoint(struct type t = DefPair.t list let equal = equal end)

    (* type t = ((Case.t list) * ind_identifier) list *)
    let empty = []
    let add_case a l = failwith "not implemented"
    let string_of_clause (f, (ident, params)) =
      (Heap.to_string f) ^ symb_ind_implies.sep ^ ident ^
      (bracket (Blist.to_string symb_comma.str Term.to_string params))

    let string_of_case (cls, ident) =
      ident ^ symb_lb.sep ^ "\n" ^
      (Blist.to_string ((symb_ind_sep.sep) ^ "\n") string_of_clause cls)
      ^ "\n" ^ symb_rb.str

    let to_string defs =
      Blist.to_string (symb_semicolon.sep ^ "\n\n") string_of_case defs

    let to_melt d = ltx_text (to_string d)
    
    let pp fmt d = Format.fprintf fmt "%s" (to_string d)

    let mem ident (defs:t) = Blist.exists (fun (_, ident') -> Strng.equal ident ident') defs

    let is_defined ((_,(ident, _)):Ind.t) (defs:t) = mem ident defs

    let get_def ident (defs:t) = Blist.find (fun (_,ident') -> Strng.equal ident ident') defs

    module BasePair =
      struct
        include PairTypes(Term.Set)(Heap)

				let to_string (v,g) =
					"(" ^
					  "{" ^
						  (Blist.to_string "," Term.to_string (Term.Set.to_list v)) ^
						"}, " ^
						(Heap.to_string g) ^
					")"

        let project (v,g) case =
          let (_,(_,formals)) = Case.dest case in
          (Term.Set.inter v (Term.Set.of_list formals), Heap.project g formals)

        let subst theta (v,g) =
          let v' = Term.Set.endomap (fun z -> Term.subst theta z) v in
          let g' = Heap.subst theta g in
          (v',g')

        (* assumes phys neq distinguishes occurrences *)
        let unfold (v,h) ((_,(_,params)) as ind) (case, (v',g')) =
          (* simultaneously freshen case and (v',g') *)
          let avoidvars = Term.Set.union v (Heap.vars h) in
          let theta = Term.avoid_theta avoidvars (Case.vars case) in
          let case = Case.subst theta case in
          let (v',g') = subst theta (v',g') in
          (* now carry on with substitution as normal *)
          let (_,(_,formals)) = Case.dest case in
          let theta = Term.Map.of_list (Blist.combine formals params) in
          (* let formals = Term.Set.of_list (Blist.map fst (Term.Map.to_list theta)) in     *)
          (* let substs =  Term.Set.of_list (Blist.map snd (Term.Map.to_list theta)) in     *)
          (* let () = assert (Term.Set.subset (Heap.vars g') formals) in                   *)
          (* let () = assert (Term.Set.subset v' formals) in                               *)
          (* let () = assert (Term.Set.is_empty (Term.Set.inter (Heap.vars g') substs)) in *)
          (* let () = assert (Term.Set.is_empty (Term.Set.inter v' substs)) in             *)
          let (v',g') = subst theta (v',g') in
          let h' = { h with inds=Inds.remove ind h.inds } in
          let h' = Heap.star h' g' in
          let cv = Blist.cartesian_product (Term.Set.to_list v) (Term.Set.to_list v') in
          let h' = { h' with deqs=Deqs.union h'.deqs (Deqs.of_list cv) } in
          let v = Term.Set.union v v' in
          (v,h')

        (* assumes case is built with Heap.star so ys are already unequal *)
        let unfold_all case cbps =
          let (h,_) = Case.dest case in
          (* let () = assert (Inds.cardinal h.inds = Blist.length cbps) in *)
          let ys = Term.Set.of_list (Blist.rev_map fst (Ptos.to_list h.ptos)) in
          let h = { h with ptos=Ptos.empty } in
          Blist.fold_left2 unfold (ys,h) (Inds.to_list h.inds) cbps

        let gen case cbps =
          let (_,(_,args)) = Case.dest case in
          let (v,h) = unfold_all case cbps in
          if Heap.inconsistent h then None else
          let l = Blist.rev_append (Term.Set.to_list (Heap.vars h)) args in
    			let l = Blist.rev_filter
            (fun u -> Term.Set.exists (fun z -> Heap.equates h u z) v) l in
          let v = Term.Set.of_list l in
          Some (project (v,h) case)

      end

    module BasePairSet = MakeTreeSet(BasePair)
    (* module BasePairSet =                                        *)
    (*   struct                                                    *)
    (*     include Hashtbl.Make(BasePair)                          *)

    (*     let to_list h = fold (fun x _ l -> x::l) h []           *)
    (*     let to_string h =                                       *)
    (*       "{" ^                                                 *)
    (*       (Blist.to_string "," BasePair.to_string (to_list h)) ^ *)
    (*       "}"                                                   *)
    (*   end                                                       *)

    module CaseMap =
			struct
				include MakeMap(Case)
				let to_string cmap =
					let aux (c,s) =
						(Case.to_string c) ^ "\nBase pairs: " ^
						(BasePairSet.to_string s) ^ "\n" in
          Blist.to_string "\n" aux (to_list cmap)
			end

    let get_bps cmap (_,(ident,_)) =
      let l =
        Blist.filter
          (fun (c, _) -> Strng.equal ident (fst (snd (Case.dest c))))
          (CaseMap.to_list cmap) in
      Blist.bind
        (fun (c, s) -> Blist.map (fun bp -> (c,bp)) (BasePairSet.to_list s))
        l

    let gen_pairs case cmap =
      let (h,_) = Case.dest case in
      let candidates =
				Blist.map (fun i -> get_bps cmap i) (Inds.to_list h.inds) in
      (* let () = prerr_endline "+" in *)
      let l = Blist.choose candidates in
      (* let () = prerr_endline "-" in *)
      let poss_bps =
				Blist.rev_map (fun cbps -> BasePair.gen case cbps) l in
      (* let () = prerr_endline "*" in *)
      let bps = Option.list_get poss_bps in
      BasePairSet.of_list bps

    let gen_all_pairs defs =
      let cmap =
        Blist.fold_left
          (fun m (cl,_) ->
            Blist.fold_left
              (fun m' c -> CaseMap.add c BasePairSet.empty m') m cl)
          CaseMap.empty
          defs
          in
      let onestep cmap =
        let r = CaseMap.endomap
          (fun (c,s) -> (c,BasePairSet.union s (gen_pairs c cmap)))
          cmap in
				let () = debug (fun () -> "=======\n" ^ (CaseMap.to_string r) ^ "\n") in
				r in
      CaseMap.fixpoint BasePairSet.equal onestep cmap

    let consistent defs =
      Stats.CC.call () ;
      (* let () = print_endline "CHECK BEGINS" in *)
      let res = gen_all_pairs defs in
      (* let () = CaseMap.iter                                                            *)
      (*   (fun c s ->                                                                    *)
      (*     (* let b = not (BasePairSet.is_empty s) in *)                                *)
      (*     (* print_endline ((if b then "O" else "X") ^ " " ^ (Case.to_string c)))   *) *)
      (*     print_endline                                                                *)
      (*       ((Case.to_string c) ^ " " ^                                                *)
      (*       (BasePairSet.to_string s)))                                                *)
        (* res in *)
      let retval = CaseMap.for_all (fun _ s -> not (BasePairSet.is_empty s)) res in
      if retval then Stats.CC.accept () else Stats.CC.reject () ;
      retval

		(* NB correctness relies on rules being explicit about x->_ implying x!=nil !!! *)
    let consistent_plus_output defs =
      Stats.CC.call () ;
      let res = gen_all_pairs defs in
      let element_conv = (fun (c,s) ->
           ((Case.to_string c) ^ " has base " ^ (BasePairSet.to_string s))) in
      let case_to_base_string = Blist.to_string "\n" element_conv (CaseMap.to_list res) in
      let retval = CaseMap.for_all (fun _ s -> not (BasePairSet.is_empty s)) res in
      if retval then Stats.CC.accept () else Stats.CC.reject () ;
      (retval, case_to_base_string)


  end

let has_ident ident ((_,(ident',_)):Ind.t) = ident=ident'

