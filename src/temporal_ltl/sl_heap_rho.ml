open Lib

open Symbols
open MParser

let split_heaps = ref true

type abstract1 = Sl_term.Set.t option
type abstract2 = Tags.t option

type symheap =
  {
		rho : Sl_rho.t;
    eqs : Sl_uf.t;
    deqs : Sl_deqs.t;
    ptos : Sl_ptos.t;
    inds : Sl_tpreds.t;
    mutable _terms : Sl_term.Set.t option;
    mutable _vars : Sl_term.Set.t option;
    mutable _tags : Tags.t option
  }

type t = symheap

(* accessors *)

let equal h h' =
  h == h' ||
	Sl_rho.equal h.rho h'.rho &&
  Sl_uf.equal h.eqs h'.eqs &&
  Sl_deqs.equal h.deqs h'.deqs &&
  Sl_ptos.equal h.ptos h'.ptos &&
  Sl_tpreds.equal h.inds h'.inds

let equal_upto_tags h h' =
  h == h' ||
	Sl_rho.equal h.rho h'.rho &&
  Sl_uf.equal h.eqs h'.eqs &&
  Sl_deqs.equal h.deqs h'.deqs &&
  Sl_ptos.equal h.ptos h'.ptos &&
  Sl_tpreds.equal_upto_tags h.inds h'.inds

include Fixpoint.Make(struct type t = symheap let equal = equal end)

let compare f g =
  if f == g then 0 else
		match Sl_rho.compare f.rho g.rho with
		| n when n<>0 -> n
		| _ -> match Sl_uf.compare f.eqs g.eqs with
   				 | n when n <>0 -> n
    			 | _ -> match Sl_deqs.compare f.deqs g.deqs with
        					| n when n <>0 -> n
        					| _ -> match Sl_ptos.compare f.ptos g.ptos with
            						| n when n <>0 -> n
            						| _ -> Sl_tpreds.compare f.inds g.inds

(* custom hash function so that memoization fields are ignored when hashing *)
(* so that the hash invariant is preserved [a = b => hash(a) = hash(b)] *)
(* FIXME: memoize hash as well? *)
let hash h =
	genhash
  (genhash
    (genhash
      (genhash
        (Sl_tpreds.hash h.inds)
        (Sl_ptos.hash h.ptos))
      (Sl_deqs.hash h.deqs))
    (Sl_uf.hash h.eqs))
	(Sl_rho.hash h.rho)

let terms f =
  match f._terms with
  | Some trms -> trms
  | None ->
    let trms =
      Sl_term.Set.union_of_list
        [ Sl_rho.terms f.rho;
					Sl_uf.terms f.eqs;
          Sl_deqs.terms f.deqs;
          Sl_ptos.terms f.ptos;
          Sl_tpreds.terms f.inds] in
    f._terms <- Some trms;
    trms

let vars f =
  match f._vars with
  | Some v -> v
  | None ->
    let v = Sl_term.filter_vars (terms f) in
    f._vars <- Some v;
    v

let tags h =
  match h._tags with
  | Some tgs -> tgs
  | None ->
    let tgs = Sl_tpreds.tags h.inds in
    h._tags <- Some tgs;
    tgs

let tag_pairs f = Tagpairs.mk (tags f)

let has_untagged_preds h = not (Sl_tpreds.for_all Sl_tpred.is_tagged h.inds)

let to_string f =
  let res = String.concat symb_star.sep
      ((Sl_rho.to_string_list f.rho) @ (Sl_uf.to_string_list f.eqs) @ (Sl_deqs.to_string_list f.deqs) @
        (Sl_ptos.to_string_list f.ptos) @ (Sl_tpreds.to_string_list f.inds)) in
  if res = "" then keyw_emp.str else res

let pp fmt h =
  let l =
    ((Sl_rho.to_string_list h.rho) @ (Sl_uf.to_string_list h.eqs) @ (Sl_deqs.to_string_list h.deqs) @
      (Sl_ptos.to_string_list h.ptos) @ (Sl_tpreds.to_string_list h.inds)) in
  Format.fprintf fmt "@[%a@]" (Blist.pp pp_star Format.pp_print_string)
    (if l<>[] then l else [keyw_emp.str])

let equates h x y = Sl_uf.equates h.eqs x y (* IT MIGHT NEED TO CHECK IN RHO *)

let disequates h x y =
  Sl_deqs.exists
    (fun (w, z) ->
          equates h x w && equates h y z
          ||
          equates h x z && equates h y w)
    h.deqs

let find_lval x h =
  Sl_ptos.find_opt (fun (y, _) -> equates h x y) h.ptos

let inconsistent h = Sl_deqs.exists (fun (x, y) -> equates h x y) h.deqs

let idents p = Sl_tpreds.idents p.inds

let subsumed_upto_tags ?(total=true) h h' =
	Sl_rho.subsumed h.rho h'.rho &&
  Sl_uf.subsumed h.eqs h'.eqs &&
  Sl_deqs.subsumed h'.eqs h.deqs h'.deqs &&
  Sl_ptos.subsumed ~total h'.eqs h.ptos h'.ptos &&
  Sl_tpreds.subsumed_upto_tags ~total h'.eqs h.inds h'.inds

let subsumed ?(total=true) h h' =
  Sl_rho.subsumed h.rho h'.rho &&
  Sl_uf.subsumed h.eqs h'.eqs &&
  Sl_deqs.subsumed h'.eqs h.deqs h'.deqs &&
  Sl_ptos.subsumed ~total h'.eqs h.ptos h'.ptos &&
  Sl_tpreds.subsumed ~total h'.eqs h.inds h'.inds


(* Constructors *)

let mk rho eqs deqs ptos inds =
  assert
    (Tags.cardinal (Sl_tpreds.map_to Tags.add Tags.empty fst inds)
    =
    Sl_tpreds.cardinal inds) ;
  { rho; eqs; deqs; ptos; inds; _terms=None; _vars=None; _tags=None }

let dest h = (h.rho, h.eqs, h.deqs, h.ptos, h.inds)

let empty = mk Sl_rho.empty Sl_uf.empty Sl_deqs.empty Sl_ptos.empty Sl_tpreds.empty

let is_empty h = equal h empty

let subst theta h =
  { rho = Sl_rho.subst theta h.rho;
		eqs = Sl_uf.subst theta h.eqs;
    deqs = Sl_deqs.subst theta h.deqs;
    ptos = Sl_ptos.subst theta h.ptos;
    inds = Sl_tpreds.subst theta h.inds;
    _terms = None;
    _vars = None;
    _tags=None
  }

let with_rho h rho = { h with rho; _terms=None; _vars=None; _tags=None}
let with_eqs h eqs = { h with eqs; _terms=None; _vars=None; _tags=None }
let with_deqs h deqs = { h with deqs; _terms=None; _vars=None; _tags=None }
let with_ptos h ptos = { h with ptos; _terms=None; _vars=None; _tags=None }
let with_inds h inds = mk h.rho h.eqs h.deqs h.ptos inds

let del_deq h deq = with_deqs h (Sl_deqs.remove deq h.deqs)
let del_pto h pto = with_ptos h (Sl_ptos.remove pto h.ptos)
let del_ind h ind =
  { h with inds = Sl_tpreds.remove ind h.inds; _terms=None; _vars=None; _tags=None }

let mk_rho (k,v) =
	{ empty with rho = (Sl_rho.add k v Sl_rho.empty); _terms=None; _vars=None; _tags=None }
let mk_pto pto =
  { empty with ptos = Sl_ptos.singleton pto; _terms=None; _vars=None; _tags=None }
let mk_eq p =
  { empty with eqs = Sl_uf.add p Sl_uf.empty; _terms=None; _vars=None; _tags=None }
let mk_deq p =
  { empty with deqs = Sl_deqs.singleton p; _terms=None; _vars=None; _tags=None }
let mk_ind pred =
  { empty with inds = Sl_tpreds.singleton pred; _terms=None; _vars=None; _tags=None }

let combine h h' =
	let rho = Sl_rho.union h.rho h'.rho in
  let eqs = Sl_uf.union h.eqs h'.eqs in
  let deqs = Sl_deqs.union h.deqs h'.deqs in
  let ptos = Sl_ptos.union h.ptos h'.ptos in
  let inds = Sl_tpreds.union h.inds h'.inds in
  mk rho eqs deqs ptos inds


let proj_sp h = mk Sl_rho.empty Sl_uf.empty Sl_deqs.empty h.ptos h.inds
let proj_pure h = mk h.rho h.eqs h.deqs Sl_ptos.empty Sl_tpreds.empty (* CREATE PROJ_RHO?? *)

let complete_tags avoid h =
  if Sl_tpreds.for_all Sl_tpred.is_tagged h.inds then h
  else
    let inds =
      Sl_tpreds.fold
        (fun ((_, pred) as p) inds' ->
          let p' =
            if Sl_tpred.is_tagged p then p
            else
              let avoid' = Tags.union avoid (Sl_tpreds.tags inds') in
              let t = Tags.fresh_evar avoid' in
              (t, pred) in
          Sl_tpreds.add p' inds')
        h.inds
        Sl_tpreds.empty in
    with_inds h inds

(* star two formulae together *)
let star f g =
  (* computes all deqs due to a list of ptos *)
  let explode_deqs ptos =
    let cp = Blist.cartesian_hemi_square ptos in
    let s1 =
      (Blist.fold_left (fun s p -> Sl_deqs.add (fst p, Sl_term.nil) s) Sl_deqs.empty ptos) in
    (Blist.fold_left (fun s (p, q) -> Sl_deqs.add (fst p, fst q) s) s1 cp) in
  let newptos = Sl_ptos.union f.ptos g.ptos in
  mk
		(Sl_rho.union f.rho g.rho)
    (Sl_uf.union f.eqs g.eqs)
    (Sl_deqs.union_of_list [f.deqs; g.deqs; explode_deqs (Sl_ptos.elements newptos)])
    newptos
    (Sl_tpreds.union f.inds g.inds)

let diff h h' =
  mk
		(Sl_rho.diff h.rho h'.rho)
        (* FIXME hacky stuff in SH.eqs : in reality a proper way to diff *)
        (* two union-find structures is required *)
    (Sl_uf.of_list
      (Sl_deqs.to_list
        (Sl_deqs.diff
          (Sl_deqs.of_list (Sl_uf.bindings h.eqs))
          (Sl_deqs.of_list (Sl_uf.bindings h'.eqs))
        )))
    (Sl_deqs.diff h.deqs h'.deqs)
    (Sl_ptos.diff h.ptos h'.ptos)
    (Sl_tpreds.diff h.inds h'.inds)

let parse_atom ?(allow_tags=true) st =
  ( attempt (parse_symb keyw_emp >>$ empty) <|>
    attempt ((Sl_tpred.parse ~allow_tags) |>> mk_ind ) <|>
    attempt (Sl_uf.parse |>> mk_eq) <|>
(*    attempt (Sl_rho.parse |>> mk_rho) <|> *)
    attempt (Sl_deqs.parse |>> mk_deq) <|>
    (Sl_pto.parse |>> mk_pto) <?> "atom"
  ) st

let parse ?(allow_tags=true) st =
  (sep_by1 (parse_atom ~allow_tags) (parse_symb symb_star) >>= (fun atoms ->
          return (Blist.foldl star empty atoms)) <?> "symheap") st

let of_string s =
  handle_reply (MParser.parse_string parse s ())

let add_rho h t v =
	{ h with rho = Sl_rho.add t v h.rho; _terms=None; _vars=None; _tags=None }
let add_eq h eq =
  { h with eqs = Sl_uf.add eq h.eqs; _terms=None; _vars=None; _tags=None }
let add_deq h deq =
  { h with deqs = Sl_deqs.add deq h.deqs; _terms=None; _vars=None; _tags=None }
let add_pto h pto = star h (mk_pto pto)
let add_ind h ind = with_inds h (Sl_tpreds.add ind h.inds)


let univ s f =
  let vs = vars f in
  let evs = Sl_term.Set.filter Sl_term.is_exist_var vs in
  let n = Sl_term.Set.cardinal evs in
  if n=0 then f else
  let uvs = Sl_term.fresh_fvars (Sl_term.Set.union s vs) n in
  let theta = Sl_term.Map.of_list (Blist.combine (Sl_term.Set.elements evs) uvs) in
  subst theta f

let subst_existentials h =
  let aux h' =
    let (ex_eqs, non_ex_eqs) =
      Blist.partition
        (fun (x, _) -> Sl_term.is_exist_var x) (Sl_uf.bindings h'.eqs) in
    if ex_eqs =[] then h' else
      (* NB order of subst is reversed so that the greater variable        *)
      (* replaces the lesser this maintains universal vars                 *)
      let h'' =
        { h' with eqs = Sl_uf.of_list non_ex_eqs; _terms=None; _vars=None; _tags=None } in
      subst (Sl_term.Map.of_list ex_eqs) h'' in
  fixpoint aux h

let norm h =
  { h with
		rho = h.rho;
    deqs = Sl_deqs.norm h.eqs h.deqs ;
    ptos = Sl_ptos.norm h.eqs h.ptos ;
    inds = Sl_tpreds.norm h.eqs h.inds;
    _terms=None;
    _vars=None;
    _tags=None
  }

(* FIXME review *)
let project f xs =
  (* let () = assert (Sl_tpreds.is_empty f.inds && Sl_ptos.is_empty f.ptos) in *)
  let trm_nin_lst x =
    not (Sl_term.is_nil x) &&
    not (Blist.exists (fun y -> Sl_term.equal x y) xs) in
  let pair_nin_lst (x, y) = trm_nin_lst x || trm_nin_lst y in
  let rec proj_eqs h =
    let do_eq x y h' =
      let x_nin_lst = trm_nin_lst x in
      let y_nin_lst = trm_nin_lst y in
      if not (x_nin_lst || y_nin_lst) then h' else
      let (x', y') = if x_nin_lst then (y, x) else (x, y) in
      let theta = Sl_subst.singleton y' x' in
      subst theta h' in
    Sl_uf.fold do_eq h.eqs h in
  let proj_deqs g =
    { g with
      deqs = Sl_deqs.filter (fun p -> not (pair_nin_lst p)) g.deqs;
      _terms=None;
      _vars=None;
      _tags=None
    } in
  proj_deqs (proj_eqs f)

(* tags and unification *)

let freshen_tags h' h =
  with_inds h (Sl_tpreds.freshen_tags h'.inds h.inds)

let subst_tags tagpairs h =
  with_inds h (Sl_tpreds.subst_tags tagpairs h.inds)

let unify_partial ?(tagpairs=true) ?(update_check=Fun._true) h h' cont init_state =
  (Sl_tpreds.unify ~total:false ~tagpairs ~update_check h.inds h'.inds
  (Sl_ptos.unify ~total:false ~update_check h.ptos h'.ptos
  (Sl_deqs.unify_partial ~update_check h.deqs h'.deqs
  (Sl_uf.unify_partial ~update_check h.eqs h'.eqs
  (cont)))))
  init_state

let classical_unify ?(inverse=false) ?(tagpairs=true)
    ?(update_check=Fun._true) h h' cont init_state =
  let (h_inv, h'_inv) = Fun.direct inverse Pair.mk h h' in
  (* NB how we don't need an "inverse" version for ptos and inds, since *)
  (* we unify the whole multiset, not a subformula *)
  (Sl_tpreds.unify ~tagpairs ~update_check h_inv.inds h'_inv.inds
  (Sl_ptos.unify ~update_check h_inv.ptos h'_inv.ptos
  (Sl_deqs.unify_partial ~inverse ~update_check h.deqs h'.deqs
  (Sl_uf.unify_partial ~inverse ~update_check h.eqs h'.eqs
  (cont)))))
  init_state

let all_subheaps h =
  let all_ptos = Sl_ptos.subsets h.ptos in
  let all_preds = Sl_tpreds.subsets h.inds in
  let all_deqs = Sl_deqs.subsets h.deqs in
  let all_ufs =
    Blist.map
      (fun xs -> Blist.foldr Sl_uf.remove xs h.eqs)
      (Blist.map
        Sl_term.Set.to_list
        (Sl_term.Set.subsets (Sl_uf.vars h.eqs))) in
   Blist.flatten
    (Blist.map
      (fun ptos ->
        Blist.flatten
          (Blist.map
            (fun preds ->
              Blist.flatten
                (Blist.map
                  (fun deqs ->
                    Blist.map
                      (fun eqs -> mk h.rho eqs deqs ptos preds)
                      all_ufs)
                  all_deqs))
            all_preds))
      all_ptos)
