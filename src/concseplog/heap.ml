open Lib
open   Symbols

open Generic

open MParser

let split_heaps = ref true

type abstract1 = Term.Set.t option

type abstract2 = Tags.t option

type symheap =
  { eqs: Uf.t
  ; deqs: Deqs.t
  ; ptos: Ptos.t
  ; inds: Tpreds.t
  ; mutable _terms: Term.Set.t option
  ; mutable _vars: Term.Set.t option
  ; mutable _tags: Tags.t option }

type t = symheap

(* accessors *)

let equal h h' =
  h == h'
  || Uf.equal h.eqs h'.eqs
     && Deqs.equal h.deqs h'.deqs
     && Ptos.equal h.ptos h'.ptos
     && Tpreds.equal h.inds h'.inds

let equal_upto_tags h h' =
  h == h'
  || Uf.equal h.eqs h'.eqs
     && Deqs.equal h.deqs h'.deqs
     && Ptos.equal h.ptos h'.ptos
     && Tpreds.equal_upto_tags h.inds h'.inds

include Fixpoint.Make (struct
  type t = symheap

  let equal = equal
end)

let compare f g =
  if f == g then 0
  else
    match Uf.compare f.eqs g.eqs with
    | n when not (Int.equal n 0) -> n
    | _ -> (
      match Deqs.compare f.deqs g.deqs with
      | n when not (Int.equal n 0) -> n
      | _ -> (
        match Ptos.compare f.ptos g.ptos with
        | n when not (Int.equal n 0) -> n
        | _ -> Tpreds.compare f.inds g.inds ) )

(* custom hash function so that memoization fields are ignored when hashing *)
(* so that the hash invariant is preserved [a = b => hash(a) = hash(b)] *)
(* FIXME: memoize hash as well? *)
let hash h =
  genhash
    (genhash
       (genhash (Tpreds.hash h.inds) (Ptos.hash h.ptos))
       (Deqs.hash h.deqs))
    (Uf.hash h.eqs)

let terms f =
  match f._terms with
  | Some trms -> trms
  | None ->
      let trms =
        Term.Set.union_of_list
          [ Uf.terms f.eqs
          ; Deqs.terms f.deqs
          ; Ptos.terms f.ptos
          ; Tpreds.terms f.inds ]
      in
      f._terms <- Some trms ;
      trms

let vars f =
  match f._vars with
  | Some v -> v
  | None ->
      let v = Term.filter_vars (terms f) in
      f._vars <- Some v ;
      v

let tags h =
  match h._tags with
  | Some tgs -> tgs
  | None ->
      let tgs = Tpreds.tags h.inds in
      h._tags <- Some tgs ;
      tgs

let tag_pairs f = Tagpairs.mk (tags f)

let has_untagged_preds h = not (Tpreds.for_all Tpred.is_tagged h.inds)

let to_string f =
  let res =
    String.concat symb_star.sep
      ( Uf.to_string_list f.eqs
      @ Deqs.to_string_list f.deqs
      @ Ptos.to_string_list f.ptos
      @ Tpreds.to_string_list f.inds )
  in
  if String.equal res "" then keyw_emp.str else res

let pp fmt h =
  let l =
    Uf.to_string_list h.eqs
    @ Deqs.to_string_list h.deqs
    @ Ptos.to_string_list h.ptos
    @ Tpreds.to_string_list h.inds
  in
  Format.fprintf fmt "@[%a@]"
    (Blist.pp pp_star Format.pp_print_string)
    (if not (Blist.is_empty l) then l else [keyw_emp.str])

let equates h x y = Uf.equates h.eqs x y

let disequates h x y =
  Deqs.exists
    (fun (w, z) ->
      (equates h x w && equates h y z) || (equates h x z && equates h y w) )
    h.deqs

let find_lval x h = Ptos.find_opt (fun (y, _) -> equates h x y) h.ptos

let inconsistent h =
  Deqs.exists (Fun.uncurry Term.equal) h.deqs
  || Deqs.exists (fun (x, y) -> equates h x y) h.deqs

let idents p = Tpreds.idents p.inds

let subsumed_upto_tags ?(total = true) h h' =
  Uf.subsumed h.eqs h'.eqs
  && Deqs.subsumed h'.eqs h.deqs h'.deqs
  && Ptos.subsumed ~total h'.eqs h.ptos h'.ptos
  && Tpreds.subsumed_upto_tags ~total h'.eqs h.inds h'.inds

let subsumed ?(total = true) h h' =
  Uf.subsumed h.eqs h'.eqs
  && Deqs.subsumed h'.eqs h.deqs h'.deqs
  && Ptos.subsumed ~total h'.eqs h.ptos h'.ptos
  && Tpreds.subsumed ~total h'.eqs h.inds h'.inds

(* Constructors *)

let mk eqs deqs ptos inds =
  {eqs; deqs; ptos; inds; _terms= None; _vars= None; _tags= None}

let dest h = (h.eqs, h.deqs, h.ptos, h.inds)

let empty = mk Uf.empty Deqs.empty Ptos.empty Tpreds.empty

let is_empty h = equal h empty

let subst theta h =
  { eqs= Uf.subst theta h.eqs
  ; deqs= Deqs.subst theta h.deqs
  ; ptos= Ptos.subst theta h.ptos
  ; inds= Tpreds.subst theta h.inds
  ; _terms= None
  ; _vars= None
  ; _tags= None }

let with_eqs h eqs = {h with eqs; _terms= None; _vars= None; _tags= None}

let with_deqs h deqs = {h with deqs; _terms= None; _vars= None; _tags= None}

let with_ptos h ptos = {h with ptos; _terms= None; _vars= None; _tags= None}

let with_inds h inds = mk h.eqs h.deqs h.ptos inds

let del_deq h deq = with_deqs h (Deqs.remove deq h.deqs)

let del_pto h pto = with_ptos h (Ptos.remove pto h.ptos)

let del_ind h ind =
  { h with
    inds= Tpreds.remove ind h.inds; _terms= None; _vars= None; _tags= None
  }

let mk_pto pto =
  { empty with
    ptos= Ptos.singleton pto; _terms= None; _vars= None; _tags= None }

let mk_eq p =
  { empty with
    eqs= Uf.add p Uf.empty; _terms= None; _vars= None; _tags= None }

let mk_deq p =
  {empty with deqs= Deqs.singleton p; _terms= None; _vars= None; _tags= None}

let mk_ind pred =
  { empty with
    inds= Tpreds.singleton pred; _terms= None; _vars= None; _tags= None }

let proj_sp h = mk Uf.empty Deqs.empty h.ptos h.inds

let proj_pure h = mk h.eqs h.deqs Ptos.empty Tpreds.empty

let complete_tags avoid h =
  if Tpreds.for_all Tpred.is_tagged h.inds then h
  else
    let inds =
      Tpreds.fold
        (fun ((_, pred) as p) inds' ->
          let p' =
            if Tpred.is_tagged p then p
            else
              let avoid' = Tags.union avoid (Tpreds.tags inds') in
              let t = Tags.fresh_evar avoid' in
              (t, pred)
          in
          Tpreds.add p' inds' )
        h.inds Tpreds.empty
    in
    with_inds h inds

(* computes all deqs due to a list of ptos *)
let explode_deqs h =
  let ptos = Ptos.elements h.ptos in
  let cp = Blist.cartesian_hemi_square ptos in
  let s1 =
    Blist.fold_left
      (fun s p -> Deqs.add (fst p, Term.nil) s)
      Deqs.empty ptos
  in
  let new_deqs =
    Blist.fold_left (fun s (p, q) -> Deqs.add (fst p, fst q) s) s1 cp
  in
  with_deqs h (Deqs.union h.deqs new_deqs)

(* star two formulae together *)
let star ?(augment_deqs = true) f g =
  let h =
    mk (Uf.union f.eqs g.eqs)
      (Deqs.union f.deqs g.deqs)
      (Ptos.union f.ptos g.ptos)
      (Tpreds.union f.inds g.inds)
  in
  if augment_deqs then explode_deqs h else h

let diff h h' =
  mk
    (* FIXME hacky stuff in SH.eqs : in reality a proper way to diff *)
    (* two union-find structures is required *)
    (Uf.of_list
       (Deqs.to_list
          (Deqs.diff
             (Deqs.of_list (Uf.bindings h.eqs))
             (Deqs.of_list (Uf.bindings h'.eqs)))))
    (Deqs.diff h.deqs h'.deqs)
    (Ptos.diff h.ptos h'.ptos)
    (Tpreds.diff h.inds h'.inds)

let parse_atom ?(allow_tags = true) st =
  ( attempt (parse_symb keyw_emp >>$ empty)
  <|> attempt (Tpred.parse ~allow_tags |>> mk_ind)
  <|> attempt (Uf.parse |>> mk_eq)
  <|> attempt (Deqs.parse |>> mk_deq)
  <|> (Pto.parse |>> mk_pto) <?> "atom" )
    st

let parse ?(allow_tags = true) ?(augment_deqs = true) st =
  ( sep_by1 (parse_atom ~allow_tags) (parse_symb symb_star)
  >>= (fun atoms -> return (Blist.foldl (star ~augment_deqs) empty atoms))
  <?> "symheap" )
    st

let of_string ?(allow_tags = true) ?(augment_deqs = true) s =
  handle_reply (MParser.parse_string (parse ~allow_tags ~augment_deqs) s ())

let add_eq h eq =
  {h with eqs= Uf.add eq h.eqs; _terms= None; _vars= None; _tags= None}

let add_deq h deq =
  {h with deqs= Deqs.add deq h.deqs; _terms= None; _vars= None; _tags= None}

let add_pto h pto = star h (mk_pto pto)

let add_ind h ind = with_inds h (Tpreds.add ind h.inds)

let univ s f =
  let vs = vars f in
  let theta =
    Subst.mk_free_subst (Term.Set.union s vs)
      (Term.Set.filter Term.is_exist_var vs)
  in
  if Term.Map.is_empty theta then f else subst theta f

let subst_existentials h =
  let aux h =
    try
      let eqs = Uf.bindings h.eqs in
      let ((x, y) as eq) =
        Blist.find (fun eq -> Pair.disj (Pair.map Term.is_exist_var eq)) eqs
      in
      let eqs = Blist.filter (fun eq' -> eq' != eq) eqs in
      let x, y = if Term.is_exist_var x then eq else (y, x) in
      let h' =
        {h with eqs= Uf.of_list eqs; _terms= None; _vars= None; _tags= None}
      in
      subst (Term.Map.singleton x y) h'
    with Not_found -> h
  in
  fixpoint aux h

let norm h =
  { h with
    deqs= Deqs.norm h.eqs h.deqs
  ; ptos= Ptos.norm h.eqs h.ptos
  ; inds= Tpreds.norm h.eqs h.inds
  ; _terms= None
  ; _vars= None
  ; _tags= None }

(* FIXME review *)
let project f xs =
  (* let () = assert (Tpreds.is_empty f.inds && Ptos.is_empty f.ptos) in *)
  let trm_nin_lst x =
    (not (Term.is_nil x))
    && not (Blist.exists (fun y -> Term.equal x y) xs)
  in
  let pair_nin_lst (x, y) = trm_nin_lst x || trm_nin_lst y in
  let rec proj_eqs h =
    let do_eq x y h' =
      let x_nin_lst = trm_nin_lst x in
      let y_nin_lst = trm_nin_lst y in
      if not (x_nin_lst || y_nin_lst) then h'
      else
        let x', y' = if x_nin_lst then (y, x) else (x, y) in
        let theta = Subst.singleton y' x' in
        subst theta h'
    in
    Uf.fold do_eq h.eqs h
  in
  let proj_deqs g =
    { g with
      deqs= Deqs.filter (fun p -> not (pair_nin_lst p)) g.deqs
    ; _terms= None
    ; _vars= None
    ; _tags= None }
  in
  proj_deqs (proj_eqs f)

(* tags and unification *)

let freshen_tags h' h = with_inds h (Tpreds.freshen_tags h'.inds h.inds)

let subst_tags tagpairs h = with_inds h (Tpreds.subst_tags tagpairs h.inds)

let unify_partial ?(tagpairs = true) ?(update_check = Fun._true) h h' cont
    init_state =
  (Tpreds.unify ~total:false ~tagpairs ~update_check h.inds h'.inds
     (Ptos.unify ~total:false ~update_check h.ptos h'.ptos
        (Deqs.unify_partial ~update_check h.deqs h'.deqs
           (Uf.unify_partial ~update_check h.eqs h'.eqs cont))))
    init_state

let biunify_partial ?(tagpairs = true) ?(update_check = Fun._true) h h' cont
    init_state =
  (Tpreds.biunify ~total:false ~tagpairs ~update_check h.inds h'.inds
     (Ptos.biunify ~total:false ~update_check h.ptos h'.ptos
        (Deqs.biunify_partial ~update_check h.deqs h'.deqs
           (Uf.biunify_partial ~update_check h.eqs h'.eqs cont))))
    init_state

let classical_unify ?(inverse = false) ?(tagpairs = true)
    ?(update_check = Fun._true) h h' cont init_state =
  let h_inv, h'_inv = Fun.direct inverse Pair.mk h h' in
  (* NB how we don't need an "inverse" version for ptos and inds, since *)
  (* we unify the whole multiset, not a subformula *)
  (Tpreds.unify ~tagpairs ~update_check h_inv.inds h'_inv.inds
     (Ptos.unify ~update_check h_inv.ptos h'_inv.ptos
        (Deqs.unify_partial ~inverse ~update_check h.deqs h'.deqs
           (Uf.unify_partial ~inverse ~update_check h.eqs h'.eqs cont))))
    init_state

let classical_biunify ?(tagpairs = true) ?(update_check = Fun._true) h h' cont
    init_state =
  (Tpreds.biunify ~tagpairs ~update_check h.inds h'.inds
     (Ptos.biunify ~update_check h.ptos h'.ptos
        (Deqs.biunify_partial ~update_check h.deqs h'.deqs
           (Uf.biunify_partial ~update_check h.eqs h'.eqs cont))))
    init_state

let all_subheaps h =
  let all_ptos = Ptos.subsets h.ptos in
  let all_preds = Tpreds.subsets h.inds in
  let all_deqs = Deqs.subsets h.deqs in
  let all_ufs =
    Blist.map
      (fun xs -> Blist.foldr Uf.remove xs h.eqs)
      (Blist.map Term.Set.to_list (Term.Set.subsets (Uf.vars h.eqs)))
  in
  Blist.flatten
    (Blist.map
       (fun ptos ->
         Blist.flatten
           (Blist.map
              (fun preds ->
                Blist.flatten
                  (Blist.map
                     (fun deqs ->
                       Blist.map (fun eqs -> mk eqs deqs ptos preds) all_ufs )
                     all_deqs) )
              all_preds) )
       all_ptos)

let memory_consuming h =
  Tpreds.is_empty h.inds || not (Ptos.is_empty h.ptos)

let constructively_valued h =
  let freevars = Term.Set.filter Term.is_free_var (vars h) in
  let existvars = Term.Set.filter Term.is_exist_var (vars h) in
  let is_cvalued cvalued v =
    Term.Set.exists (equates h v) cvalued
    || Ptos.exists
         (fun (y, zs) ->
           Term.Set.mem y cvalued && Blist.exists (Term.equal v) zs )
         h.ptos
  in
  let rec aux cvalued rest =
    let new_cvalued = Term.Set.filter (is_cvalued cvalued) rest in
    if Term.Set.is_empty new_cvalued then Term.Set.is_empty rest
    else
      aux
        (Term.Set.union cvalued new_cvalued)
        (Term.Set.diff rest new_cvalued)
  in
  aux freevars existvars
