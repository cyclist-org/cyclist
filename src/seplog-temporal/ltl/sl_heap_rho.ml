open Generic
open Lib
open Symbols
open MParser
open Seplog

let split_heaps = ref true

type abstract1 = Term.Set.t option
type abstract2 = Tags.t option

type symheap =
  {
		rho : Sl_rho.t;
    eqs : Uf.t;
    deqs : Deqs.t;
    ptos : Ptos.t;
    inds : Tpreds.t;
    mutable _terms : Term.Set.t option;
    mutable _vars : Term.Set.t option;
    mutable _tags : Tags.t option
  }

type t = symheap

(* accessors *)

let equal h h' =
  h == h' ||
	Sl_rho.equal h.rho h'.rho &&
  Uf.equal h.eqs h'.eqs &&
  Deqs.equal h.deqs h'.deqs &&
  Ptos.equal h.ptos h'.ptos &&
  Tpreds.equal h.inds h'.inds

let equal_upto_tags h h' =
  h == h' ||
	Sl_rho.equal h.rho h'.rho &&
  Uf.equal h.eqs h'.eqs &&
  Deqs.equal h.deqs h'.deqs &&
  Ptos.equal h.ptos h'.ptos &&
  Tpreds.equal_upto_tags h.inds h'.inds

include Fixpoint.Make(struct type t = symheap let equal = equal end)

let compare f g =
  if f == g then 0 else
		match Sl_rho.compare f.rho g.rho with
		| n when n<>0 -> n
		| _ -> match Uf.compare f.eqs g.eqs with
   				 | n when n <>0 -> n
    			 | _ -> match Deqs.compare f.deqs g.deqs with
        					| n when n <>0 -> n
        					| _ -> match Ptos.compare f.ptos g.ptos with
            						| n when n <>0 -> n
            						| _ -> Tpreds.compare f.inds g.inds

(* custom hash function so that memoization fields are ignored when hashing *)
(* so that the hash invariant is preserved [a = b => hash(a) = hash(b)] *)
(* FIXME: memoize hash as well? *)
let hash h =
	genhash
  (genhash
    (genhash
      (genhash
        (Tpreds.hash h.inds)
        (Ptos.hash h.ptos))
      (Deqs.hash h.deqs))
    (Uf.hash h.eqs))
	(Sl_rho.hash h.rho)

let terms f =
  match f._terms with
  | Some trms -> trms
  | None ->
    let trms =
      Term.Set.union_of_list
        [ Sl_rho.terms f.rho;
					Uf.terms f.eqs;
          Deqs.terms f.deqs;
          Ptos.terms f.ptos;
          Tpreds.terms f.inds] in
    f._terms <- Some trms;
    trms

let vars f =
  match f._vars with
  | Some v -> v
  | None ->
    let v = Term.filter_vars (terms f) in
    f._vars <- Some v;
    v

let tags h =
  match h._tags with
  | Some tgs -> tgs
  | None ->
    let tgs = Tpreds.tags h.inds in
    h._tags <- Some tgs;
    tgs

let tag_pairs f = Tagpairs.mk (tags f)

let has_untagged_preds h = not (Tpreds.for_all Tpred.is_tagged h.inds)

let to_string f =
  let res = String.concat symb_star.sep
      ((Sl_rho.to_string_list f.rho) @ (Uf.to_string_list f.eqs) @ (Deqs.to_string_list f.deqs) @
        (Ptos.to_string_list f.ptos) @ (Tpreds.to_string_list f.inds)) in
  if res = "" then keyw_emp.str else res

(* let to_melt f =
  let sep = if !split_heaps then Latex.text " \\\\ \n" else symb_star.melt in
  let content = Latex.concat (Latex.list_insert sep
          (Blist.filter (fun l -> not (Latex.is_empty l))
              [Sl_rho.to_melt f.rho; Uf.to_melt f.eqs; Deqs.to_melt f.deqs;
              Ptos.to_melt f.ptos; Tpreds.to_melt f.inds])) in
  let content = if !split_heaps then
      Latex.concat
        [
        ltx_newl;
        Latex.environment
          (* ~opt: (Latex.A, Latex.text "b") *)
          (* ~args:[(Latex.A, Latex.text "l")] *)
          "gathered" (Latex.M, content) Latex.M;
        ltx_newl
        ]
    else
      content in
  ltx_mk_math content *)

let pp fmt h =
  let l =
    ((Sl_rho.to_string_list h.rho) @ (Uf.to_string_list h.eqs) @ (Deqs.to_string_list h.deqs) @
      (Ptos.to_string_list h.ptos) @ (Tpreds.to_string_list h.inds)) in
  Format.fprintf fmt "@[%a@]" (Blist.pp pp_star Format.pp_print_string)
    (if l<>[] then l else [keyw_emp.str])

let equates h x y = Uf.equates h.eqs x y (* IT MIGHT NEED TO CHECK IN RHO *)

let disequates h x y =
  Deqs.exists
    (fun (w, z) ->
          equates h x w && equates h y z
          ||
          equates h x z && equates h y w)
    h.deqs

let find_lval x h =
  Ptos.find_first_opt (fun (y, _) -> equates h x y) h.ptos

let inconsistent h = Deqs.exists (fun (x, y) -> equates h x y) h.deqs

let idents p = Tpreds.idents p.inds

let subsumed_upto_tags ?(total=true) h h' =
	Sl_rho.subsumed h.rho h'.rho &&
  Uf.subsumed h.eqs h'.eqs &&
  Deqs.subsumed h'.eqs h.deqs h'.deqs &&
  Ptos.subsumed ~total h'.eqs h.ptos h'.ptos &&
  Tpreds.subsumed_upto_tags ~total h'.eqs h.inds h'.inds

let subsumed ?(total=true) h h' =
  Sl_rho.subsumed h.rho h'.rho &&
  Uf.subsumed h.eqs h'.eqs &&
  Deqs.subsumed h'.eqs h.deqs h'.deqs &&
  Ptos.subsumed ~total h'.eqs h.ptos h'.ptos &&
  Tpreds.subsumed ~total h'.eqs h.inds h'.inds


(* Constructors *)

let mk rho eqs deqs ptos inds =
  assert
    (Tags.cardinal (Tpreds.map_to Tags.add Tags.empty fst inds)
    =
    Tpreds.cardinal inds) ;
  { rho; eqs; deqs; ptos; inds; _terms=None; _vars=None; _tags=None }

let dest h = (h.rho, h.eqs, h.deqs, h.ptos, h.inds)

let empty = mk Sl_rho.empty Uf.empty Deqs.empty Ptos.empty Tpreds.empty

let is_empty h = equal h empty

let subst theta h =
  { rho = Sl_rho.subst theta h.rho;
		eqs = Uf.subst theta h.eqs;
    deqs = Deqs.subst theta h.deqs;
    ptos = Ptos.subst theta h.ptos;
    inds = Tpreds.subst theta h.inds;
    _terms = None;
    _vars = None;
    _tags=None
  }

let with_rho h rho = { h with rho; _terms=None; _vars=None; _tags=None}
let with_eqs h eqs = { h with eqs; _terms=None; _vars=None; _tags=None }
let with_deqs h deqs = { h with deqs; _terms=None; _vars=None; _tags=None }
let with_ptos h ptos = { h with ptos; _terms=None; _vars=None; _tags=None }
let with_inds h inds = mk h.rho h.eqs h.deqs h.ptos inds

let del_deq h deq = with_deqs h (Deqs.remove deq h.deqs)
let del_pto h pto = with_ptos h (Ptos.remove pto h.ptos)
let del_ind h ind =
  { h with inds = Tpreds.remove ind h.inds; _terms=None; _vars=None; _tags=None }

let mk_rho (k,v) =
	{ empty with rho = (Sl_rho.add k v Sl_rho.empty); _terms=None; _vars=None; _tags=None }
let mk_pto pto =
  { empty with ptos = Ptos.singleton pto; _terms=None; _vars=None; _tags=None }
let mk_eq p =
  { empty with eqs = Uf.add p Uf.empty; _terms=None; _vars=None; _tags=None }
let mk_deq p =
  { empty with deqs = Deqs.singleton p; _terms=None; _vars=None; _tags=None }
let mk_ind pred =
  { empty with inds = Tpreds.singleton pred; _terms=None; _vars=None; _tags=None }

let combine h h' =
	let rho = Sl_rho.union h.rho h'.rho in
  let eqs = Uf.union h.eqs h'.eqs in
  let deqs = Deqs.union h.deqs h'.deqs in
  let ptos = Ptos.union h.ptos h'.ptos in
  let inds = Tpreds.union h.inds h'.inds in
  mk rho eqs deqs ptos inds


let proj_sp h = mk Sl_rho.empty Uf.empty Deqs.empty h.ptos h.inds
let proj_pure h = mk h.rho h.eqs h.deqs Ptos.empty Tpreds.empty (* CREATE PROJ_RHO?? *)

(* star two formulae together *)
let star f g =
  (* computes all deqs due to a list of ptos *)
  let explode_deqs ptos =
    let cp = Blist.cartesian_hemi_square ptos in
    let s1 =
      (Blist.fold_left (fun s p -> Deqs.add (fst p, Term.nil) s) Deqs.empty ptos) in
    (Blist.fold_left (fun s (p, q) -> Deqs.add (fst p, fst q) s) s1 cp) in
  let newptos = Ptos.union f.ptos g.ptos in
  mk
		(Sl_rho.union f.rho g.rho)
    (Uf.union f.eqs g.eqs)
    (Deqs.union_of_list [f.deqs; g.deqs; explode_deqs (Ptos.elements newptos)])
    newptos
    (Tpreds.union f.inds g.inds)

let diff h h' =
  mk
		(Sl_rho.diff h.rho h'.rho)
        (* FIXME hacky stuff in SH.eqs : in reality a proper way to diff *)
        (* two union-find structures is required *)
    (Uf.of_list
      (Deqs.to_list
        (Deqs.diff
          (Deqs.of_list (Uf.bindings h.eqs))
          (Deqs.of_list (Uf.bindings h'.eqs))
        )))
    (Deqs.diff h.deqs h'.deqs)
    (Ptos.diff h.ptos h'.ptos)
    (Tpreds.diff h.inds h'.inds)

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

let parse_atom st =
  ( attempt (parse_symb keyw_emp >>$ empty) <|>
    attempt (Tpred.parse |>> mk_ind ) <|>
    attempt (Uf.parse |>> mk_eq) <|>
(*    attempt (Sl_rho.parse |>> mk_rho) <|> *)
    attempt (Deqs.parse |>> mk_deq) <|>
    (Pto.parse |>> mk_pto) <?> "atom"
  ) st

let parse st =
  (sep_by1 parse_atom (parse_symb symb_star) >>= (fun atoms ->
          return (Blist.foldl star empty atoms)) <?> "symheap") st

let of_string s =
  handle_reply (MParser.parse_string parse s ())

let add_rho h t v =
	{ h with rho = Sl_rho.add t v h.rho; _terms=None; _vars=None; _tags=None }
let add_eq h eq =
  { h with eqs = Uf.add eq h.eqs; _terms=None; _vars=None; _tags=None }
let add_deq h deq =
  { h with deqs = Deqs.add deq h.deqs; _terms=None; _vars=None; _tags=None }
let add_pto h pto = star h (mk_pto pto)
let add_ind h ind = with_inds h (Tpreds.add ind h.inds)


let univ s f =
  let vs = vars f in
  let evs = Term.Set.filter Term.is_exist_var vs in
  let n = Term.Set.cardinal evs in
  if n=0 then f else
  let uvs = Term.fresh_fvars (Term.Set.union s vs) n in
  let theta = Term.Map.of_list (Blist.combine (Term.Set.elements evs) uvs) in
  subst theta f

let subst_existentials h =
  let aux h' =
    let (ex_eqs, non_ex_eqs) =
      Blist.partition
        (fun (x, _) -> Term.is_exist_var x) (Uf.bindings h'.eqs) in
    if ex_eqs =[] then h' else
      (* NB order of subst is reversed so that the greater variable        *)
      (* replaces the lesser this maintains universal vars                 *)
      let h'' =
        { h' with eqs = Uf.of_list non_ex_eqs; _terms=None; _vars=None; _tags=None } in
      subst (Term.Map.of_list ex_eqs) h'' in
  fixpoint aux h

let norm h =
  { h with
		rho = h.rho;
    deqs = Deqs.norm h.eqs h.deqs ;
    ptos = Ptos.norm h.eqs h.ptos ;
    inds = Tpreds.norm h.eqs h.inds;
    _terms=None;
    _vars=None;
    _tags=None
  }

(* FIXME review *)
let project f xs =
  (* let () = assert (Tpreds.is_empty f.inds && Ptos.is_empty f.ptos) in *)
  let trm_nin_lst x =
    not (Term.is_nil x) &&
    not (Blist.exists (fun y -> Term.equal x y) xs) in
  let pair_nin_lst (x, y) = trm_nin_lst x || trm_nin_lst y in
  let rec proj_eqs h =
    let do_eq x y h' =
      let x_nin_lst = trm_nin_lst x in
      let y_nin_lst = trm_nin_lst y in
      if not (x_nin_lst || y_nin_lst) then h' else
      let (x', y') = if x_nin_lst then (y, x) else (x, y) in
      let theta = Term.Subst.singleton y' x' in
      subst theta h' in
    Uf.fold do_eq h.eqs h in
  let proj_deqs g =
    { g with
      deqs = Deqs.filter (fun p -> not (pair_nin_lst p)) g.deqs;
      _terms=None;
      _vars=None;
      _tags=None
    } in
  proj_deqs (proj_eqs f)

(* tags and unification *)

let freshen_tags h' h =
  with_inds h (Tpreds.freshen_tags h'.inds h.inds)

let subst_tags tagpairs h =
  with_inds h (Tpreds.subst_tags tagpairs h.inds)

let unify_partial ?(tagpairs=false)
    ?(update_check=Fun._true)
    h h' cont =
  let f1 theta' = Uf.unify_partial ~update_check h.eqs h'.eqs cont theta' in
  let f2 theta' = Deqs.unify_partial ~update_check h.deqs h'.deqs f1 theta' in
  let f3 theta' = Ptos.unify ~total:false ~update_check h.ptos h'.ptos f2 theta' in
  Tpreds.unify ~total:false ~tagpairs ~update_check h.inds h'.inds f3

let classical_unify ?(inverse=false) ?(tagpairs=false)
    ?(update_check=Fun._true)
    h h' cont =
  let f1 theta' = Uf.unify_partial ~inverse ~update_check h.eqs h'.eqs cont theta' in
  let f2 theta' = Deqs.unify_partial ~inverse ~update_check h.deqs h'.deqs f1 theta' in
  (* NB how we don't need an "inverse" version for ptos and inds, since *)
  (* we unify the whole multiset, not a subformula *)
  let f3 theta' = Fun.direct inverse (Ptos.unify ~update_check) h.ptos h'.ptos f2 theta' in
  Fun.direct inverse (Tpreds.unify ~tagpairs ~update_check) h.inds h'.inds f3

let compute_frame ?(freshen_existentials=true) ?(avoid=Term.Set.empty) f f' =
  Option.flatten
    ( Option.mk_lazily
        ((Sl_rho.all_members_of f.rho f'.rho)
					&& (Uf.all_members_of f.eqs f'.eqs)
          && (Deqs.subset f.deqs f'.deqs)
          && (Ptos.subset f.ptos f'.ptos)
          && (Tpreds.subset f.inds f'.inds))
        (fun _ ->
          let frame = { rho = Sl_rho.diff f.rho f'.rho;
												eqs = Uf.diff f.eqs f'.eqs;
                        deqs = Deqs.diff f'.deqs f.deqs;
                        ptos = Ptos.diff f'.ptos f.ptos;
                        inds = Tpreds.diff f'.inds f.inds;
                        _terms=None;
                        _vars=None;
                        _tags=None
                      } in
          let vs =
            Term.Set.to_list
              (Term.Set.inter
                (Term.Set.filter Term.is_exist_var (terms f))
                (Term.Set.filter Term.is_exist_var (terms frame))) in
          Option.mk_lazily
            ((not freshen_existentials) || (Blist.is_empty vs))
            (fun _ ->
              if (freshen_existentials) then
                let freshvars =
                  Term.fresh_evars
                  (Term.Set.union avoid (vars f'))
                  (Blist.length vs) in
                let theta =
                  Term.Map.of_list
                    (Blist.map2 Pair.mk vs freshvars) in
                subst theta frame
              else frame)) )

let all_subheaps h =
  let all_ptos = Ptos.subsets h.ptos in
  let all_preds = Tpreds.subsets h.inds in
  let all_deqs = Deqs.subsets h.deqs in
  let all_ufs =
    Blist.map
      (fun xs -> Blist.foldr Uf.remove xs h.eqs)
      (Blist.map
        Term.Set.to_list
        (Term.Set.subsets (Uf.vars h.eqs))) in
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
