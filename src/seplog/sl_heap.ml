open Lib
open Util
open Symbols
open MParser

let split_heaps = ref true

type abstract1 = Sl_term.Set.t option
type abstract2 = Tags.t option

type symheap =
  {
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
  Sl_uf.equal h.eqs h'.eqs &&
  Sl_deqs.equal h.deqs h'.deqs &&
  Sl_ptos.equal h.ptos h'.ptos &&
  Sl_tpreds.equal h.inds h'.inds

let equal_upto_tags h h' =
  h == h' ||
  Sl_uf.equal h.eqs h'.eqs &&
  Sl_deqs.equal h.deqs h'.deqs &&
  Sl_ptos.equal h.ptos h'.ptos &&
  Sl_tpreds.equal_upto_tags h.inds h'.inds

include Fixpoint(struct type t = symheap let equal = equal end)

let compare f g =
  if f == g then 0 else
    match Sl_uf.compare f.eqs g.eqs with
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
        (Sl_tpreds.hash h.inds) 
        (Sl_ptos.hash h.ptos)) 
      (Sl_deqs.hash h.deqs)) 
    (Sl_uf.hash h.eqs)

let terms f =
  match f._terms with
  | Some trms -> trms
  | None ->
    let trms = 
      Sl_term.Set.union_of_list
        [ Sl_uf.terms f.eqs; 
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

let tag_pairs f = TagPairs.mk (tags f)

let to_string f =
  let res = String.concat symb_star.sep
      ((Sl_uf.to_string_list f.eqs) @ (Sl_deqs.to_string_list f.deqs) @
        (Sl_ptos.to_string_list f.ptos) @ (Sl_tpreds.to_string_list f.inds)) in
  if res = "" then keyw_emp.str else res

let to_melt f =
  let sep = if !split_heaps then Latex.text " \\\\ \n" else symb_star.melt in
  let content = Latex.concat (Latex.list_insert sep
          (Blist.filter (fun l -> not (Latex.is_empty l))
              [Sl_uf.to_melt f.eqs; Sl_deqs.to_melt f.deqs;
              Sl_ptos.to_melt f.ptos; Sl_tpreds.to_melt f.inds])) in
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
  ltx_mk_math content

let pp fmt h =
  let l =
    ((Sl_uf.to_string_list h.eqs) @ (Sl_deqs.to_string_list h.deqs) @
      (Sl_ptos.to_string_list h.ptos) @ (Sl_tpreds.to_string_list h.inds)) in
  Format.fprintf fmt "@[%a@]" (Blist.pp pp_star Format.pp_print_string) 
    (if l<>[] then l else [keyw_emp.str])

let equates h x y = Sl_uf.equates h.eqs x y

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
  Sl_uf.subsumed h.eqs h'.eqs &&
  Sl_deqs.subsumed h'.eqs h.deqs h'.deqs &&
  Sl_ptos.subsumed ~total h'.eqs h.ptos h'.ptos &&
  Sl_tpreds.subsumed_upto_tags ~total h'.eqs h.inds h'.inds 

let subsumed ?(total=true) h h' = 
  Sl_uf.subsumed h.eqs h'.eqs &&
  Sl_deqs.subsumed h'.eqs h.deqs h'.deqs &&
  Sl_ptos.subsumed ~total h'.eqs h.ptos h'.ptos &&
  Sl_tpreds.subsumed ~total h'.eqs h.inds h'.inds 

  
(* Constructors *)

let mk eqs deqs ptos inds =
  assert 
    (Tags.cardinal (Sl_tpreds.map_to Tags.add Tags.empty fst inds) 
    =
    Sl_tpreds.cardinal inds) ;
  { eqs; deqs; ptos; inds; _terms=None; _vars=None; _tags=None }
  
let dest h = (h.eqs, h.deqs, h.ptos, h.inds)
  
let empty = mk Sl_uf.empty Sl_deqs.empty Sl_ptos.empty Sl_tpreds.empty 

let is_empty h = equal h empty

let subst theta h =
  { eqs = Sl_uf.subst theta h.eqs;
    deqs = Sl_deqs.subst theta h.deqs;
    ptos = Sl_ptos.subst theta h.ptos;
    inds = Sl_tpreds.subst theta h.inds;
    _terms = None;
    _vars = None; 
    _tags=None
  }

let with_eqs h eqs = { h with eqs; _terms=None; _vars=None; _tags=None }
let with_deqs h deqs = { h with deqs; _terms=None; _vars=None; _tags=None }
let with_ptos h ptos = { h with ptos; _terms=None; _vars=None; _tags=None }
let with_inds h inds = mk h.eqs h.deqs h.ptos inds

let del_deq h deq = with_deqs h (Sl_deqs.remove deq h.deqs)
let del_pto h pto = with_ptos h (Sl_ptos.remove pto h.ptos)
let del_ind h ind = 
  { h with inds = Sl_tpreds.remove ind h.inds; _terms=None; _vars=None; _tags=None }

let mk_pto pto = 
  { empty with ptos = Sl_ptos.singleton pto; _terms=None; _vars=None; _tags=None }
let mk_eq p = 
  { empty with eqs = Sl_uf.add p Sl_uf.empty; _terms=None; _vars=None; _tags=None }
let mk_deq p = 
  { empty with deqs = Sl_deqs.singleton p; _terms=None; _vars=None; _tags=None }
let mk_ind pred = 
  { empty with inds = Sl_tpreds.singleton pred; _terms=None; _vars=None; _tags=None }

let combine h h' =
  let eqs = Sl_uf.union h.eqs h'.eqs in
  let deqs = Sl_deqs.union h.deqs h'.deqs in
  let ptos = Sl_ptos.union h.ptos h'.ptos in
  let inds = Sl_tpreds.union h.inds h'.inds in
  mk eqs deqs ptos inds
    

let proj_sp h = mk Sl_uf.empty Sl_deqs.empty h.ptos h.inds
let proj_pure h = mk h.eqs h.deqs Sl_ptos.empty Sl_tpreds.empty

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
    (Sl_uf.union f.eqs g.eqs)
    (Sl_deqs.union_of_list [f.deqs; g.deqs; explode_deqs (Sl_ptos.elements newptos)])
    newptos
    (Sl_tpreds.union f.inds g.inds)
    
let diff h h' =
  mk
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

let parse_atom st =
  ( attempt (parse_symb keyw_emp >>$ empty) <|>
    attempt (Sl_tpred.parse |>> mk_ind ) <|>
    attempt (Sl_uf.parse |>> mk_eq) <|>
    attempt (Sl_deqs.parse |>> mk_deq) <|>
    (Sl_pto.parse |>> mk_pto) <?> "atom"
  ) st

let parse st =
  (sep_by1 parse_atom (parse_symb symb_star) >>= (fun atoms ->
          return (Blist.foldl star empty atoms)) <?> "symheap") st

let of_string s =
  handle_reply (MParser.parse_string parse s ())

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
        (fun p' -> Pair.disj (Pair.map Sl_term.is_exist_var p')) 
        (Sl_uf.bindings h'.eqs) in
    if ex_eqs =[] then h' else
    let ex_eqs = 
      Blist.map (fun ((x,y) as p) -> if Sl_term.is_exist_var x then p else (y,x)) ex_eqs in
      let h'' = 
        { h' with eqs = Sl_uf.of_list non_ex_eqs; _terms=None; _vars=None; _tags=None } in
      subst (Sl_term.Map.of_list ex_eqs) h'' in
  fixpoint aux h

let norm h =
  { h with
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
      let theta = Sl_term.singleton_subst y' x' in
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

let unify_partial ?(tagpairs=false) 
    ?(sub_check=Sl_term.trivial_sub_check)
    ?(cont=Sl_term.trivial_continuation)
    ?(init_state=Sl_term.empty_state) h h' =
  let f1 theta' = Sl_uf.unify_partial ~sub_check ~cont ~init_state:theta' h.eqs h'.eqs in
  let f2 theta' = Sl_deqs.unify_partial ~sub_check ~cont:f1 ~init_state:theta' h.deqs h'.deqs in
  let f3 theta' = Sl_ptos.unify ~total:false ~sub_check ~cont:f2 ~init_state:theta' h.ptos h'.ptos in
  Sl_tpreds.unify ~total:false ~tagpairs ~sub_check ~cont:f3 ~init_state h.inds h'.inds

let classical_unify ?(inverse=false) ?(tagpairs=false)
    ?(sub_check=Sl_term.trivial_sub_check)
    ?(cont=Sl_term.trivial_continuation)
    ?(init_state=Sl_term.empty_state) h h' =
  let f1 theta' = Sl_uf.unify_partial ~inverse ~sub_check ~cont ~init_state:theta' h.eqs h'.eqs in 
  let f2 theta' = Sl_deqs.unify_partial ~inverse ~sub_check ~cont:f1 ~init_state:theta' h.deqs h'.deqs in
  (* NB how we don't need an "inverse" version for ptos and inds, since *)
  (* we unify the whole multiset, not a subformula *)
  let f3 theta' = Fun.direct inverse (Sl_ptos.unify ~sub_check ~cont:f2 ~init_state:theta') h.ptos h'.ptos in 
  Fun.direct inverse (Sl_tpreds.unify ~tagpairs ~sub_check ~cont:f3 ~init_state) h.inds h'.inds
  
let compute_frame ?(freshen_existentials=true) ?(avoid=Sl_term.Set.empty) f f' =
  Option.flatten
    ( Option.mk_lazily
        ((Sl_uf.all_members_of f.eqs f'.eqs)
          && (Sl_deqs.all_members_of f.deqs f'.deqs)
          && (Sl_ptos.all_members_of f.ptos f'.ptos)
          && (Sl_tpreds.all_members_of f.inds f'.inds))
        (fun _ -> 
          let frame = { eqs = Sl_uf.diff f.eqs f'.eqs;
                        deqs = Sl_deqs.diff f'.deqs f.deqs;
                        ptos = Sl_ptos.diff f'.ptos f.ptos;
                        inds = Sl_tpreds.diff f'.inds f.inds;
                        _terms=None; 
                        _vars=None; 
                        _tags=None 
                      } in
          let vs = 
            Sl_term.Set.to_list 
              (Sl_term.Set.inter
                (Sl_term.Set.filter Sl_term.is_exist_var (terms f))
                (Sl_term.Set.filter Sl_term.is_exist_var (terms frame))) in
          Option.mk_lazily
            ((not freshen_existentials) || (Blist.is_empty vs))
            (fun _ -> 
              if (freshen_existentials) then
                let freshvars = 
                  Sl_term.fresh_evars 
                  (Sl_term.Set.union avoid (vars f')) 
                  (Blist.length vs) in
                let theta = 
                  Sl_term.Map.of_list 
                    (Blist.map2 Pair.mk vs freshvars) in
                subst theta frame
              else frame)) )
    
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
                      (fun eqs -> mk eqs deqs ptos preds)
                      all_ufs)
                  all_deqs))
            all_preds))
      all_ptos)


let memory_consuming h =
  Sl_tpreds.is_empty h.inds || not (Sl_ptos.is_empty h.ptos)

let constructively_valued h =
  let freevars = Sl_term.Set.filter Sl_term.is_free_var (vars h) in
  let existvars = Sl_term.Set.filter Sl_term.is_exist_var (vars h) in
  let is_cvalued cvalued v =
    Sl_term.Set.exists (equates h v) cvalued ||
    Sl_ptos.exists 
      (fun (y,zs) -> 
        Sl_term.Set.mem y cvalued && Blist.exists (Sl_term.equal v) zs) 
      h.ptos in
  let rec aux cvalued rest =
    let new_cvalued = Sl_term.Set.filter (is_cvalued cvalued) rest in
    if Sl_term.Set.is_empty new_cvalued 
    then
      Sl_term.Set.is_empty rest 
    else
      aux 
        (Sl_term.Set.union cvalued new_cvalued) 
        (Sl_term.Set.diff rest new_cvalued) in
  aux freevars existvars
    
    
    
    