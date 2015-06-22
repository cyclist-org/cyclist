open Lib
open Util
open Symbols
open MParser

let split_heaps = ref true

type symheap =
  {
    eqs : Sld_uf.t;
    deqs : Sld_deqs.t;
    ptos : Sld_ptos.t;
    inds : Sld_tpreds.t
  }

type t = symheap

(* accessors *)

let equal h h' =
  h == h' ||
  Sld_uf.equal h.eqs h'.eqs &&
  Sld_deqs.equal h.deqs h'.deqs &&
  Sld_ptos.equal h.ptos h'.ptos &&
  Sld_tpreds.equal h.inds h'.inds

let equal_upto_tags h h' =
  h == h' ||
  Sld_uf.equal h.eqs h'.eqs &&
  Sld_deqs.equal h.deqs h'.deqs &&
  Sld_ptos.equal h.ptos h'.ptos &&
  Sld_tpreds.equal_upto_tags h.inds h'.inds

include Fixpoint(struct type t = symheap let equal = equal end)

let compare f g =
  if f == g then 0 else
    match Sld_uf.compare f.eqs g.eqs with
    | n when n <>0 -> n
    | _ -> match Sld_deqs.compare f.deqs g.deqs with
        | n when n <>0 -> n
        | _ -> match Sld_ptos.compare f.ptos g.ptos with
            | n when n <>0 -> n
            | _ -> Sld_tpreds.compare f.inds g.inds

let hash = Hashtbl.hash

let terms f =
  Sld_term.Set.union_of_list
    [ Sld_uf.terms f.eqs; 
      Sld_deqs.terms f.deqs; 
      Sld_ptos.terms f.ptos; 
      Sld_tpreds.terms f.inds]

let vars f = Sld_term.filter_vars (terms f)

let tags h = Sld_tpreds.tags h.inds

let tag_pairs f = TagPairs.mk (tags f)

let to_string f =
  let res = String.concat symb_star.sep
      ((Sld_uf.to_string_list f.eqs) @ (Sld_deqs.to_string_list f.deqs) @
        (Sld_ptos.to_string_list f.ptos) @ (Sld_tpreds.to_string_list f.inds)) in
  if res = "" then keyw_emp.str else res

let to_melt f =
  let sep = if !split_heaps then Latex.text " \\\\ \n" else symb_star.melt in
  let content = Latex.concat (Latex.list_insert sep
          (Blist.filter (fun l -> not (Latex.is_empty l))
              [Sld_uf.to_melt f.eqs; Sld_deqs.to_melt f.deqs;
              Sld_ptos.to_melt f.ptos; Sld_tpreds.to_melt f.inds])) in
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
    ((Sld_uf.to_string_list h.eqs) @ (Sld_deqs.to_string_list h.deqs) @
      (Sld_ptos.to_string_list h.ptos) @ (Sld_tpreds.to_string_list h.inds)) in
  Format.fprintf fmt "@[%a@]" (Blist.pp pp_star Format.pp_print_string) 
    (if l<>[] then l else [keyw_emp.str])

let equates h x y = Sld_uf.equates h.eqs x y

let disequates h x y =
  Sld_deqs.exists
    (fun (w, z) ->
          equates h x w && equates h y z
          || 
          equates h x z && equates h y w) 
    h.deqs

let find_lval x h =
  try
    Some (Sld_ptos.find (fun (y, _) -> equates h x y) h.ptos)
  with Not_found -> None

let inconsistent h = Sld_deqs.exists (fun (x, y) -> equates h x y) h.deqs

let idents p = Sld_tpreds.idents p.inds

let subsumed_upto_tags ?(total=true) h h' = 
  Sld_uf.subsumed h.eqs h'.eqs &&
  Sld_deqs.subsumed h'.eqs h.deqs h'.deqs &&
  Sld_ptos.subsumed ~total h'.eqs h.ptos h'.ptos &&
  Sld_tpreds.subsumed_upto_tags ~total h'.eqs h.inds h'.inds 

let subsumed ?(total=true) h h' = 
  Sld_uf.subsumed h.eqs h'.eqs &&
  Sld_deqs.subsumed h'.eqs h.deqs h'.deqs &&
  Sld_ptos.subsumed ~total h'.eqs h.ptos h'.ptos &&
  Sld_tpreds.subsumed ~total h'.eqs h.inds h'.inds 

  
(* Constructors *)

let mk eqs deqs ptos inds =
  assert 
    (Tags.cardinal (Sld_tpreds.map_to Tags.add Tags.empty fst inds) 
    =
    Sld_tpreds.cardinal inds) ;
  { eqs; deqs; ptos; inds }
  
let dest h = (h.eqs, h.deqs, h.ptos, h.inds)
  
let empty = mk Sld_uf.empty Sld_deqs.empty Sld_ptos.empty Sld_tpreds.empty 

let is_empty h = equal h empty

let subst theta h =
  { eqs = Sld_uf.subst theta h.eqs;
    deqs = Sld_deqs.subst theta h.deqs;
    ptos = Sld_ptos.subst theta h.ptos;
    inds = Sld_tpreds.subst theta h.inds
  }

let with_eqs h eqs = { h with eqs }
let with_deqs h deqs = { h with deqs }
let with_ptos h ptos = { h with ptos }
let with_inds h inds = mk h.eqs h.deqs h.ptos inds

let del_deq h deq = with_deqs h (Sld_deqs.remove deq h.deqs)
let del_pto h pto = with_ptos h (Sld_ptos.remove pto h.ptos)
let del_ind h ind = { h with inds = Sld_tpreds.remove ind h.inds }

let mk_pto pto = { empty with ptos = Sld_ptos.singleton pto }
let mk_eq p = { empty with eqs = Sld_uf.add p Sld_uf.empty }
let mk_deq p = { empty with deqs = Sld_deqs.singleton p }
let mk_ind pred = { empty with inds = Sld_tpreds.singleton pred }

let combine h h' =
  let eqs = Sld_uf.union h.eqs h'.eqs in
  let deqs = Sld_deqs.union h.deqs h'.deqs in
  let ptos = Sld_ptos.union h.ptos h'.ptos in
  let inds = Sld_tpreds.union h.inds h'.inds in
  mk eqs deqs ptos inds
    

let proj_sp h = mk Sld_uf.empty Sld_deqs.empty h.ptos h.inds
let proj_pure h = mk h.eqs h.deqs Sld_ptos.empty Sld_tpreds.empty

(* star two formulae together *)
let star f g =
  (* computes all deqs due to a list of ptos *)
  let explode_deqs ptos =
    let cp = Blist.cartesian_hemi_square ptos in
    let s1 =
      (Blist.fold_left (fun s p -> Sld_deqs.add (fst p, Sld_term.nil) s) Sld_deqs.empty ptos) in
    (Blist.fold_left (fun s (p, q) -> Sld_deqs.add (fst p, fst q) s) s1 cp) in
  let newptos = Sld_ptos.union f.ptos g.ptos in
  mk 
    (Sld_uf.union f.eqs g.eqs)
    (Sld_deqs.union_of_list [f.deqs; g.deqs; explode_deqs (Sld_ptos.elements newptos)])
    newptos
    (Sld_tpreds.union f.inds g.inds)
    
let diff h h' =
  mk
        (* FIXME hacky stuff in SH.eqs : in reality a proper way to diff *)
        (* two union-find structures is required *)
    (Sld_uf.of_list
      (Sld_deqs.to_list
        (Sld_deqs.diff
          (Sld_deqs.of_list (Sld_uf.bindings h.eqs))
          (Sld_deqs.of_list (Sld_uf.bindings h'.eqs))
        )))
    (Sld_deqs.diff h.deqs h'.deqs)
    (Sld_ptos.diff h.ptos h'.ptos)
    (Sld_tpreds.diff h.inds h'.inds)  

let parse_atom st =
  ( attempt (parse_symb keyw_emp >>$ empty) <|>
    attempt (Sld_tpred.parse |>> mk_ind ) <|>
    attempt (Sld_uf.parse |>> mk_eq) <|>
    attempt (Sld_deqs.parse |>> mk_deq) <|>
    (Sld_ptos.parse |>> mk_pto) <?> "atom"
  ) st

let parse st =
  (sep_by1 parse_atom (parse_symb symb_star) >>= (fun atoms ->
          return (Blist.foldl star empty atoms)) <?> "symheap") st

let of_string s =
  handle_reply (MParser.parse_string parse s ())

let add_eq h eq = { h with eqs = Sld_uf.add eq h.eqs }
let add_deq h deq = { h with deqs = Sld_deqs.add deq h.deqs }
let add_pto h pto = star h (mk_pto pto) 
let add_ind h ind = with_inds h (Sld_tpreds.add ind h.inds)


let univ s f =
  let vs = vars f in
  let evs = Sld_term.Set.filter Sld_term.is_exist_var vs in
  let n = Sld_term.Set.cardinal evs in
  if n=0 then f else
  let uvs = Sld_term.fresh_uvars (Sld_term.Set.union s vs) n in
  let theta = Sld_term.Map.of_list (Blist.combine (Sld_term.Set.elements evs) uvs) in
  subst theta f

let subst_existentials h =
  let aux h' =
    let (ex_eqs, non_ex_eqs) =
      Blist.partition
        (fun (x, _) -> Sld_term.is_exist_var x) (Sld_uf.bindings h'.eqs) in
    if ex_eqs =[] then h' else
      (* NB order of subst is reversed so that the greater variable        *)
      (* replaces the lesser this maintains universal vars                 *)
      let h'' = { h' with eqs = Sld_uf.of_list non_ex_eqs } in
      subst (Sld_term.Map.of_list ex_eqs) h'' in
  fixpoint aux h

let norm h =
  { h with
    deqs = Sld_deqs.norm h.eqs h.deqs ;
    ptos = Sld_ptos.norm h.eqs h.ptos ;
    inds = Sld_tpreds.norm h.eqs h.inds
  }

(* FIXME review *)
let project f xs =
  (* let () = assert (Sld_tpreds.is_empty f.inds && Sld_ptos.is_empty f.ptos) in *)
  let trm_nin_lst x =
    not (Sld_term.is_nil x) &&
    not (Blist.exists (fun y -> Sld_term.equal x y) xs) in
  let pair_nin_lst (x, y) = trm_nin_lst x || trm_nin_lst y in
  let rec proj_eqs h =
    let do_eq x y h' =
      let x_nin_lst = trm_nin_lst x in
      let y_nin_lst = trm_nin_lst y in
      if not (x_nin_lst || y_nin_lst) then h' else
      let (x', y') = if x_nin_lst then (y, x) else (x, y) in
      let theta = Sld_term.singleton_subst y' x' in
      subst theta h' in
    Sld_uf.fold do_eq h.eqs h in
  let proj_deqs g =
    { g with deqs = Sld_deqs.filter (fun p -> not (pair_nin_lst p)) g.deqs } in
  proj_deqs (proj_eqs f)

(* tags and unification *)

let freshen_tags h' h =
  with_inds h (Sld_tpreds.freshen_tags h'.inds h.inds)

let subst_tags tagpairs h =
  with_inds h (Sld_tpreds.subst_tags tagpairs h.inds)

let unify_partial ?(tagpairs=false) 
    ?(sub_check=Sld_term.trivial_sub_check)
    ?(cont=Sld_term.trivial_continuation)
    ?(init_state=Sld_term.empty_state) h h' =
  let f1 theta' = Sld_uf.unify_partial ~sub_check ~cont ~init_state:theta' h.eqs h'.eqs in
  let f2 theta' = Sld_deqs.unify_partial ~sub_check ~cont:f1 ~init_state:theta' h.deqs h'.deqs in
  let f3 theta' = Sld_ptos.unify ~total:false ~sub_check ~cont:f2 ~init_state:theta' h.ptos h'.ptos in
  Sld_tpreds.unify ~total:false ~tagpairs ~sub_check ~cont:f3 ~init_state h.inds h'.inds

let classical_unify ?(inverse=false) ?(tagpairs=false)
    ?(sub_check=Sld_term.trivial_sub_check)
    ?(cont=Sld_term.trivial_continuation)
    ?(init_state=Sld_term.empty_state) h h' =
  let f1 theta' = Sld_uf.unify_partial ~inverse ~sub_check ~cont ~init_state:theta' h.eqs h'.eqs in 
  let f2 theta' = Sld_deqs.unify_partial ~inverse ~sub_check ~cont:f1 ~init_state:theta' h.deqs h'.deqs in
  (* NB how we don't need an "inverse" version for ptos and inds, since *)
  (* we unify the whole multiset, not a subformula *)
  let f3 theta' = Fun.direct inverse (Sld_ptos.unify ~sub_check ~cont:f2 ~init_state:theta') h.ptos h'.ptos in 
  Fun.direct inverse (Sld_tpreds.unify ~tagpairs ~sub_check ~cont:f3 ~init_state) h.inds h'.inds
  
let compute_frame ?(freshen_existentials=true) ?(avoid=Sld_term.Set.empty) f f' =
  Option.flatten
    ( Option.mk_lazily
        ((Sld_uf.all_members_of f.eqs f'.eqs)
          && (Sld_deqs.all_members_of f.deqs f'.deqs)
          && (Sld_ptos.all_members_of f.ptos f'.ptos)
          && (Sld_tpreds.all_members_of f.inds f'.inds))
        (fun _ -> 
          let frame = { eqs = Sld_uf.diff f.eqs f'.eqs;
                        deqs = Sld_deqs.diff f'.deqs f.deqs;
                        ptos = Sld_ptos.diff f'.ptos f.ptos;
                        inds = Sld_tpreds.diff f'.inds f.inds; } in
          let vs = 
            Sld_term.Set.to_list 
              (Sld_term.Set.inter
                (Sld_term.Set.filter Sld_term.is_exist_var (terms f))
                (Sld_term.Set.filter Sld_term.is_exist_var (terms frame))) in
          Option.mk_lazily
            ((not freshen_existentials) || (Blist.is_empty vs))
            (fun _ -> 
              if (freshen_existentials) then
                let freshvars = 
                  Sld_term.fresh_evars 
                  (Sld_term.Set.union avoid (vars f')) 
                  (Blist.length vs) in
                let theta = 
                  Sld_term.Map.of_list 
                    (Blist.map2 Pair.mk vs freshvars) in
                subst theta frame
              else frame)) )
    
let all_subheaps h =
  let all_ptos = Sld_ptos.subsets h.ptos in
  let all_preds = Sld_tpreds.subsets h.inds in
  let all_deqs = Sld_deqs.subsets h.deqs in
  let all_ufs =
    Blist.map
      (fun xs -> Blist.foldr Sld_uf.remove xs h.eqs) 
      (Blist.map 
        Sld_term.Set.to_list 
        (Sld_term.Set.subsets (Sld_uf.vars h.eqs))) in
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
