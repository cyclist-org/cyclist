open Lib
open Util
open Symbols
open MParser

let split_heaps = ref true

type symheap =
  {
    eqs : Sl_uf.t;
    deqs : Sl_deqs.t;
    ptos : Sl_ptos.t;
    inds : Sl_tpreds.t
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

let hash = Hashtbl.hash

let terms f =
  Sl_term.Set.union_of_list
    [ Sl_uf.terms f.eqs; 
      Sl_deqs.terms f.deqs; 
      Sl_ptos.terms f.ptos; 
      Sl_tpreds.terms f.inds]

let vars f = Sl_term.filter_vars (terms f)

let tags h = Sl_tpreds.tags h.inds

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
          ~opt: (Latex.A, Latex.text "b")
          ~args:[(Latex.A, Latex.text "l")]
          "array" (Latex.M, content) Latex.M;
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
  try
    Some (Sl_ptos.find (fun (y, _) -> equates h x y) h.ptos)
  with Not_found -> None

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
  { eqs; deqs; ptos; inds }
  
let empty = mk Sl_uf.empty Sl_deqs.empty Sl_ptos.empty Sl_tpreds.empty 

let is_empty h = equal h empty

let subst theta h =
  { eqs = Sl_uf.subst theta h.eqs;
    deqs = Sl_deqs.subst theta h.deqs;
    ptos = Sl_ptos.subst theta h.ptos;
    inds = Sl_tpreds.subst theta h.inds
  }

let with_eqs h eqs = { h with eqs }
let with_deqs h deqs = { h with deqs }
let with_ptos h ptos = { h with ptos }
let with_inds h inds = mk h.eqs h.deqs h.ptos inds

let del_deq h deq = with_deqs h (Sl_deqs.remove deq h.deqs)
let del_pto h pto = with_ptos h (Sl_ptos.remove pto h.ptos)
let del_ind h ind = { h with inds = Sl_tpreds.remove ind h.inds }

let mk_pto pto = { empty with ptos = Sl_ptos.singleton pto }
let mk_eq p = { empty with eqs = Sl_uf.add p Sl_uf.empty }
let mk_deq p = { empty with deqs = Sl_deqs.singleton p }
let mk_ind pred = { empty with inds = Sl_tpreds.singleton pred }

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

let parse_atom st =
  ( attempt (parse_symb keyw_emp >>$ empty) <|>
    attempt (Sl_tpred.parse |>> mk_ind ) <|>
    attempt (Sl_uf.parse |>> mk_eq) <|>
    attempt (Sl_deqs.parse |>> mk_deq) <|>
    (Sl_ptos.parse |>> mk_pto) <?> "atom"
  ) st

let parse st =
  (sep_by1 parse_atom (parse_symb symb_star) >>= (fun atoms ->
          return (Blist.foldl star empty atoms)) <?> "symheap") st

let of_string s =
  handle_reply (MParser.parse_string parse s ())

let add_eq h eq = { h with eqs = Sl_uf.add eq h.eqs }
let add_deq h deq = { h with deqs = Sl_deqs.add deq h.deqs }
let add_pto h pto = star h (mk_pto pto) 
let add_ind h ind = with_inds h (Sl_tpreds.add ind h.inds)


let univ s f =
  let vs = vars f in
  let evs = Sl_term.Set.filter Sl_term.is_exist_var vs in
  let n = Sl_term.Set.cardinal evs in
  if n=0 then f else
  let uvs = Sl_term.fresh_uvars (Sl_term.Set.union s vs) n in
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
      let h'' = { h' with eqs = Sl_uf.of_list non_ex_eqs } in
      subst (Sl_term.Map.of_list ex_eqs) h'' in
  fixpoint aux h

let norm h =
  { h with
    deqs = Sl_deqs.norm h.eqs h.deqs ;
    ptos = Sl_ptos.norm h.eqs h.ptos ;
    inds = Sl_tpreds.norm h.eqs h.inds
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
    { g with deqs = Sl_deqs.filter (fun p -> not (pair_nin_lst p)) g.deqs } in
  proj_deqs (proj_eqs f)

(* tags and unification *)

let freshen_tags h' h =
  with_inds h (Sl_tpreds.freshen_tags h'.inds h.inds)

let subst_tags tagpairs h =
  with_inds h (Sl_tpreds.subst_tags tagpairs h.inds)

let unify_partial ?(tagpairs=false) cont theta h h' =
  let f1 theta' = Sl_uf.unify_partial cont theta' h.eqs h'.eqs in
  let f2 theta' = Sl_deqs.unify_partial f1 theta' h.deqs h'.deqs in
  let f3 theta' = Sl_ptos.unify ~total:false f2 theta' h.ptos h'.ptos in
  Sl_tpreds.unify ~total:false ~tagpairs f3 theta h.inds h'.inds

let direct inverse = 
  if not inverse then 
    Fun.id
  else
    Fun.swap
  
let classical_unify ?(inverse=false) ?(tagpairs=false) cont theta h h' =
  let f1 theta' = Sl_uf.unify_partial ~inverse cont theta' h.eqs h'.eqs in 
  let f2 theta' = Sl_deqs.unify_partial ~inverse f1 theta' h.deqs h'.deqs in
  (* NB how we don't need an "inverse" version for ptos and inds, since *)
  (* we unify the whole multiset, not a subformula *)
  let f3 theta' = direct inverse (Sl_ptos.unify f2 theta') h.ptos h'.ptos in 
  direct inverse (Sl_tpreds.unify ~tagpairs f3 theta) h.inds h'.inds
  
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
                        inds = Sl_tpreds.diff f'.inds f.inds; } in
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
    
