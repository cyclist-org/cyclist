open Lib
open Util
open Symbols
open MParser
open Symheap

type symheap =
  {
    eqs : UF.t;
    deqs : Deqs.t;
    ptos : Ptos.t;
    inds : Inds.t
  }

type t = symheap

let empty =
  { eqs = UF.empty; deqs = Deqs.empty; ptos = Ptos.empty; inds = Inds.empty }

let subst theta h =
  { eqs = UF.subst theta h.eqs;
    deqs = Deqs.subst theta h.deqs;
    ptos = Ptos.subst theta h.ptos;
    inds = Inds.subst theta h.inds
  }

let norm h =
  let theta = UF.to_subst h.eqs in
  { h with
    deqs = Deqs.subst theta h.deqs;
    ptos = Ptos.subst theta h.ptos;
    inds = Inds.subst theta h.inds
  }

let get_idents p =
  Inds.map_to Strng.MSet.add Strng.MSet.empty (fun (_, (id, _)) -> id) p.inds

let terms f =
  Sl_term.Set.union_of_list
    [UF.vars f.eqs; Deqs.vars f.deqs; Ptos.vars f.ptos; Inds.vars f.inds]

let vars f = Sl_term.filter_vars (terms f)

let tags l =
  Inds.map_to Tags.add Tags.empty (fun i -> fst i) l.inds

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
  let content = if (Latex.is_empty content) then symb_emp.melt else content in
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
    ((UF.to_string_list h.eqs) @ (Deqs.to_string_list h.deqs) @
      (Ptos.to_string_list h.ptos) @ (Inds.to_string_list h.inds)) in
  if l <> [] then
    Format.fprintf fmt "@[%a@]" (Blist.pp pp_star Format.pp_print_string) l
  else
    Format.fprintf fmt "@[emp@]"

(* star two formulae together *)
let star f g =
  (* computes all deqs due to a list of ptos - no normalization *)
  let explode_deqs ptos =
    let cp = Blist.cartesian_hemi_square ptos in
    let s1 =
      (Blist.fold_left (fun s p -> Deqs.add (fst p, Sl_term.nil) s) Deqs.empty ptos) in
    (Blist.fold_left (fun s (p, q) -> Deqs.add (fst p, fst q) s) s1 cp) in
  let newptos = Ptos.union f.ptos g.ptos in
  (* norm *)
  {
    eqs = UF.union f.eqs g.eqs;
    deqs = Deqs.union_of_list
        [f.deqs; g.deqs; explode_deqs (Ptos.elements newptos)];
    ptos = newptos;
    inds = Inds.union f.inds g.inds
  }

let univ s f =
  let vs = vars f in
  let evs = Sl_term.Set.filter Sl_term.is_exist_var vs in
  let n = Sl_term.Set.cardinal evs in
  if n =0 then f else
    let uvs = Sl_term.fresh_uvars (Sl_term.Set.union s vs) n in
    let theta = Sl_term.Map.of_list (Blist.combine (Sl_term.Set.elements evs) uvs) in
    subst theta f

let repl_tags t f =
  { f with inds = Inds.endomap (fun (_, p) -> (t, p)) f.inds }

let tag_pairs f = TagPairs.mk (tags f)

let mk_pto v1 v2 = { empty with ptos = Ptos.singleton (v1, v2) }
let mk_eq v1 v2 = { empty with eqs = UF.add (v1, v2) UF.empty }
let mk_deq v1 v2 = { empty with deqs = Deqs.singleton (v1, v2) }
let mk_ind tag ident vs =
  { empty with inds = Inds.singleton (tag, (ident, vs)) }

let equates h x y = UF.equates h.eqs x y
let disequates h x y =
  Deqs.exists
    (fun (w, z) ->
          (equates h x w && equates h y z)
          || (equates h x z && equates h y w) ) h.deqs

let eq_class h x = Sl_term.Set.filter (equates h x) (terms h)

let aux_subsumption left spw hook theta h h' =
  let f1 theta' = UF.uni_subsumption left hook theta' h.eqs h'.eqs in
  let f2 theta' = Deqs.uni_subsumption left f1 theta' h.deqs h'.deqs in
  let f3 theta' = Ptos.aux_subsumption left spw f2 theta' h.ptos h'.ptos in
  Inds.aux_subsumption left spw f3 theta h.inds h'.inds

let spw_left_subsumption hook theta h h' =
  aux_subsumption true true hook theta h h'

let equal h h' =
  h == h' ||
  UF.equal h.eqs h'.eqs &&
  Deqs.equal h.deqs h'.deqs &&
  Ptos.equal h.ptos h'.ptos &&
  Inds.equal h.inds h'.inds

include Fixpoint(struct type t = symheap let equal = equal end)

let compare f g =
  if f == g then 0 else
    match UF.compare f.eqs g.eqs with
    | n when n <>0 -> n
    | _ -> match Deqs.compare f.deqs g.deqs with
        | n when n <>0 -> n
        | _ -> match Ptos.compare f.ptos g.ptos with
            | n when n <>0 -> n
            | _ -> Inds.compare f.inds g.inds

(* h' |- h if spw=false h' |- h * true if spw=true *)
let aux_subsumed_wrt_tags spw tags h h' =
  let h = subst (UF.to_subst h'.eqs) h in
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
    Some (Ptos.find (fun (y, _) -> equates h x y) h.ptos)
  with Not_found -> None

let inconsistent h =
  (* let lvalues = Ptos.map_to Sl_term.Set.add Sl_term.Set.empty fst h.ptos in   *)
  (* Sl_term.Set.cardinal lvalues <> Ptos.cardinal h.ptos ||                  *)
  Deqs.exists (fun (x, y) -> equates h x y) h.deqs
(* Sl_term.Set.mem Sl_term.nil lvalues *)

let subst_existentials h =
  let aux h' =
    let (ex_eqs, non_ex_eqs) =
      Blist.partition
        (fun (x, _) -> Sl_term.is_exist_var x) (UF.bindings h'.eqs) in
    if ex_eqs =[] then h' else
      (* NB order of subst is reversed so that the greater variable        *)
      (* replaces the lesser this maintains universal vars                 *)
      let h'' = { h' with eqs = UF.of_list non_ex_eqs } in
      subst (Sl_term.Map.of_list ex_eqs) h'' in
  fixpoint aux h

let is_fresh_in x h = not (Sl_term.Set.mem x (vars h))

let hash (h: t) = Hashtbl.hash h

let project f xs =
  (* let () = assert (Inds.is_empty f.inds && Ptos.is_empty f.ptos) in *)
  let trm_nin_lst x =
    not (Sl_term.is_nil x) &&
    not (Blist.exists (fun y -> Sl_term.equal x y) xs) in
  let pair_nin_lst (x, y) = trm_nin_lst x || trm_nin_lst y in
  let rec proj_eqs h =
    let orig_eqs = UF.bindings h.eqs in
    let p = Blist.find_first pair_nin_lst orig_eqs in
    if Option.is_none p then h else
      let (x, y) = Option.get p in
      (* let new_eqs = List.filter (fun (x',y') -> not (Sl_term.equal x x')   *)
      (* || not (Sl_term.equal y y')) orig_eqs in                             *)
      let (x', y') = if trm_nin_lst x then (y, x) else (x, y) in
      let theta = Sl_term.singleton_subst y' x' in
      proj_eqs (subst theta h) in
  let proj_deqs g =
    { g with deqs = Deqs.filter (fun p -> not (pair_nin_lst p)) g.deqs } in
  proj_deqs (proj_eqs f)

let parse_atom st =
  ( attempt (parse_symb keyw_emp >>$ empty) <|>
    attempt (Inds.parse |>> (fun (tag, (ident, vs)) -> mk_ind tag ident vs)) <|>
    attempt (UF.parse |>> Fun.uncurry mk_eq) <|>
    attempt (Deqs.parse |>> Fun.uncurry mk_deq) <|>
    (Ptos.parse |>> Fun.uncurry mk_pto) <?> "atom"
  ) st

let parse st =
  (sep_by1 parse_atom (parse_symb symb_star) >>= (fun atoms ->
          return (Blist.foldl star empty atoms)) <?> "symheap") st
