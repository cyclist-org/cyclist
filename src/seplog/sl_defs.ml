open Lib
open Util
open Symheap
open Symbols
open MParser

module CaseList = MakeFList(Case)
module DefPair = PairTypes(CaseList)(Strng)
include MakeFList(DefPair)
include Fixpoint(struct type t = DefPair.t list let equal = equal end)

(* type t = ((Case.t list) * ind_identifier) list *)
let empty = []
let add_case a l = failwith "not implemented"
let string_of_case c =
  let (f, (ident, params)) = Case.dest c in
  (Heap.to_string f) ^ symb_ind_implies.sep ^ ident ^
  (bracket (Blist.to_string symb_comma.str Term.to_string params))

let string_of_caseset (cls, ident) =
  ident ^ symb_lb.sep ^ "\n" ^
  (Blist.to_string ((symb_ind_sep.sep) ^ "\n") string_of_case cls)
  ^ "\n" ^ symb_rb.str

let to_string defs =
  Blist.to_string (symb_semicolon.sep ^ "\n\n") string_of_caseset defs

let to_melt d = ltx_text (to_string d)

let pp fmt d = Format.fprintf fmt "%s" (to_string d)

let parse_section st =
  (parse_ident >>= (fun name ->
          Tokens.braces (sep_by1 Case.parse (parse_symb symb_ind_sep)) <<
          spaces >>= (fun cases -> return (cases, name))) <?> "defs section") st

let parse st =
  (sep_by1 parse_section (parse_symb symb_semicolon) <?> "defs") st

let of_channel c =
  handle_reply (MParser.parse_channel parse c ())

let mem ident (defs: t) = Blist.exists (fun (_, ident') -> Strng.equal ident ident') defs

let is_defined (_, (ident, _)) defs = mem ident defs

let get_def ident (defs: t) = Blist.find (fun (_, ident') -> Strng.equal ident ident') defs

module BasePair =
struct
  include PairTypes(Term.Set)(Heap)
  
  let to_string (v, g) =
    "(" ^
    "{" ^
    (Blist.to_string "," Term.to_string (Term.Set.to_list v)) ^
    "}, " ^
    (Heap.to_string g) ^
    ")"
  
  let project (v, g) case =
    let (_, (_, formals)) = Case.dest case in
    (Term.Set.inter v (Term.Set.of_list formals), Heap.project g formals)
  
  let subst theta (v, g) =
    let v' = Term.Set.endomap (fun z -> Term.subst theta z) v in
    let g' = Heap.subst theta g in
    (v', g')
  
  let unfold (v, h) ((_, (_, params)) as ind) (case, (v', g')) =
    (* simultaneously freshen case and (v',g') *)
    let avoidvars = Term.Set.union v (Heap.vars h) in
    let theta = Term.avoid_theta avoidvars (Case.vars case) in
    let case = Case.subst theta case in
    let (v', g') = subst theta (v', g') in
    (* now carry on with substitution as normal *)
    let (_, (_, formals)) = Case.dest case in
    let theta = Term.Map.of_list (Blist.combine formals params) in
    (* let formals = Term.Set.of_list (Blist.map fst                 *)
    (* (Term.Map.to_list theta)) in let substs = Term.Set.of_list    *)
    (* (Blist.map snd (Term.Map.to_list theta)) in let () = require  *)
    (* (fun () -> Term.Set.subset (Heap.vars g') formals) in let ()  *)
    (* = require (fun () -> Term.Set.subset v' formals) in let () =  *)
    (* require (fun () -> Term.Set.is_empty (Term.Set.inter          *)
    (* (Heap.vars g') substs)) in let () = require (fun () ->        *)
    (* Term.Set.is_empty (Term.Set.inter v' substs)) in              *)
    let (v', g') = subst theta (v', g') in
    let h' = { h with inds = Inds.remove ind h.inds } in
    let h' = Heap.star h' g' in
    let cv = Blist.cartesian_product (Term.Set.to_list v) (Term.Set.to_list v') in
    let h' = { h' with deqs = Deqs.union h'.deqs (Deqs.of_list cv) } in
    let v = Term.Set.union v v' in
    (v, h')
  
  (* assumes case is built with Heap.star so ys are already unequal *)
  let unfold_all case cbps =
    let (h, _) = Case.dest case in
    (* let () = require (fun () -> Inds.cardinal h.inds =            *)
    (* Blist.length cbps) in                                         *)
    let ys = Term.Set.of_list (Blist.rev_map fst (Ptos.to_list h.ptos)) in
    let h = { h with ptos = Ptos.empty } in
    Blist.fold_left2 unfold (ys, h) (Inds.to_list h.inds) cbps
  
  let gen case cbps =
    let (_, (_, args)) = Case.dest case in
    let (v, h) = unfold_all case cbps in
    if Heap.inconsistent h then None else
      let l = Blist.rev_append (Term.Set.to_list (Heap.vars h)) args in
      let l = Blist.rev_filter
          (fun u -> Term.Set.exists (fun z -> Heap.equates h u z) v) l in
      let v = Term.Set.of_list l in
      Some (project (v, h) case)
  
end

module BasePairSet = MakeTreeSet(BasePair)

module CaseMap =
struct
  include MakeMap(Case)
  let to_string cmap =
    let aux (c, s) =
      (Case.to_string c) ^ "\nBase pairs: " ^
      (BasePairSet.to_string s) ^ "\n" in
    Blist.to_string "\n" aux (to_list cmap)
end

let get_bps cmap (_, (ident, _)) =
  let l =
    Blist.filter
      (fun (c, _) -> Strng.equal ident (fst (snd (Case.dest c))))
      (CaseMap.to_list cmap) in
  Blist.bind
    (fun (c, s) -> Blist.map (fun bp -> (c, bp)) (BasePairSet.to_list s))
    l

let gen_pairs case cmap =
  let (h, _) = Case.dest case in
  let candidates =
    Blist.map (fun i -> get_bps cmap i) (Inds.to_list h.inds) in
  let l = Blist.choose candidates in
  let poss_bps =
    Blist.rev_map (fun cbps -> BasePair.gen case cbps) l in
  let bps = Option.list_get poss_bps in
  BasePairSet.of_list bps

let first_pred_not_empty defs =
  let first_pred = snd (Blist.hd defs) in
  fun cm ->
      CaseMap.exists
        (fun k v ->
              Strng.equal (fst (snd (Case.dest k))) first_pred &&
              not (BasePairSet.is_empty v)
        )
        cm

let gen_all_pairs defs only_first =
  let first_not_empty = first_pred_not_empty defs in
  let cmap =
    Blist.fold_left
      (fun m (cl, _) ->
            Blist.fold_left
              (fun m' c -> CaseMap.add c BasePairSet.empty m') m cl)
      CaseMap.empty
      defs
  in
  let onestep cmap =
    let r = CaseMap.endomap
        (fun (c, s) -> (c, BasePairSet.union s (gen_pairs c cmap)))
        cmap in
    let () = debug (fun () -> "\n" ^ (CaseMap.to_string r) ^ "\n") in
    r in
  let rec fixp cm =
    let cm' = onestep cm in
    if
    only_first && first_not_empty cm' ||
    CaseMap.equal BasePairSet.equal cm cm'
    then
      cm'
    else
      fixp cm' in
  fixp cmap

(* NB correctness relies on rules being explicit about x->_        *)
(* implying x!=nil !!!                                             *)
let satisfiable defs only_first output =
  Stats.CC.call () ;
  let res = gen_all_pairs defs only_first in
  if output then
    begin
      let element_conv (c, s) =
        ((Case.to_string c) ^ " has base " ^ (BasePairSet.to_string s)) in
      print_endline
        (Blist.to_string "\n" element_conv (CaseMap.to_list res))
    end ;
  let retval =
    only_first && first_pred_not_empty defs res ||
    not only_first &&
    CaseMap.for_all (fun _ s -> not (BasePairSet.is_empty s)) res in
  if retval then Stats.CC.accept () else Stats.CC.reject () ;
  retval

let of_formula pname params f defs =
  let head = (pname, params) in
  let caselist = Blist.map (fun h -> Case.mk h head) f in
  (caselist, pname):: defs
