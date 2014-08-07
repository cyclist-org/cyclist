open Lib
open Util
open Symbols
open MParser

module SH = Sl_heap

module IndRuleList = MakeFList(Sl_indrule)
module DefPair = PairTypes(IndRuleList)(Strng)
include MakeFList(DefPair)
include Fixpoint(struct type t = DefPair.t list let equal = equal end)

(* type t = ((Sl_indrule.t list) * Sl_pred.ident_t) list *)
let empty = []
let add_case a l = failwith "not implemented"
let string_of_case c =
  let (f, (ident, params)) = Sl_indrule.dest c in
  (Sl_heap.to_string f) ^ symb_ind_implies.sep ^ ident ^
  (bracket (Blist.to_string symb_comma.str Sl_term.to_string params))

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
          Tokens.braces (sep_by1 Sl_indrule.parse (parse_symb symb_ind_sep)) <<
          spaces >>= (fun cases -> return (cases, name))) <?> "defs section") st

let parse st =
  (sep_by1 parse_section (parse_symb symb_semicolon) <?> "defs") st

let of_channel c =
  handle_reply (MParser.parse_channel parse c ())

let mem ident (defs: t) =
  Blist.exists (fun (_, ident') -> Strng.equal ident ident') defs

let is_defined defs (_, (ident, _)) =
  mem ident defs

let is_undefined defs pred = not (is_defined defs pred)

let get_def ident (defs: t) =
  fst (Blist.find (fun (_, ident') -> Strng.equal ident ident') defs)

let unfold vars h pred defs = 
  let (_, (ident, _)) = pred in
  let def = get_def ident defs in
  Blist.map (Sl_indrule.unfold vars h pred) def
  
  
module BasePair =
struct
  include PairTypes(Sl_term.Set)(Sl_heap)
  
  let to_string (v, g) =
    "(" ^
    "{" ^
    (Blist.to_string "," Sl_term.to_string (Sl_term.Set.to_list v)) ^
    "}, " ^
    (Sl_heap.to_string g) ^
    ")"
  
  let project (v, g) case =
    let (_, (_, formals)) = Sl_indrule.dest case in
    (Sl_term.Set.inter v (Sl_term.Set.of_list formals), Sl_heap.project g formals)
  
  let subst theta (v, g) =
    let v' = Sl_term.Set.endomap (fun z -> Sl_term.subst theta z) v in
    let g' = Sl_heap.subst theta g in
    (v', g')
  
  let unfold (v, h) ((_, (_, params)) as ind) (case, (v', g')) =
    (* simultaneously freshen case and (v',g') *)
    let avoidvars = Sl_term.Set.union v (Sl_heap.vars h) in
    let theta = Sl_term.avoid_theta avoidvars (Sl_indrule.vars case) in
    let case = Sl_indrule.subst theta case in
    let (v', g') = subst theta (v', g') in
    (* now carry on with substitution as normal *)
    let (_, (_, formals)) = Sl_indrule.dest case in
    let theta = Sl_term.Map.of_list (Blist.combine formals params) in
    (* let formals = Sl_term.Set.of_list (Blist.map fst (Sl_term.Map.to_list     *)
    (* theta)) in let substs = Sl_term.Set.of_list (Blist.map snd             *)
    (* (Sl_term.Map.to_list theta)) in let () = require (fun () ->            *)
    (* Sl_term.Set.subset (Sl_heap.vars g') formals) in let () = assert       *)
    (* (Sl_term.Set.subset v' formals) in let () = assert (Sl_term.Set.is_empty  *)
    (* (Sl_term.Set.inter (Sl_heap.vars g') substs)) in let () = assert (     *)
    (* Sl_term.Set.is_empty (Sl_term.Set.inter v' substs)) in                    *)
    let (v', g') = subst theta (v', g') in
    let h' = SH.with_inds h (Sl_tpreds.remove ind h.SH.inds) in
    let h' = Sl_heap.star h' g' in
    let cv = Blist.cartesian_product (Sl_term.Set.to_list v) (Sl_term.Set.to_list v') in
    let h' = SH.with_deqs h' (Sl_deqs.union h'.SH.deqs (Sl_deqs.of_list cv)) in
    let v = Sl_term.Set.union v v' in
    (v, h')
  
  (* assumes case is built with Sl_heap.star so ys are already unequal *)
  let unfold_all case cbps =
    let (h, _) = Sl_indrule.dest case in
    (* let () = assert (Sl_tpreds.cardinal h.inds = Blist.length cbps) in *)
    let ys = Sl_term.Set.of_list (Blist.rev_map fst (Sl_ptos.to_list h.SH.ptos)) in
    let h = SH.with_ptos h Sl_ptos.empty in
    Blist.fold_left2 unfold (ys, h) (Sl_tpreds.to_list h.SH.inds) cbps
  
  let gen case cbps =
    let (_, (_, args)) = Sl_indrule.dest case in
    let (v, h) = unfold_all case cbps in
    if Sl_heap.inconsistent h then None else
      let l = Blist.rev_append (Sl_term.Set.to_list (Sl_heap.vars h)) args in
      let l = Blist.rev_filter
          (fun u -> Sl_term.Set.exists (fun z -> Sl_heap.equates h u z) v) l in
      let v = Sl_term.Set.of_list l in
      Some (project (v, h) case)
  
end

module BasePairSet = MakeTreeSet(BasePair)

module CaseMap =
struct
  include MakeMap(Sl_indrule)
  let to_string cmap =
    let aux (c, s) =
      (Sl_indrule.to_string c) ^ "\nBase pairs: " ^
      (BasePairSet.to_string s) ^ "\n" in
    Blist.to_string "\n" aux (to_list cmap)
end

let get_bps cmap (_, (ident, _)) =
  let l =
    Blist.filter
      (fun (c, _) -> Strng.equal ident (fst (snd (Sl_indrule.dest c))))
      (CaseMap.to_list cmap) in
  Blist.bind
    (fun (c, s) -> Blist.map (fun bp -> (c, bp)) (BasePairSet.to_list s))
    l

let gen_pairs case cmap =
  let (h, _) = Sl_indrule.dest case in
  let candidates =
    Blist.map (fun i -> get_bps cmap i) (Sl_tpreds.to_list h.SH.inds) in
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
              Strng.equal (fst (snd (Sl_indrule.dest k))) first_pred &&
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

(* NB correctness relies on rules being explicit about x->_ implying       *)
(* x!=nil !!!                                                              *)
let satisfiable defs only_first output =
  Stats.CC.call () ;
  let res = gen_all_pairs defs only_first in
  if output then
    begin
      let element_conv (c, s) =
        ((Sl_indrule.to_string c) ^ " has base " ^ (BasePairSet.to_string s)) in
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
  let caselist = Blist.map (fun h -> Sl_indrule.mk h head) f in
  (caselist, pname):: defs
