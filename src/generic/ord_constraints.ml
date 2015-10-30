open Lib
open Util
open Symbols
open MParser

module Constraint =
  struct
    type t =
      | LT of (Tags.elt * Tags.elt)
      | LTE of (Tags.elt * Tags.elt)

    let compare (c : t) (c' : t) = compare c c'
    let equal (c : t) (c' : t) = (c = c')
    let hash (c : t) = Hashtbl.hash c

    let to_string = function
      | LT(t, t') ->
          (tag_to_string t) ^ symb_lt.sep ^ (tag_to_string t')
      | LTE(t, t') ->
          (tag_to_string t) ^ symb_leq.sep ^ (tag_to_string t')

    let to_melt = function
      | LT(t, t') -> Latex.concat [
          tag_to_melt t ;
          symb_lt.melt ;
          tag_to_melt t' ]
      | LTE(t, t') -> Latex.concat [
          tag_to_melt t ;
          symb_leq.melt ;
          tag_to_melt t' ]

    let pp fmt c = Format.fprintf fmt "@[%s@]" (to_string c)

    let parse st =
      ( parse_tag >>= (fun t ->
        (parse_symb symb_lt) >>
        (option (parse_symb symb_eq)) >>= (fun op ->
        parse_tag >>= (fun t' ->
        return (if Option.is_none op then LT(t,t') else LTE(t,t')))))) st

    let tags = function
      | LT(t,t') -> Tags.of_list [t; t']
      | LTE(t,t') -> Tags.of_list [t; t']

    let subst_tags theta = function
      | LT(ts) -> LT(Pair.map (TagPairs.apply_to_tag theta) ts)
      | LTE(ts) -> LTE(Pair.map (TagPairs.apply_to_tag theta) ts)
    
    let satisfiable = function
      | LT(t, t') -> not (Tags.Elt.equal t t')
      | _ -> true

    let valid = function
      | LTE(t, t') -> Tags.Elt.equal t t'
      | _ -> false

  end
  
module Elt = Constraint

include MakeListSet(Constraint)

let to_string cs = if is_empty cs then "" else to_string cs
let pp fmt cs = if is_empty cs then Format.fprintf fmt "" else pp fmt cs

let to_melt cs = ltx_comma (map_to_list Constraint.to_melt cs)
let to_string_list cs = map_to_list Constraint.to_string cs

let parse st =
  ( sep_by Constraint.parse (parse_symb symb_comma) >>= (fun cs ->
    let cs = of_list cs in 
    if (is_empty cs) then return cs
    else Tokens.colon >> (return cs))) st

let of_string s = handle_reply (MParser.parse_string parse s ())

let tags cs =
  fold (fun c ts -> Tags.union ts (Constraint.tags c)) cs Tags.empty
  
let tag_pairs cs = TagPairs.mk (tags cs)

let subst_tags theta cs = endomap (Constraint.subst_tags theta) cs

let generate t ts =
  Tags.map_to add empty (fun t' -> Constraint.LT(t', t)) ts
  
let infer_constraint = function
  | (Constraint.LT(t1, t2), Constraint.LT(t1', t2')) -> 
      Option.mk (t2 = t1') (Constraint.LT(t1, t2'))
  | (Constraint.LT(t1, t2), Constraint.LTE(t1', t2')) -> 
      Option.mk (t2 = t1') (Constraint.LT(t1, t2'))
  | (Constraint.LTE(t1, t2), Constraint.LT(t1', t2')) -> 
      Option.mk (t2 = t1') (Constraint.LT(t1, t2'))
  | (Constraint.LTE(t1, t2), Constraint.LTE(t1', t2')) -> 
      Option.mk (t2 = t1') (Constraint.LTE(t1, t2'))

let close cs =
  let ts = tags cs in
  let cs = Tags.map_to add cs (fun t -> Constraint.LTE(t, t)) ts in
  let cs = opt_map_to add cs (function | Constraint.LT(t, t') -> Some (Constraint.LTE(t, t')) | _ -> None ) cs in
  let gen cs =
    let cs_list = to_list cs in
    let allpairs = Blist.cartesian_product cs_list cs_list in
    Blist.opt_map_to add cs infer_constraint allpairs in
  fixpoint gen cs
  
let remove_schema cs used =
  let tags = Tags.filter 
    (fun t -> Tags.is_exist_var t && not (Tags.mem t used))
    (tags cs) in
  let get_schema t =
    let (cs', cs) = partition (fun c -> Tags.mem t (Constraint.tags c)) cs in
    let ident =
      if Fun.swap for_all cs' (function 
          | Constraint.LT(t', t'') -> 
              Tags.Elt.equal t t'' && not (Tags.Elt.equal t t')  
          | Constraint.LTE(_, t') -> Tags.Elt.equal t t') 
        then Some "UBound"
      else if Fun.swap for_all cs' (function 
          | Constraint.LTE(t', _) -> Tags.Elt.equal t t'  
          | _ -> false) 
        then Some "LBound"
      else
        None in
    Option.map (Pair.mk cs) ident in
  Tags.find_map get_schema tags
    
let upper_bounds ?(strict=false) t cs =
  let f = function
    | Constraint.LT(t', t'') ->
        Option.mk (strict && Tags.Elt.equal t t') t''
    | Constraint.LTE(t', t'') -> 
        Option.mk (not strict && Tags.Elt.equal t t') t''
    in
  opt_map_to Tags.add Tags.empty f cs

let all_pairs cs =
  let extract = (function
    | Constraint.LT(tp) -> tp
    | Constraint.LTE(tp) -> tp
    ) in
  map_to TagPairs.add TagPairs.empty extract cs

let prog_pairs cs =
  let extract = (function
    | Constraint.LT(tp) -> Some(tp)
    | Constraint.LTE(tp) -> None
    ) in
  opt_map_to TagPairs.add TagPairs.empty extract cs

let inconsistent cs = 
  let lte ts = exists (function | Constraint.LTE(ts') -> ts = ts' | _ -> false) cs in
  not (for_all Constraint.satisfiable (close cs)) || 
  exists (function
    | Constraint.LT(ts) -> lte (Pair.swap ts)
    | _ -> false)
    cs

let subsumed cs cs' = 
  let cs' = filter 
    (function | Constraint.LTE(t, t') -> not (Tags.Elt.equal t t') | _ -> true)
    cs' in
  for_all (Fun.swap mem cs) cs'

let unify ?(inverse=false) ?(update_check=Fun._true)
    cs cs' cont init_state =
  let (domain, range) = 
    Pair.map tags (Fun.direct inverse Pair.mk cs cs') in
  let rec aux theta unmapped =
    let (cs, cs') = 
      if inverse then (cs, subst_tags theta cs')
      else (subst_tags theta cs, cs') in
    if Tags.is_empty unmapped then 
      if subsumed cs' cs then cont theta else None
    else
      let tag = Tags.choose unmapped in
      let unmapped = Tags.remove tag unmapped in
      let valid_mapping img = 
        let theta' = TagPairs.add (tag, img) theta in
        if update_check (theta, TagPairs.singleton (tag, img))
        then aux theta' unmapped
        else None in
      Tags.find_map valid_mapping range
    in
  let unmapped = TagPairs.map_to Tags.remove domain fst init_state in
  aux init_state unmapped
  
let mk_update_check f (state, tps) = 
  Option.pred_dest (Fun.curry f state) (TagPairs.dest_singleton tps)