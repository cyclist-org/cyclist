open Lib
open   Symbols

open MParser

module Tag = Tags.Elt

module Constraint = struct
  type t = LT of (Tag.t * Tag.t) | LTE of (Tag.t * Tag.t)

  let compare c c' =
    match (c, c') with
    | LT (t, t'), LT (t'', t''') | LTE (t, t'), LTE (t'', t''') ->
        let fst = Tag.compare t t'' in
        if not (Int.equal fst 0) then fst else Tag.compare t' t'''
    | LT _, _ -> -1
    | LTE _, _ -> 1

  let equal c c' =
    match (c, c') with
    | LT (t, t'), LT (t'', t''') | LTE (t, t'), LTE (t'', t''') ->
        Tag.equal t t'' && Tag.equal t' t'''
    | _ -> false

  let hash (c : t) = Hashtbl.hash c

  let to_string = function
    | LT (t, t') -> Tag.to_string t ^ symb_lt.sep ^ Tag.to_string t'
    | LTE (t, t') -> Tag.to_string t ^ symb_leq.sep ^ Tag.to_string t'

  let pp fmt c = Format.fprintf fmt "@[%s@]" (to_string c)

  let parse st =
    ( Tag.parse
    >>= fun t ->
    parse_symb symb_lt
    >> option (parse_symb symb_eq)
    >>= fun op ->
    Tag.parse
    >>= fun t' -> return (if Option.is_none op then LT (t, t') else LTE (t, t'))
    )
      st

  let tags = function
    | LT (t, t') -> Tags.of_list [t; t']
    | LTE (t, t') -> Tags.of_list [t; t']

  let subst_tags theta = function
    | LT ts -> LT (Pair.map (Tagpairs.apply_to_tag theta) ts)
    | LTE ts -> LTE (Pair.map (Tagpairs.apply_to_tag theta) ts)

  let satisfiable = function
    | LT (t, t') -> not (Tags.Elt.equal t t')
    | _ -> true

  let valid = function LTE (t, t') -> Tags.Elt.equal t t' | _ -> false

  let mk_unify elt_unify c c' cont init_state =
    let do_unify (t, t') (t'', t''') =
      (elt_unify t t'' (elt_unify t' t''' cont)) init_state
    in
    match (c, c') with
    | LT tp, LT tp' -> do_unify tp tp'
    | LTE tp, LTE tp' -> do_unify tp tp'
    | _ -> None

  let unify ?(update_check = Fun._true) c c' cont init_state =
    mk_unify (Tags.Elt.unify ~update_check) c c' cont init_state

  let biunify ?(update_check = Fun._true) c c' cont init_state =
    mk_unify (Tags.Elt.biunify ~update_check) c c' cont init_state
end

module Elt = Constraint
include Listset.Make (Constraint)

let to_string cs = if is_empty cs then "" else to_string cs

let pp fmt cs = if is_empty cs then Format.fprintf fmt "" else pp fmt cs

let to_string_list cs = map_to_list Constraint.to_string cs

let parse st =
  ( attempt (sep_by Constraint.parse (parse_symb symb_comma))
  <|> return []
  >>= fun cs ->
  let cs = of_list cs in
  if is_empty cs then return cs else Tokens.colon >> return cs )
    st

let of_string s = handle_reply (MParser.parse_string parse s ())

let tags cs =
  fold (fun c ts -> Tags.union ts (Constraint.tags c)) cs Tags.empty

let tag_pairs cs = Tagpairs.mk (tags cs)

let subst_tags theta cs = map (Constraint.subst_tags theta) cs

let generate ?(avoid = Tags.empty) ?(augment = true) t ts =
  let ts =
    if augment && Tags.is_empty ts then Tags.singleton (Tags.fresh_evar avoid)
    else ts
  in
  Tags.map_to add empty (fun t' -> Constraint.LT (t', t)) ts

let infer_constraint = function
  | Constraint.LT (t1, t2), Constraint.LT (t1', t2') ->
      Option.mk (Tag.equal t2 t1') (Constraint.LT (t1, t2'))
  | Constraint.LT (t1, t2), Constraint.LTE (t1', t2') ->
      Option.mk (Tag.equal t2 t1') (Constraint.LT (t1, t2'))
  | Constraint.LTE (t1, t2), Constraint.LT (t1', t2') ->
      Option.mk (Tag.equal t2 t1') (Constraint.LT (t1, t2'))
  | Constraint.LTE (t1, t2), Constraint.LTE (t1', t2') ->
      Option.mk (Tag.equal t2 t1') (Constraint.LTE (t1, t2'))

let close cs =
  let ts = tags cs in
  let cs = Tags.map_to add cs (fun t -> Constraint.LTE (t, t)) ts in
  let cs =
    opt_map_to add cs
      (function
        | Constraint.LT (t, t') -> Some (Constraint.LTE (t, t')) | _ -> None)
      cs
  in
  let gen cs =
    let cs_list = to_list cs in
    let allpairs = Blist.cartesian_product cs_list cs_list in
    Blist.map_to (Option.dest Fun.id add) cs infer_constraint allpairs
  in
  fixpoint gen cs

let remove_schema cs used =
  let tags =
    Tags.filter
      (fun t -> Tags.is_exist_var t && not (Tags.mem t used))
      (tags cs)
  in
  let get_schema t =
    let cs', cs = partition (fun c -> Tags.mem t (Constraint.tags c)) cs in
    let ident =
      if
        Fun.swap for_all cs' (function
          | Constraint.LT (t', t'') ->
              Tags.Elt.equal t t'' && not (Tags.Elt.equal t t')
          | Constraint.LTE (_, t') -> Tags.Elt.equal t t' )
      then Some "UBound"
      else if
        Fun.swap for_all cs' (function
          | Constraint.LTE (t', _) -> Tags.Elt.equal t t'
          | _ -> false )
      then Some "LBound"
      else None
    in
    Option.map (Pair.mk cs) ident
  in
  Tags.find_map get_schema tags

let verify_schemas used_tags cs =
  let rec f cs =
    Option.dest_lazily
      (fun _ -> cs)
      (fun (cs, _) -> f cs)
      (remove_schema cs used_tags)
  in
  is_empty (f cs)

let upper_bounds ?(strict = false) t cs =
  let f = function
    | Constraint.LT (t', t'') -> Option.mk (strict && Tags.Elt.equal t t') t''
    | Constraint.LTE (t', t'') ->
        Option.mk ((not strict) && Tags.Elt.equal t t') t''
  in
  opt_map_to Tags.add Tags.empty f cs

let all_pairs cs =
  let extract = function Constraint.LT tp -> tp | Constraint.LTE tp -> tp in
  map_to Tagpairs.add Tagpairs.empty extract cs

let prog_pairs cs =
  let extract = function
    | Constraint.LT tp -> Some tp
    | Constraint.LTE _ -> None
  in
  opt_map_to Tagpairs.add Tagpairs.empty extract cs

let inconsistent cs =
  let lte (t1, t2) =
    exists
      (function
        | Constraint.LTE (t1', t2') -> Tag.equal t1 t1' && Tag.equal t2 t2'
        | _ -> false)
      cs
  in
  (not (for_all Constraint.satisfiable (close cs)))
  || exists (function Constraint.LT ts -> lte (Pair.swap ts) | _ -> false) cs

let subsumes cs cs' =
  let cs' =
    filter
      (function
        | Constraint.LTE (t, t') -> not (Tags.Elt.equal t t') | _ -> true)
      cs'
  in
  for_all (Fun.swap mem cs) cs'

let unify ?(total = false) ?(inverse = false) ?(update_check = Fun._true) cs
    cs' cont init_state =
  mk_unifier total false
    (Fun.direct inverse (Elt.unify ~update_check))
    cs cs' cont init_state

let biunify ?(total = false) ?(update_check = Fun._true) cs cs' cont init_state
    =
  mk_unifier total false (Elt.biunify ~update_check) cs cs' cont init_state

let mk_update_check f (state, tps) =
  Option.pred_dest (Fun.curry f state) (Tagpairs.dest_singleton tps)
