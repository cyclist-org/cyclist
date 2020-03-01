open Lib
open Symbols
open MParser

type t = Term.t Term.Map.t

let equal u u' = Term.Map.equal Term.equal u u'

let compare m m' = Term.Map.compare Term.compare m m'

let hash m = Term.Map.hash Term.hash m

let bindings m = Term.Map.bindings m

let empty = Term.Map.empty

let is_empty = Term.Map.is_empty

let to_string_list v =
  Blist.map (Tpair.to_string_sep symb_eq.str) (bindings v)

let to_string v =
  Blist.to_string symb_star.sep
    (Tpair.to_string_sep symb_eq.str)
    (bindings v)

let pp fmt v =
  Blist.pp pp_star
    (fun fmt (a, b) ->
      Format.fprintf fmt "@[%a%s%a@]" Term.pp a symb_eq.str Term.pp b )
    fmt (bindings v)

let fold f a uf = Term.Map.fold f a uf

let for_all f uf = Term.Map.for_all f uf

let rec find x m =
  if Term.Map.mem x m then find (Term.Map.find x m) m else x

let add (x, y) m =
  let x, y = Tpair.order (find x m, find y m) in
  (* do not add trivial identities *)
  if Term.equal x y then m
  else
    (* first split into the pairs that do not/do map to x *)
    let to_x, rest = Term.Map.partition (fun _ z -> Term.equal z x) m in
    (* now change all the values of keys that map to x to y *)
    let to_y = Term.Map.map (fun _ -> y) to_x in
    (* now union and add *)
    Term.Map.add x y (Term.Map.union rest to_y)

let union m m' = Term.Map.fold (fun x y m'' -> add (x, y) m'') m' m

let of_list ls = Blist.fold_left (fun m pair -> add pair m) empty ls

let equates m x y = Term.equal (find x m) (find y m)

let diff eqs eqs' =
  let eqs_list = bindings eqs in
  let eqs'_list = bindings eqs' in
  let diffs_list =
    Blist.foldl
      (fun xs eq -> Blist.del_first (Tpair.equal eq) xs)
      eqs'_list eqs_list
  in
  of_list diffs_list

let subsumed m m' = Term.Map.for_all (fun x y -> equates m' x y) m

let subst theta m =
  Term.Map.fold (fun x y m' -> add (Tpair.subst theta (x, y)) m') m empty

let terms m = Tpair.FList.terms (bindings m)

let vars m = Term.filter_vars (terms m)

let parse st =
  ( Term.parse
  >>= (fun x ->
        parse_symb symb_eq >> Term.parse << spaces |>> fun y -> (x, y) )
  <?> "eq" )
    st

(* let eqclasses m =                                                 *)
(*   let classes =                                                   *)
(*     Term.Map.fold                                              *)
(*       (fun k v c ->                                               *)
(*         Term.Map.add v                                         *)
(*           (k :: (try Term.Map.find v c with Not_found -> [v])) *)
(*           c                                                       *)
(*       )                                                           *)
(*       m                                                           *)
(*       Term.Map.empty in                                        *)
(*   Term.Map.fold (fun _ v ls -> v::ls) classes []               *)

let saturate m =
  let ts = Term.Set.to_list (terms m) in
  let pairs = Blist.cartesian_product ts ts in
  Blist.filter (Fun.uncurry (equates m)) pairs

let unify_partial ?(inverse = false) ?(update_check = Fun._true) m m' cont
    init_state =
  let eqs = Tpair.ListSet.of_list (bindings m) in
  let eqs' = Tpair.ListSet.of_list (saturate m') in
  Tpair.ListSet.mk_unifier false false
    (Fun.direct inverse (Tpair.unify ~update_check))
    eqs eqs' cont init_state

let biunify_partial ?(update_check = Fun._true) m m' cont init_state =
  let eqs = Tpair.ListSet.of_list (bindings m) in
  let eqs' = Tpair.ListSet.of_list (saturate m') in
  Tpair.ListSet.mk_unifier false false
    (Tpair.biunify ~update_check)
    eqs eqs' cont init_state

let subst_subsumed eqs ((theta, _) as state) =
  Option.mk (Term.Map.for_all (equates eqs) theta) state

(* FIXME *)
let remove x m =
  let xs = Term.Set.filter (equates m x) (vars m) in
  let rest =
    Term.Map.filter
      (fun y z -> not (Term.Set.mem y xs || Term.Set.mem z xs))
      m
  in
  let xs' = Term.Set.to_list (Term.Set.remove x xs) in
  Blist.fold_left (fun m p -> add p m) rest (Blist.pairs xs')
