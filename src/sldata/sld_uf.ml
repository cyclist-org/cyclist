open Lib
open Util
open Symbols
open MParser

type t = Sld_term.t Sld_term.Map.t

let equal u u' = Sld_term.Map.equal Sld_term.equal u u'
let compare m m' = Sld_term.Map.compare Sld_term.compare m m'
let bindings m = Sld_term.Map.bindings m
let empty = Sld_term.Map.empty
let is_empty = Sld_term.Map.is_empty
let all_members_of = Sld_term.Map.all_members_of Sld_term.equal

let to_string_list v = 
  Blist.map (Sld_tpair.to_string_sep symb_eq.str) (bindings v)
let to_string v =
  Blist.to_string symb_star.sep (Sld_tpair.to_string_sep symb_eq.str) (bindings v)
let pp fmt v = 
  Blist.pp 
    pp_star   
    (fun fmt (a,b) -> 
      Format.fprintf fmt "@[%a%s%a@]" Sld_term.pp a symb_eq.str Sld_term.pp b) 
    fmt
    (bindings v)
   
let fold f a uf = Sld_term.Map.fold f a uf

let rec find x m =
  if Sld_term.Map.mem x m then
    find (Sld_term.Map.find x m) m
  else
    x

let add (x, y) m =
  let (x, y) = Sld_tpair.order (find x m, find y m) in
  (* do not add trivial identities *)
  if Sld_term.equal x y then m else
    (* first split into the pairs that do not/do map to x *)
    let (to_x, rest) =
      Sld_term.Map.partition (fun _ z -> Sld_term.equal z x) m in
    (* now change all the values of keys that map to x to y *)
    let to_y = Sld_term.Map.map (fun _ -> y) to_x in
    (* now union and add *)
    Sld_term.Map.add x y (Sld_term.Map.union rest to_y)

let union m m' =
  Sld_term.Map.fold (fun x y m'' -> add (x, y) m'') m' m
let of_list ls =
  Blist.fold_left (fun m pair -> add pair m) empty ls

let equates m x y = Sld_term.equal (find x m) (find y m)

let diff eqs eqs' =
  let eqs_list = bindings eqs in
  let eqs'_list = bindings eqs' in
  let diffs_list = Blist.foldl (fun xs eq -> Blist.del_first (Sld_tpair.equal eq) xs) eqs'_list eqs_list in
  of_list diffs_list

let subsumed m m' =
  Sld_term.Map.for_all (fun x y -> equates m' x y) m

let subst theta m = 
  Sld_term.Map.fold (fun x y m' -> add (Sld_tpair.subst theta (x, y)) m') m empty

let to_melt v =
  ltx_star (Blist.map (Sld_tpair.to_melt_sep symb_eq.melt) (bindings v))

let terms m = Sld_tpair.FList.terms (bindings m)

let vars m = Sld_term.filter_vars (terms m)

let parse st =
  (Sld_term.parse >>= (fun x ->
          parse_symb symb_eq >>
          Sld_term.parse << spaces |>> (fun y -> (x, y))) <?> "eq") st

(* let eqclasses m =                                                 *)
(*   let classes =                                                   *)
(*     Sld_term.Map.fold                                              *)
(*       (fun k v c ->                                               *)
(*         Sld_term.Map.add v                                         *)
(*           (k :: (try Sld_term.Map.find v c with Not_found -> [v])) *)
(*           c                                                       *)
(*       )                                                           *)
(*       m                                                           *)
(*       Sld_term.Map.empty in                                        *)
(*   Sld_term.Map.fold (fun _ v ls -> v::ls) classes []               *)
      
let saturate m = 
  let ts = Sld_term.Set.to_list (terms m) in
  let pairs = Blist.cartesian_product ts ts in
  Blist.filter (Fun.uncurry (equates m)) pairs 

let unify_partial ?(inverse=false) 
    ?(sub_check=Sld_term.trivial_sub_check)
    ?(cont=Sld_term.trivial_continuation)
    ?(init_state=Sld_term.empty_state) m m' =
  Sld_tpair.FList.unify_partial ~inverse ~sub_check ~cont ~init_state (bindings m) (saturate m')

let subst_subsumed eqs ((theta,_) as state) = 
  Option.mk (Sld_term.Map.for_all (equates eqs) theta) state

(* FIXME *)
let remove x m =
  let xs = Sld_term.Set.filter (equates m x) (vars m) in
  let rest =
    Sld_term.Map.filter
      (fun y z -> not (Sld_term.Set.mem y xs || Sld_term.Set.mem z xs))
      m in
  let xs' = Sld_term.Set.to_list (Sld_term.Set.remove x xs) in
  Blist.fold_left (fun m p -> add p m) rest (Blist.pairs xs')

