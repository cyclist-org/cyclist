open Lib
open Util
open Symbols
open MParser

type t = Sl_term.t Sl_term.Map.t

let equal u u' = Sl_term.Map.equal Sl_term.equal u u'
let compare m m' = Sl_term.Map.compare Sl_term.compare m m'
let bindings m = Sl_term.Map.bindings m
let empty = Sl_term.Map.empty
let is_empty = Sl_term.Map.is_empty

let to_string_list v = 
  Blist.map (Sl_tpair.to_string_sep symb_eq.str) (bindings v)
let to_string v =
  Blist.to_string symb_star.sep (Sl_tpair.to_string_sep symb_eq.str) (bindings v)

let rec find x m =
  if Sl_term.Map.mem x m then
    find (Sl_term.Map.find x m) m
  else
    x

let add (x, y) m =
  let (x, y) = Sl_tpair.order (find x m, find y m) in
  (* do not add trivial identities *)
  if Sl_term.equal x y then m else
    (* first split into the pairs that do not/do map to x *)
    let (to_x, rest) =
      Sl_term.Map.partition (fun _ z -> Sl_term.equal z x) m in
    (* now change all the values of keys that map to x to y *)
    let to_y = Sl_term.Map.map (fun _ -> y) to_x in
    (* now union and add *)
    Sl_term.Map.add x y (Sl_term.Map.union rest to_y)

let union m m' =
  Sl_term.Map.fold (fun x y m'' -> add (x, y) m'') m' m
let of_list ls =
  Blist.fold_left (fun m pair -> add pair m) empty ls

let equates m x y = Sl_term.equal (find x m) (find y m)

let subsumed m m' =
  Sl_term.Map.for_all (fun x y -> equates m' x y) m

let subst theta m = 
  Sl_term.Map.fold (fun x y m' -> add (Sl_tpair.subst theta (x, y)) m') m empty

let to_melt v =
  ltx_star (Blist.map (Sl_tpair.to_melt_sep symb_eq.melt) (bindings v))

let terms m = Sl_tpair.FList.terms (bindings m)

let vars m = Sl_term.filter_vars (terms m)

let parse st =
  (Sl_term.parse >>= (fun x ->
          parse_symb symb_eq >>
          Sl_term.parse << spaces |>> (fun y -> (x, y))) <?> "eq") st

let part_unify cont theta m m' =
  Sl_tpair.FList.part_unord_unify cont theta (bindings m) (bindings m')

let subst_subsumed eqs theta = 
  Option.mk (Sl_term.Map.for_all (equates eqs) theta) theta

let remove x m =
  failwith "FIXME"
  (* let xs = Sl_term.Set.filter (equates m x) (vars m) in               *)
  (* let rest =                                                          *)
  (*   Sl_term.Map.filter                                                *)
  (*     (fun y z -> not (Sl_term.Set.mem y xs || Sl_term.Set.mem z xs)) *)
  (*     m in                                                            *)
  (* let xs' = Sl_term.Set.to_list (Sl_term.Set.remove x xs) in          *)
  (* Blist.fold_left (fun m p -> add p m) rest (Blist.pairs xs')         *)

