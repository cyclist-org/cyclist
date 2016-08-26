open Lib
open Util
open Symbols
open MParser
module F = Fopl

type t = Asl_term.t Asl_term.Map.t

let equal u u' = Asl_term.Map.equal Asl_term.equal u u'
let compare m m' = Asl_term.Map.compare Asl_term.compare m m'
let hash m = Asl_term.Map.hash Asl_term.hash m
let bindings m = Asl_term.Map.bindings m
let empty = Asl_term.Map.empty
let is_empty = Asl_term.Map.is_empty
let all_members_of = Asl_term.Map.all_members_of Asl_term.equal

let to_string_list v = 
  Blist.map (Asl_tpair.to_string_sep symb_eq.str) (bindings v)
let to_string v =
  Blist.to_string symb_star.sep (Asl_tpair.to_string_sep symb_eq.str) (bindings v)
let pp fmt v = 
  Blist.pp 
    pp_star   
    (fun fmt (a,b) -> 
      Format.fprintf fmt "@[%a%s%a@]" Asl_term.pp a symb_eq.str Asl_term.pp b) 
    fmt
    (bindings v)
   
let fold f a uf = Asl_term.Map.fold f a uf
let for_all f uf = Asl_term.Map.for_all f uf

let rec find x m =
  if Asl_term.Map.mem x m then
    find (Asl_term.Map.find x m) m
  else
    x

(* let rec deep_find x m =
  let aux m x =
    if Asl_term.Map.mem x m then Asl_term.simplify (find x m) else
    match Asl_term.dest_add x with
    | Some(t1, t2) -> Asl_term.simplify (Asl_term.mk_add (deep_find t1 m, deep_find t2 m))
    | None -> match Asl_term.dest_mult x with
              | Some(i, t) -> Asl_term.simplify (Asl_term.mk_mult(i, deep_find t m))
              | None -> Asl_term.simplify (find x m)
  in Lib.fixpoint Asl_term.equal (aux m) x*)

let add (x, y) m =
  let (x, y) = Asl_tpair.order (find x m, find y m) in
  (* do not add trivial identities *)
  if Asl_term.equal x y then m else
    (* first split into the pairs that do not/do map to x *)
    let (to_x, rest) =
      Asl_term.Map.partition (fun _ z -> Asl_term.equal z x) m in
    (* now change all the values of keys that map to x to y *)
    let to_y = Asl_term.Map.map (fun _ -> y) to_x in
    (* now union and add *)
    Asl_term.Map.add x y (Asl_term.Map.union rest to_y)

let subst theta m = 
  Asl_term.Map.fold (fun x y m' -> add (Asl_tpair.subst theta (x, y)) m') m empty

let union m m' =
  Asl_term.Map.fold (fun x y m'' -> add (x, y) m'') m' m

let of_list ls =
  Blist.fold_left (fun m pair -> add pair m) empty ls

let diff eqs eqs' =
  let eqs_list = bindings eqs in
  let eqs'_list = bindings eqs' in
  let diffs_list = Blist.foldl (fun xs eq -> Blist.del_first (Asl_tpair.equal eq) xs) eqs'_list eqs_list in
  of_list diffs_list


let to_melt v =
  ltx_star (Blist.map (Asl_tpair.to_melt_sep symb_eq.melt) (bindings v))

let terms m = Blist.foldl (fun a p -> Pair.fold Asl_term.Set.add p a) Asl_term.Set.empty (bindings m)

let vars m = Asl_term.filter_vars (terms m)

let parse st =
  (Asl_term.parse >>= (fun x ->
          parse_symb symb_eq >>
          Asl_term.parse << spaces |>> (fun y -> (x, y))) <?> "eq") st

let to_fopl m =
  let eq_to_fopl a b acc = F.And (F.PF(F.Eq (a, b)), acc)
  in fold eq_to_fopl m F.trivially_true

(* Cannot prove all equalities but helps to cut down calls to z3 *)
let rec simple_equates m x y =
  let (x, y) = (find x m, find y m) in
  Asl_term.equal x y

(* ∏ |= (x=y) <=> ∏ /\ (x!=y) is unsat *)
let entail_to_fopl m x y =
  let free_in_m = Asl_term.Set.filter (Asl_term.is_free_var) (vars m)
  in let free = Asl_term.Set.union (Asl_term.filter_vars (Asl_term.Set.singleton y))
    (Asl_term.Set.union (Asl_term.filter_vars (Asl_term.Set.singleton x)) free_in_m)
  in F.Exists (free, F.And (to_fopl  m, F.PF(F.Ne(x, y))))

let equates m x y = 
  if simple_equates m x y then true else
  let f = entail_to_fopl m x y
  in Asl_sat.is_unsat (F.to_z3 f)


(* FIXME: rewrite m' |- m with single z3 call? *)
let subsumed m m' =
  Asl_term.Map.for_all (fun x y -> equates m' x y) m
     
(* FIXME: rewrite without equates? *) 
let saturate m = 
  let ts = Asl_term.Set.to_list (terms m) in
  let pairs = Blist.cartesian_product ts ts in
  Blist.filter (Fun.uncurry (equates m)) pairs

let unify_partial ?(inverse=false) 
    ?(sub_check=Asl_subst.trivial_check)
    ?(cont=Asl_unifier.trivial_continuation)
    ?(init_state=Asl_unifier.empty_state) m m' =
  Asl_tpair.FList.unify_partial ~inverse ~sub_check ~cont ~init_state (bindings m) (bindings m')

let remove x m =
  let xs = Asl_term.Set.filter (Asl_term.equal x) (vars m) in
  let rest =
    Asl_term.Map.filter
      (fun y z -> not (Asl_term.Set.mem y xs || Asl_term.Set.mem z xs))
      m in
  let xs' = Asl_term.Set.to_list (Asl_term.Set.remove x xs) in
  Blist.fold_left (fun m p -> add p m) rest (Blist.pairs xs')
