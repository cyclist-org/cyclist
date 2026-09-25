open Lib
open Symbols
open MParser
open Seplog

type t = int Term.Map.t

let equal u u' = Term.Map.equal Int.equal u u'
let compare m m' = Term.Map.compare Int.compare m m'
let hash m = Term.Map.hash Int.hash m
let bindings m = Term.Map.bindings m
let empty = Term.Map.empty
let is_empty = Term.Map.is_empty
let all_members_of = Term.Map.submap Int.equal

let to_string_sep sep t v =
   Term.to_string t ^ sep ^ (string_of_int v)

let to_string_list v =
  Blist.map (fun (t,v) -> to_string_sep symb_eq.str t v) (bindings v)
let to_string v =
  Blist.to_string symb_star.sep (fun (t,v) -> (to_string_sep symb_eq.str t v)) (bindings v)
let pp fmt v =
  Blist.pp
    pp_star
    (fun fmt (a,b) ->
      Format.fprintf fmt "@[%a%s%s@]" Term.pp a symb_eq.str (string_of_int b))
    fmt
    (bindings v)

let fold f a uf = Term.Map.fold f a uf
let for_all f uf = Term.Map.for_all f uf

let find = Term.Map.find

let add = Term.Map.add

let union m m' =
  Term.Map.fold (fun x y m'' -> add x y m'') m' m
let of_list ls =
  Blist.fold_left (fun m (k,v) -> add k v m) empty ls

let equates m x y = (find x m) = y

let diff eqs eqs' =
  let eqs_list = bindings eqs in
  let eqs'_list = bindings eqs' in
  let diffs_list = Blist.foldl (fun xs (k,v) -> Blist.del_first (fun (k',v') -> Term.equal k k' && Int.equal v v') xs) eqs'_list eqs_list in
  of_list diffs_list

let subsumed m m' =
  Term.Map.for_all (fun x y -> equates m' x y) m

let subst theta m =
  Term.Map.fold (fun x y m' -> (add (Term.Subst.apply theta x) y m')) m empty

(* let to_melt v =
  ltx_star (Blist.map (fun (k,v) -> Latex.concat [(Term.to_melt k); symb_eq.melt; Latex.text (string_of_int v)]) (bindings v)) *)

let terms m = Term.FList.terms (Blist.map (fun (x,_) -> x) (bindings m))

let vars m = Term.filter_vars (terms m)

let parse st =
  (Term.parse |>> (fun x -> (x, -2)) <?> "rho") st

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
