open Lib
open Util
open Symbols
open MParser

type t = int Sl_term.Map.t

let equal u u' = Sl_term.Map.equal Int.equal u u'
let compare m m' = Sl_term.Map.compare Int.compare m m'
let hash m = Sl_term.Map.hash Int.hash m
let bindings m = Sl_term.Map.bindings m
let empty = Sl_term.Map.empty
let is_empty = Sl_term.Map.is_empty
let all_members_of = Sl_term.Map.submap Int.equal

let to_string_sep sep t v =
   Sl_term.to_string t ^ sep ^ (string_of_int v)

let to_string_list v = 
  Blist.map (fun (t,v) -> to_string_sep symb_eq.str t v) (bindings v)
let to_string v =
  Blist.to_string symb_star.sep (fun (t,v) -> (to_string_sep symb_eq.str t v)) (bindings v)
let pp fmt v = 
  Blist.pp 
    pp_star   
    (fun fmt (a,b) -> 
      Format.fprintf fmt "@[%a%s%s@]" Sl_term.pp a symb_eq.str (string_of_int b)) 
    fmt
    (bindings v)
   
let fold f a uf = Sl_term.Map.fold f a uf
let for_all f uf = Sl_term.Map.for_all f uf

let find = Sl_term.Map.find

let add = Sl_term.Map.add

let union m m' =
  Sl_term.Map.fold (fun x y m'' -> add x y m'') m' m
let of_list ls =
  Blist.fold_left (fun m (k,v) -> add k v m) empty ls

let equates m x y = (find x m) = y

let diff eqs eqs' =
  let eqs_list = bindings eqs in
  let eqs'_list = bindings eqs' in
  let diffs_list = Blist.foldl (fun xs (k,v) -> Blist.del_first (fun (k',v') -> Sl_term.equal k k' && Int.equal v v') xs) eqs'_list eqs_list in
  of_list diffs_list

let subsumed m m' =
  Sl_term.Map.for_all (fun x y -> equates m' x y) m

let subst theta m = 
  Sl_term.Map.fold (fun x y m' -> (add (Sl_term.subst theta x) y m')) m empty

let to_melt v =
  ltx_star (Blist.map (fun (k,v) -> Latex.concat [(Sl_term.to_melt k); symb_eq.melt; Latex.text (string_of_int v)]) (bindings v))

let terms m = Sl_term.FList.terms (Blist.map (fun (x,_) -> x) (bindings m))

let vars m = Sl_term.filter_vars (terms m)

let parse st =
  (Sl_term.parse |>> (fun x -> (x, -2)) <?> "rho") st

(* let eqclasses m =                                                 *)
(*   let classes =                                                   *)
(*     Sl_term.Map.fold                                              *)
(*       (fun k v c ->                                               *)
(*         Sl_term.Map.add v                                         *)
(*           (k :: (try Sl_term.Map.find v c with Not_found -> [v])) *)
(*           c                                                       *)
(*       )                                                           *)
(*       m                                                           *)
(*       Sl_term.Map.empty in                                        *)
(*   Sl_term.Map.fold (fun _ v ls -> v::ls) classes []               *)
