open Lib
open Util
open Symbols
open MParser

module TPair = PairTypes(Sl_term)(Sl_term)

include TPair

let _unify update_check (x, y) (x', y') cont init_state =
  let cont state' =
    Sl_unify.unify_trm ~update_check y y' cont state' in
  Sl_unify.unify_trm ~update_check x x' cont init_state
   
let unify ?(order=false) ?(update_check=Fun._true) 
    p p' cont init_state =
  if order then 
    _unify update_check p p' cont init_state
  else
    Blist.find_some 
      (fun p' -> _unify update_check p p' cont init_state)
      [ p'; Pair.swap p' ]

let order ((x,y) as pair) =
  if Sl_term.compare x y <= 0 then pair else (y,x)

let subst theta (x,y) = (Sl_term.subst theta x, Sl_term.subst theta y)
      
let to_string_sep sep p =
  let (x,y) = Pair.map Sl_term.to_string p in x ^ sep ^ y
let to_melt_sep sep p =
  let (x,y) = Pair.map Sl_term.to_melt p in Latex.concat [x; sep; y]

module FList =
  struct
    include Util.MakeFList(TPair)
    
    let rec unify_partial ?(order=false) ?(inverse=false) 
        ?(update_check=Fun._true)
        xs ys cont init_state =
      match (xs, ys) with
      | ([], _) -> cont init_state
      | (_, []) -> None
      | (p::ps, _) ->
        Blist.find_some 
          (fun q ->
            let (x,y) = if inverse then (q,p) else (p,q) in
            let cont state' =
              unify_partial ~order ~inverse ~update_check ps ys cont state' in
            unify ~order ~update_check x y cont init_state) 
          ys
    
    let terms ps = 
      Blist.foldl 
        (fun a p -> Pair.fold Sl_term.Set.add p a) 
        Sl_term.Set.empty 
        ps 
  end 

