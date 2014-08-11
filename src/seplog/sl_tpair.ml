open Lib
open Util
open Symbols
open MParser

module TPair = PairTypes(Sl_term)(Sl_term)

include TPair

let unify cont state (x, y) (x', y') =
  Sl_term.unify (fun state' -> Sl_term.unify cont state' y y') state x x'
   
let unord_unify cont state p p' =
  Blist.find_some (unify cont state p) [ p'; Pair.swap p' ]

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
    
    let rec unord_unify_within cont state xs ys =
      match (xs, ys) with
      | ([], _) -> cont state
      | (_, []) -> None
      | (p::ps, _) ->
        Blist.find_some 
          (unord_unify (fun state' -> unord_unify_within cont state' ps ys) state p) 
          ys
    
    let rec inverse_unord_unify_within cont state xs ys =
      match (xs, ys) with
      | ([], _) -> cont state
      | (_, []) -> None
      | (p::ps, _) ->
        Blist.find_some 
          (fun q -> 
            unord_unify (fun state' -> inverse_unord_unify_within cont state' ps ys) state q p) 
          ys

    let terms ps = 
      Blist.foldl 
        (fun a p -> Pair.fold Sl_term.Set.add p a) 
        Sl_term.Set.empty 
        ps 
  end 

