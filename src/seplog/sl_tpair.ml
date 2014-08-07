open Lib
open Util
open Symbols
open MParser

module TPair = PairTypes(Sl_term)(Sl_term)

include TPair

let unify theta (x, y) (x', y') =
  Option.bind (fun theta' -> Sl_term.unify theta' y y') (Sl_term.unify theta x x')
  
let unord_unify cont theta p p' =
  Blist.find_some cont 
    (Option.list_get (Blist.map (unify theta p) [ p'; Pair.swap p' ]))

let norm ((x,y) as pair) =
  if Sl_term.compare x y <= 0 then pair else (y,x)
  
let to_string_sep sep p =
  let (x,y) = Pair.map Sl_term.to_string p in x ^ sep ^ y
let to_melt_sep sep p =
  let (x,y) = Pair.map Sl_term.to_melt p in Latex.concat [x; sep; y]

module FList =
  struct
    include Util.MakeFList(TPair)
    
    let rec part_unify cont theta xs ys =
      match (xs, ys) with
      | ([], _) -> cont theta
      | (_, []) -> None
      | (p::ps, qs) ->
        Blist.find_some 
          (unord_unify (fun theta' -> part_unify cont theta' ps qs) theta p) 
          qs
    
  end 

