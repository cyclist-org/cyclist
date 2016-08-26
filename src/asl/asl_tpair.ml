open Lib
open Util
open Symbols
open MParser

module TPair = PairTypes(Asl_term)(Asl_term)

include TPair

let to_string_sep sep p =
  let (x,y) = Pair.map Asl_term.to_string p in x ^ sep ^ y
let to_melt_sep sep p =
  let (x,y) = Pair.map Asl_term.to_melt p in Latex.concat [x; sep; y]

let _unify sub_check cont state (x, y) (x', y') =
  Asl_term.unify ~sub_check ~cont:(fun state' -> Asl_term.unify ~sub_check ~cont ~init_state:state' y' y) ~init_state:state x' x
   
let unify ?(order=false) 
    ?(sub_check=Asl_subst.trivial_check) 
    ?(cont=Asl_unifier.trivial_continuation)
    ?(init_state=Asl_unifier.empty_state) p p' =
  if order then 
    _unify sub_check cont init_state p p'
  else
    Blist.find_some (_unify sub_check cont init_state p) [ p'; Pair.swap p' ]

let order ((x,y) as pair) =
  if Asl_term.compare x y <= 0 then pair else (y,x)

let subst theta (x,y) = (Asl_term.subst theta x, Asl_term.subst theta y)

module FList =
  struct
    include Util.MakeFList(TPair)
    
    let rec unify_partial ?(order=false) ?(inverse=false) 
        ?(sub_check=Asl_subst.trivial_check)
        ?(cont=Asl_unifier.trivial_continuation)
        ?(init_state=Asl_unifier.empty_state) xs ys =
      match (xs, ys) with
      | ([], _) -> cont init_state
      | (_, []) -> None
      | (p::ps, _) ->
        Blist.find_some 
          (fun q ->
            let (x,y) = if inverse then (q,p) else (p,q) in
            unify ~order ~sub_check
              ~cont:(fun state' -> 
                unify_partial ~order ~inverse ~sub_check ~cont ~init_state:state' ps ys) ~init_state x y) 
          ys
    
    let terms ps = 
      Blist.foldl 
        (fun a p -> Pair.fold Asl_term.Set.add p a) 
        Asl_term.Set.empty 
        ps 
  end 

