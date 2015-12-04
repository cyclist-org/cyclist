open Lib
open Util
open Symbols
open MParser

module TPair = PairTypes(Sl_term)(Sl_term)

include TPair

let _unify update_check (x, y) (x', y') cont init_state =
  Sl_unify.Unidirectional.unify_trm ~update_check x x' 
  (Sl_unify.Unidirectional.unify_trm ~update_check y y' cont) 
  init_state
  
let _biunify update_check (x, y) (x', y') cont init_state =
  Sl_unify.Bidirectional.unify_trm ~update_check x x' 
  (Sl_unify.Bidirectional.unify_trm ~update_check y y' cont) 
  init_state
   
let mk_unify unify order p p' cont init_state =
  if order then 
    unify p p' cont init_state
  else
    Blist.find_some 
      (fun p' -> unify p p' cont init_state)
      [ p'; Pair.swap p' ]

let unify ?(order=false) ?(update_check=Fun._true)
    p p' cont init_state =
  mk_unify (_unify update_check) order p p' cont init_state
  
let biunify ?(order=false) ?(update_check=Fun._true)
    p p' cont init_state =
  mk_unify (_biunify update_check) order p p' cont init_state

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
    
    let terms ps = 
      Blist.foldl 
        (fun a p -> Pair.fold Sl_term.Set.add p a) 
        Sl_term.Set.empty 
        ps 
  end 

module ListSet = MakeListSet(TPair)
