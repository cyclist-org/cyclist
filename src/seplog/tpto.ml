(** Tagged pto, as a pair of (a) a tag and permission and (b) a pto. *)

open Lib
open Symbols
open MParser

include Generic

module LabelnPerm = Pair.Make (Tags.Elt) (Permission)
include LabelnPerm


module TPto = Pair.Make (LabelnPerm) (Pto)
include TPto

(*Comment 26 sept: I first try these functions to compile. I'll then check if their functionality is fully implemented*)
let subst theta (lb, pto) =
  (lb, Pto.subst theta pto)

let terms (lb, pto) = 
  Pto.terms pto

let vars (lb, pto) = 
  Pto.vars pto

  let parse st =
   (Pto.parse
    >>= (fun y -> y)
    <?> "tpto")
    st
     