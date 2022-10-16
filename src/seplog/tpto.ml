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

let parse ?(allow_tags = true) st =
  (Pto.parse
   >>= (fun pt ->
       (if allow_tags then option Tags.Elt.parse else return None)
       >>= fun opt_tag ->
       Permission.parse
       >>= fun perm ->
       (
         let tag = Option.dest Tags.anonymous Fun.id opt_tag in
         return ((tag, perm), pt)
       )
     )
       <?> "tpto")
    st  

let of_string = mk_of_string parse

let pp fmt ((tag, perm), pt) =
  Format.fprintf fmt "@[%a%s%s@]" Pto.pp pt
    (if Tags.is_anonymous tag then "" else sqbracket (Tags.Elt.to_string tag))
    (Permission.to_string perm)
