(** Type representing permissions (for Concurrent Separation Logic automation) *)

open Lib 

(* Using zarith package Q for arbitrary precision rational constants  *)

(* a) FRACTION Let's start by defining it as a fraction. I'll implement the rest of the polynomial (see .mli file) later *)
type permFraction = Q.t

(* b) VARMANAGER *)
(* Function needed for instantiation of VarManager to define the labels *)
let classify_labelname s =
   assert (not (String.equal s "")) ;
   if String.equal s "_" then VarManager.ANONYMOUS
   else VarManager.FREE
 
 (* Instantiation of varManager for the Labels *)
 module LabelMgr = (val VarManager.mk 0 "_" classify_labelname : VarManager.S)
 
 
module Label = LabelMgr.Var

(* type prodTerm = Multiset.Make (permFraction) *) 

(* module PermPair = Pair.Make (Q) (Label) // This doesn't work because Q is not a BasicType *)
 type t =  {
   permis: Q.t; 
   label: Label.t 
   } 
 
 

(* Constructors *)
let one = {
   permis= Q.one; 
   label= LabelMgr.anonymous
   }
 
let pi = {
      permis= Q.one; 
      label= LabelMgr.anonymous
      }
    
let two = {
   permis= Q.mul Q.one (Q.of_int 2); 
   label= LabelMgr.anonymous
   }
  
let half = {
   permis= Q.div Q.one (Q.of_int 2); 
   label= LabelMgr.anonymous
   }
 


(* BasicType required procedures *)  

let compare p1 p2 = 
   if (p1.label == p2.label) 
      then Q.compare p1.permis p2.permis
      else -1

let equal p1 p2 = (compare p1 p2 == 0)

let hash p = Float.hash (Q.to_float p.permis)
       
(* let to_string p = String.cat (Q.to_string p.permis) (Var.to_string p.label) *)
let to_string p = Q.to_string p.permis   (*TEMPORAL. MODIFY THIS*)

let pp fmt p = Format.fprintf fmt "%s" (to_string p) 
  

(* Some other arithmetic fraction manipulation procedures *)
 
(**  NOT IN USE Change denominator of fraction f, by newDen *) 
(*let changeDen newDen f = 
    if newDen mod f.den == 0
      then {num = f.num * (newDen/f.den); den = newDen}
      else f
*)
(* NOT IMPLEMENTED calculate minimum common multiple of a and b*)
(* let mcm a b = 
     *)

(** Computes the gcd of a and b*) (**WRONG! JUST FOR TESTING  *)
(* let rec gcd a b =  
          if b == 0 then a else gcd b (a mod b)
*)
(** Simplifies a fraction. It gives a equivalent fraction such that (gcd f.num f.den) == 1 *)
(*let rec  simplifyFrac f =  
   if (gcd f.num f.den) == 1 then f else simplifyFrac {num = f.num/(gcd f.num f.den); den = f.den/(gcd f.num f.den)}  
*)

(* Permission specific procedures *)

(* Returns a i-th fraction of p
let split_ith p i = Q.mul p (Q.div Q.one (Q.of_int i)) *)
      
(* Returns a half of p   
let half p = split_ith p 2  *)
    
(* Returns a third of p 
let third p = split_ith p 3*)

(* Addition
 let add p1 p2 = p1 + p2  *)
  


 