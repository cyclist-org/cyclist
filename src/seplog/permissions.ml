(** Type representing permissions (for Concurrent Separation Logic automation) *)

open Lib 

(* open Generic *)

 (* Let's start by defining it as a fraction. I'll implement the rest of the polynomial (see .mli file) later *)
type permFraction = Q.t
 

type t = permFraction
 
(* Constructors *)
let one = Q.one
     
 
 

(* BasicType required procedures *)  

let compare p1 p2 = Q.compare p1 p2 


let equal p1 p2 =  Q.equal p1 p2 

let hash p = Float.hash (Q.to_float p)
       
let to_string p =  Q.to_string p 

let pp fmt p = Format.fprintf fmt "%s" (to_string p) 
  

(* Some other arithmetic fraction manipulation procedures *)
 
(**  NOT IN USE Chane denominator of fraction f, by newDen *) 
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

(* Returns a i-th fraction of p *)
let split_ith p i = Q.mul p (Q.div Q.one (Q.of_int i))
      
(* Returns a half of p    *)
let half p = split_ith p 2 
    
(* Returns a third of p *)
let third p = split_ith p 3

(* Addition *)
 let add p1 p2 = p1 + p2 
  


 