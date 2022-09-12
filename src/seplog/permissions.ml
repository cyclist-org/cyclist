(** Type representing permissions (for Concurrent Separation Logic automation) *)

open Lib 

(* open Generic *)

 
type permFraction = 
  { num: int
  ; den: int}
 

type t = permFraction
 
(* Constructors *)
let one = 
    {num = 1; den = 1}
 

(* Some other arithmetic fraction manipulation procedures *)
(* Multiply both num and den by n, getting an equiv fraction *)
let equivFract f n =
    {num = f.num * n; den = f.den * n}


(* BasicType required procedures *)  

let compare p1 p2 = 
    (equivFract p1 p2.den).num - 
    (equivFract p2 p1.den).num 


let equal p1 p2 =  
      (compare p1 p2 == 0)

let hash p =                (* CHECK this procedure. Not sure what it does or what it should do *)
    genhash p.num p.den
       
let to_string p =  (*Procedure not tested, but it should be ok*)
    Float.to_string
    (Float.div
      (Float.round 
        (Float.mul 
          (Float.div (Float.of_int p.num) (Float.of_int p.den))
          100.0
        )
      )
    100.00
    )

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
let rec gcd a b =  
          if b == 0 then a else gcd b (a mod b)

(** Simplifies a fraction. It gives a equivalent fraction such that (gcd f.num f.den) == 1 *)
let rec  simplifyFrac f =  
   if (gcd f.num f.den) == 1 then f else simplifyFrac {num = f.num/(gcd f.num f.den); den = f.den/(gcd f.num f.den)}  


(* Permission specific procedures *)

(* Returns a i-th fraction of p *)
let split_ith p i = 
      simplifyFrac {num = p.num; den = p.den*i}

(* Returns a half of p    *)
let half p = 
      split_ith p 2 
    

(* Returns a third of p *)
let third p = 
      split_ith p 3


(* Addition *)
 let add p1 p2 = 
      simplifyFrac {num = p1.num * p2.den + p2.num * p1.den; den=p1.den * p2.den}
(* let add p1 p2 = 
    if (p1.den == p2.den) 
      then 
        {num = p1.num + p2.num; den=p2.den}
      else
        simplifyFrac {num = p1.num * p2.den + p2.num * p1.den; den=p1.den * p2.den}
*) 

