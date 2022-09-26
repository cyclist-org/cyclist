(** Type representing permissions (for Concurrent Separation Logic automation) *)

open Lib 


(* Using zarith package Q for arbitrary precision rational constants  *)
(* a) FRACTION Let's start by defining it as a fraction. I'll implement the rest of the polynomial (see .mli file) later *)
(* type permFraction = Q.t *)

(* b) VARMANAGER *)
(* Function needed for instantiation of VarManager to define the labels *)
let classify_perVarName s =
   assert (not (String.equal s "")) ;
   if String.equal s "_" then VarManager.ANONYMOUS
   else VarManager.FREE
 
 (* Instantiation of varManager for the Labels *)
module PerVarMgr = (val VarManager.mk 0 "_" classify_perVarName : VarManager.S)
   
module PerVar = PerVarMgr.Var
 
type t =  Q.t * PerVar.t option


(* Constructors *)
(*let one =  (Q.one, PerVarMgr.anonymous) *)
let one =  (Q.one, None)

let mk s = (Q.one, PerVarMgr.mk s)

(* let mk_var s = (Q.one, PerVarMgr.mk s) *)

let const q = (q, None)

(* Split *)
let split_ith (p, l) i = (Q.mul p (Q.div Q.one (Q.of_int i)), l)

let half t = split_ith t 2
let third t = split_ith t 3

(*These two below are just for testing *)
let oneHalf = split_ith one 2
let pi = (Q.one, PerVarMgr.mk "pi")


 
(* BasicType required procedures *)  

let compare (f1, v1) (f2, v2) =  
   if (PerVar.equal v1 v2) 
      then Q.compare f1 f2
      else -1
let compare (f1, _) (f2, _) =  
   Q.compare f1 f2
      

let equal p1 p2 = (compare p1 p2 == 0)

let hash (f,v) =  (Float.hash (Q.to_float f))  (* Incorrect*)
       
(* let to_string p = String.cat (Q.to_string p.permis) (Var.to_string p.label) *)
let to_string (f,v) = String.cat (Q.to_string f) (PerVar.to_string v)
let to_string (f,_) = Q.to_string f

let pp fmt p = Format.fprintf fmt "%s" (to_string p) 
  
(* Permission specific procedures *)

(* Returns a i-th fraction of p
let split_ith p i = Q.mul p (Q.div Q.one (Q.of_int i)) *)
      
(* Returns a half of p   
let half p = split_ith p 2  *)
    
(* Returns a third of p 
let third p = split_ith p 3*)

(* Addition *)
let add (f1, v1) (f2, v2) = 
   if (v1 == v2)
      then (Q.add f1 f2, v1)
      else (Q.of_int(-1), v1) (*  WRONG. IT SHOULD RETURN ERROR *)


let mul (f, v) n = (Q.mul f n, v) 