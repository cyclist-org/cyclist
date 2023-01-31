(** Type representing permissions (for Concurrent Separation Logic automation) *)

open Lib 
open MParser
open MParser_RE

(* A permission is a set of a rational number and zero or one label. TO DO: Implement it as an arbitrary polynomial.*)
(* Using zarith package Q for arbitrary precision rational constants  *)
(* Using VarManager for the label  *)

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
let one =  (Q.one, None)
let const q = (q, None)

let mk s = (Q.one, Some (PerVarMgr.mk s))
let mk_var s = (Q.one, PerVarMgr.mk s)  


(* Split *)
(* let split_ith (p, l) i = (Q.mul p (Q.div Q.one (Q.of_int i)), l) *)
let split_ith (p, l) i = (Q.div p (Q.of_int i), l)

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

let hash (f,v) = Float.hash (Q.to_float f) + PerVar.hash v 
let hash (f,_) = Float.hash (Q.to_float f)
       
let to_string (f,v) = (Q.to_string f) ^ (PerVar.to_string v)
let to_string (f,_) = Q.to_string f

let pp fmt p = Format.fprintf fmt "%s" (to_string p)

  
(* Permission specific procedures *) 
exception IncorrectPermission of string

(* Addition *)
let add (f1, v1) (f2, v2) = 
   if (v1 != v2 || (Q.add f1 f2) > Q.one) 
      then raise (IncorrectPermission "Permission sum not defined for these two Permission values")
      else (Q.add f1 f2, v1) 
  
(* Multiplication *)
let mul (f, v) n = 
   if  (Q.mul f n < Q.zero || (Q.mul f n) > Q.one) 
      then raise (IncorrectPermission "Permission multiplication not defined")
      else (Q.mul f n, v) 


(* Parse a permission *)
let parse st =
   ( Tokens.squares
       ( regexp (make_regexp "[0-9]([/][0-9])?")  (*(make_regexp "[a-z]*")*)
       << spaces
       >>= fun frac ->
       (option parse_ident)
       >>= fun ident_opt ->
         if (Q.of_string frac > Q.one) 
            then raise (IncorrectPermission "Permission bigger than 1 not defined") 
            else let f = Q.of_string frac in  
               let id = Option.map PerVarMgr.mk ident_opt in
    return (f, id) )   (*  return (mk frac varName) )*)
   <?> "Perm" )
   st
   
(* let parse st =
 *   (
 *     Tokens.squares
 *      (parse_ident
 *       >>= fun f ->
 *       try
 *         return (Q.of_string f, None)
 *       with _ ->
 *         return (mk f)
 *      ) <?> "Perm"
 *   )  st *)
