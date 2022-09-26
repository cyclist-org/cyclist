(** Type representing permissions (for Concurrent Separation Logic automation) *)


open Lib
open Generic

(* Keep this type abstract *)
 
(* Underlying representation will have to be terms over variables and numeric constants *)
(*
  e.g. 0.25x + 0.5x^2 + 0.5yz

  i.e. you need to represent arbitrary polynomials  
       which are going to be sums of products of variables and constants

       So maybe you'd choose a list of lists representation?
       The outer list is the sum, the inner lists are the individual products
       Each product will contain one constant and a multiset of variables.
       For the multiset, use the Multiset container module in Lib
*)

include BasicType

(* Add an instance of VarManager for representing variables in permissions *)

(* Constructors *)
 val one : t  
(* val mk_var : string -> t   *)
 val const : Q.t -> t  
 
(* Basic arithmetics*)
 
(* Throw an exception if the int < 1 *)
val split_ith: t -> int -> t
val half: t -> t
val third: t -> t 
 

(* Should probably be an error if:
   - the two arguments use different permission variables
   - the two coefficients sum to greater than 1  *)
val add : t -> t -> t (* option *) (*ERROR ! MODIFY THIS. MAKE IT option*)


(* Throw exception if 0 >= given Q.t or given Q.t >= 1 *)
val mul : t -> Q.t -> t