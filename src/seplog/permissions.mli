(** Type representing permissions (for Concurrent Separation Logic automation) *)


open Lib
open Generic

(* Keep this type abstract? *)
(* type permFraction = private
  { num: int
  ; den: int} *)

(* Underlying representation will have to be terms over variables and numeric constants *)
(*
  e.g. 0.25x + 0.5yz

  i.e. you need to represent arbitrary polynomials
       which are going to be sums of products of variables and constants

       So maybe you'd choose a list of lists representation?
       The outer list is the sum, the inner lists are the individual products
       Each product will contain one constant and a multiset of variables.
       For the multiset, use the Multiset container module in Lib
*)

(* Use zarith package for arbitrary precision rational constants? *)

include BasicType (* with type t  = permFraction *)

(* Add an instance of VarManager for representing variables in permissions *)

(* Constructors *)
val one : t
 
 
(* Basic arithmetics *)
 

val half: t -> t
val third: t -> t
val split_ith: t -> int -> t

val add: t -> t -> t
  