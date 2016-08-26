(* First order predicate logic formula
 *
 * Copyright (C) 2016 Kareem Khazem <karkhaz@karkhaz.com>
 *
 *)

type pure_formula =
  | Eq of (Asl_term.t * Asl_term.t)
  | Ne of (Asl_term.t * Asl_term.t)
  | Le of (Asl_term.t * Asl_term.t)
  | Lt of (Asl_term.t * Asl_term.t)


type formula =
  | PF of pure_formula
  | And of (formula * formula)
  | Or of (formula * formula)
  | Not of formula
  | Forall of (Asl_term.Set.t * formula)
  | Exists of (Asl_term.Set.t * formula)


(* Simplify formula by removing trivially true terms and collapsing
 * strings of existentials and foralls
 *)
val remove_trivia : formula -> formula


(* A formula that is equal to true *)
val trivially_true : formula

(* A formula that is equal to false *)
val trivially_false : formula


val to_z3 : formula -> string
(* Convert FOPL fmla to z3 format string. *)

