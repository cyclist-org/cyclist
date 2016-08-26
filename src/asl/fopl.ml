(* First order predicate logic formula
 *
 * Copyright (C) 2016 Kareem Khazem <karkhaz@karkhaz.com>
 *
 *)

open Lib
open Printf


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

(* Z3 does not accpet ' as a valid character, rename all existentials vars
 * with other names *)
let rename_exist f =
  let all_vars f =
    let vars_in_pf f vars = 
      match f with
      | Eq (t, t') | Ne (t, t') | Le (t, t') | Lt (t, t') ->
        Asl_term.Set.union (Asl_term.filter_vars (Asl_term.Set.singleton t))
        (Asl_term.Set.union (Asl_term.filter_vars (Asl_term.Set.singleton t')) vars)
    in let rec vars_in_fmla f vars =
      match f with
      | PF pf -> vars_in_pf pf vars
      | And (f1, f2) | Or (f1, f2) ->
        Asl_term.Set.union (vars_in_fmla f1 vars) (vars_in_fmla f2 Asl_term.Set.empty)
      | Not f' -> vars_in_fmla f' vars
      | Forall (vs, f') | Exists (vs, f') ->
        Asl_term.Set.union vs (vars_in_fmla f' vars)
  in vars_in_fmla f Asl_term.Set.empty
  in let all = (all_vars f)
  in let (free, exs) = Asl_term.Set.partition Asl_term.is_free_var all
  in let fresh = Asl_term.fresh_fvars all (Asl_term.Set.cardinal exs)
  in let theta = Asl_term.Map.of_list (Blist.combine (Asl_term.Set.elements exs) fresh)
  in let subst theta f =
    let subst_in_pf theta f =
      match f with
      | Eq (t, t') -> Eq (Asl_term.subst theta t, Asl_term.subst theta t')
      | Ne (t, t') -> Ne (Asl_term.subst theta t, Asl_term.subst theta t')
      | Le (t, t') -> Le (Asl_term.subst theta t, Asl_term.subst theta t')
      | Lt (t, t') -> Lt (Asl_term.subst theta t, Asl_term.subst theta t')
    in let rec subst_in_fmla theta f =
      match f with
      | PF pf -> PF (subst_in_pf theta pf)
      | And (f1, f2) -> And (subst_in_fmla theta f1, subst_in_fmla theta f2)
      | Or (f1, f2) -> Or (subst_in_fmla theta f1, subst_in_fmla theta f2)
      | Not f' -> Not (subst_in_fmla theta f')
      | Forall (vs, f') -> Forall (Asl_term.Set.endomap (Asl_term.subst theta) vs, subst_in_fmla theta f')
      | Exists (vs, f') -> Exists (Asl_term.Set.endomap (Asl_term.subst theta) vs, subst_in_fmla theta f')
  in subst_in_fmla theta f
  in (Asl_term.Set.endomap (Asl_term.subst theta) exs, subst theta f)


let trivially_true = PF (Eq (Asl_term.mk_const(0), Asl_term.mk_const(0)))


let trivially_false = PF (Eq (Asl_term.mk_const(1), Asl_term.mk_const(0)))


let remove_trivia formula =
  let rec remove_trivia = function
    | PF f          -> PF f
    | And (f1, f2) -> (
      if f1 = trivially_true
      then f2
      else if f2 = trivially_true
           then f1
           else And (remove_trivia f1, remove_trivia f2)
    )
    | Or (f1, f2)   -> (
      if f1 = trivially_false
      then f2
      else if f2 = trivially_false
           then f1
           else Or (remove_trivia f1, remove_trivia f2)
    )
    | Not f         -> Not (remove_trivia f)
    | Forall (s, f) -> (
      if Asl_term.Set.cardinal s = 0
      then remove_trivia f
      else Forall (s, remove_trivia f)
    )
    | Exists (s, f) -> (
      if Asl_term.Set.cardinal s = 0
      then remove_trivia f
      else Exists (s, remove_trivia f)
    )
  in let rec iterate_till_fixed formula =
    let new_form = remove_trivia formula
    in if new_form = formula
    then new_form
    else iterate_till_fixed new_form
  in iterate_till_fixed (remove_trivia formula)

let to_z3 f =
  let declare_free_vars free_vars =
    Asl_term.Set.fold (fun v acc -> (sprintf "(declare-const %s Int)\n" (Asl_term.to_z3 v)) ^ acc) free_vars ""
  in let pf_to_z3 = function
      | Eq (t1, t2)   -> sprintf "(= %s %s)"  (Asl_term.to_z3 t1)
                                              (Asl_term.to_z3 t2)
      | Le (t1, t2)   -> sprintf "(<= %s %s)" (Asl_term.to_z3 t1)
                                              (Asl_term.to_z3 t2)
      | Ne (t1, t2)   -> sprintf "(not (= %s %s))" (Asl_term.to_z3 t1)
                                                   (Asl_term.to_z3 t2)
      | Lt (t1, t2)   -> sprintf "(< %s %s)"  (Asl_term.to_z3 t1)
                                              (Asl_term.to_z3 t2)
  in let rec to_z3 f level =
    let indent = Bytes.to_string (Bytes.make (2 * level) ' ')
    in let level = level + 1
    in let rec var_list v acc = sprintf " (%s Int)" (Asl_term.to_z3 v) ^ acc
    in match f with
      | PF f          -> sprintf "%s%s" indent (pf_to_z3 f)
      | Not f         -> sprintf "%s(not\n%s\n%s)" indent
                         (to_z3 f level) indent
      | Forall (v, f) -> sprintf "%s(forall (%s)\n%s\n%s)"
                         indent (Asl_term.Set.fold var_list v "") (to_z3 f level) indent
      | Exists (v, f) -> sprintf "%s(exists (%s)\n%s\n%s)"
                         indent (Asl_term.Set.fold var_list v "") (to_z3 f level) indent
      | And (f1, f2)  -> sprintf "%s(and\n%s\n%s\n%s)"
                         indent (to_z3 f1 level)
                         (to_z3 f2 level) indent
      | Or  (f1, f2)  -> sprintf "%s(or\n%s\n%s\n%s)"
                         indent (to_z3 f1 level)
                         (to_z3 f2 level) indent
  in let (free_vars, f) = rename_exist f
  in sprintf "%s(assert\n%s\n)\n(check-sat)" (declare_free_vars free_vars) (to_z3 (remove_trivia f) 1)
