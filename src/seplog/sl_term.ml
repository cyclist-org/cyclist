open Lib
open Util
open Symbols
open MParser

include Int
include Var

let nil = 0

let is_nil v = v = nil
let is_var v = v <> nil && Var.is_var v
let filter_vars s = Set.filter is_var s

type substitution = t Map.t
let empty_subst : substitution = Map.empty
let singleton_subst x y = Map.add x y empty_subst
let subst theta v =
  if not (equal v nil) && Map.mem v theta then Map.find v theta else v
(* above is significantly faster than exception handling *)
let subst_list theta l = Blist.map (fun v -> subst theta v) l

let unify theta t t' =
  if Map.mem t theta then
    if equal (Map.find t theta) t' then Some theta else None
  else if equal t nil then
    if equal t' nil then Some theta else None
  else if
  is_exist_var t &&
  is_exist_var t' &&
  Map.exists (fun _ t'' -> equal t' t'') theta then
    (* avoid capture *)
    None
  else Some (Map.add t t' theta)

let rec unify_list theta args args' = match (args, args') with
  | ([], []) -> Some theta
  | (_, []) | ([], _) -> None
  | (hd:: tl, hd':: tl') ->
      match unify theta hd hd' with
      | None -> None
      | Some theta' -> unify_list theta' tl tl'

let unify_ordered_pairs theta (x, y) (x', y') =
  match unify theta x x' with
  | None -> None
  | Some theta' -> unify theta' y y'

let unify_pairs theta p p' =
  Option.list_get [
    unify_ordered_pairs theta p p';
    unify_ordered_pairs theta p (Pair.swap p')
    ]

let to_string v = if v = nil then keyw_nil.str else Var.to_string v
let list_to_string l = Blist.to_string symb_comma.str to_string l
let to_melt v =
  ltx_mk_math (if v = nil then keyw_nil.melt else Latex.text (Var.to_string v))
let pp fmt trm = Format.fprintf fmt "@[%s@]" (to_string trm)
let pp_list fmt l = Blist.pp pp_comma pp fmt l

(* return a substitution that takes all vars in subvars to new         *)
(* variables that are outside vars U subvars, respecting exist/univ    *)
let avoid_theta vars subvars =
  let allvars = Set.union vars subvars in
  let (exist_vars, univ_vars) =
    Pair.map Set.elements (Set.partition is_exist_var subvars) in
  let fresh_u_vars = fresh_uvars allvars (Blist.length univ_vars) in
  let fresh_e_vars = fresh_evars allvars (Blist.length exist_vars) in
  let theta = Map.of_list
      (Blist.rev_append (Blist.combine univ_vars fresh_u_vars)
          (Blist.combine exist_vars fresh_e_vars)) in
  theta

let parse st =
  (   attempt (parse_symb keyw_nil >>$ 0 <?> "nil") 
  <|> Var.parse 
  <?> "Sl_term") st
