open Lib
open Util
open Symbols
open MParser

let nil = 0

module Trm = 
  struct
    type t = int
    let equal = Int.equal
    let compare = Int.compare
    let hash = Int.hash
    let to_string v = 
      if equal v nil then keyw_nil.str else Var.to_string v
    let pp fmt trm = 
      Format.fprintf fmt "@[%s@]" (to_string trm)
    let to_melt v =
      ltx_mk_math 
        (if v = nil then keyw_nil.melt else Latex.text (Var.to_string v))
    let parse st =
      (   attempt (parse_symb keyw_nil >>$ 0 <?> "nil") 
      <|> Var.parse 
      <?> "Sl_term") st

    let is_nil v = v = nil
    let is_var v = v <> nil && Var.is_var v
    let is_exist_var : t -> bool = Var.is_exist_var
    let is_univ_var : t -> bool = Var.is_univ_var
    let mk_univ_var : string -> t = Var.mk_univ_var
    let mk_exist_var : string -> t = Var.mk_exist_var 
    let is_exist_name = Var.is_exist_name
    let is_univ_name = Var.is_univ_name

  end


module Set = Util.MakeListSet(Trm) 
module Map = Util.MakeMap(Trm)

include Trm

(* FIXME *)
(* conversion from integer sets to term sets should be a runtime noop *)
let fresh_evar s = Var.fresh_evar (Int.Set.of_list (Set.to_list s))
let fresh_uvar s = Var.fresh_uvar (Int.Set.of_list (Set.to_list s))
let fresh_evars s n = Var.fresh_evars (Int.Set.of_list (Set.to_list s)) n
let fresh_uvars s n = Var.fresh_uvars (Int.Set.of_list (Set.to_list s)) n

let filter_vars s = Set.filter is_var s

type substitution = t Map.t

type 'a unifier = substitution -> 'a -> 'a -> substitution option 
type 'a gen_unifier = (substitution -> substitution option) -> 'a unifier

let empty_subst : substitution = Map.empty
let singleton_subst x y = Map.add x y empty_subst
let subst theta v =
  if not (equal v nil) && Map.mem v theta then Map.find v theta else v
(* above is significantly faster than exception handling *)

let trm_unify theta t t' =
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

module FList =
  struct
    include Util.MakeFList(Trm)
    let rec unify theta args args' = 
      match (args, args') with
      | ([], []) -> Some theta
      | (_, []) | ([], _) -> None
      | (hd:: tl, hd':: tl') ->
        Option.bind (fun theta' -> unify theta' tl tl') (trm_unify theta hd hd')
    
    let subst theta xs = Blist.map (subst theta) xs
    
    let to_string_sep sep xs = Blist.to_string sep Trm.to_string xs
    
    let terms xs = 
      Blist.foldl 
        (fun a x -> Set.add x a) 
        Set.empty 
        xs 
        
    let vars xs = filter_vars (terms xs)
  end 

let unify = trm_unify