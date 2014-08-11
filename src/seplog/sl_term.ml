open Lib
open Util
open Symbols
open MParser

module Trm = 
  struct
    type t = int
    let nil = 0

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
    let of_string s =
      handle_reply (MParser.parse_string parse s ())

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

type 'a unifier_state = substitution * 'a

type ('a,'b) unifier = 
  ('b unifier_state -> 'b unifier_state option) ->
    'b unifier_state -> 'a -> 'a ->
      'b unifier_state option


let empty_subst : substitution = Map.empty
let singleton_subst x y = Map.add x y empty_subst
let subst theta v =
  if not (is_nil v) && Map.mem v theta then Map.find v theta else v
(* above is significantly faster than exception handling *)
let pp_subst = Map.pp pp

let trm_unify cont ((theta, rest) as state) t t' =
  let res = 
    if Map.mem t theta then
      Option.mk (equal (Map.find t theta) t') state 
    else if is_nil t then
      Option.mk (is_nil t') state
    else if is_univ_var t || is_exist_var t && (is_exist_var t' || is_nil t') then 
      Some (Map.add t t' theta, rest)
    else 
      None in
  Option.bind cont res
    
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
    let rec unify cont ((theta, rest) as state) args args' = 
      match (args, args') with
      | ([], []) -> cont state
      | (_, []) | ([], _) -> None
      | (x::xs, y::ys) ->
        trm_unify (fun state' -> unify cont state' xs ys) state x y
    
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