open Lib
open Util
open Symbols
open MParser

include MakeVarManager(
  struct
    let map = Int.Hashmap.create 997
    let inv_map = Strng.Hashmap.create 997
    let max_var = ref 0
    let min_var = ref 0
    let default_varname exist = if exist then "v" else "u"
  end)

module Trm =
  struct

    type t = int
    let nil = 0

    let equal = Int.equal
    let compare = Int.compare
    let hash = Int.hash

    let to_string v =
      if equal v nil then keyw_nil.str else to_string v
    let pp fmt trm =
      Format.fprintf fmt "@[%s@]" (to_string trm)
    let to_melt v =
      ltx_mk_math
        (Latex.mathit
          (if v = nil then keyw_nil.melt else Latex.text (to_string v)))

    let parse st =
      (   attempt (parse_symb keyw_nil >>$ 0 <?> "nil")
      <|> (parse_ident >>= (fun name -> return (mk_var name (is_exist_name name))))
      <?> "Sl_term") st
    let of_string s =
      handle_reply (MParser.parse_string parse s ())
  end


include Trm

module Set = MakeListSet(Trm)
module Map = MakeMap(Trm)

include MakeGenerators(Set)

let is_nil v = v = nil
let is_var v = not (is_nil v)

let filter_vars s = Set.filter is_var s

type substitution = t Map.t

let empty_subst : substitution = Map.empty
let singleton_subst x y = Map.add x y empty_subst
let subst theta v =
  if not (is_nil v) && Map.mem v theta then Map.find v theta else v
(* above is significantly faster than exception handling *)
let pp_subst = Map.pp pp

let strip_subst theta = Map.filter (fun x y -> not (equal x y)) theta

let avoid_theta vars subvars =
  let allvars = Set.union vars subvars in
  let (exist_vars, univ_vars) =
    Pair.map Set.elements (Set.partition is_exist_var subvars) in
  let fresh_u_vars = fresh_uvars allvars (Blist.length univ_vars) in
  let fresh_e_vars = fresh_evars allvars (Blist.length exist_vars) in
  Map.of_list
    (Blist.append
      (Blist.combine univ_vars fresh_u_vars)
      (Blist.combine exist_vars fresh_e_vars))
      
let mk_univ_subst avoid xs =
  let univs = fresh_uvars (Set.union xs avoid) (Set.cardinal xs) in
  Map.of_list (Blist.combine (Set.to_list xs) univs)
  
let mk_ex_subst avoid xs =
  let exs = fresh_evars (Set.union xs avoid) (Set.cardinal xs) in
  Map.of_list (Blist.combine (Set.to_list xs) exs)
      
let partition_subst theta =
  Map.partition
    (fun x y -> is_univ_var x && (is_nil y || is_univ_var y))
    theta

let trm_unify ?(update_check=Fun._true) t t' cont init_state =
  let res =
    if Map.mem t init_state then
      Option.mk (equal (Map.find t init_state) t') init_state
    else if is_nil t then
      Option.mk (is_nil t') init_state
    else if (equal t t' || update_check (init_state, (Map.singleton t t'))) then
      Some (Map.add t t' init_state)
    else
      None in
  Option.bind cont res
  
let trm_biunify ?(update_check=Fun._true) t t' cont ((subst, subst') as state) =
  let mapped = (Map.mem t subst, Map.mem t' subst') in
  let opts =
    if Pair.both mapped then
      [ Option.mk (equal (Map.find t subst) (Map.find t' subst')) state ]
    else if fst mapped && is_nil t' then
      [ Option.mk (is_nil (Map.find t subst)) state ]
    else if fst mapped then
      let t'' = Map.find t subst in
      [ Option.mk_lazily
          (equal t' t'' || 
            update_check (state, (empty_subst, Map.singleton t' t'')))
          (fun _ -> (subst, Map.add t' t'' subst')) ]
    else if snd mapped && is_nil t then
      [ Option.mk (is_nil (Map.find t' subst')) state ]
    else if snd mapped then
      let t'' = Map.find t' subst' in
      [ Option.mk_lazily
          (equal t t'' ||
            update_check (state, (Map.singleton t t'', empty_subst)))
          (fun _ -> (Map.add t t'' subst, subst')) ]
    else if is_nil t && is_nil t' then
      [ Some state ]
    else
      [ Option.mk_lazily
          (not (is_nil t) 
            && (equal t t' || 
                update_check (state, (Map.singleton t t', empty_subst))))
          (fun _ -> 
            (Map.add t t' subst, 
              if not (is_nil t') then Map.add t' t' subst' else subst')) ;
        Option.mk_lazily
          (not (is_nil t') 
            && (equal t' t ||
                update_check (state, (empty_subst, Map.singleton t' t))))
          (fun _ -> 
            ((if not (is_nil t) then Map.add t t subst else subst), 
              Map.add t' t subst')) ] in
  Blist.find_some (Option.bind cont) opts

module FList =
  struct
    include Util.MakeFList(Trm)
    let mk_unify 
        (unify:
          ?update_check:('a Unification.state_update) Fun.predicate 
            -> ('a, 'b, 'c) Unification.cps_unifier) 
        ?(update_check=Fun._true) = 
      let rec u args args' cont init_state =
      match (args, args') with
      | ([], []) -> cont init_state
      | (_, []) | ([], _) -> None
      | (x::xs, y::ys) ->
          unify ~update_check x y 
          (u xs ys cont)
          init_state in
      u
    
    let unify ?(update_check=Fun._true) args args' cont init_state =
      mk_unify trm_unify ~update_check args args' cont init_state
      
    let biunify ?(update_check=Fun._true) args args' cont init_state =
      mk_unify trm_biunify ~update_check args args' cont init_state

    let subst theta xs = Blist.map (fun x -> subst theta x) xs

    let to_string_sep sep xs = Blist.to_string sep Trm.to_string xs

    let terms xs =
      Blist.foldl
        (fun a x -> Set.add x a)
        Set.empty
        xs

    let vars xs = filter_vars (terms xs)
  end

let unify = trm_unify
let biunify = trm_biunify