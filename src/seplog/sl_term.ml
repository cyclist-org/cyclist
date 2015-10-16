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

let trm_unify ?(update_check=Fun._true) t t' cont init_state =
  let res =
    if Map.mem t init_state then
      Option.mk (equal (Map.find t init_state) t') init_state
    else if is_nil t then
      Option.mk (is_nil t') init_state
    else if (update_check (init_state, (Map.singleton t t'))) then
      Some (Map.add t t' init_state)
    else
      None in
  Option.bind cont res

module FList =
  struct
    include Util.MakeFList(Trm)
    let rec unify ?(update_check=Fun._true) args args' cont init_state =
      match (args, args') with
      | ([], []) -> cont init_state
      | (_, []) | ([], _) -> None
      | (x::xs, y::ys) ->
          let cont state' = unify ~update_check xs ys cont state' in
          trm_unify ~update_check x y cont init_state

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