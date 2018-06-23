open Lib

open Symbols
open MParser

let classify_varname s =
  let l = String.length s in
  assert (String.length s > 0);
  if s="nil" then VarManager.ANONYMOUS else
  if l > 1 && s.[l-1] = '\'' then VarManager.BOUND
  else VarManager.FREE

include (val (VarManager.mk 4 "nil" classify_varname) : VarManager.S)

(* BasicType signature *)
type t = Var.t
let compare = Var.compare
let equal = Var.equal
let hash = Var.hash
let pp = Var.pp
let to_string = Var.to_string

module Set = Var.Set
module Map = Var.Map

let nil = anonymous
let is_nil = is_anonymous
let is_var t = not (is_nil t)

let parse st =
  ( (parse_ident >>= (fun name -> return (mk name))) <?> "Sl_term") st
let of_string s =
  handle_reply (MParser.parse_string parse s ())

let filter_vars s = Set.filter is_var s

let trm_unify ?(update_check=Fun._true) t t' cont init_state =
  let res =
    if Map.mem t init_state then
      Option.mk (equal (Map.find t init_state) t') init_state
    else if is_nil t then
      Option.mk (is_nil t') init_state
    else if (equal t t' || update_check (init_state, (Subst.singleton t t'))) then
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
            update_check (state, (Subst.empty, Subst.singleton t' t'')))
          (fun _ -> (subst, Map.add t' t'' subst')) ]
    else if snd mapped && is_nil t then
      [ Option.mk (is_nil (Map.find t' subst')) state ]
    else if snd mapped then
      let t'' = Map.find t' subst' in
      [ Option.mk_lazily
          (equal t t'' ||
            update_check (state, (Subst.singleton t t'', Subst.empty)))
          (fun _ -> (Map.add t t'' subst, subst')) ]
    else if is_nil t && is_nil t' then
      [ Some state ]
    else
      [ Option.mk_lazily
          (not (is_nil t)
            && (equal t t' ||
                update_check (state, (Subst.singleton t t', Subst.empty))))
          (fun _ ->
            (Map.add t t' subst,
              if not (is_nil t') then Map.add t' t' subst' else subst')) ;
        Option.mk_lazily
          (not (is_nil t')
            && (equal t' t ||
                update_check (state, (Subst.empty, Subst.singleton t' t))))
          (fun _ ->
            ((if not (is_nil t) then Map.add t t subst else subst),
              Map.add t' t subst')) ] in
  Blist.find_map (Option.bind cont) opts

module FList =
  struct
    include Var.FList

    let terms = Set.of_list
    let vars ts = Set.of_list (Blist.filter (fun t -> not (is_nil t)) ts)

    let to_string_sep sep ts = Blist.to_string sep Var.to_string ts

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

    let subst theta xs = Blist.map (fun x -> Subst.apply theta x) xs
  end

let unify = trm_unify
let biunify = trm_biunify
