open Lib
open   Symbols

open Generic

open MParser

let anon_var_name = "_"

let classify_varname s =
  let l = String.length s in
  assert (not (String.equal s "")) ;
  if String.equal s anon_var_name then VarManager.ANONYMOUS
  else if Int.( > ) l 1 && Char.equal s.[l - 1] '\'' then VarManager.BOUND
  else VarManager.FREE

include (val VarManager.mk (-4) anon_var_name classify_varname : VarManager.S)

type term_t =
  | Const of int
  | Var of Var.t
  | Fun of string * term_t list

module rec TermT : BasicType with type t = term_t =
  struct
    type t = term_t
    
    module TermList = Flist.Make(TermT)

    let compare t t' = match (t,t') with
      | (Var(n), Var(n')) -> Var.compare n n'
      | (Const(n), Const(n')) -> Int.compare n n'
      | (Fun(f, l), Fun(f', l')) ->
        (match Strng.compare f f' with
          | 0 -> TermList.compare l l'
          | n -> n)
      | (Fun _, _) | (_, Const _) -> 1
      | (Const _, _) | (_, Fun _) -> -1

    let equal t t' = Int.equal (compare t t') 0
    let hash = Hashtbl.hash

    let to_string = function
      | Const(i) -> string_of_int i
      | Var(v) -> Var.to_string v
      | Fun(f, tl) -> f ^ "(" ^ (TermList.to_string tl) ^ ")"

    let rec pp fmt = function
      | Const(i) -> Format.fprintf fmt "@[%i@]" i
      | Var(v) -> Format.fprintf fmt "@[%s@]" (Var.to_string v)
      | Fun(f, args) ->
        Format.fprintf fmt "@[%s(%a)@]" f (Blist.pp pp_comma pp) args
  end

include Containers.Make(TermT)
include TermT

let mk_const i = Const(i)
let zero = mk_const 0
let is_zero x = equal zero x
let mk_var i = assert (not (Int.equal (Var.to_int i) 0)) ; Var(i)
let mk_univ_var name = mk_var (mk name)
let mk_exist_var name = mk_var (mk name)
let mk_fun ident args = Fun(ident, args)

let dest_var = function
  | Var(v) -> v
  | _ -> invalid_arg "Term.dest_var"
let dest_const = function
  | Const(c) -> c
  | _ -> invalid_arg "Term.dest_const"
let dest_fun = function
  | Fun(f,args) -> (f,args)
  | _ -> invalid_arg "Term.dest_fun"

let is_var = function
  | Var _ -> true
  | _ -> false
let is_exist_var = function
  | Var(v) when is_exist_var v -> true
  | _ -> false
let is_free_var = function
  | Var(v) when is_free_var v -> true
  | _ -> false
let is_fun = function
  | Fun _ -> true
  | _ -> false
let is_succ = function
  | Fun("s", _) -> true
  | _ -> false
let is_cons = function
  | Fun("cons", _) -> true
  | _ -> false

let filter_vars s = Set.filter is_var s

let rec terms = function
  | Fun(_, tl) as t -> Set.add t (terms_of_list tl)
  | any -> Set.singleton any
and terms_of_list tl = Set.union_of_list (Blist.map terms tl)

let vars trm = filter_vars (terms trm)

let vars_of_list tl = Set.union_of_list (Blist.map vars tl)

let latex_of_var v =
  (if is_exist_var v then "e" else "a") ^
  "^{" ^ (Var.to_string (dest_var v)) ^ "}"

type substitution = TermT.t Map.t

let empty_subst = Map.empty
let singleton_subst x y = Map.add x y empty_subst

let subst_is_identity theta =
  Map.for_all equal theta

let rec subst theta = function
  | Var _ as v when Map.mem v theta -> Map.find v theta
  | Var _ as v -> v
  | Fun(s, tl) -> Fun(s, subst_list theta tl)
  | c -> c
and subst_list theta l = Blist.map (subst theta) l

(* find extension theta' of theta such that *)
(* t[theta'] = t' *)
let rec unify theta t t' =
  if Map.mem t theta then
    if TermT.equal (Map.find t theta) t' then Some theta else None
  else
  match t with
  | Const _ when TermT.equal t t' -> Some theta
  | Const _ -> None
  | Var _ when is_exist_var t ->
    if
      not (is_exist_var t') ||
      (* avoid capture *)
      Map.exists (fun _ t'' -> TermT.equal t' t'') theta
    then
      None
    else
      Some (Map.add t t' theta)
  | Var _ -> Some (Map.add t t' theta)
  | Fun(f, args) when is_fun t' ->
    let (f', args') = dest_fun t' in
    if
      not (Strng.equal f f') ||
      not (Int.equal (Blist.length args) (Blist.length args'))
    then
      None
    else
      unify_list theta args args'
  | Fun _ -> None
and unify_list theta args args' = match (args,args') with
  | ([], []) -> Some theta
  | (_,  []) | ([], _) -> None
  | (hd::tl, hd'::tl') ->
    match unify theta hd hd' with
      | None -> None
      | Some theta' -> unify_list theta' tl tl'

let unify_ordered_pairs theta (x,y) (x', y') =
  match unify theta x x' with
    | None -> None
    | Some theta' -> unify theta' y y'

let unify_pairs theta p p' =
  Option.list_get [
    unify_ordered_pairs theta p p';
    unify_ordered_pairs theta p (Pair.swap p')
  ]

(* unifies two terms, computing a substitution s  such that s(t1) = t2 and *)
(* only substitutes variables in t1 with terms of t2 *)
(* thus t1 is in the formula to be unfolded and t2 is in the ind. def. *)
(* the result is a list of pairs of terms of t2 to substitute over the variables of t1*)
(* find extension theta' of theta such that *)
(* t[theta'] = t' *)
let def_add theta x y =
  let to_add =
    if Map.mem x theta then
      Set.add y (Map.find x theta)
    else
      Set.singleton y in
  Map.add x to_add theta

let rec multi_unify theta t t' = match t with
  | Const _ when TermT.equal t t' -> Some theta
  | Const _ -> None
  | Var _ -> Some (def_add theta t t')
  | Fun(f, args) when is_fun t' ->
    let (f', args') = dest_fun t' in
    if
      not (Strng.equal f f') ||
      not (Int.equal (Blist.length args) (Blist.length args'))
    then
      None
    else
      multi_unify_list theta args args'
  | Fun _ -> None
and multi_unify_list theta args args' = match (args,args') with
  | ([], []) -> Some theta
  | (_,  []) | ([], _) -> None
  | (hd::tl, hd'::tl') ->
    match multi_unify theta hd hd' with
      | None -> None
      | Some theta' -> multi_unify_list theta' tl tl'

let multi_unify_args ts ts' =
  let res = multi_unify_list Map.empty ts ts' in
  if Option.is_none res then None else
  let m = Option.get res in
  let theta = Map.map (fun v -> Set.choose v) m in
  let set_to_eqs s =
    let size = Set.cardinal s in
    if Int.(size < 2) then [] else
    let first = Set.choose s in
    let s = Set.remove first s in
    Set.map_to_list (fun el -> (first,el)) s in
  let eqs = Blist.bind (fun (_,v) -> set_to_eqs v) (Map.bindings m) in
  Some (theta, eqs)

let varset_of_termset s =
  Set.map_to
    Var.Set.add
    Var.Set.empty
    dest_var
    s

let fresh_fvar s = mk_var (fresh_fvar (varset_of_termset s))
let fresh_evar s = mk_var (fresh_evar (varset_of_termset s))

let fresh_fvars s i =
  Blist.map mk_var (fresh_fvars (varset_of_termset s) i)
let fresh_evars s i =
  Blist.map mk_var (fresh_evars (varset_of_termset s) i)

let rec parse st =
  ( attempt (Tokens.integer |>> mk_const)
    <|>
    attempt (parse_ident >>= (fun f ->
    Tokens.parens (sep_by1 parse (parse_symb symb_comma)) >>= (fun tl ->
    return (mk_fun f tl))))
    <|>
    (parse_ident |>> (fun s -> mk_var (mk s)))
  ) st
