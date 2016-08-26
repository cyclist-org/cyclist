open Lib
open Util
open Symbols
open MParser
open Sl_term


module T = Util.Strng

type term =
| Var of Sl_term.t
| Const of int
| Add of (term * term)
| Mult of (int * term)
    

module T' = 
  struct
    type t = term
    let rec to_string = function
      | Var v        -> Printf.sprintf "%s" (Sl_term.to_string v)
      | Const n      -> Printf.sprintf "%d" n
      | Mult (n, t)  -> Printf.sprintf "(%d x %s)" n (to_string t)
      | Add (t1, t2) -> Printf.sprintf "(%s + %s)" (to_string t1) (to_string t2)
    let pp fmt s = T.pp fmt (to_string s)
    let rec equal s s' =
      match (s, s') with
      (* allow for commutative prop in Add *)
      | (Add(t1, t2), Add(t1', t2')) -> (equal t1 t1' && equal t2 t2') || (equal t1 t2' && equal t2 t1')
      | (Mult(i1, t1), Mult(i2, t2)) -> i1=i2 && equal t1 t2
      | _ -> s=s'
    let compare s s' = 
      if equal s s' then 0 else
      match (s, s') with
      | (Const(n1), Const(n2)) -> Pervasives.compare n1 n2
      | (Var(x), Var(y)) -> Sl_term.compare x y
      | (Var(x), _) -> -1
      | (_, Var(y)) -> 1
      | (Const(n1), _) -> -1
      | (_, Const(n2)) -> 1
      | _ -> String.compare (to_string s) (to_string s')
    let hash s = Hashtbl.hash (to_string s)
  end
include T'

module Set = Util.MakeTreeSet(T')
module Map = Util.MakeMap(T')

let mk_const i = Const(i)
let mk_var v = Var(Sl_term.of_string v)
let mk_add (t1, t2) = Add(t1, t2)
let mk_mult (t1, t2) = Mult(t1, t2)
let parse_const st = (MParser_PCRE.Tokens.decimal |>> mk_const) st
let parse_var st = (parse_ident |>> mk_var) st
let rec parse_add st = (parse >>= (fun x-> parse_symb symb_plus
                        >> parse << spaces |>> (fun y -> Add(x, y)))) st
  and parse_mult st = (MParser_PCRE.Tokens.decimal >>=
                      (fun x-> MParser_PCRE.Tokens.parens parse |>> (fun y -> Mult(x, y)))) st
  and parse st = ( attempt(MParser_PCRE.Tokens.parens parse_add) <|>
                  attempt(parse_mult) <|>
                  attempt(parse_const) <|>
                  attempt(parse_var) <?> "trm") st
let of_string s = handle_reply (MParser.parse_string parse s ())

let rec to_melt trm = 
  let aux s =
    match s with
      | Var v        -> Sl_term.to_melt v
      | Const n      -> Latex.latex_of_int n
      | Mult (n, t)  ->
        Latex.concat[ symb_lp.melt; (Latex.latex_of_int n); symb_times.melt; to_melt t; symb_rp.melt ]
      | Add (t1, t2) ->
        Latex.concat[ symb_lp.melt; to_melt t1; symb_plus.melt; to_melt t2; symb_rp.melt ]
in ltx_mk_math (aux trm)

let rec to_z3 = function
  | Var v         -> Printf.sprintf "%s" (Sl_term.to_string v)
  | Const n       -> Printf.sprintf "%d" n
  | Mult (n, t)   -> Printf.sprintf "(* %d %s)" n (to_z3 t)
  | Add (t1, t2)  -> Printf.sprintf "(+ %s %s)" (to_z3 t1) (to_z3 t2)

let get_var v =
  match v with
  | Var x -> x
  | _ -> failwith "Not a Var"

let is_var v =
  match v with
  | Var v -> Sl_term.is_var v
  | _ -> false

let simplify s =
  let rec single t =
    match t with
    | Add (t, Const(0)) -> single t
    | Mult (1, t) -> single t
    | Mult (0, t) -> Const(0)
    | Add(Const(i1), Const(i2)) -> Const(i1+i2)
    | Add(Add(t, Const(i1)), Const(i2)) | Add(Add(Const(i1), t), Const(i2))
    | Add(Const(i2), Add(t, Const(i1))) | Add(Const(i2), Add(Const(i1), t)) -> Add(single t, Const(i1+i2))
    | Mult(i1, Const(i2)) -> Const(i1*i2)
    | Add(t1, t2) -> if equal t1 t2 then Mult(2, t1) else Add(single t1, single t2)
    | _ -> t
  in Lib.fixpoint equal single s

let filter_vars s =
  let rec get_vars_in_term n acc =
    match n with
    | Var v -> assert(Sl_term.is_var v); Set.add n acc (* nil is not defined in ASL *)
    | Const _ -> acc
    | Add (t1, t2) ->
      Set.union_of_list [(get_vars_in_term t1 acc); (get_vars_in_term t2 acc); acc]
    | Mult (_, t2) -> Set.union (get_vars_in_term t2 acc) acc
  in Set.fold get_vars_in_term s Set.empty

let filter_vars_to_sl_terms s =
  Set.fold (fun v acc -> Sl_term.Set.add (get_var v) acc) (filter_vars s) Sl_term.Set.empty

let sl_terms_to_asl s = List.fold_left (fun acc v -> (Var(v))::acc) [] s

let fresh_fvars s n = sl_terms_to_asl (Sl_term.fresh_fvars (filter_vars_to_sl_terms s) n) 
let fresh_evars s n = sl_terms_to_asl (Sl_term.fresh_evars (filter_vars_to_sl_terms s) n)
let fresh_fvar s = Var(Sl_term.fresh_fvar (filter_vars_to_sl_terms s))
let fresh_evar s = Var(Sl_term.fresh_evar (filter_vars_to_sl_terms s))

let is_free_var v = (is_var v) && (Sl_term.is_free_var (get_var v))
let is_exist_var v = (is_var v) && (Sl_term.is_exist_var (get_var v))

let rec subst theta t = 
  if Map.mem t theta then Map.find t theta
  else match t with
  | Var v -> Var v
  | Const i -> Const i
  | Mult(i, t1) -> Mult(i, (subst theta t1))
  | Add(t1, t2) -> Add((subst theta t1), (subst theta t2))

 
type term_t = t

module type AslSubstSig =
sig
  type t = term_t Map.t
  type check = t -> term_t -> term_t -> bool
  val empty : t
  val singleton : term_t -> term_t -> t
  val pp : Format.formatter -> t -> unit
  val trivial_check : check
  val basic_lhs_down_check : check
  val avoids_replacing_check : ?inverse:bool -> Set.t -> check
  val combine_checks : check list -> check
end

module Asl_subst : AslSubstSig =
  struct
    type t = term_t Map.t
    type check = t -> term_t -> term_t -> bool

    let empty = Map.empty
    let singleton x y = Map.add x y empty
    
    let pp = Map.pp pp
    let trivial_check _ _ _ = true
    
    let basic_lhs_down_check theta t t' =
      is_free_var t' ||
      Set.for_all (is_free_var) (filter_vars (Set.singleton t)) ||
      is_exist_var t && is_exist_var t' &&
        Map.for_all (fun _ t'' -> not (equal t' t'')) theta

    let avoids_replacing_check ?(inverse=false) vars =
      fun _ -> Fun.direct inverse (fun x y -> 
        equal x y || not (Set.mem y vars)) 
    let rec combine_checks cs theta x y =
      Blist.for_all (fun f -> f theta x y) cs
  end

module type AslUnifierSig =
  sig
    type state = Asl_subst.t * Util.TagPairs.t
    val empty_state : state
    type continuation = state -> state option 
    val trivial_continuation : continuation
    val basic_lhs_down_verifier : continuation
    type 'a t = 
      ?sub_check:Asl_subst.check ->
        ?cont:continuation ->
          ?init_state:state -> 'a -> 'a ->
            state option
  
    val backtrack : 
      'a t -> 
        ?sub_check:Asl_subst.check -> ?cont:continuation -> ?init_state:state -> 
          'a -> 'a -> state list
    type state_check = state -> bool
    val mk_assert_check : state_check -> state_check
    val mk_verifier : state_check -> continuation
    val combine_state_checks : state_check list -> state_check
    val lift_subst_check : Asl_subst.check -> state_check
  end

module Asl_unifier : AslUnifierSig =
  struct
    type state = Asl_subst.t * TagPairs.t
    let empty_state = Asl_subst.empty, TagPairs.empty

    type continuation = state -> state option 
    let trivial_continuation state = Some state
  

    type 'a t = 
      ?sub_check:Asl_subst.check ->
        ?cont:continuation ->
          ?init_state:state -> 'a -> 'a ->
            state option
  
    let backtrack (u:'a t)
        ?(sub_check=Asl_subst.trivial_check) 
        ?(cont=trivial_continuation) ?(init_state=empty_state) x y =
      let res = ref [] in
      let valid state' =
        match cont state' with
        | None -> None
        | Some state'' -> res := state'' :: !res ; None in
      let _ = u ~sub_check ~cont:valid ~init_state x y in 
      !res
  
    type state_check = state -> bool
    let mk_assert_check c state =
      let v = (c state) in 
      assert (v); v

    let mk_verifier check state =
      Option.mk (check state) state
  
    let rec combine_state_checks cs state =
      Blist.for_all (fun f -> f state) cs 
    
    let lift_subst_check c (theta, _) = Map.for_all (c theta) theta
      
    let basic_lhs_down_verifier = 
      mk_verifier (lift_subst_check Asl_subst.basic_lhs_down_check)
  end

(* sub_check needs to be performed for the Var right before being added to the Map. *)
exception Failed_Unification;;
let rec unify_trm sub_check theta t t' =
  let subset m m' =
    let mem (k, v) s =
      Blist.for_all (fun (k', v') -> if (equal k k') then (equal v v') else true) s in
    let xs = Map.to_list m in
    let ys = Map.to_list m' in
  Blist.for_all (Fun.swap mem ys) xs in
  let fail () = raise Failed_Unification in
    if equal t t' then Map.empty
    else match (t, t') with
    | (_, Var v) ->
      if (sub_check theta t t' && (not (Map.mem t' theta) || equal (Map.find t' theta) t )) then Map.add t' t theta else fail ()
    | (Mult(i, e), Mult(i', e')) -> if i=i' then unify_trm sub_check theta e e' else fail ()
    | (Add(e1, e2), Add(e1', e2')) -> let (map1, map2) = (unify_trm sub_check theta e1 e1', unify_trm sub_check theta e2 e2') in
      if subset map1 map2 then Map.union map1 map2 else fail ()
    | _ -> fail ()

(* NB: t and t' are inverted compared to the sl_term version *)
let unify 
    ?(sub_check=Asl_subst.trivial_check)
    ?(cont=Asl_unifier.trivial_continuation) ?(init_state=Asl_unifier.empty_state) t t' =
  let (theta, rest) = init_state in
  let res = 
    if equal t t' && is_var t && not (Map.mem t theta) then Some init_state
    else if Map.mem t' theta then
      Option.mk (equal (Map.find t' theta) t) init_state 
    else 
      try
        (Some (unify_trm sub_check theta t t', rest))
      with Failed_Unification -> None;
  in Option.bind cont res
