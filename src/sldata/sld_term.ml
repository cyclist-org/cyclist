open Lib
open Util
open Symbols
open MParser

let map = ref Int.Map.empty 
let inv_map = ref Strng.Map.empty
let max_var = ref 0
let min_var = ref 0

let get_limit exist = if exist then !min_var else !max_var
let get_diff exist = if exist then (-1) else 1
         
let present v = Int.Map.mem v !map
let name_present n = Strng.Map.mem n !inv_map 
        
let to_string v = Int.Map.find v !map  
let get_idx n = Strng.Map.find n !inv_map  
        
let is_var t = t<>0
let is_exist_var v = (is_var v) && v<0
let is_univ_var v = (is_var v) && v>0 
let is_valid_var v exist = 
  exist && is_exist_var v || not exist && is_univ_var v
                
let is_exist_name n = n.[(String.length n)-1] = '\''
let is_univ_name n = not (is_exist_name n)
let is_valid_name v exist = 
  exist && is_exist_name v || not exist && is_univ_name v
      
let set v name = 
  assert (not (present v) && not (name_present name)) ;
  assert 
    (is_exist_var v && is_exist_name name || 
     is_univ_var v && is_univ_name name);
  map := Int.Map.add v name !map ;
  inv_map := Strng.Map.add name v !inv_map ;
  max_var := max !max_var v ;
  min_var := min !min_var v  

let mk_var name exist =
  assert (is_valid_name name exist);
  if name_present name then
    let v = get_idx name in assert (is_valid_var v exist) ; v
  else
    let v = (get_diff exist) + (get_limit exist) in 
    set v name ; v

let mk_exist_var name = mk_var name true
let mk_univ_var name = mk_var name false
              
let fresh_varname exist =
  let suffix = if exist then "'" else "" in
  let idx = ref 0 in
  let letter = ref 'a' in
  let gen_name () = 
    (string_of_char !letter) ^ 
    (if !idx = 0 then "" else string_of_int !idx) ^ 
    suffix in
  let name = ref (gen_name ()) in
  while name_present !name && !letter < 'z' do
    letter := char_of_int (1 + (int_of_char !letter)) ;
    name := gen_name () 
  done ;
  if not (name_present !name) then !name else
  begin
    letter := if exist then 'v' else 'u';
    idx := 1;
    name := gen_name () ;
    while name_present !name do
      incr idx ; 
      name := gen_name () 
    done ;
    assert (not (name_present !name)) ; 
    !name
  end
   
  
        
let fresh_var s exist =
  let d = get_diff exist in
  let limit = abs (get_limit exist) in
  let i = ref d in
  let found = ref false in
  while abs (!i) <= limit && not !found do
    if Int.Set.mem !i s then
      i := !i + d
    else
      found := true
  done ;
  if !found then !i else
    mk_var (fresh_varname exist) exist 

let fresh_evar s = fresh_var s true    
let fresh_uvar s = fresh_var s false
  
let rec fresh_vars s i exist = match i with 
  | 0 -> [] 
  | n -> 
    let v = fresh_var s exist in
    v::(fresh_vars (Int.Set.add v s) (n-1) exist)

let fresh_evars s i = fresh_vars s i true
let fresh_uvars s i = fresh_vars s i false

open MParser
let parse st = 
  (parse_ident >>= (fun name ->
    return (mk_var name (is_exist_name name))) <?> "variable identifier") st



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
      <|> parse 
      <?> "Sld_term") st
    let of_string s =
      handle_reply (MParser.parse_string parse s ())

    let is_nil v = v = nil
    let is_var v = not (is_nil v) 
    
    let is_exist_name n = n.[(String.length n)-1] = '\''
    let is_univ_name n = not (is_exist_name n)
    
    let is_exist_var : t -> bool = is_exist_var
    let is_univ_var : t -> bool = is_univ_var
    let mk_univ_var : string -> t = mk_univ_var
    let mk_exist_var : string -> t = mk_exist_var 

  end


include Trm

module Set = Util.MakeListSet(Trm) 
module Map = Util.MakeMap(Trm)


(* FIXME *)
(* conversion from integer sets to term sets should be a runtime noop *)
let fresh_evar s = fresh_evar (Int.Set.of_list (Set.to_list s))
let fresh_uvar s = fresh_uvar (Int.Set.of_list (Set.to_list s))
let fresh_evars s n = fresh_evars (Int.Set.of_list (Set.to_list s)) n
let fresh_uvars s n = fresh_uvars (Int.Set.of_list (Set.to_list s)) n

let filter_vars s = Set.filter is_var s

type substitution = t Map.t

let empty_subst : substitution = Map.empty
let singleton_subst x y = Map.add x y empty_subst
let subst theta v =
  if not (is_nil v) && Map.mem v theta then Map.find v theta else v
(* above is significantly faster than exception handling *)
let pp_subst = Map.pp pp

type unifier_state = substitution * TagPairs.t

let empty_state = empty_subst, TagPairs.empty

type subst_check = t Map.t -> Map.key -> t -> bool

let trivial_sub_check _ _ _ = true

type continuation = unifier_state -> unifier_state option 

let trivial_continuation state = Some state

type 'a unifier = 
  ?sub_check:subst_check ->
    ?cont:continuation ->
      ?init_state:unifier_state -> 'a -> 'a ->
        unifier_state option

let trm_unify 
    ?(sub_check=trivial_sub_check)
    ?(cont=trivial_continuation) ?(init_state=empty_state) t t' =
  let (theta, rest) = init_state in
  let res = 
    if Map.mem t theta then
      Option.mk (equal (Map.find t theta) t') init_state 
    else if is_nil t then
      Option.mk (is_nil t') init_state
    else if (sub_check theta t t') then
      Some (Map.add t t' theta, rest)
    else
      None in
  Option.bind cont res
    
let backtrack (u:'a unifier)
    ?(sub_check=trivial_sub_check) 
    ?(cont=trivial_continuation) ?(init_state=empty_state) x y =
  let res = ref [] in
  let valid state' =
    match cont state' with
    | None -> None
    | Some state'' -> res := state'' :: !res ; None in
  let _ = u ~sub_check ~cont:valid ~init_state x y in !res;;


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
      
type state_check = unifier_state -> bool

let rec combine_subst_checks cs theta x y =
  match cs with
  | [] -> true
  | c::rest -> (c theta x y) && (combine_subst_checks rest theta x y)

let rec combine_state_checks cs state =
  match cs with
  | [] -> true
  | c::rest -> (c state) && (combine_state_checks rest state)

let lift_subst_check c (theta, _) = Map.for_all (c theta) theta

let mk_assert_check c state =
  let v = (c state) in 
  assert (v); v

let mk_verifier check state =
  Option.mk (check state) state

let basic_lhs_down_check theta t t' =
  is_univ_var t || 
  is_univ_var t' ||
  is_exist_var t && is_nil t' ||
  is_exist_var t && is_exist_var t' &&
    Map.for_all (fun _ t'' -> not (equal t' t'')) theta
  
let basic_lhs_down_verifier = mk_verifier (lift_subst_check basic_lhs_down_check)

let avoids_replacing_check ?(inverse=false) vars =
  fun _ -> Fun.direct inverse (fun x y -> equal x y || not (Set.mem x vars)) 

module FList =
  struct
    include Util.MakeFList(Trm)
    let rec unify ?(sub_check=trivial_sub_check) ?(cont=trivial_continuation) 
        ?(init_state=empty_state) args args' =
      match (args, args') with
      | ([], []) -> cont init_state
      | (_, []) | ([], _) -> None
      | (x::xs, y::ys) ->
        trm_unify ~sub_check ~cont:(fun state' -> unify ~sub_check ~cont ~init_state:state' xs ys) ~init_state x y
    
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
