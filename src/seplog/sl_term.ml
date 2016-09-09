open Lib
open Util
open Symbols
open MParser

module Trm :
  sig
    include Util.BasicType
    module Set : Util.OrderedContainer with type elt = t
    module Map : Util.OrderedMap with type key = t
    
    val to_melt : t -> Latex.t
    val parse : (t, 'a) MParser.parser
    
    val nil : t
    
    val is_nil : t -> bool
    val is_var : t -> bool
    val is_exist_var : t -> bool
    val is_free_var : t -> bool
    
    val fresh_fvar : Set.t -> t
    val fresh_fvars : Set.t -> int -> t list
    val fresh_evar : Set.t -> t
    val fresh_evars : Set.t -> int -> t list
  end
  =
  struct
    module T = Util.Strng
    module H = Hashcons.Make(T)

    module T' = 
      struct
        type t = T.t Hashcons.hash_consed
        let to_string s = s.Hashcons.node
        let pp fmt s = T.pp fmt (to_string s)
        let compare s s' = Pervasives.compare s.Hashcons.tag s'.Hashcons.tag
        let equal s s' = s==s'
        let hash s = s.Hashcons.hkey
      end
    include T'
    
    module Set = Treeset.Make(T')
    module Map = Treemap.Make(T')
    
    let termtbl = H.create 997
    let mk s = H.hashcons termtbl s
    
    let parse st = (parse_ident |>> mk) st
    let to_melt s = Latex.mathit (Latex.text (to_string s))
    
    let nil = mk "nil"
    let is_nil n = equal n nil
    let is_var n = not (is_nil n)
    let is_exist_name s =
      let l = String.length s in l > 1 && s.[l-1] = '\''
    let is_exist_var n = is_var n && is_exist_name n.Hashcons.node
    let is_free_var n = is_var n && not (is_exist_name n.Hashcons.node)

    let search_vars =
      let explode s = 
        let rec loop p acc = 
          if p < 0 then acc else loop (p-1) (String.sub s p 1::acc) in
        loop ((String.length s) - 1) [] in
      let letters = Blist.rev (explode "xyzwabcdefghijklmnopqrstuv") in
      let mk_var free lvl acc n = 
        let n = if lvl=0 then n else (n ^ "_" ^ (string_of_int lvl)) in
        (if free then (mk n) else (mk (n ^ "'"))) :: acc in 
      let mk_seg free lvl = 
         Blist.fold_left (mk_var free lvl) [] letters in
      let rec _search s acc vs n = match vs,n with
        | _, 0 -> Some (Blist.rev acc)
        | [],_ -> None
        | v::vs,n ->
          begin 
            let (acc', n') = 
              if Set.mem v s then (acc, n) else (v::acc, n-1) in
            _search s acc' vs n' 
          end in
      let curr_lvl = ref 1 in
      let _fvars, _evars = ref (mk_seg true 0), ref (mk_seg false 0) in
      let rec search s free n = 
        match _search s [] (if free then !_fvars else !_evars) n with
        | Some vs -> vs
        | None ->
          _fvars := !_fvars @ mk_seg true !curr_lvl;
          _evars := !_evars @ mk_seg false !curr_lvl;
          incr curr_lvl ;
          search s free n in 
      search
  
    let fresh_fvars s n = search_vars s true n
    let fresh_evars s n = search_vars s false n
    let fresh_fvar s = Blist.hd (fresh_fvars s 1)
    let fresh_evar s = Blist.hd (fresh_evars s 1)
  
  end

(* module B :                                                                               *)
(*   sig                                                                                    *)
(*     include Util.BasicType                                                               *)
(*     module Set : Util.OrderedContainer with type elt = t                                 *)
(*     module Map : Util.OrderedMap with type key = t                                       *)
    
(*     val to_melt : t -> Latex.t                                                           *)
(*     val parse : (t, 'a) MParser.parser                                                   *)
    
(*     val nil : t                                                                          *)
    
(*     val is_nil : t -> bool                                                               *)
(*     val is_var : t -> bool                                                               *)
(*     val is_exist_var : t -> bool                                                         *)
(*     val is_free_var : t -> bool                                                          *)
    
(*     val fresh_fvar : Set.t -> t                                                          *)
(*     val fresh_fvars : Set.t -> int -> t list                                             *)
(*     val fresh_evar : Set.t -> t                                                          *)
(*     val fresh_evars : Set.t -> int -> t list                                             *)
(*   end                                                                                    *)
(*   =                                                                                      *)
(*   struct                                                                                 *)
    
(*     let map = Int.Hashmap.create 997                                                     *)
(*     let inv_map = Strng.Hashmap.create 997                                               *)
(*     let max_var = ref 0                                                                  *)
(*     let min_var = ref 0                                                                  *)
    
(*     let get_limit exist = if exist then !min_var else !max_var                           *)
(*     let get_diff exist = if exist then (-1) else 1                                       *)
             
(*     let present v = Int.Hashmap.mem map v                                                *)
(*     let name_present n = Strng.Hashmap.mem inv_map n                                     *)
            
(*     let to_string v = Int.Hashmap.find map v                                             *)
(*     let get_idx n = Strng.Hashmap.find inv_map n                                         *)
            
(*     let is_var t = t<>0                                                                  *)
(*     let is_exist_var v = (is_var v) && v<0                                               *)
(*     let is_free_var v = (is_var v) && v>0                                                *)
(*     let is_valid_var v exist =                                                           *)
(*       exist && is_exist_var v || not exist && is_free_var v                              *)
                    
(*     let is_exist_name n = n.[(String.length n)-1] = '\''                                 *)
(*     let is_univ_name n = not (is_exist_name n)                                           *)
(*     let is_valid_name v exist =                                                          *)
(*       exist && is_exist_name v || not exist && is_univ_name v                            *)
          
(*     let mk_var name exist =                                                              *)
(*       assert (is_valid_name name exist);                                                 *)
(*       if name_present name then                                                          *)
(*         let v = get_idx name in assert (is_valid_var v exist) ; v                        *)
(*       else                                                                               *)
(*         let v = (get_diff exist) + (get_limit exist) in                                  *)
(*         assert (not (present v) && not (name_present name)) ;                            *)
(*         assert                                                                           *)
(*           (is_exist_var v && is_exist_name name ||                                       *)
(*            is_free_var v && is_univ_name name);                                          *)
(*         Int.Hashmap.add map v name ;                                                     *)
(*         Strng.Hashmap.add inv_map name v ;                                               *)
(*         max_var := max !max_var v ;                                                      *)
(*         min_var := min !min_var v ;                                                      *)
(*         v                                                                                *)
    
(*     module Trm =                                                                         *)
(*       struct                                                                             *)
        
(*         type t = int                                                                     *)
(*         let nil = 0                                                                      *)
    
(*         let equal = Int.equal                                                            *)
(*         let compare = Int.compare                                                        *)
(*         let hash = Int.hash                                                              *)
        
(*         let to_string v =                                                                *)
(*           if equal v nil then keyw_nil.str else to_string v                              *)
(*         let pp fmt trm =                                                                 *)
(*           Format.fprintf fmt "@[%s@]" (to_string trm)                                    *)
(*         let to_melt v =                                                                  *)
(*           ltx_mk_math                                                                    *)
(*             (Latex.mathit                                                                *)
(*               (if v = nil then keyw_nil.melt else Latex.text (to_string v)))             *)
            
(*         let parse st =                                                                   *)
(*           (   attempt (parse_symb keyw_nil >>$ 0 <?> "nil")                              *)
(*           <|> (parse_ident >>= (fun name -> return (mk_var name (is_exist_name name))))  *)
(*           <?> "Sl_term") st                                                              *)
(*       end                                                                                *)
    
    
(*     include Trm                                                                          *)
    
(*     module Set = Util.Listset.Make(Trm)                                                   *)
(*     module Map = Treemap.Make(Trm)                                                       *)
    
(*     let is_nil v = v = nil                                                               *)
(*     let is_var v = not (is_nil v)                                                        *)
    
(*     let bound s exist =                                                                  *)
(*       if Set.is_empty s then                                                             *)
(*         (if exist then -1 else 1)                                                        *)
(*       else                                                                               *)
(*         (if exist then (min (-1) (Set.min_elt s)) else (max 1 (Set.max_elt s)))          *)
                  
(*     let fresh_varname exist =                                                            *)
(*       let suffix = if exist then "'" else "" in                                          *)
(*       let idx = ref 0 in                                                                 *)
(*       let letter = ref 'a' in                                                            *)
(*       let gen_name () =                                                                  *)
(*         (string_of_char !letter) ^                                                       *)
(*         (if !idx = 0 then "" else string_of_int !idx) ^                                  *)
(*         suffix in                                                                        *)
(*       let name = ref (gen_name ()) in                                                    *)
(*       while name_present !name && !letter < 'z' do                                       *)
(*         letter := char_of_int (1 + (int_of_char !letter)) ;                              *)
(*         name := gen_name ()                                                              *)
(*       done ;                                                                             *)
(*       if not (name_present !name) then !name else                                        *)
(*       begin                                                                              *)
(*         letter := if exist then 'v' else 'u';                                            *)
(*         idx := 1;                                                                        *)
(*         name := gen_name () ;                                                            *)
(*         while name_present !name do                                                      *)
(*           incr idx ;                                                                     *)
(*           name := gen_name ()                                                            *)
(*         done ;                                                                           *)
(*         assert (not (name_present !name)) ;                                              *)
(*         !name                                                                            *)
(*       end                                                                                *)
    
(*     let fresh_var m exist =                                                              *)
(*       let limit = abs (get_limit exist) in                                               *)
(*       let i = m + (get_diff exist) in                                                    *)
(*       if abs i <= limit then i else mk_var (fresh_varname exist) exist                   *)
    
(*     let fresh_evar s = fresh_var (bound s true) true                                     *)
(*     let fresh_fvar s = fresh_var (bound s false) false                                   *)
      
(*     let rec fresh_vars m i exist = match i with                                          *)
(*       | 0 -> []                                                                          *)
(*       | n ->                                                                             *)
(*         let v = fresh_var m exist in                                                     *)
(*         v::(fresh_vars (m + (get_diff exist)) (n-1) exist)                               *)
    
(*     let fresh_evars s n = fresh_vars (bound s true) n true                               *)
(*     let fresh_fvars s n = fresh_vars (bound s false) n false                             *)
    
(*   end                                                                                    *)

include Trm

let of_string s =
  handle_reply (MParser.parse_string parse s ())

let filter_vars s = Set.filter is_var s
 
type term_t = t

module type SubstSig =
sig
  type t = term_t Map.t
  type check = t -> term_t -> term_t -> bool
  val empty : t
  val singleton : term_t -> term_t -> t
  val of_list : (term_t * term_t) list -> t
  val avoid : Set.t -> Set.t -> t
  val pp : Format.formatter -> t -> unit
  val trivial_check : check
  val basic_lhs_down_check : check
  val avoids_replacing_check : ?inverse:bool -> Set.t -> check
  val combine_checks : check list -> check
end

module Subst =
  struct
    type t = term_t Map.t
    type check = t -> term_t -> term_t -> bool

    let empty = Map.empty
    let singleton x y = Map.add x y empty
    let of_list = Map.of_list
    let avoid vars subvars =
      let allvars = Set.union vars subvars in
      let (exist_vars, free_vars) =
        Pair.map Set.elements (Set.partition is_exist_var subvars) in
      let fresh_f_vars = fresh_fvars allvars (Blist.length free_vars) in
      let fresh_e_vars = fresh_evars allvars (Blist.length exist_vars) in
      Map.of_list
        (Blist.append 
          (Blist.combine free_vars fresh_f_vars)
          (Blist.combine exist_vars fresh_e_vars))
    
    
    
    let pp = Map.pp pp
    let trivial_check _ _ _ = true
    let basic_lhs_down_check theta t t' =
      is_free_var t || 
      is_free_var t' ||
      is_exist_var t && is_nil t' ||
      is_exist_var t && is_exist_var t' &&
        Map.for_all (fun _ t'' -> not (equal t' t'')) theta
    let avoids_replacing_check ?(inverse=false) vars =
      fun _ -> Fun.direct inverse (fun x y -> equal x y || not (Set.mem x vars)) 
    let rec combine_checks cs theta x y =
      Blist.for_all (fun f -> f theta x y) cs
  end

let subst theta v =
  if is_var v && Map.mem v theta then Map.find v theta else v
(* above is significantly faster than exception handling *)

module type UnifierSig =
  sig
    type state = Subst.t * Util.TagPairs.t
    val empty_state : state
    val pp_state : Format.formatter -> state -> unit
    
    type continuation = state -> state option 
    val trivial_continuation : continuation
    val basic_lhs_down_verifier : continuation
    type 'a t = 
      ?sub_check:Subst.check ->
        ?cont:continuation ->
          ?init_state:state -> 'a -> 'a ->
            state option
  
    val backtrack : 
      'a t -> 
        ?sub_check:Subst.check -> ?cont:continuation -> ?init_state:state -> 
          'a -> 'a -> state list
    type state_check = state -> bool
    val mk_assert_check : state_check -> state_check
    val mk_verifier : state_check -> continuation
    val combine_state_checks : state_check list -> state_check
    val lift_subst_check : Subst.check -> state_check
  end

module Unifier =
  struct
    type state = Subst.t * TagPairs.t
    let empty_state = Subst.empty, TagPairs.empty
    let pp_state fmt (theta, tagpairs) = 
      Format.fprintf fmt "@[(%a,@ %a)@]" Subst.pp theta TagPairs.pp tagpairs
    
    type continuation = state -> state option 
    let trivial_continuation state = Some state
  

    type 'a t = 
      ?sub_check:Subst.check ->
        ?cont:continuation ->
          ?init_state:state -> 'a -> 'a ->
            state option
  
    let backtrack (u:'a t)
        ?(sub_check=Subst.trivial_check) 
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
      mk_verifier (lift_subst_check Subst.basic_lhs_down_check)
  end

let trm_unify 
    ?(sub_check=Subst.trivial_check)
    ?(cont=Unifier.trivial_continuation) ?(init_state=Unifier.empty_state) t t' =
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
    
module FList =
  struct
    include Flist.Make(Trm)
    let rec unify ?(sub_check=Subst.trivial_check) ?(cont=Unifier.trivial_continuation) 
        ?(init_state=Unifier.empty_state) args args' =
      match (args, args') with
      | ([], []) -> cont init_state
      | (_, []) | ([], _) -> None
      | (x::xs, y::ys) ->
        trm_unify ~sub_check ~cont:(fun state' -> unify ~sub_check ~cont ~init_state:state' xs ys) ~init_state x y
    
    let subst theta xs = Blist.map (fun x -> subst theta x) xs
    
    let to_string_sep sep xs = Blist.to_string sep Trm.to_string xs
    
    let terms xs = Set.of_list xs 
        
    let vars xs = filter_vars (terms xs)
  end 

let unify = trm_unify