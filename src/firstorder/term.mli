open Lib
open Generic

include BasicType

include Containers.S
  with type Set.elt = t
  with type Map.key = t
  with type Hashmap.key = t
  with type Hashset.elt = t
  with type MSet.elt = t
  with type FList.t = t list


type substitution = t Map.t

val empty_subst : substitution
val singleton_subst : t -> t -> substitution
val subst_is_identity : substitution -> bool
val subst : substitution -> t -> t
val subst_list : substitution -> t list -> t list

val zero : t

val is_zero : t -> bool
val is_var : t -> bool
val is_exist_var : t -> bool
val is_free_var : t -> bool
val is_fun : t -> bool
val is_succ : t -> bool
val is_cons : t -> bool

val mk_const : int -> t
val mk_univ_var : string -> t
val mk_exist_var : string -> t
val mk_fun : string -> t list -> t

val dest_fun : t -> (string * t list)

val vars : t -> Set.t
val terms : t -> Set.t

val fresh_fvar : Set.t -> t
val fresh_fvars : Set.t -> int -> t list
val fresh_evar : Set.t -> t
val fresh_evars : Set.t -> int -> t list

val filter_vars : Set.t -> Set.t

val unify_list : substitution -> t list -> t list -> substitution option
val unify_pairs : substitution -> (t * t) -> (t * t) -> substitution list
val multi_unify_args : t list -> t list -> (substitution * (t * t) list) option

val parse : (t, 'a) MParser.t