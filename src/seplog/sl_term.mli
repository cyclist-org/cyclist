include Util.BasicType
module Set : Util.OrderedContainer with type elt = t
module Map : Util.OrderedMap with type key = t
module FList : Util.BasicType with type t = t list

val nil : t

val is_nil : t -> bool
val is_var : t -> bool
val is_exist_var : t -> bool
val is_univ_var : t -> bool

val mk_exist_var : string -> t
val mk_univ_var : string -> t
val filter_vars : Set.t -> Set.t

val fresh_uvar : Set.t -> t
val fresh_uvars : Set.t -> int -> t list
val fresh_evar : Set.t -> t
val fresh_evars : Set.t -> int -> t list

type substitution = t Map.t

val empty_subst : substitution
val singleton_subst : t -> t -> substitution
val subst : substitution -> t -> t
val avoid_theta : Set.t -> Set.t -> substitution
val unify_pairs: substitution -> (t * t) -> (t * t) -> substitution list 
val unify_list: substitution -> t list -> t list -> substitution option

val list_to_string : t list -> string
val to_melt : t -> Latex.t
val parse : (t, 'a) MParser.parser
