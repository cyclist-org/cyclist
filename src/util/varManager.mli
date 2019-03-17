(** An abstract datatype for managing variables. *)
module type I = sig
  type var

  type var_container

  val anonymous : var
  (** An "unidentified" variable *)

  val is_anonymous : var Fun.predicate
  (** [is_unnamed v] returns [true] if and only if [v] is anonymous *)

  val mk : string -> var
  (** [mk_var n exist] returns a variable with name [n]; if [exist] is [true] then 
        the returned variable will be existential, otherwise it will be free. If the 
        VarManager already knows about a variable named [n], then this will be returned,
        otherwise it will generate a fresh variable. *)

  val is_exist_var : var Fun.predicate
  (** [is_exist_var v] returns [true] if and only if [v] is an existential variable. *)

  val is_free_var : var Fun.predicate
  (** [is_exist_var v] returns [true] if and only if [v] is a free variable. *)

  val fresh_evar : var_container -> var
  (** [fresh_evar s] returns an existential variable that is fresh for the set of variables [s]. *)

  val fresh_evars : var_container -> int -> var list
  (** [fresh_evars s n] returns [n] distinct existential variables that are all fresh for the set of variables [s]. *)

  val fresh_fvar : var_container -> var
  (** [fresh_uvar s] returns a free variable that is fresh for the set of variables [s]. *)

  val fresh_fvars : var_container -> int -> var list
  (** [fresh_uvars s n] returns [n] distinct free variables that are all fresh for the set of variables [s]. *)
end

module type SubstSig = sig
  (** Abstract type of substitutions *)
  type t

  (** Abstract type of variables substituted *)
  type var

  (** abstract type of containers of variables *)
  type var_container

  val empty : t
  (** The empty substitution, which has no effect when applied. *)

  val singleton : var -> var -> t
  (** Constructor for a substitution mapping one variable to a term. *)

  val of_list : (var * var) list -> t
  (** Make a substitution from a list of bindings *)

  val avoid : var_container -> var_container -> t
  (** [avoid vars subvars] 
        returns a substitution that takes all variables in [subvars] to  
        variables fresh in [vars U subvars], respecting existential   
        quantification / free variables. *)

  val pp : Format.formatter -> t -> unit
  (** Pretty printer. *)

  val to_string : t -> string
  (** Convert a substitution to a string *)

  val apply : t -> var -> var
  (** Apply a substitution to a variable *)

  val partition : t -> t * t
  (** [partition theta] will partition [theta] into ([theta_1], [theta_2])
        such that [theta_1] contains all and only the mappings in [theta] from 
        a free variable to either an anonymous variable or another free variable; 
        that is [theta_1] is the part of [theta] which is a proper
        (proof-theoretic) substitution. *)

  val strip : t -> t
  (** Removes all identity bindings from the substitution map *)

  val mk_free_subst : var_container -> var_container -> t
  (** [mk_free_subst avoid vs] produces a substitution of pairwise distinct
        free variables (fresh for all the variables in [avoid]) for the variables in [vs]. *)

  val mk_ex_subst : var_container -> var_container -> t
  (** [mk_ex_subst avoid vs] produces a substitution of pairwise distinct existentially
        quantified variables (fresh for all the variables in [avoid]) for the variables in [vs]. *)
end

module type S = sig
  (** Abstract type of variables *)
  module Var : sig
    include Utilsigs.BasicType

    include
      Containers.S
      with type Set.elt = t
      with type Map.key = t
      with type Hashmap.key = t
      with type Hashset.elt = t
      with type MSet.elt = t
      with type FList.t = t list

    val to_int : t -> Int.t
    (** Returns an integer representation *)
  end

  module Subst :
    SubstSig
    with type t = Var.t Var.Map.t
    with type var = Var.t
    with type var_container = Var.Set.t

  include I with type var = Var.t and type var_container = Var.Set.t

  val to_ints : Var.Set.t -> Int.Set.t
  (** Convenience method to return a set of integer representatives of a set of variables.
        This is equivalent to [Var.Set.map_to Int.Set.add Int.Set.empty Var.to_int] *)
end

type varname_class = FREE | BOUND | ANONYMOUS

val mk : int -> string -> (string -> varname_class) -> (module S)
(** [mk seed anon_str classify] creates a new variable manager module where:
      [seed] specifies a cyclic permutation of the alphabet, which is used 
        internally to create new variable names;
      [anon_str] specifies how to represent "anonymous" variables as a string;
      [classify varname] returns a value of type [varname_class] classifying [varname]. *)
