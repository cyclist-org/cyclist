include Util.BasicType with type t = Sl_term.Set.t * Sl_heap.t

module Set : Util.OrderedContainer with type elt = t

val gen_all_pairs : 
  ?only_first:bool -> Sl_defs.t -> Set.t Sl_indrule.Map.t
(** When the optional argument [~only_first=false] is set to [true] then 
    the computation of the fixpoint stop as soon as a base pair has been 
    computed for any of the rules defining the first predicate in the definition
    set. *)

val satisfiable : ?only_first:bool -> ?output:bool -> Sl_defs.t -> bool
(** See [gen_all_pairs] regarding [~only_first]. 

    When the optional argument [~output=false] is set to [true] then the
    result is reported on stdout.  FIXME this should be moved to the caller. *) 
    
val form_sat : Sl_defs.t -> Sl_form.t -> bool
(** Decide the satisfiability of a formula with predicates out of the definitions
    provided, using the base pairs algorithm. *)
    
val pairs_of_form : Sl_defs.t -> Sl_form.t -> Set.t