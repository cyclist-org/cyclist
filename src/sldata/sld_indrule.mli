(** Inductive rule type consisting of a symbolic heap and a predicate head. *)

include Util.BasicType

val mk : Sld_heap.t -> Sld_pred.t -> t
val dest: t -> Sld_heap.t * Sld_pred.t

val vars : t -> Sld_term.Set.t

val predsym : t -> Sld_predsym.t
val arity : t -> int
val formals : t -> Sld_term.t list
val body : t -> Sld_heap.t

val freshen : Sld_term.Set.t -> t -> t
(** Replace all variables in rule such that they are disjoint with the set 
    provided. *)

val subst : Sld_term.substitution -> t -> t
val parse : (t, 'a) MParser.t

val unfold : 
  Sld_term.Set.t -> Sld_heap.t -> Sld_tpred.t -> t -> (Sld_heap.t * Util.TagPairs.t)
(** [unfold vars h p r] does the following:
- Removes the (tagged) occurrence of [p] in [h].
- Inlines the body of [r] in [h], taking care to pick existential variables
  outside [vars].
- Returns the tag pairs representing progressing tag pairs. I.e. if [p] is equal
  to [(tag, pred)] then new tags will be introduced for all predicates in the
  body of [r] (disjoint to those in [h]), and for each such new tag [tag'] the
  pair [(tag,tag')] will be returned.
*)  
    
val fold : t -> Sld_heap.t -> (Sld_term.substitution * Sld_heap.t) list
(** [fold r h] returns a list of pairs of substitutions over the formal parameters of 
    the rule [r] such that its body, when one of these substitutions is applied,
    becomes a subformula of [h]; and the result of removing that subformula from
    [h]. NB the pure part is removed on a best effort basis. *)
