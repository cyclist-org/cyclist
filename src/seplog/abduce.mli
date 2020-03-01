(** This module contains functions to answer various abduction questions for
    use in program verification. *)

open Generic

val max_depth : int
(** Returns the maximum depth for the underlying proof search *)

val set_depth : int -> unit
(** Set the maximum depth for the underlying proof search *)

val set_defs : Defs.t -> unit
(** Specify the set of inductive definitions available for the proof search *)

val abd_substs :
     ?used_tags:Tags.t
  -> ?init_state:Unify.Unidirectional.state
  -> ?update_check:Unify.Unidirectional.update_check
  -> ?verify:Unify.Unidirectional.state_check
  -> ?allow_frame:bool
  -> Form.t
  -> Form.t
  -> ((Form.t * Form.t) * Unify.Unidirectional.state list) option
(** [abd_substs f f'] returns an option result that consists of a pair of 
    formulas (g, g') and a list of substitutions [substs] such that:
      - [f] entails [g]
      - [g'] entails [f']
      - for all substitutions [theta] in [substs], [theta] applied to [g'] is a
        subformula of [g], and if the optional argument [allow_frame=true] is
        set to false, then also the spatial parts are equal.
*)

val abd_bi_substs :
     ?init_state:Unify.Bidirectional.state
  -> ?update_check:Unify.Bidirectional.update_check
  -> ?verify:Unify.Bidirectional.state_check
  -> ?allow_frame:bool
  -> Form.t
  -> Form.t
  -> ((Form.t * Form.t) * Unify.Bidirectional.state list) option
