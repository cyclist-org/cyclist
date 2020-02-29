(** This module contains functions to answer various abduction questions for
    use in program verification. *)

open Generic

val max_depth : int
(** Returns the maximum depth for the underlying proof search *)

val set_depth : int -> unit
(** Set the maximum depth for the underlying proof search *)

val set_defs : Sl_defs.t -> unit
(** Specify the set of inductive definitions available for the proof search *)

val abd_substs :
     ?used_tags:Tags.t
  -> ?init_state:Sl_unify.Unidirectional.state
  -> ?update_check:Sl_unify.Unidirectional.update_check
  -> ?verify:Sl_unify.Unidirectional.state_check
  -> ?allow_frame:bool
  -> Sl_form.t
  -> Sl_form.t
  -> ((Sl_form.t * Sl_form.t) * Sl_unify.Unidirectional.state list) option
(** [abd_substs f f'] returns an option result that consists of a pair of 
    formulas (g, g') and a list of substitutions [substs] such that:
      - [f] entails [g]
      - [g'] entails [f']
      - for all substitutions [theta] in [substs], [theta] applied to [g'] is a
        subformula of [g], and if the optional argument [allow_frame=true] is
        set to false, then also the spatial parts are equal.
*)

val abd_bi_substs :
     ?init_state:Sl_unify.Bidirectional.state
  -> ?update_check:Sl_unify.Bidirectional.update_check
  -> ?verify:Sl_unify.Bidirectional.state_check
  -> ?allow_frame:bool
  -> Sl_form.t
  -> Sl_form.t
  -> ((Sl_form.t * Sl_form.t) * Sl_unify.Bidirectional.state list) option
