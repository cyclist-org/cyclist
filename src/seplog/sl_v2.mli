module type CommonInterface = 
  sig
    include Utilsigs.BasicType
    val equal_upto_tags : t -> t -> bool
    val parse : (t, 'a) MParser.parser
    val of_string : string -> t
    
    val mk_true : unit -> t
    val mk_emp : unit -> t
    val mk_eq : Sl_tpair.t -> t
    val mk_deq : Sl_tpair.t -> t
    val mk_pto : Sl_pto.t -> t 
    val mk_pred  : Sl_tpred.t -> t
  
    val is_true : t -> bool
    val is_emp : t -> bool
    val is_eq : t -> bool
    val is_deq : t -> bool
    val is_pto : t -> bool  
    val is_pred  : t -> bool
  
    val dest_true : t -> unit
    val dest_emp : t -> unit
    val dest_eq : t -> Sl_tpair.t
    val dest_deq : t -> Sl_tpair.t
    val dest_pto : t -> Sl_pto.t
    val dest_pred  : t -> Sl_tpred.t
    
    val terms : t -> Sl_term.Set.t
    val free_vars : t -> Sl_term.Set.t
    val subst : Sl_subst.t -> t -> t
    val tags : t -> Tags.t
    val tag_pairs : t -> Tagpairs.t
  end

module Atom : 
sig
  include CommonInterface
  
  val unify : t Sl_unifier.t
end

module SymHeap :
sig
  include CommonInterface

  val bound_vars : t -> Sl_term.Set.t
  val is_atom : t -> bool
  val dest_atom : t -> Atom.t
  val of_atom : Atom.t -> t
  val fold : (Atom.t -> 'a -> 'a) -> t -> 'a -> 'a
  val exists : (Atom.t -> bool) -> t -> bool
  val find : (Atom.t -> bool) -> t -> t option
  
  val equates : t -> Sl_term.t -> Sl_term.t -> bool
  val disequates : t -> Sl_term.t -> Sl_term.t -> bool
  val find_lval : Sl_term.t -> t -> Sl_pto.t option
  val idents : t -> Sl_predsym.MSet.t
  val inconsistent : t -> bool
  val memory_consuming : t -> bool
  val constructively_valued : t -> bool
  
  val unify: t Sl_unifier.t
  
(*   val to_melt : t -> Latex.t                           *)
(*   val of_dsh : DSH.t -> t                              *)  
(*   val unify : t Sl_unifier.t                           *)
end


(* module Formula :                                       *)
(* sig                                                    *)
(*   include Utilsigs.BasicType                               *)
(*   val is_dsh : t -> bool                               *)
(* end                                                    *)

(* module DSH :                                           *)
(* sig                                                    *)
(*   val is_sh : t -> bool                                *)
(*   val of_formula : Formula.t -> t                      *)
(* end                                                    *)

