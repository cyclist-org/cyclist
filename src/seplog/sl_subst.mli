(** Module providing SL term substitution-related functionality. *)

include
  Lib.VarManager.SubstSig
  with type t = Sl_term.t Sl_term.Map.t
  with type var = Sl_term.t
  with type var_container = Sl_term.Set.t
