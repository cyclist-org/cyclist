(** Module providing SL term substitution-related functionality. *)

include
  Lib.VarManager.SubstSig
  with type t = Term.t Term.Map.t
  with type var = Term.t
  with type var_container = Term.Set.t
