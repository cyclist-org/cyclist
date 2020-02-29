module Make (T : sig
  type t

  val equal : t -> t -> bool
end) =
struct
  let fixpoint f x = Misc.fixpoint T.equal f x
end
