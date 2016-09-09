module Make(T: sig type t val equal : t -> t -> bool end) =
  struct
    let rec fixpoint f x = Lib.fixpoint T.equal f x
  end
