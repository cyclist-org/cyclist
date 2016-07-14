module Make(T : Util.BasicType) :
sig
  val lru_cache : (T.t -> 'a) -> int -> (T.t -> 'a)
  (** [lru_cache f n] memoises the [f] function, using an LRU cache
      (implemented as a hashtable) of up to [n] entries. *)
end
(** Implementation of an LRU cache for memoising functions whose arguments
    can be compared for equality and can be hashed.  The code is a modified 
    version of that in "batteries included". *)