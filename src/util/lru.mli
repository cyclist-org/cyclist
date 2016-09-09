(** Implementation of an LRU cache for memoising functions. *)
    
module Make(T : Utilsigs.BasicType) :
sig
  val lru_cache : (T.t -> 'a) -> int -> (T.t -> 'a)
  (** [lru_cache f n] memoises the non-recursive function [f], using an LRU cache
      (implemented as a hashtable) of up to [n] entries. *)
  
  val lru_cache_rec : ((T.t -> 'a) -> T.t -> 'a) -> int -> (T.t -> 'a)
  (** [lru_cache f n] memoises the recursive function [f], using an LRU cache
      (implemented as a hashtable) of up to [n] entries. [f] must
      have untied the recursive knot and accept its own continuation
      for recursion. *)
end
(** Implementation of an LRU cache for memoising functions whose arguments
    can be compared for equality and can be hashed.  The code is a modified 
    version of that in "batteries included". *)