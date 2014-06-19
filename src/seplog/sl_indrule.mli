include Util.BasicType
val mk : Symheap.Heap.t -> Symheap.ind_identifier * Symheap.Term.t list -> t
val dest: t -> Symheap.Heap.t * (Symheap.ind_identifier * Symheap.Term.t list)
val vars : t -> Symheap.Term.Set.t
val freshen : Symheap.Term.Set.t -> t -> t
val subst : Symheap.Term.substitution -> t -> t
val parse : (t, 'a) MParser.t


