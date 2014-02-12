module Make :
  functor (Seq : Cycprover.S) ->
    sig
      type sequent = Seq.t
      type t
      
      val get_seq : t -> Seq.t
      val get_par : t -> int
      val dest : t -> Seq.t * int
      val is_open : t -> bool
      val is_backlinkable : t -> bool
      val is_backlink : t -> bool
      val mk_open : Seq.t -> int -> t
      val mk_axiom : Seq.t -> int -> string -> t
      val mk_inf :
        Seq.t ->
        int ->
        int list ->
        Util.TagPairs.t list ->
        Util.TagPairs.t list -> string -> bool -> t
      val mk_back :
        Seq.t -> int -> int -> Util.TagPairs.t -> string -> t
      val mk_abd : Seq.t -> int -> int -> string -> t
      val to_abstract_node : t -> Cchecker.abstract_proof_node
      val pp :
        Format.formatter ->
        int -> t -> (Format.formatter -> int -> unit) -> unit
      val to_melt :
        bool -> int -> t -> (bool -> int -> Latex.t) -> Latex.t
      val is_closed_at_helper : bool -> t -> (int -> bool) -> bool
    end
