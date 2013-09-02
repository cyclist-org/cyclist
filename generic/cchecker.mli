type abstract_proof_node = 
  | LeafNode of Util.Tags.t 
  | InternalNode of Util.Tags.t * int list * Util.TagPairs.t list * Util.TagPairs.t list 
type abstract_proof_t = abstract_proof_node Util.Int.Map.t

val check_proof : abstract_proof_t -> bool

val compose_tag_pairs : Util.TagPairs.t -> Util.TagPairs.t -> Util.TagPairs.t

val pp : Format.formatter -> abstract_proof_t -> unit
