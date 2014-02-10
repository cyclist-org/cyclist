type abstract_proof_node 

val mk_abs_node :   
  Util.Tags.t -> int list -> Util.TagPairs.t list -> Util.TagPairs.t list -> 
    abstract_proof_node 
  
type t = abstract_proof_node Util.Int.Map.t

val check_proof : t -> bool

val compose_tag_pairs : Util.TagPairs.t -> Util.TagPairs.t -> Util.TagPairs.t

val pp : Format.formatter -> t -> unit
