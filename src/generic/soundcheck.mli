type abstract_proof_node 

val mk_abs_node :   
  Util.Tags.t -> ((int * Util.TagPairs.t* Util.TagPairs.t ) list) -> 
    abstract_proof_node 
  
type t = abstract_proof_node Util.Int.Map.t

val check_proof : t -> bool

val pp : Format.formatter -> t -> unit
