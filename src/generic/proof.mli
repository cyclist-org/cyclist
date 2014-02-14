module Make (Node : Sigs.NODE) : Sigs.PROOF 
  with type node_t = Node.t with type seq_t = Node.seq_t
  with module Node = Node
   
