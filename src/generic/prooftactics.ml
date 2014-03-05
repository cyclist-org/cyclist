open Util

module Make(Proof: Sigs.PROOF) =
struct
  module Proof = Proof
  type infrule = int -> Proof.t -> (int list * Proof.t) list
  
  let apply_to_subgoals r (subgoals, prf) =
    Blist.fold_left
      (* close one subgoal each time *)
      (fun apps idx ->
        Blist.flatten
          (* actually apply the rule *)
          (Blist.map
            (fun (opened, oldprf) ->
              (* add new subgoals to the list of opened ones *)
              Blist.map
                (fun (newsubgoals, newprf) -> (opened @ newsubgoals, newprf))
                (r idx oldprf))
          apps))
      [ ([], prf) ]
      subgoals

  let compose r r' idx prf =
    Blist.flatten (Blist.map (apply_to_subgoals r') (r idx prf))


      
end