open Util

module Make(Proof: Sigs.PROOF) =
struct
  module Proof = Proof
  type infrule = int -> Proof.t -> (Proof.t * int list) list
  
  let apply_to_subgoals r (prf, subgoals) =
    Blist.fold_left
      (* close one subgoal each time *)
      (fun apps idx ->
        Blist.flatten
          (* actually apply the rule *)
          (Blist.map
            (fun (oldprf, opened) ->
              (* add new subgoals to the list of opened ones *)
              Blist.map
                (fun (newprf, newsubgoals) -> (newprf, opened @ newsubgoals))
                (r idx oldprf))
          apps))
      [ (prf, []) ]
      subgoals

  let compose r r' idx prf =
    Blist.flatten (Blist.map (apply_to_subgoals r') (r idx prf))

end