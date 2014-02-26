open Util

module Make(Seq: Sigs.SEQUENT) =
struct
  type ruleapp_t = (Seq.t * Util.TagPairs.t * Util.TagPairs.t) list * string
  type rule_t = Seq.t -> ruleapp_t list
  
  module Seq = Seq
  
  let attempt rl seq =
    match rl seq with
    | [] ->
      [ ([(seq, TagPairs.mk (Seq.tags seq), TagPairs.empty)], "Try") ]
    | apps -> apps
        
  let apply_to_subgoal r (seq,tv,tp) =
    let fix_subgoal (seq', tv', tp') =
      (seq',
      TagPairs.compose tv tv',
      TagPairs.union_of_list
        [
          TagPairs.compose tp tp';
          TagPairs.compose tv tp';
          TagPairs.compose tp tv'
        ]
      ) in
    Blist.map (fun (l,d) -> (Blist.map fix_subgoal l, d)) (r seq)

  let apply_to_application r' (subgoals,d) =
    let apps =
      Blist.map (apply_to_subgoal r') subgoals in
    let choices = Blist.choose apps in
    Blist.map
      (fun l ->
        let (xs,ds) = Blist.split l in
        (Blist.flatten xs, d ^ "/" ^ (String.concat "," ds)))
      choices
        
  let compose r r' seq =
    Blist.flatten (Blist.map (apply_to_application r') (r seq))
        
  let rec first lr seq = match lr with
    | [] -> []
    | r::rs -> match r seq with
      | [] -> first rs seq
      | apps -> apps
 
  let sequence = function
    | [] -> invalid_arg "seq"
    | r::rs -> Blist.foldr compose rs r

  let disjunction rs seq =
    Blist.flatten (Blist.map (fun r -> r seq) rs)

  let repeat r seq = 
    let rec aux app =
      match apply_to_application r app with
      | [] -> [app]
      | apps -> Blist.flatten (Blist.map aux apps) in
    Blist.flatten (Blist.map aux (r seq)) 
       


end
