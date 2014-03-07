open Util

module Make(Seq: Sigs.SEQUENT) =
struct
  type seq_t = Seq.t
  type ruleapp_t = (seq_t * Util.TagPairs.t * Util.TagPairs.t) list * string
  type rule_t = seq_t -> ruleapp_t list
  
  let relabel descr rl seq = 
    Blist.map (fun (app,_) -> (app,descr)) (rl seq)
  
  let attempt rl seq =
    match rl seq with
    | [] ->
      [ ([(seq, TagPairs.mk (Seq.tags seq), TagPairs.empty)], "") ]
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

  let filt_emp ss = Blist.filter (fun s -> String.length s <> 0) ss
  
  let apply_to_application r (subgoals,d) =
    let apps = Blist.map (apply_to_subgoal r) subgoals in
    let choices = Blist.choose apps in
    Blist.map
      (fun l ->
        let (xs,ds) = Blist.split l in
        (Blist.flatten xs, 
        String.concat "/" (filt_emp [d; String.concat "," (filt_emp ds)])))
      choices
        
  let compose r r' seq = Blist.bind (apply_to_application r') (r seq)
        
  let rec first lr seq = match lr with
    | [] -> []
    | r::rs -> match r seq with
      | [] -> first rs seq
      | apps -> apps
 
  let sequence = function
    | [] -> invalid_arg "sequence"
    | r::rs -> Blist.foldr compose rs r

  (* let choice rs seq = Blist.bind (fun r -> r seq) rs *)

  let repeat r seq = 
    let rec aux app =
      match apply_to_application r app with
      | [] -> [app]
      | apps -> Blist.bind aux apps in
    Blist.bind aux (r seq) 
end
