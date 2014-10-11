let partition_strengthening = ref false

let partitions trm_list pi =
  assert (not (Sl_heap.inconsistent pi)) ; 
  let pairs = Blist.cartesian_hemi_square trm_list in
  let pairs = 
    Blist.filter 
      (fun (x,y) -> not (Sl_heap.equates pi x y || Sl_heap.disequates pi x y))
      pairs in
  let aux sub_partitions ((x,y) as q) = 
    Blist.fold_left
      (fun l pi' -> 
        if Sl_heap.equates pi' x y || Sl_heap.disequates pi' x y 
        then 
          pi'::l
        else 
          (Sl_heap.add_eq pi' q)::(Sl_heap.add_deq pi' q)::l 
      )
      []
      sub_partitions in 
  Blist.fold_left aux [pi] pairs


(* paper: GRAY CODES, LOOPLESS ALGORITHM AND PARTITIONS *)
(* let _partitions n =                                                   *)
(*   let pi = Array.make n 1 in                                          *)
(*   let firsttime = ref true in                                         *)
(*   let j = ref n in                                                    *)
(*   let prefixmax pi j =                                                *)
(*     let maxj = ref 0 in                                               *)
(*     for i = 0 to j-2 do maxj := max !maxj pi.(i) ; done ;             *)
(*     !maxj in                                                          *)
(*   fun () ->                                                           *)
(*     if !j<=0 then None else                                           *)
(*     if !firsttime then (firsttime := false; Some pi) else             *)
(*     let () =                                                          *)
(*       while !j>0 && pi.(!j-1) = 1 + prefixmax pi !j do decr j done in *)
(*     if !j>0 then                                                      *)
(*       begin                                                           *)
(*         pi.(!j-1) <- 1 + pi.(!j-1) ;                                  *)
(*         for i = !j to n-1 do pi.(i) <- 1 done ;                       *)
(*         j := n ;                                                      *)
(*         Some pi                                                       *)
(*       end                                                             *)
(*     else                                                              *)
(*       None                                                            *)

(* let heap_partitions trm_list =                                       *)
(*   let terms = Array.of_list trm_list in                              *)
(*   let size = Array.length terms in                                   *)
(*   let parts_enum = _partitions size in                               *)
(*   let to_heap part =                                                 *)
(*     let heap = ref Sl_heap.empty in                                  *)
(*     let representatives = ref [] in                                  *)
(*     let active_block = ref 1 in                                      *)
(*     while !active_block <= size do                                   *)
(*       let repr = ref None in                                         *)
(*       for i = 0 to size-1 do                                         *)
(*         if part.(i) = !active_block then                             *)
(*           begin                                                      *)
(*             let term = terms.(i) in                                  *)
(*             match !repr with                                         *)
(*             | Some r ->                                              *)
(*               heap := Sl_heap.add_eq !heap (r, term)                 *)
(*             | None ->                                                *)
(*                 repr := Some term ;                                  *)
(*                 representatives := term::!representatives            *)
(*           end                                                        *)
(*       done ;                                                         *)
(*       incr active_block                                              *)
(*     done ;                                                           *)
(*     let deqs = Blist.cartesian_hemi_square !representatives in       *)
(*     Sl_heap.norm (Sl_heap.with_deqs !heap (Sl_deqs.of_list deqs)) in *)
(*   Enum.map to_heap parts_enum                                        *)


let _invalid defs seq =
  Stats.Invalidity.call () ;
  let (lbps, rbps) = Pair.map (Sl_basepair.pairs_of_form defs) seq in
  let (lbps, rbps) = Pair.map Sl_basepair.minimise (lbps, rbps) in
  let b_vars = 
    Sl_basepair.Set.fold
      (fun bp vs -> Sl_term.Set.union (Sl_basepair.vars bp) vs)
      rbps 
      (Sl_term.Set.singleton Sl_term.nil) in 
  let trm_list bp =
    Sl_term.Set.to_list (Sl_term.Set.union b_vars (Sl_basepair.vars bp)) in 
  let strengthen trms pi =
    if not !partition_strengthening then pi else
    let free_deqs = 
      Blist.cartesian_hemi_square trms in
    let free_deqs = 
      Blist.filter 
        (fun (x,y) ->
          not (Sl_heap.equates pi x y) && 
          Sl_basepair.Set.for_all 
            (fun (_,pi') ->
              Sl_deqs.for_all
                (fun (w,z) -> Sl_heap.equates pi x w = Sl_heap.equates pi x z)
                pi'.Sl_heap.deqs
            ) 
            rbps) 
        free_deqs in
    Blist.fold_left Sl_heap.add_deq pi free_deqs in
  let map_through sigma v =
    Sl_term.Set.endomap (fun x -> Sl_uf.find x sigma.Sl_heap.eqs) v in
  let b_move sigma (v,_) (v',pi') =
    Sl_heap.subsumed pi' sigma
    && 
    let (v, v') = Pair.map (map_through sigma) (v, v') in
    Sl_term.Set.subset v' v in     
  let a_partition ((v, pi) as bp) sigma =
    not (Sl_basepair.Set.exists (fun bp' -> b_move sigma bp bp') rbps) in    
  let a_move ((v,pi) as bp) =
    let trms = trm_list bp in 
    Blist.exists 
      (fun sigma -> a_partition bp sigma) 
      (partitions trms (strengthen trms pi)) in
  let result = Sl_basepair.Set.exists a_move lbps in
  if result then Stats.Invalidity.reject () else Stats.Invalidity.accept () ;
  result

let check =
  let cache = Hashtbl.create 997 in
  fun defs seq ->
    let key = (defs, seq) in
    try
      Hashtbl.find cache key
    with Not_found ->
      let v = _invalid defs seq in
      Hashtbl.add cache key v ;
      v
