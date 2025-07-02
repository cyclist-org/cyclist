open Lib
open Generic

let partition_strengthening = ref false

let partitions trm_list pi =
  assert (not (Heap.inconsistent pi)) ;
  let pairs = Blist.cartesian_hemi_square trm_list in
  let pairs =
    Blist.filter
      (fun (x, y) -> not (Heap.equates pi x y || Heap.disequates pi x y))
      pairs
  in
  let aux sub_partitions ((x, y) as q) =
    Blist.fold_left
      (fun l pi' ->
        if Heap.equates pi' x y || Heap.disequates pi' x y then pi' :: l
        else Heap.add_eq pi' q :: Heap.add_deq pi' q :: l )
      [] sub_partitions
  in
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
(*     let heap = ref Heap.empty in                                  *)
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
(*               heap := Heap.add_eq !heap (r, term)                 *)
(*             | None ->                                                *)
(*                 repr := Some term ;                                  *)
(*                 representatives := term::!representatives            *)
(*           end                                                        *)
(*       done ;                                                         *)
(*       incr active_block                                              *)
(*     done ;                                                           *)
(*     let deqs = Blist.cartesian_hemi_square !representatives in       *)
(*     Heap.norm (Heap.with_deqs !heap (Deqs.of_list deqs)) in *)
(*   Enum.map to_heap parts_enum                                        *)

let invalidity_witness defs seq =
  Stats.Invalidity.call () ;
  let lbps, rbps = Pair.map (Basepair.pairs_of_form defs) seq in
  let lbps, rbps = Pair.map Basepair.minimise (lbps, rbps) in
  let lbps =
    Basepair.Set.filter
      (fun bp ->
        Basepair.Set.for_all (fun bp' -> not (Basepair.leq bp' bp)) rbps
        )
      lbps
  in
  let b_vars =
    Basepair.Set.fold
      (fun bp vs -> Term.Set.union (Basepair.vars bp) vs)
      rbps
      (Term.Set.singleton Term.nil)
  in
  let trm_list bp =
    Term.Set.to_list (Term.Set.union b_vars (Basepair.vars bp))
  in
  let strengthen trms pi =
    if not !partition_strengthening then pi
    else
      let free_deqs = Blist.cartesian_hemi_square trms in
      let free_deqs =
        Blist.filter
          (fun (x, y) ->
            (not (Heap.equates pi x y))
            && Basepair.Set.for_all
                 (fun (_, pi') ->
                   Deqs.for_all
                     (fun (w, z) ->
                       Stdlib.( = ) (Heap.equates pi x w)
                         (Heap.equates pi x z) )
                     pi'.Heap.deqs )
                 rbps )
          free_deqs
      in
      Blist.fold_left Heap.add_deq pi free_deqs
  in
  let map_through sigma v =
    Basepair.Allocated.map
      (fun (x, i) -> (Uf.find x sigma.Heap.eqs, i))
      v
  in
  let b_move sigma (v, _) (v', pi') =
    Heap.subsumed pi' sigma
    &&
    let v, v' = Pair.map (map_through sigma) (v, v') in
    Basepair.Allocated.subset v' v
  in
  let a_partition ((v, pi) as bp) sigma =
    not (Basepair.Set.exists (fun bp' -> b_move sigma bp bp') rbps)
  in
  let a_move ((v, pi) as bp) =
    let trms = trm_list bp in
    Blist.exists
      (fun sigma -> a_partition bp sigma)
      (partitions trms (strengthen trms pi))
  in
  let result = Basepair.Set.find_suchthat_opt a_move lbps in
  if Option.is_none result then Stats.Invalidity.reject ()
  else Stats.Invalidity.accept () ;
  result

let _invalid defs seq = Option.is_some (invalidity_witness defs seq)

let check = _invalid

(* let check =                         *)
(*   let cache = Hashtbl.create 997 in *)
(*   fun defs seq ->                   *)
(*     let key = (defs, seq) in        *)
(*     try                             *)
(*       Hashtbl.find cache key        *)
(*     with Not_found ->               *)
(*       let v = _invalid defs seq in  *)
(*       Hashtbl.add cache key v ;     *)
(*       v                             *)

(* let to_z3 defs seq =                                                  *)
(*   (* Stats.Invalidity.call () ; *)                                    *)
(*   let (lbps, rbps) = Pair.map (Basepair.pairs_of_form defs) seq in *)
(*   let (lbps, rbps) = Pair.map Basepair.minimise (lbps, rbps) in    *)
(*   let lbps =                                                          *)
(*     Basepair.Set.filter                                            *)
(*       (fun bp ->                                                      *)
(*         Basepair.Set.for_all                                       *)
(*           (fun bp' -> not (Basepair.leq bp' bp))                   *)
(*           rbps)                                                       *)
(*       lbps in                                                         *)
(*   let vars =                                                          *)
(*     Basepair.Set.fold                                              *)
(*       (fun bp vs -> Term.Set.union (Basepair.vars bp) vs)       *)
(*       (Basepair.Set.union lbps rbps)                               *)
(*       (Term.Set.singleton Term.nil) in                          *)
(*   Term.Set.iter                                                    *)
(*     (fun x -> Format.printf "(declare-const %a Int)\n" Term.pp x)  *)
(*     vars                                                              *)

(* let trm_list bp =                                                            *)
(*   Term.Set.to_list (Term.Set.union b_vars (Basepair.vars bp)) in    *)
(* let strengthen trms pi =                                                     *)
(*   if not !partition_strengthening then pi else                               *)
(*   let free_deqs =                                                            *)
(*     Blist.cartesian_hemi_square trms in                                      *)
(*   let free_deqs =                                                            *)
(*     Blist.filter                                                             *)
(*       (fun (x,y) ->                                                          *)
(*         not (Heap.equates pi x y) &&                                      *)
(*         Basepair.Set.for_all                                              *)
(*           (fun (_,pi') ->                                                    *)
(*             Deqs.for_all                                                  *)
(*               (fun (w,z) -> Heap.equates pi x w = Heap.equates pi x z) *)
(*               pi'.Heap.deqs                                               *)
(*           )                                                                  *)
(*           rbps)                                                              *)
(*       free_deqs in                                                           *)
(*   Blist.fold_left Heap.add_deq pi free_deqs in                            *)
(* let map_through sigma v =                                                    *)
(*   Term.Set.map (fun x -> Uf.find x sigma.Heap.eqs) v in         *)
(* let b_move sigma (v,_) (v',pi') =                                            *)
(*   Heap.subsumed pi' sigma                                                 *)
(*   &&                                                                         *)
(*   let (v, v') = Pair.map (map_through sigma) (v, v') in                      *)
(*   Term.Set.subset v' v in                                                 *)
(* let a_partition ((v, pi) as bp) sigma =                                      *)
(*   not (Basepair.Set.exists (fun bp' -> b_move sigma bp bp') rbps) in      *)
(* let a_move ((v,pi) as bp) =                                                  *)
(*   let trms = trm_list bp in                                                  *)
(*   Blist.exists                                                               *)
(*     (fun sigma -> a_partition bp sigma)                                      *)
(*     (partitions trms (strengthen trms pi)) in                                *)
(* let result = Basepair.Set.exists a_move lbps in                           *)
(* if result then Stats.Invalidity.reject () else Stats.Invalidity.accept () ;  *)
(* result                                                                       *)
