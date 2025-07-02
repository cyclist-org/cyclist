open Lib
open Generic

include Base

let prf () =
  let num_nodes = List.hd !params in
  let num_heights = List.hd (List.tl !params) in
  let heights = List.init num_heights (fun n -> n) in
  let slopes = List.init num_heights (fun h -> (h, (h+1) mod num_heights)) in
  let nodes = 
    List.init 
      num_nodes
      (fun n -> (n+1, heights, [(0, slopes, [(num_heights-1, 0)])])) in
  let root =
    let slopes = List.init num_heights (fun h -> (h, h)) in
    let succs = List.init num_nodes (fun n -> (n+1, slopes, [])) in
    (0, heights, succs) in
  let prf = root :: nodes in
  Soundcheck.build_proof prf

;;

runtest ~minimize:false prf

;;