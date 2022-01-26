open Lib
open Generic

include Base

let prf () =
  let size = List.hd !params in
  let nodes =
    List.init
      size
      (fun n -> 
        let n = n + 1 in
        (n, [n], [(0, [(n, 0)], [])])) in
  let root =
    let succs =
      List.init
        size
        (fun n -> 
          let n = n + 1 in
          if n mod 2 == 0 then (n, [(0, n)], []) 
          else (n, [(0, n)], [(0, n)]))  in
    (0, [0], succs) in
  let prf = root :: nodes in
  Soundcheck.build_proof prf

;;

runtest ~minimize:false prf

;;