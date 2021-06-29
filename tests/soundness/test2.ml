open Lib
open Generic

include Base

(* let prf =
  let tags =
    List.fold_right
      Tags.add
      (List.init !size (fun i -> Tags.mk (string_of_int i)))
      Tags.empty in
  let tps =
    List.fold_right 
      Tagpairs.add
      (List.init !size
        (fun i ->
          let t = Tags.mk (string_of_int i) in
          if Int.((i+1) < !size)
            then (t, Tags.mk (string_of_int (i+1)))
            else (t, Tags.mk "0")))
      Tagpairs.empty in
  let node =
    Soundcheck.mk_abs_node tags [0] [(tps, tps)] in  
  Int.Map.singleton 0 node *)

let prf () =
  let tags = List.init !size (fun n -> n) in
  let tps = List.init !size (fun h -> (h, (h+1) mod !size)) in
  Soundcheck.build_proof [(0, tags, [(0, tps, [(!size-1,0)])])]

;;

runtest ~minimize:false prf

;;