open Lib
open Generic

include Base

(* let prf =
  let () = prerr_endline (string_of_int !size) in
  let nodes =
    List.init
      !size
      (fun n -> 
        let n = n + 1 in
        let ((tag_n, tag_0) as p) = ((Tags.mk (string_of_int n)), (Tags.mk "0")) in
        let () = 
          debug (fun () -> 
            "(" ^ mk_to_string Tags.Elt.pp tag_n ^ "," ^ mk_to_string Tags.Elt.pp tag_0 ^ ") -> " ^
            "(" ^ string_of_int (Tags.Elt.to_int tag_n) ^ "," ^ string_of_int (Tags.Elt.to_int tag_0) ^ ")") in
        Soundcheck.mk_abs_node
          (Tags.singleton (Tags.mk (string_of_int n)))
          [0]
          [(Tagpairs.singleton p, Tagpairs.empty)]) in
  let root =
    let succs = List.init !size (fun i -> i + 1) in
    let tps = 
      List.init 
        !size
        (fun i ->
          let ((tag_0, tag_n) as p) = ((Tags.mk "0"), Tags.mk (string_of_int (i+1))) in
          let () = 
            debug (fun () -> 
              "(" ^ mk_to_string Tags.Elt.pp tag_0 ^ "," ^ mk_to_string Tags.Elt.pp tag_n ^ ") -> " ^
              "(" ^ string_of_int (Tags.Elt.to_int tag_0) ^ "," ^ string_of_int (Tags.Elt.to_int tag_n) ^ ")") in
            let tps = Tagpairs.singleton p in
          (tps, tps)) in
    Soundcheck.mk_abs_node 
      (Tags.singleton (Tags.mk "0"))
      succs
      tps in
  let nodes = root :: nodes in
  let (prf, _) =
    List.fold_left
      (fun (prf, i) node -> Int.Map.add i node prf, i+1)
      (Int.Map.empty, 0)
      nodes in
  prf *)

let prf () =
  let nodes =
    List.init
      !size
      (fun n -> 
        let n = n + 1 in
        (n, [n], [(0, [(n, 0)], [])])) in
  let root =
    let succs =
      List.init
        !size
        (fun n -> 
          let n = n + 1 in
          (n, [(0, n)], [(0, n)])) in
    (0, [0], succs) in
  let prf = root :: nodes in
  Soundcheck.build_proof prf

;;

runtest ~minimize:false prf

;;