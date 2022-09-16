
open OUnit2
 
open Seplog
  

let p1 = Permission.one

let p2 = Permission.half

(* let used_labels = Permission.Label.empty *)

(* let p3 = Permission.pi used_labels *)

(* "equalp3"  >:: (fun _ -> assert_equal p3 p3); *)

let tests = "Test suite for pheap" >::: [
   "equalp1"  >:: (fun _ -> assert_equal p1 p1); 

   "equalp2"  >:: (fun _ -> assert_equal p2 p2); 
]


let _ = run_test_tt_main tests


