
open OUnit2
 
open Seplog
  

let p1 = Permission.one

let p05 = Permission.half p1

let p25 = Permission.split_ith p05 2

(* let used_labels = Permission.Label.empty *)

(* let p3 = Permission.pi used_labels *)

(* "equalp3"  >:: (fun _ -> assert_equal p3 p3); *)

let tests = "Test suite for pheap" >::: [
   "equalp1"  >:: (fun _ -> assert_equal p1 p1); 

   "equalp2"  >:: (fun _ -> assert_equal p1 (Permission.add p05 p05));
   
   "equalp3"  >:: (fun _ -> assert_equal p05 (Permission.add p25 p25)); 
]


let _ = run_test_tt_main tests


