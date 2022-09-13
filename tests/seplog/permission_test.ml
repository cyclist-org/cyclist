
open OUnit2
 
open Seplog
  

let p1 = Permission.one

let p2 = Permission.half

let tests = "Test suite for pheap" >::: [
   "empty"  >:: (fun _ -> assert_equal p1 p1); 

   "false"  >:: (fun _ -> assert_equal p1 p1); 
]


let _ = run_test_tt_main tests
