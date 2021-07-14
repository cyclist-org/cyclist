 
open OUnit2
 
open Seplog
 
open Pheap


let pheap1 = {
  permis = { 
    num= Z.one; 
    den= Z.of_int(2)
  };
  label=1;
  heap = Heap.empty
}

let pheap2 = {
  permis = { 
    num= Z.one; 
    den= Z.of_int(4)
  };
  label=1;
  heap = Heap.empty
}

let tests = "Test suite for pheap" >::: [
   "empty"  >:: (fun _ -> assert_equal pheap1 pheap1); 

   "false"  >:: (fun _ -> assert_equal pheap2 pheap2); 
]


let _ = run_test_tt_main tests

 