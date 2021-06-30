open Lib
open   Symbols

open Generic

open MParser


type psymheap = 
  {permis: Q.t;
  heap: Heap.t}
 

(* accessors *)

let equal (h:psymheap) h' =
  h == h'
  || Q.equal h.permis h'.permis
  && Heap.equal h.heap h'.heap
;;
    