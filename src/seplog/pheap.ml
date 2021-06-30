open Lib
open   Symbols

open Generic

open MParser


type psymheap = 
  {permis: Q.t
  ; heap: Heap.t}
 
type t = psymheap

(* accessors *)

let equal (h:psymheap) h' =
  h == h'
  || Q.equal h.permis h'.permis
  && Heap.equal h.heap h'.heap
 
  

let compare f g = 
  if f == g then 0
  else Heap.compare f.heap g.heap
 

