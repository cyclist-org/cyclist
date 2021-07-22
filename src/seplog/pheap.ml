open Lib
open Symbols

open Generic

open MParser


(* Function needed for instantiation of VarManager to define the labels needed for CSL *)
let classify_labelname s =
  assert (not (String.equal s "")) ;
  if String.equal s "_" then VarManager.ANONYMOUS
  else VarManager.FREE

(* Instantiation of varManager for the heapLabels *)
module HeapLabelMgr = (val VarManager.mk 0 "_" classify_labelname : VarManager.S)


module HeapLabel = HeapLabelMgr.Var

type psymheap = 
  {permis: Q.t 
  ; label: HeapLabel.t
  ; heap: Heap.t}
 
type t = psymheap

(* accessors *)

let equal (h:psymheap) h' =
  h == h'
  || Q.equal h.permis h'.permis
(*  && h.label == h'.label *)
  && Heap.equal h.heap h'.heap
 
  

let compare f g = 
  if f == g then 0
  else Heap.compare f.heap g.heap
 

