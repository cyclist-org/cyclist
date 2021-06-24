open Lib
open   Symbols

open Generic

open MParser
  

type psymheap = 
  {permis: Q.t;
  heap: Heap.t}

(* type psymheap =
  { eqs: Uf.t
  ; deqs: Deqs.t
  ; ptos: Ptos.t
  ; inds: Tpreds.t
  ; mutable _terms: Term.Set.t option
  ; mutable _vars: Term.Set.t option
  ; mutable _tags: Tags.t option } *)
 

(* accessors *)

let equal (h:psymheap) h' =
  h == h'
  || Q.equal h.permis h'.permis
  (* &&  *)
       