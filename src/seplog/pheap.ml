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
  {permis: float 
  ; label: HeapLabel.t  (* string *)
  ; heap: Heap.t}
 
 (* 27/09/2021: E.F.: I've replaced HeapLabel.t by String temporally until I get to debug these errors*)


type t = psymheap
  


(* accessors *)

let equal (h:psymheap) h' =
  h == h'
  || Float.equal h.permis h'.permis
  && HeapLabel.equal h.label h'.label (* String.equal h.label h'.label   *)  
  && Heap.equal h.heap h'.heap
   

let compare f g = 
  if f == g then 0
  else Heap.compare f.heap g.heap


let check_F_is_split (_:psymheap) =
  true


(* constructors *)

let empty = 
  {permis=1.0; label= HeapLabelMgr.anonymous; heap = Heap.empty}

let mk p l h =
  {permis=p; label= l; heap = h}
 
let mk_h eqs deqs ptos inds = 
  mk 1.0 HeapLabelMgr.anonymous (Heap.mk eqs deqs ptos inds) 

let mk_pto p =
  mk 1.0 HeapLabelMgr.anonymous (Heap.mk_pto p)

let mk_eq p =
  mk 1.0 HeapLabelMgr.anonymous (Heap.mk_eq p)

let mk_deq p = 
  mk 1.0 HeapLabelMgr.anonymous (Heap.mk_deq p)

let mk_ind pred = 
  mk 1.0 HeapLabelMgr.anonymous (Heap.mk_ind pred)


(*   
let mk_h eqs deqs ptos inds = 
  {permis=1.0; label= HeapLabelMgr.anonymous; heap = Heap.mk eqs deqs ptos inds}
*)

(* let empty = mk 1.0 HeapLabel.anonymous Heap.empty *) 


(* let mk_from_heap h = 
  {Float.one; empty; h}
 *)


(*
 let mk eqs deqs ptos inds = 
  {permis=1.0; label=empty; heap=Heap.mk eqs deqs ptos inds}
*)



 
let with_heap h = 
    {permis=1.0
    ; label=HeapLabelMgr.anonymous
    ; heap=h }
    

(* hash function *)
(* it is needed to use src/lib/utilsigs/Pair *)
let hash p = 
  genhash
    (genhash
       (genhash (Tpreds.hash p.heap.inds) (Ptos.hash p.heap.ptos))
       (Deqs.hash p.heap.deqs))
    (Uf.hash p.heap.eqs)


  let to_string p =
  let res =
    String.concat (Float.to_string p.permis) (* symb_star.sep *) 
      ( Uf.to_string_list p.heap.eqs
      @ Deqs.to_string_list p.heap.deqs
      @ Ptos.to_string_list p.heap.ptos
      @ Tpreds.to_string_list p.heap.inds )
  in
  if String.equal res "" then keyw_emp.str else res

let complete_tags tags h  = 
  Heap.complete_tags tags h 

let equal_upto_tags ph ph' =
   Heap.equal_upto_tags ph.heap ph'.heap

let pp fmt ph =
   Heap.pp fmt ph.heap


let terms ph = 
    Heap.terms ph.heap

let tags ph = 
    Heap.tags ph.heap

let subst theta ph =
    Heap.subst theta ph.heap

let subst_tags tagpairs ph  = 
    Heap.subst_tags tagpairs ph.heap

let idents p =
    Tpreds.idents p.heap.inds

let parse ?(allow_tags = true) st = 
    Heap.parse ~allow_tags st

let vars f =
    Heap.vars f.heap

let classical_unify ?(inverse = false) ?(tagpairs = true)
    ?(update_check = Fun._true) ph ph' cont init_state = 
    Heap.classical_unify ~inverse ~tagpairs ph ph' cont init_state  
 
(* star two formulae together *)
let star ?(augment_deqs = true) f g =
  Heap.star ~augment_deqs f g


