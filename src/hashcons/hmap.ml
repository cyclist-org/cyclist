(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(*s Maps of integers implemented as Patricia trees, following Chris
    Okasaki and Andrew Gill's paper {\em Fast Mergeable Integer Maps}
    ({\tt\small http://www.cs.columbia.edu/\~{}cdo/papers.html\#ml98maps}).
    See the documentation of module [Ptset] which is also based on the
    same data-structure. *)

open Hashcons

type 'a key = 'a hash_consed

type ('a, 'b) t =
  | Empty
  | Leaf of 'a key * 'b
  | Branch of int * int * ('a, 'b) t * ('a, 'b) t

let empty = Empty

let zero_bit k m = (k land m) == 0

let rec mem k = function
  | Empty -> false
  | Leaf (j,_) -> k.tag == j.tag
  | Branch (_, m, l, r) -> mem k (if zero_bit k.tag m then l else r)

let rec find k = function
  | Empty -> raise Not_found
  | Leaf (j,x) -> if k.tag == j.tag then x else raise Not_found
  | Branch (_, m, l, r) -> find k (if zero_bit k.tag m then l else r)

let lowest_bit x = x land (-x)

let branching_bit p0 p1 = lowest_bit (p0 lxor p1)

let mask p m = p land (m-1)

let join (p0,t0,p1,t1) =
  let m = branching_bit p0 p1 in
  if zero_bit p0 m then
    Branch (mask p0 m, m, t0, t1)
  else
    Branch (mask p0 m, m, t1, t0)

let match_prefix k p m = (mask k m) == p

let add k x t =
  let rec ins = function
    | Empty -> Leaf (k,x)
    | Leaf (j,_) as t ->
	if j.tag == k.tag then
	  Leaf (k,x)
	else
	  join (k.tag, Leaf (k,x), j.tag, t)
    | Branch (p,m,t0,t1) as t ->
	if match_prefix k.tag p m then
	  if zero_bit k.tag m then
	    Branch (p, m, ins t0, t1)
	  else
	    Branch (p, m, t0, ins t1)
	else
	  join (k.tag, Leaf (k,x), p, t)
  in
  ins t

let branch = function
  | (_,_,Empty,t) -> t
  | (_,_,t,Empty) -> t
  | (p,m,t0,t1)   -> Branch (p,m,t0,t1)

let remove k t =
  let rec rmv = function
    | Empty -> Empty
    | Leaf (j,_) as t -> if k.tag == j.tag then Empty else t
    | Branch (p,m,t0,t1) as t ->
	if match_prefix k.tag p m then
	  if zero_bit k.tag m then
	    branch (p, m, rmv t0, t1)
	  else
	    branch (p, m, t0, rmv t1)
	else
	  t
  in
  rmv t

let rec iter f = function
  | Empty -> ()
  | Leaf (k,x) -> f k x
  | Branch (_,_,t0,t1) -> iter f t0; iter f t1

let rec map f = function
  | Empty -> Empty
  | Leaf (k,x) -> Leaf (k, f x)
  | Branch (p,m,t0,t1) -> Branch (p, m, map f t0, map f t1)

let rec mapi f = function
  | Empty -> Empty
  | Leaf (k,x) -> Leaf (k, f k x)
  | Branch (p,m,t0,t1) -> Branch (p, m, mapi f t0, mapi f t1)

let rec fold f s accu = match s with
  | Empty -> accu
  | Leaf (k,x) -> f k x accu
  | Branch (_,_,t0,t1) -> fold f t0 (fold f t1 accu)

(* changes by NG *)
let is_empty = function
  | Empty -> true
  | _ -> false

let rec choose = function
  | Empty -> raise Not_found
  | Leaf (k,x) -> (k,x)
  | Branch (_, _,t0,_) -> choose t0   (* we know that [t0] is non-empty *)

let rec min_binding = function
  | Empty -> raise Not_found
  | Leaf (k,v) -> (k,v)
  | Branch (_,_,s,t) ->
    let ((k,v) as p), ((k',v') as p') = min_binding s, min_binding t in
    if k.tag <= k'.tag then p else p'

let rec max_binding = function
  | Empty -> raise Not_found
  | Leaf (k,v) -> (k,v)
  | Branch (_,_,s,t) ->
    let ((k,v) as p), ((k',v') as p') = max_binding s, max_binding t in
    if k.tag >= k'.tag then p else p'

let rec equal eq m m' =
  match m, m' with
  | Empty, Empty -> true
  | Leaf (k,x), Leaf(k',x') -> k.tag = k'.tag && eq x x'
  | Branch (_,_,t0,t1), Branch(_,_,t0',t1') -> equal eq t0 t0' && equal eq t1 t1'
  | _ -> false

let rec compare comp m m' =
  match m, m' with
  | Empty, Empty -> 0
  | Empty, _ -> -1
  | _, Empty -> 1
  | Leaf (k,x), Leaf(k',x') ->
    let r = Int.compare k.tag k'.tag in if r<>0 then r else comp x x'
  | Leaf _, _ -> -1
  | _, Leaf _ -> 1
  | Branch (_,_,t0,t1), Branch(_,_,t0',t1') ->
    let r = compare comp t0 t0' in if r<>0 then r else compare comp t1 t1'
