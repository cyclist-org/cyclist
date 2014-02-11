open Lib

let genhash h v = h lxor (v + (h lsl 5) + (h lsr 2))

module type BasicType =
  sig
    type t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val hash : t -> int
    val to_string : t -> string
    val pp : Format.formatter -> t -> unit
  end

module type OrderedContainer =
  sig
    include Set.S
    val of_list : elt list -> t
    val to_list: t -> elt list
    val endomap : (elt -> elt) -> t -> t
    val map_to : ('b -> 'a -> 'a) -> 'a -> (elt -> 'b) -> t -> 'a
    val map_to_list : (elt -> 'a) -> t -> 'a list
    val find : (elt -> bool) -> t -> elt
    val find_opt : (elt -> bool) -> t -> elt option
    val find_map : (elt -> 'a option) -> t -> 'a option
    val union_of_list : t list -> t
    val pp : Format.formatter -> t -> unit
    val to_string : t -> string
    val hash : t -> int
		val subsets : t -> t list
    val fixpoint : (t -> t) -> t -> t
  end

module type OrderedMap =
  sig
    include Map.S
    val of_list : (key * 'a) list -> 'a t
    val to_list : 'a t -> (key * 'a) list
    val endomap : ((key * 'a) -> (key * 'a)) -> 'a t -> 'a t
    val union : 'a t -> 'a t -> 'a t
		val find_some : (key -> 'a -> bool) -> 'a t -> (key * 'a) option
    val fixpoint : ('a -> 'a -> bool) -> ('a t -> 'a t) -> 'a t -> 'a t
(*    val map_to : ('b -> 'c -> 'c) -> 'c -> (key -> 'v -> 'b) -> 'v t -> 'c *)
(*    val map_to_list : (key -> 'b -> 'a) -> 'b t -> 'a list                 *)
  end

module Fixpoint(T: sig type t val equal : t -> t -> bool end) =
  struct
    let rec fixpoint f x =
      let y = f x in if T.equal x y then x else fixpoint f y
  end

module MakeFList(T: BasicType) : BasicType with type t = T.t list =
  struct
    type t = T.t list

    let rec compare l l' = match (l,l') with
      | ([], []) -> 0
      | ([], _) -> -1
      | (_, []) -> 1
      | (hd::tl, hd'::tl') -> match T.compare hd hd' with
        | 0 -> compare tl tl'
        | n -> n

    let rec equal l l' = match (l,l') with
      | ([], []) -> true
      | ([], _) | (_, []) -> false
      | (hd::tl, hd'::tl') -> T.equal hd hd' && equal tl tl'
(*    let hash l = Blist.fold_left (fun h v -> genhash h (T.hash v)) 0 l*)
    let hash = Hashtbl.hash
    let to_string (l:t) = String.concat ", " (Blist.map T.to_string l)
    let pp fmt l =
      Format.fprintf fmt "@[[%a]@]" (Blist.pp pp_semicolonsp T.pp) l
  end

module MakeListMultiset(T: BasicType) =
  struct
    type elt = T.t
    include MakeFList(T)

    let empty = []
    let is_empty xs = xs=[]
    let singleton x = [x]

		let of_list l = Blist.fast_sort T.compare l
    let to_list xs = xs

		let min_elt = Blist.hd
    let elements = to_list

    let rec add x = function
			| [] -> [x]
			| (y::ys) as zs -> match T.compare x y with
				| n when n <= 0 -> x::zs
				| _ -> y::(add x ys)

		let fold f xs a = Blist.fold_left (fun y a' -> f a' y) a xs
    let cardinal = Blist.length
    let choose = min_elt
    let union xs ys = Blist.merge T.compare xs ys
    let union_of_list l = Blist.fold_left union [] l
    let endomap f xs = of_list (Blist.map f xs)
		let partition = Blist.partition
		let find = Blist.find
		let filter = Blist.filter
		let exists = Blist.exists
		let for_all = Blist.for_all
		let iter = Blist.iter

		let rec mem x = function
			| [] -> false
			| y::ys -> match T.compare x y with
				| 0 -> true
				| n -> n > 0 && mem x ys

    let rec max_elt = function
      | [] -> raise Not_found
      | [x] -> x
      | _::xs -> max_elt xs

    (* only removes first occurence *)
    let rec remove x = function
      | [] -> []
      | (y::ys) as zs -> match T.compare x y with
				| 0 -> ys
    		| n when n > 0 -> y::(remove x ys)
				| _ -> zs

    let rec inter xs ys = match (xs,ys) with
      | ([], _) | (_, []) -> []
      | (w::ws, z::zs) -> match T.compare w z with
				| 0 -> w::(inter ws zs)
      	| n when n > 0 -> inter xs zs
      	| _ -> inter ws ys

    let rec subset xs ys = match (xs,ys) with
      | ([], _) -> true
      | (_, []) -> false
      | (w::ws, z::zs) -> match T.compare w z with
				| 0 -> subset ws zs
      	| n when n > 0 -> subset xs zs
      	| _ -> false

    let rec diff xs ys = match (xs,ys) with
      | ([], _) -> []
      | (_, []) -> xs
      | (w::ws, z::zs) -> match T.compare w z with
				| 0 -> diff ws zs
      	| n when n < 0 -> w::(diff ws ys)
      	| _ -> diff xs zs

		let split x xs =
			let rec div ys = function
				| [] -> (ys, false, [])
				| (z::zs) as ws -> match T.compare x z with
					| 0 -> (ys, true, ws)
					| n when n > 0 -> div (z::ys) zs
					| _ -> (ys, false, ws) in
			let (l, f, r) = div [] xs in (Blist.rev l, f, r)

    let map_to oadd oempty f xs =
      fold (fun z ys -> oadd (f z) ys) xs oempty

    let map_to_list f xs = Blist.map f xs

    let rec find_map f = function
      | [] -> None
      | x::xs -> match f x with
        | None -> find_map f xs
        | r -> r

    let find_opt f xs =
      find_map (fun x -> if f x then Some x else None) xs

		let rec subsets xs =
			if is_empty xs then [empty] else
			let x = choose xs in
			let xs = remove x xs in
    	let xxs = subsets xs in
			xxs @ (Blist.map (fun y -> add x y) xxs)

	end

module MakeMultiset(T: BasicType) : OrderedContainer with type elt = T.t =
	struct
		include MakeListMultiset(T)
    include Fixpoint(MakeListMultiset(T))
	end

module MakeListSet(T: BasicType) : OrderedContainer with type elt = T.t =
  struct
		include MakeListMultiset(T)
    include Fixpoint(MakeListMultiset(T))

		let rec uniq = function
			| [] | [_] as l -> l
			| x::((x'::_) as tl) -> match T.compare x x' with
			  | 0 -> uniq tl
				| i when i<0 -> x::(uniq tl)
				| _ -> failwith "uniq"

    let of_list l = uniq (Blist.fast_sort T.compare l)
    let union xs ys = uniq (Blist.merge T.compare xs ys)
    let union_of_list l = uniq (Blist.fold_left union [] l)
    let endomap f xs = of_list (Blist.map f xs)

    let rec add x = function
			| [] -> [x]
			| (y::ys) as zs -> match T.compare x y with
				| 0 -> zs
				| n when n < 0 -> x::zs
				| _ -> y::(add x ys)

		let rec subsets s =
			if is_empty s then [empty] else
			let x = choose s in
			let s = remove x s in
    	let xxs = subsets s in
			xxs @ (Blist.map (fun y -> add x y) xxs)

	end

(* module MakeHashSet(T: BasicType) : OrderedContainer with type elt = T.t = *)
(*   struct                                                                  *)
(*     module H = Hashtbl.Make(T)                                            *)

(*     type elt = T.t                                                        *)
(*     type t = unit H.t                                                     *)

(*     let empty = H.create 11                                               *)
(*     let cardinal h = H.length h                                           *)
(*     let is_empty h = cardinal h = 0                                       *)
(*     let mem x h = H.mem h x                                               *)
(*     let add x h =                                                         *)
(*       if mem x h then h else                                              *)
(*       let h'=H.copy h in H.add h' x () ; h'                               *)
(*     let singleton x = add x empty                                         *)
(*     let remove x h = let h'=H.copy h in H.remove h' x ; h'                *)
(*     let fold f h a = H.fold f h a                                         *)
(*     let iter f h = H.iter f h                                             *)
(*     let exists p h = fold (fun x _ a -> a || p x) h false                 *)
(*     let for_all p h = fold (fun x _ a -> a && p x) h true                 *)
(*     let elements h = fold (fun x _ l -> x::l) h []                        *)
(*     let of_list l =                                                       *)
(*       let h = H.copy empty in Blist.iter (fun x -> H.add h x () ) l ; h    *)
(*     let to_list = elements                                                *)


(*     let min_elt h = assert false                                          *)
(*     let max_elt h = assert false                                          *)
(*     let choose h = assert false                                           *)
(*     let split a h = assert false                                          *)

(*   end                                                                     *)

module MakeTreeSet(T: BasicType) : OrderedContainer with type elt = T.t =
  struct
    include Set.Make(T)
    include Fixpoint(Set.Make(T))

    let of_list l =
      Blist.fold_left (fun a b -> add b a) empty l

    let map_to oadd oempty f s =
      fold (fun el s' -> oadd (f el) s') s oempty

    let map_to_list f s =
      Blist.rev (map_to Blist.cons [] f s)

    let endomap f s =
      map_to add empty f s

    let union_of_list l =
      Blist.fold_left (fun s i -> union s i) empty l

    exception Found of elt
    let find f s =
      try
        iter (fun x -> if f x then raise (Found(x))) s ; raise Not_found
      with Found(x) -> x

    let find_map f s =
      let elem = ref None in
      iter
        begin fun e ->
          if Option.is_none !elem then
            let r = f e in
            if Option.is_some r then
              elem := r
        end
        s ;
        !elem

    let find_opt f s =
      find_map (fun e -> if f e then Some e else None) s

    let to_list = elements

    let pp fmt s =
      Format.fprintf fmt "@[{%a}@]" (Blist.pp pp_commasp T.pp) (to_list s)

    let to_string s = "{" ^ (Blist.to_string ", " T.to_string (to_list s)) ^ "}"

    let hash = Hashtbl.hash

		let rec subsets s =
			if is_empty s then [empty] else
			let x = choose s in
			let s = remove x s in
    	let xxs = subsets s in
			xxs @ (Blist.map (add x) xxs)

  end

module MakeMap(T: BasicType) : OrderedMap with type key = T.t =
  struct
    include Map.Make(T)

    let rec fixpoint eq f x =
      let y = f x in if equal eq x y then x else fixpoint eq f y

    let endomap f m =
      fold (fun k v m' -> let (k', v') = f (k,v) in add k' v' m') m empty

    (* NB this prioritises bindings of the first argument *)
    let union m m' = fold add m m'

    let of_list l =
      Blist.fold_left (fun m (k,v) -> add k v m) empty l

    let to_list = bindings

		exception Found
		let find_some f (m : 'a t) =
			let found = ref None in
			try
				iter (fun k v -> if f k v then (found:=Some(k,v) ; raise Found)) m ; None
			with Found -> !found

(*    let map_to oadd oempty f m =                   *)
(*      fold (fun k v m' -> oadd (f k v) m') m oempty*)
(*                                                   *)
(*    let map_to_list f m =                          *)
(*      Blist.rev (map_to Blist.cons [] f m)                *)
  end

module PairTypes(T: BasicType) (S: BasicType) :
  BasicType with type t = T.t * S.t =
  struct
    type t = T.t * S.t

    let compare i j =
      match T.compare (fst i) (fst j) with
        | 0 -> S.compare (snd i) (snd j)
        | n -> n

    let equal i j = T.equal (fst i) (fst j) && S.equal (snd i) (snd j)

(*    let hash (i:t) = genhash (T.hash (fst i)) (S.hash (snd i))*)
    let hash = Hashtbl.hash

    let to_string i =
      "(" ^ (T.to_string (fst i)) ^ "," ^ (S.to_string (snd i)) ^ ")"

    let pp fmt (i,j) =
      Format.fprintf fmt "@[(%a,@ %a)@]" T.pp i S.pp j
  end


module type CTsig =
  sig
    module Set : OrderedContainer
    module Map : OrderedMap
    module Hashmap : Hashtbl.S
    module MSet : OrderedContainer
    module FList : BasicType
  end

module ContaineriseType(T: BasicType) : CTsig
  with type Set.elt=T.t
  with type Map.key=T.t
  with type Hashmap.key=T.t
  with type MSet.elt=T.t
  with type FList.t=T.t list
  =
  struct
    module Set = MakeListSet(T)
    module Map = MakeMap(T)
    module Hashmap = Hashtbl.Make(T)
    module MSet = MakeMultiset(T)
    module FList = MakeFList(T)
  end

module type MCTsig =
  sig
    include BasicType
    module T : BasicType
    include CTsig
    module Pairing : CTsig
  end

module MakeComplexType(T: BasicType) : MCTsig
  with type t=T.t
  with type T.t=T.t
  with type Set.elt=T.t
  with type Map.key=T.t
  with type Hashmap.key=T.t
  with type MSet.elt=T.t
  with type FList.t=T.t list
  with type Pairing.Set.elt=T.t * T.t
  with type Pairing.Map.key=T.t * T.t
  with type Pairing.Hashmap.key=T.t * T.t
  with type Pairing.MSet.elt=T.t * T.t
  with type Pairing.FList.t=(T.t * T.t) list
  =
  struct
    include T
    module T = T
    include ContaineriseType(T)
    module Pairing = ContaineriseType(PairTypes(T)(T))
  end

module Int = MakeComplexType
  (struct
    type t = int
    let compare (i:t) (j:t) = if i<j then -1 else if i>j then +1 else 0
    let equal (i:t) (j:t) = i=j
    let hash (i:t) = i
    let to_string = string_of_int
    let pp = Format.pp_print_int
  end)

module Strng = MakeComplexType
  (struct
    type t = string
    let compare (i:t) (j:t) = Pervasives.compare i j
    let equal (i:t) (j:t) = i=j
    let hash (i:t) = Hashtbl.hash i
    let to_string (i:t) = i
    let pp = Format.pp_print_string
  end)

module Tags =
  struct
    include Int.Set
    let to_string s =
      String.concat "," (map_to_list string_of_int s)
  end

module TagPairs =
  struct
    include Int.Pairing.Set
    let mk s = Tags.fold (fun i p -> add (i,i) p) s empty
  end


