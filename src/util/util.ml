open Lib

(* boost::hash_combine *)
(* size_t hash_combine( size_t lhs, size_t rhs ) {     *)
(*   lhs^= rhs + 0x9e3779b9 + (lhs << 6) + (lhs >> 2); *)
(*   return lhs;                                       *)
(* }                                                   *)

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

module type NaturalType =
  sig
    include BasicType
    val zero : t
    val succ : t -> t
  end

module type OrderedContainer =
  sig
    include Set.S
    val of_list : elt list -> t
    val to_list: t -> elt list
    val endomap : (elt -> elt) -> t -> t
    val map_to : ('b -> 'a -> 'a) -> 'a -> (elt -> 'b) -> t -> 'a
    val opt_map_to : ('b -> 'a -> 'a) -> 'a -> (elt -> 'b option) -> t -> 'a
    val map_to_list : (elt -> 'a) -> t -> 'a list
    val weave : (elt -> 'a -> 'a list) -> (elt -> 'a -> 'b) -> ('b list -> 'b) -> t -> 'a -> 'b
    val find : (elt -> bool) -> t -> elt
    val find_opt : (elt -> bool) -> t -> elt option
    val find_map : (elt -> 'a option) -> t -> 'a option
    val union_of_list : t list -> t
    val pp : Format.formatter -> t -> unit
    val to_string : t -> string
    val hash : t -> int
    val subsets : t -> t list
    val fixpoint : (t -> t) -> t -> t
    val del_first : (elt -> bool) -> t -> t
    val all_members_of : t -> t -> bool
    val disjoint : t -> t -> bool
    val mk_unifier : bool -> bool -> ('a, 'b, elt) Unification.cps_unifier
          -> ('a, 'b, t) Unification.cps_unifier
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
    val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
    val to_string : ('a -> string) -> 'a t -> string
    val hash : ('a -> int) -> 'a t -> int
(*    val map_to : ('b -> 'c -> 'c) -> 'c -> (key -> 'v -> 'b) -> 'v t -> 'c *)
(*    val map_to_list : (key -> 'b -> 'a) -> 'b t -> 'a list                 *)
    val all_members_of : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val add_bindings : (key * 'a) list -> 'a t -> 'a t
  end

module Fixpoint(T: sig type t val equal : t -> t -> bool end) =
  struct
    let rec fixpoint f x =
      let y = f x in if T.equal x y then x else fixpoint f y
  end

module MakeFList(T: BasicType) : BasicType with type t = T.t list =
  struct
    type t = T.t list

    let rec compare l l' =
      if l==l' then 0 else
      match (l,l') with
      | ([], []) -> 0
      | ([], _) -> -1
      | (_, []) -> 1
      | (hd::tl, hd'::tl') -> match T.compare hd hd' with
        | 0 -> compare tl tl'
        | n -> n

    let rec equal l l' =
      l==l' ||
      match (l,l') with
      | ([], []) -> true
      | (hd::tl, hd'::tl') -> T.equal hd hd' && equal tl tl'
      | ([], _) | (_, []) -> false

    let hash l = Blist.fold_left (fun h v -> genhash h (T.hash v)) 0x9e3779b9 l
    (* let hash l =                                    *)
    (*   let rec aux h = function                      *)
    (*     | [] -> h                                   *)
    (*     | x::xs -> aux (genhash h (T.hash x)) xs in *)
    (*   aux 0x9e3779b9 l                              *)
    (* let hash = Hashtbl.hash *)

    let to_string (l:t) = Format.sprintf "(%s)" (String.concat ", " (Blist.map T.to_string l))
    let pp fmt l =
      Format.fprintf fmt "@[[%a]@]" (Blist.pp pp_semicolonsp T.pp) l
  end

module MakeUnifier(
  T : sig 
    type t 
    type elt
    val empty : t
    val is_empty : t -> bool
    val equal : t -> t -> bool
    val add : elt -> t -> t
    val choose : t -> elt
    val remove : elt -> t -> t
    val find_map : (elt -> 'a option) -> t -> 'a option
  end) =
struct
  let mk_unifier total linear u =
    let rec unify marked_ys xs ys cont state =
      if T.is_empty xs then
        if (not total) || 
           (linear && T.is_empty ys) ||
           ((not linear) && (T.equal marked_ys ys)) 
        then cont state else None
      else
        let x = T.choose xs in
        let xs = T.remove x xs in
        let f y =
          let ys = if linear then T.remove y ys else ys in
          let marked_ys = if not linear then T.add y marked_ys else marked_ys in
          u x y 
          (unify marked_ys xs ys cont)
          state in
        T.find_map f ys in
    unify T.empty
end

module MakeListMultiset(T: BasicType) =
  struct
    type elt = T.t
    module FList = MakeFList(T)
    include FList

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

    let rec del_first p = function
      | [] -> []
      | y::ys -> if p y then ys else y::(del_first p ys)

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
      
    let opt_map_to oadd oempty f xs =
      map_to (Option.dest Fun.id oadd) oempty f xs

    let map_to_list f xs = Blist.map f xs

    let weave = Blist.weave

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

    let all_members_of xs ys = for_all (Fun.swap mem ys) xs

    let rec disjoint xs ys = match (xs, ys) with
      | ([], ys) -> true
      | (xs, []) -> true
      | (x::xs, y::ys) -> match T.compare x y with
        | 0 -> false
        | n when n < 0 -> disjoint xs (y::ys)
        | _ -> disjoint (x::xs) ys

    include MakeUnifier(
      struct
        type t = FList.t
        type elt = T.t
        let empty = empty
        let is_empty = is_empty
        let equal = equal
        let add = add
        let choose = choose
        let remove = remove
        let find_map = find_map
      end)

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

module MakeTreeSet(T: BasicType) : OrderedContainer with type elt = T.t =
  struct
    module Set = Set.Make(T)
    include Set
    include Fixpoint(Set)

    let of_list l =
      Blist.fold_left (fun a b -> add b a) empty l

    let map_to oadd oempty f s =
      fold (fun el s' -> oadd (f el) s') s oempty
      
    let opt_map_to oadd oempty f s =
      map_to (Option.dest Fun.id oadd) oempty f s

    let map_to_list f s =
      Blist.rev (map_to Blist.cons [] f s)

    let endomap f s =
      map_to add empty f s

    let weave split tie join xs acc =
      Blist.weave split tie join (elements xs) acc

    let union_of_list l =
      Blist.fold_left (fun s i -> union s i) empty l

    exception Found of elt
    let find f s =
      try
        iter (fun x -> if f x then raise (Found(x))) s ; raise Not_found
      with Found(x) -> x

    exception FoundMap
    let find_map f s =
      let elem = ref None in
      try
        iter
          (fun e -> match f e with
            | None -> ()
            | Some _ as r -> elem := r ; raise FoundMap)
          s ;
          None
      with FoundMap -> !elem

    let find_opt f s =
      find_map (fun e -> if f e then Some e else None) s

    let to_list = elements

    let pp fmt s =
      Format.fprintf fmt "@[{%a}@]" (Blist.pp pp_commasp T.pp) (to_list s)

    let to_string s = "{" ^ (Blist.to_string ", " T.to_string (to_list s)) ^ "}"

    let hash s =
      fold (fun x h -> genhash (T.hash x) h) s 0x9e3779b9

    let rec subsets s =
      if is_empty s then [empty] else
      let x = choose s in
      let s = remove x s in
      let xxs = subsets s in
      xxs @ (Blist.map (add x) xxs)

    let del_first p s =
      match find_opt p s with
      | None -> s
      | Some x -> remove x s

    let all_members_of xs ys = for_all (Fun.swap mem ys) xs

    let disjoint xs ys =
      let xs = to_list xs in
      let ys = to_list ys in
      let rec disjoint xs ys = match (xs, ys) with
        | ([], ys) -> true
        | (xs, []) -> true
        | (x::xs, y::ys) -> match T.compare x y with
          | 0 -> false
          | n when n < 0 -> disjoint xs (y::ys)
          | _ -> disjoint (x::xs) ys in
      disjoint xs ys

    include MakeUnifier(
      struct
        type t = Set.t
        type elt = T.t
        let empty = empty
        let is_empty = is_empty
        let equal = equal
        let add = add
        let choose = choose
        let remove = remove
        let find_map = find_map
      end)

  end

module MakeMap(T: BasicType) : OrderedMap with type key = T.t =
  struct
    include Map.Make(T)

    let equal eq m m' = m==m' || equal eq m m'

    let compare comp m m' =
      if m==m' then 0 else compare comp m m'

    let hash h m =
      fold
        (fun k v a -> genhash a (genhash (T.hash k) (h v)))
        m
        0x9e3779b9

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

    let pp pp_val fmt m =
      Format.fprintf fmt "@[@ ";
      iter
        (fun k v -> Format.fprintf fmt "@[(%a->%a);@]@ " T.pp k pp_val v)
        m;
      Format.fprintf fmt "@]"

    let to_string val_to_string m =
      let pp_val fmt v = Format.pp_print_string fmt (val_to_string v) in
      Format.asprintf "%a" (pp pp_val) m

    let all_members_of eq m m' =
      let mem (k, v) s = Blist.exists (fun (k', v') -> (T.equal k k') && (eq v v')) s in
      let xs = to_list m in
      let ys = to_list m' in
      Blist.for_all (Fun.swap mem ys) xs

    let add_bindings bs m = List.fold_left (fun m (k, v) -> add k v m) m bs
    
  end

module PairTypes(T: BasicType) (S: BasicType) :
  BasicType with type t = T.t * S.t =
  struct
    type t = T.t * S.t

    let compare i j =
      if i==j then 0 else
      match T.compare (fst i) (fst j) with
        | 0 -> S.compare (snd i) (snd j)
        | n -> n

    let equal i j =
      i==j || T.equal (fst i) (fst j) && S.equal (snd i) (snd j)

    let hash (i:t) = genhash (T.hash (fst i)) (S.hash (snd i))

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
    module Hashset : Hashset.S
    module MSet : OrderedContainer
    module FList : BasicType
  end

module ContaineriseType(T: BasicType) : CTsig
  with type Set.elt=T.t
  with type Map.key=T.t
  with type Hashmap.key=T.t
  with type Hashset.elt=T.t
  with type MSet.elt=T.t
  with type FList.t=T.t list
  =
  struct
    (* module Set = MakeListSet(T) *)
    module Set = MakeTreeSet(T)
    module Map = MakeMap(T)
    module Hashmap = Hashtbl.Make(T)
    module Hashset = Hashset.Make(T)
    module MSet = MakeMultiset(T)
    module FList = MakeFList(T)
  end

module type MCTsig =
  sig
    include BasicType
    module T : BasicType
    include CTsig
    module Pairing : CTsig
    (* val set_cross : Set.t -> Set.t -> Pairing.Set.t *)
  end

module MakeComplexType(T: BasicType) : MCTsig
  with type t=T.t
  with type T.t=T.t
  with type Set.elt=T.t
  with type Map.key=T.t
  with type Hashmap.key=T.t
  with type Hashset.elt=T.t
  with type MSet.elt=T.t
  with type FList.t=T.t list
  with type Pairing.Set.elt=T.t * T.t
  with type Pairing.Map.key=T.t * T.t
  with type Pairing.Hashmap.key=T.t * T.t
  with type Pairing.Hashset.elt=T.t * T.t
  with type Pairing.MSet.elt=T.t * T.t
  with type Pairing.FList.t=(T.t * T.t) list
  =
  struct
    include T
    module T = T
    include ContaineriseType(T)
    module Pairing = ContaineriseType(PairTypes(T)(T))
    (* let set_cross ss ss' =                                      *)
    (*   Set.map_to Pairing.Set.union Pairing.Set.empty            *)
    (*     (fun s -> Set.map_to Pairing.Set.add Pairing.Set.empty  *)
    (*       (fun s' -> (s, s')) ss')                              *)
    (*     ss                                                      *)
  end

module IntType : BasicType with type t = int =
  struct
    type t = int
    let compare (i:t) (j:t) = if i<j then -1 else if i>j then +1 else 0
    let equal (i:t) (j:t) = i=j
    let hash (i:t) = Hashtbl.hash i
    let to_string = string_of_int
    let pp = Format.pp_print_int
  end

module NatType : NaturalType with type t = int =
  struct
    include IntType
    let zero = 0
    let succ i = i + 1
  end

module StringType : BasicType with type t = string =
  struct
    type t = string
    let compare (i:t) (j:t) = String.compare i j
    let equal (i:t) (j:t) = i=j
    let hash (i:t) = Hashtbl.hash i
    let to_string (i:t) = i
    let pp = Format.pp_print_string
  end

module Int = MakeComplexType(IntType)
module Strng = MakeComplexType(StringType)

module MakeVarManager
  (Kernel :
    sig
      val map : string Int.Hashmap.t
      val inv_map : int Strng.Hashmap.t
      val max_var : int ref
      val min_var : int ref
      val default_varname : bool -> string
    end) =
  struct
    let get_limit exist = if exist then !Kernel.min_var else !Kernel.max_var
    let get_diff exist = if exist then (-1) else 1

    let present v = Int.Hashmap.mem Kernel.map v
    let name_present n = Strng.Hashmap.mem Kernel.inv_map n

    let to_string v = Int.Hashmap.find Kernel.map v
    let get_idx n = Strng.Hashmap.find Kernel.inv_map n

    let is_var t = t<>0
    let is_exist_var v = (is_var v) && v<0
    let is_univ_var v = (is_var v) && v>0
    let is_valid_var v exist =
      exist && is_exist_var v || not exist && is_univ_var v

    let is_exist_name n = n.[(String.length n)-1] = '\''
    let is_univ_name n = not (is_exist_name n)
    let is_valid_name v exist =
      exist && is_exist_name v || not exist && is_univ_name v

    let mk_var name exist =
      assert (is_valid_name name exist);
      if name_present name then
        let v = get_idx name in assert (is_valid_var v exist) ; v
      else
        let v = (get_diff exist) + (get_limit exist) in
        assert (not (present v) && not (name_present name)) ;
        assert
          (is_exist_var v && is_exist_name name ||
           is_univ_var v && is_univ_name name);
        Int.Hashmap.add Kernel.map v name ;
        Strng.Hashmap.add Kernel.inv_map name v ;
        Kernel.max_var := max !Kernel.max_var v ;
        Kernel.min_var := min !Kernel.min_var v ;
        v

    let fresh_varname exist =
      let suffix = if exist then "'" else "" in
      let idx = ref 0 in
      let base = ref "a" in
      let gen_name () =
        !base ^
        (if !idx = 0 then "" else string_of_int !idx) ^
        suffix in
      let name = ref (gen_name ()) in
      let letter = ref 'a' in
      while name_present !name && !letter < 'z' do
        letter := char_of_int (1 + (int_of_char !letter)) ;
        base := string_of_char !letter ;
        name := gen_name ()
      done ;
      if not (name_present !name) then !name else
      begin
        base := Kernel.default_varname exist ;
        idx := 1;
        name := gen_name () ;
        while name_present !name do
          incr idx ;
          name := gen_name ()
        done ;
        assert (not (name_present !name)) ;
        !name
      end

    let fresh_var m exist =
      let limit = abs (get_limit exist) in
      let i = m + (get_diff exist) in
      if abs i <= limit then i else mk_var (fresh_varname exist) exist

    let rec fresh_vars m i exist = match i with
      | 0 -> []
      | n ->
        let v = fresh_var m exist in
        v::(fresh_vars (m + (get_diff exist)) (n-1) exist)

    module MakeGenerators(T : OrderedContainer with type elt = int) =
      struct
        let bound s exist = if exist then min 0 (T.min_elt s) else max 0 (T.max_elt s)
        let fresh_evar s = let m = if T.is_empty s then 0 else bound s true in fresh_var m true
        let fresh_uvar s = let m = if T.is_empty s then 0 else bound s false in fresh_var m false
        let fresh_evars s n = let m = if T.is_empty s then 0 else bound s true in fresh_vars m n true
        let fresh_uvars s n = let m = if T.is_empty s then 0 else bound s false in fresh_vars m n false
      end
  end

module Tags =
  struct
    include MakeVarManager(
      struct
        let map = Int.Hashmap.create 997
        let inv_map = Strng.Hashmap.create 997
        let max_var = ref 0
        let min_var = ref 0
        let default_varname exist = if exist then "x" else "w"
      end)

    let anonymous_tag = 0
    let tag_name v = to_string v

    module Elt =
    struct
      include Int
      
      let unify ?(update_check=Fun._true) t t' cont init_state =
        let pair = Pairing.Set.find_opt (fun p -> equal (fst p) t) init_state in
        let res =
          if Option.is_some pair then 
            Option.mk (equal (snd (Option.get pair)) t') init_state
          else 
            Option.mk_lazily 
              (equal t t' ||
                update_check (init_state, (Pairing.Set.singleton (t, t')))) 
              (fun _ -> Pairing.Set.add (t, t') init_state) in
        Option.bind cont res
        
      let biunify 
          ?(update_check=Fun._true) t t' cont ((subst, subst') as state) =
        let lpair = Pairing.Set.find_opt (fun p -> equal (fst p) t) subst in
        let rpair = Pairing.Set.find_opt (fun p -> equal (fst p) t') subst' in
        let opts = 
          if Pair.both (Pair.map Option.is_some (lpair, rpair)) then
            [ Option.mk 
                (Fun.uncurry equal 
                  (Pair.map (fun p -> snd (Option.get p)) (lpair, rpair))) 
                state ]
          else
            let lpair' = 
              (t, if Option.is_some rpair then (snd (Option.get rpair))
                  else t') in
            let rpair' = 
              (t', if Option.is_some lpair then (snd (Option.get lpair))
                   else t) in
            [ Option.mk_lazily
                (Option.is_none lpair &&
                  (Pair.apply equal lpair' ||
                   update_check 
                    (state, (Pairing.Set.singleton lpair', Pairing.Set.empty))))
                (fun _ -> (Pairing.Set.add lpair' subst, subst')) ;
              Option.mk_lazily
                (Option.is_none rpair &&
                  (Pair.apply equal rpair' ||
                   update_check 
                    (state, (Pairing.Set.empty, Pairing.Set.singleton rpair'))))
                (fun _ -> (subst, Pairing.Set.add rpair' subst')) ] in
        Blist.find_some (Option.bind cont) opts
    end
    
    include Elt.Set
    let to_string s =
      String.concat "," (map_to_list string_of_int s)
    let to_names s =
      map_to Strng.Set.add Strng.Set.empty tag_name s

    include MakeGenerators(Elt.Set)
  end

module TagPairs =
  struct
    include Tags.Elt.Pairing.Set
    let mk s = Tags.fold (fun i p -> add (i,i) p) s empty

    let dest_singleton tps =
      Option.mk_lazily ((cardinal tps) == 1) (fun _ -> choose tps)

    let to_names tps =
      map_to Strng.Pairing.Set.add Strng.Pairing.Set.empty
        (Pair.map Tags.tag_name) tps

    let compose t1 t2 : t =
      fold
        (fun (x,y) a ->
          fold (fun (w,z) b -> if y=w then add (x,z) b else b) t2 a)
        t1 empty

    let projectl tp = map_to Tags.add Tags.empty fst tp
    let projectr tp = map_to Tags.add Tags.empty snd tp

    let reflect tps = map_to add empty Pair.swap tps

    let apply_to_tag tps t =
      try
        snd (find (fun (t',_) -> t=t') tps)
      with Not_found -> t
      
    let strip tps = filter (fun (t, t') -> not (Tags.Elt.equal t t')) tps
    
    let mk_univ_subst avoid ts =
      let univs = Tags.fresh_uvars (Tags.union ts avoid) (Tags.cardinal ts) in
      of_list (Blist.combine (Tags.to_list ts) univs)
      
    let mk_ex_subst avoid ts =
      let exs = Tags.fresh_evars (Tags.union ts avoid) (Tags.cardinal ts) in
      of_list (Blist.combine (Tags.to_list ts) exs)
      
    let partition_subst theta =
      partition (fun tp -> Pair.both (Pair.map Tags.is_univ_var tp)) theta

  end

