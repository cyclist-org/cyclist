module type S = sig
  module Set : Utilsigs.OrderedContainer

  module Map : Utilsigs.OrderedMap

  module Hashmap : Hashtbl.S

  module Hashset : 
    sig
      
      include Hashset.S

      val exists : (elt -> bool) -> t -> bool

      val for_all : (elt -> bool) -> t -> bool
    
      val left_union : t -> t -> t
    
      val is_empty : t -> bool
    
      val filter : (elt -> bool) -> t -> unit
    
      val to_string : t -> string
    
      val of_list : elt list -> t
    
      val to_list : t -> elt list
    
      val map_to : ('b -> 'a -> 'a) -> 'a -> (elt -> 'b) -> t -> 'a

    end

  module MSet : Utilsigs.OrderedContainer

  module FList : Utilsigs.BasicType
end

module Make (T : Utilsigs.BasicType) = struct
  (* module Set = Listset.Make(T) *)
  module Set = Treeset.Make (T)
  module Map = Treemap.Make (T)
  module Hashmap = Hashtbl.Make (T)
  module Hashset = 
    struct

      include Hashset.Make (T)

      let is_empty h = Stdlib.( = ) (cardinal h) 0

      let filter p h =
        iter (fun k -> if (p k) then remove h k) h 
    
      let map_to oadd oempty f xs = fold (fun z ys -> oadd (f z) ys) xs oempty
  
      let of_list l =
        let s = create (List.length l) in
        let () = List.iter (fun x -> add s x) l in
        s
    
      let to_list xs = map_to List.cons [] Fun.id xs
    
      let exists f h = fold (fun k acc -> acc || f k) h false
    
      let for_all f h = fold (fun k acc -> acc && f k) h true
    
      let to_string h =
        let elt_to_str k =
          "#" ^ string_of_int (T.hash k) ^ ": " ^ T.to_string k in
        let elems = fold (fun hd tl -> hd :: tl) h [] in
        String.concat ", " (List.map elt_to_str elems)
    
      let left_union h h' =
        let () = iter (fun x -> add h x) h' in
        h
      
    end
  module MSet = Multiset.Make (T)
  module FList = Flist.Make (T)
end
