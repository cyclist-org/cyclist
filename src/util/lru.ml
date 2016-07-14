module Make(T : Util.BasicType) = 
  struct
    module Dll =
      struct
        type 'a t = 
          { 
            mutable prev : 'a t;
            mutable next : 'a t; 
            data : T.t * 'a;  
          }
        
        let prev n = n.prev
        let next n = n.next 
        let get n = n.data
        
        let create d = 
          let rec n = { prev=n; data=d; next=n } in n
        
        let remove n =
          if n.next == n then failwith "Empty dll" else
          n.prev.next <- n.next;
          n.next.prev <- n.prev;
          n.next <- n;
          n.prev <- n;
          n
        
        let splice node1 node2 =
          let next = node1.next in
          let prev = node2.prev in
          node1.next <- node2;
          node2.prev <- node1;
          next.prev <- prev;
          prev.next <- next
        
        let pp fmt dll =
          let rec loop n = 
            Format.fprintf fmt "%a;" T.pp (fst n.data) ;
            if n.next==dll then () else loop n.next
          in
          Format.fprintf fmt "@[[";
          loop dll ;
          Format.fprintf fmt "]@]"
      end
    
    module HT = Hashtbl.Make(T)
          
    let lru_cache gen cap =
      let entries = ref None in
      let auxentries = HT.create cap in
      let len = ref 0 in
      let entry_gen k v =
        incr len;
        let n = Dll.create (k, v) in HT.add auxentries k n; n
      in
      let entry_find k dll =
        try 
          let n = HT.find auxentries k in ignore (Dll.remove n) ; n
        with Not_found -> entry_gen k (gen k)
      in
      let entry_remove n =
        let lru = Dll.prev n in
        let k = Dll.get lru |> fst in
        HT.remove auxentries k; ignore (Dll.remove lru); decr len
      in
      let get k =
        match !entries with 
        | Some dll -> (* not at head of list *)
          (* Format.fprintf Format.std_formatter "BEFORE: %a @." Dll.pp dll ; *)
          let (k0,v) = Dll.get dll in
          if T.equal k k0 then v (* special case head of list *)
          else begin
            let n = entry_find k dll in
            (* Put n at the head of the list *)
            Dll.splice n dll; entries := Some n;
            (* Remove the tail if over capacity *)
            if !len > cap then entry_remove n;
            (* return the value *)
            let res = Dll.get n |> snd in
            (* Format.fprintf Format.std_formatter "AFTER: %a @." Dll.pp n ;  *)
            res
          end
        | None -> (* no list - generate it *)
          let v = gen k in entries := Some (entry_gen k v); v
      in
      get
  end

(* module M = Make(Util.Int)                                                     *)
(* let id (x:int) = x                                                            *)
(* let cache = M.lru_cache id 9                                                  *)
(* let print i = let _ = Format.fprintf Format.std_formatter "i=%i@." i in () ;; *)
(* print (cache 1) ;                                                             *)
(* print (cache 2) ;                                                             *)
(* print (cache 3) ;                                                             *)
(* print (cache 4) ;                                                             *)
(* print (cache 5) ;                                                             *)
(* print (cache 6) ;                                                             *)
(* print (cache 7) ;                                                             *)
(* print (cache 8) ;                                                             *)
(* print (cache 9) ;                                                             *)
(* print (cache 10) ;                                                            *)
(* print (cache 11) ;                                                            *)
(* print (cache 5) ;                                                             *)
(* print (cache 7) ;                                                             *)
(* print (cache 9) ;                                                             *)

 