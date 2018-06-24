open Lib

module Make(T : Utilsigs.BasicType) =
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
          if n.next != n then
            begin
              n.prev.next <- n.next;
              n.next.prev <- n.prev;
              n.next <- n;
              n.prev <- n
            end ;
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

    let lru_cache_rec gen cap =
      assert (Int.(>) cap 0) ;
      let entries = ref None in
      let auxentries = HT.create cap in
      let len = ref 0 in
      let rec get k =
        (* Format.fprintf Format.std_formatter "ASKING for %a@." T.pp k ; *)
        let (n, v) =
          (* if Option.is_some !entries then                                                     *)
          (*   Format.fprintf Format.std_formatter "BEFORE: %a @." Dll.pp (Option.get !entries)  *)
          (* else                                                                                *)
          (*   Format.fprintf Format.std_formatter "BEFORE: <empty> @." ;                        *)
          try
            let n = HT.find auxentries k in (* success means non-empty *)
            (* Format.fprintf Format.std_formatter "FOUND. @." ; *)
            let v = Dll.get n |> snd in
            let dll = Option.get !entries in
            let _ = Dll.remove n in (* remove n from current position *)
            Dll.splice n dll; (* Put n at the head of the list *)
            (n, v)
          with Not_found ->
            (* Format.fprintf Format.std_formatter "NOT FOUND. @." ; *)
            let v = gen get k in (* this may recursively call us *)
            incr len;
            let n = Dll.create (k, v) in
            HT.add auxentries k n ;
            (* Put n at the head of the list, if non-empty *)
            Option.iter (fun dll -> Dll.splice n dll) !entries ;
            (n, v)
          in
          if Int.(>) !len cap then
            begin
              let lru = Dll.prev n in
              HT.remove auxentries (Dll.get lru |> fst);
              ignore (Dll.remove lru);
              decr len
            end ;
          entries := Some n;
          (* Format.fprintf Format.std_formatter "AFTER: %a @." Dll.pp n ; *)
          v
      in
      get

    let lru_cache gen cap =
      lru_cache_rec (fun _ x -> gen x) cap
  end

(* module M = Make(Int)  *)
(* let id (x:int) = x                                              *)
(* let cache = M.lru_cache id 9                                    *)
(* let print i = Format.fprintf Format.std_formatter "i=%i@." i ;; *)
(* print (cache 1) ;                                               *)
(* print (cache 1) ;                                               *)
(* print (cache 2) ;                                               *)
(* print (cache 3) ;                                               *)
(* print (cache 4) ;                                               *)
(* print (cache 5) ;                                               *)
(* print (cache 6) ;                                               *)
(* print (cache 7) ;                                               *)
(* print (cache 8) ;                                               *)
(* print (cache 9) ;                                               *)
(* print (cache 10) ;                                              *)
(* print (cache 11) ;                                              *)
(* print (cache 5) ;                                               *)
(* print (cache 7) ;                                               *)
(* print (cache 9) ;                                               *)
(* print (cache 15) ;;                                             *)

(* let fib f = function                                             *)
(*  | 0 -> 0                                                        *)
(*  | 1 -> 1                                                        *)
(*  | n -> f (n-1) + f (n-2)                                        *)
(* let cache = M.lru_cache_rec fib 6                                *)
(* let print i = Format.fprintf Format.std_formatter "i=%i@." i  ;; *)
(* print (cache 10)                                                 *)
