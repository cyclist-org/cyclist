let do_debug = ref false
let debug f = if !do_debug then print_endline (f ()) else ()

let skip_asserts = ref false
let require f = if !skip_asserts then () else assert (f ()) 

let pp_comma fmt () =
  Format.pp_print_char fmt ','

let pp_semicolonsp fmt () =
  Format.pp_print_char fmt ';' ; Format.pp_print_space fmt ()

let pp_commasp fmt () =
  pp_comma fmt () ; Format.pp_print_space fmt ()

let pp_star fmt () = Format.fprintf fmt " *@ "

let mk_to_string pp v =
  ignore (Format.flush_str_formatter ());
  Format.pp_set_margin Format.str_formatter 300;
  pp Format.str_formatter v ;
  Format.flush_str_formatter ()

let bracket s = "(" ^ s ^ ")"
let sqbracket s = "[" ^ s ^ "]"
let latex_bracket = bracket
let latex_sqbracket = sqbracket

let string_of_file fn =
  let cn = open_in fn in
  let a = ref "" in
  try
    while true do
      a := !a ^ (input_line cn)
    done ; 
    !a
  with End_of_file ->
    let () = close_in cn in !a

let gc_setup () =
  let cntrl = Gc.get () in 
  Gc.set 
    { 
      cntrl with 
        Gc.minor_heap_size = 10 * cntrl.Gc.minor_heap_size ; 
        Gc.major_heap_increment = 10 * cntrl.Gc.major_heap_increment 
    } 
  

(*module Funq =                                                          *)
(*  struct                                                               *)
(*    type 'a t = 'a list * 'a list                                      *)
(*                                                                       *)
(*    let make front back = match front with                             *)
(*      | [] -> (Blist.rev back, [])                                      *)
(*      | _  -> (front, back)                                            *)
(*                                                                       *)
(*    let put el (front, back) = make front (el::back)                   *)
(*                                                                       *)
(*    let hd (front, _) = match front with                               *)
(*      | []        -> raise Not_found                                   *)
(*      | el::front -> el                                                *)
(*                                                                       *)
(*    let tl (front, back) = match front with                            *)
(*      | []        -> raise Not_found                                   *)
(*      | el::front -> make front back                                   *)
(*                                                                       *)
(*    let take fq = (hd fq, tl fq)                                       *)
(*                                                                       *)
(*    let put_front el (front, back) = (el::front, back)                 *)
(*                                                                       *)
(*    let is_empty (front, _) = match front with                         *)
(*      | [] -> true                                                     *)
(*      | _  -> false                                                    *)
(*                                                                       *)
(*    let length (front, back) = (Blist.length front) + (Blist.length back)*)
(*    let singleton el = put el ([], [])                                 *)
(*    let empty = ([], [])                                               *)
(*  end                                                                  *)


(* module type PRIOQ =                                           *)
(*   sig                                                         *)
(*     type 'a left_heap                                         *)

(*     val mk_empty : ('a -> 'a -> int) -> ('a left_heap)        *)
(*     val put : 'a -> 'a left_heap -> 'a left_heap              *)
(*     val take : 'a left_heap -> ('a * 'a left_heap)            *)
(*     val hd : 'a left_heap -> 'a                               *)
(*     val tl : 'a left_heap -> 'a left_heap                     *)
(*     val is_empty : 'a left_heap -> bool                       *)
(*     val singleton : ('a -> 'a -> int) -> 'a -> ('a left_heap) *)
(*     val to_list : 'a left_heap -> 'a list                     *)
(*   end                                                         *)

(* module Prioq : PRIOQ =                                        *)
(*   struct                                                      *)
(*     type 'a heap = E | T of int * 'a * 'a heap * 'a heap      *)
(*     type 'a left_heap = ('a -> 'a -> int) * ('a heap)         *)
(*                                                               *)
(*     let rank = function E -> 0 | T (r,_,_,_) -> r             *)
(*                                                               *)
(*     let makeT x a b =                                         *)
(*       if rank a >= rank b then T (rank b + 1, x, a, b)        *)
(*       else T (rank a + 1, x, b, a)                            *)
(*                                                               *)
(*     let rec merge cmp h1 h2 = match h1, h2 with               *)
(*       | _, E -> h1                                            *)
(*       | E, _ -> h2                                            *)
(*       | T (_, x, a1, b1), T (_, y, a2, b2) ->                 *)
(*         if (cmp x y)<0 then makeT x a1 (merge cmp b1 h2)      *)
(*         else makeT y a2 (merge cmp h1 b2)                     *)
(*                                                               *)
(*     let insert cmp x h = merge cmp (T (1, x, E, E)) h         *)
(*                                                               *)
(*     let find_min = function                                   *)
(*       | E -> invalid_arg "find_min"                           *)
(*       | T (_, x, _, _) -> x                                   *)
(*                                                               *)
(*     let delete_min cmp = function                             *)
(*       | E -> invalid_arg "delete_min"                         *)
(*       | T (_, x, a, b) -> merge cmp a b                       *)
(*                                                               *)
(*     let mk_empty cmp = (cmp, E)                               *)
(*     let put el (cmp, h) = (cmp, insert cmp el h)              *)
(*     let hd (_, h) = find_min h                                *)
(*     let tl (cmp, h) = (cmp, delete_min cmp h)                 *)
(*     let take lh = (hd lh, tl lh)                              *)
(*     let is_empty (_, h) = match h with                        *)
(*       | E -> true                                             *)
(*       | _ -> false                                            *)
(*     let singleton cmp el = put el (mk_empty cmp)              *)
(*                                                               *)
(*     let rec to_list pq =                                      *)
(*       if is_empty pq then [] else                             *)
(*       let hd, tl = take pq in                                 *)
(*       hd :: (to_list tl)                                      *)
(*   end                                                         *)

exception Timeout
let sigalrm_handler = Sys.Signal_handle (fun _ -> raise Timeout)
let w_timeout f timeout =
  let old_behavior = Sys.signal Sys.sigalrm sigalrm_handler in
  let reset_sigalrm () = Sys.set_signal Sys.sigalrm old_behavior in
  if timeout > 0 then ignore (Unix.alarm timeout) ;
  try
    let res = f () in reset_sigalrm () ; Some res
  with Timeout -> (reset_sigalrm () ; None)

