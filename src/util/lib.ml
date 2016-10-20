let do_debug = ref false
let debug f = if !do_debug then print_endline (f ()) else ()

(* boost::hash_combine *)
(* size_t hash_combine( size_t lhs, size_t rhs ) {     *)
(*   lhs^= rhs + 0x9e3779b9 + (lhs << 6) + (lhs >> 2); *)
(*   return lhs;                                       *)
(* }                                                   *)
(* FIXME this should only produce >= 0  *)
let genhash h v = h lxor (v + (h lsl 5) + (h lsr 2))

module HashtablePrinter =
  struct
    module type S =
      sig
        type 'a t
        type key
        val to_string : (key -> string) -> ('a -> string) -> 'a t -> string
      end
    module Make(H : Hashtbl.S) : S with type 'a t = 'a H.t with type key = H.key =
      struct
        type 'a t = 'a H.t
        type key = H.key
        let to_string key_tos val_tos ht =
          let str_buffer = Buffer.create 0 in
          let entry_tos k v =
            Buffer.add_char str_buffer '(' ;
            Buffer.add_string str_buffer (key_tos k) ;
            Buffer.add_string str_buffer " -> " ;
            Buffer.add_string str_buffer (val_tos v) ;
            Buffer.add_string str_buffer ") " in
          let () = Buffer.add_string str_buffer "[ " in
          let () = H.iter entry_tos ht in
          let () = Buffer.add_char str_buffer ']' in
          let contents = Buffer.contents str_buffer in
          let () = Buffer.clear str_buffer in
          contents
      end
  end

let pp_comma fmt () =
  Format.pp_print_char fmt ','

let pp_semicolonsp fmt () =
  Format.pp_print_char fmt ';' ; Format.pp_print_space fmt ()

let pp_commasp fmt () =
  pp_comma fmt () ; Format.pp_print_space fmt ()

let pp_star fmt () = Format.fprintf fmt " *@ "

let mk_to_string pp v =
  ignore (Format.flush_str_formatter ());
  Format.pp_set_margin Format.str_formatter max_int;
  pp Format.str_formatter v ;
  Format.flush_str_formatter ()

(* let id s = s                                           *)

let rec fixpoint eq f x =
  let y = f x in if eq x y then x else fixpoint eq f y

let bracket s = "(" ^ s ^ ")"
let sqbracket s = "[" ^ s ^ "]"
let latex_bracket = bracket
let latex_sqbracket = sqbracket



(* FIXME should use buffers *)
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

let string_of_char c = String.make 1 c

let gc_setup () =
  let cntrl = Gc.get () in
  Gc.set
    {
      cntrl with
        Gc.minor_heap_size = 10 * cntrl.Gc.minor_heap_size ;
        Gc.major_heap_increment = 10 * cntrl.Gc.major_heap_increment
    }

exception Timeout
let sigalrm_handler = Sys.Signal_handle (fun _ -> raise Timeout)
let w_timeout f timeout =
  let old_behavior = Sys.signal Sys.sigalrm sigalrm_handler in
  let reset_sigalrm () = Sys.set_signal Sys.sigalrm old_behavior in
  if timeout > 0 then ignore (Unix.alarm timeout) ;
  try
    let res = f () in reset_sigalrm () ; Some res
  with Timeout -> (reset_sigalrm () ; None)

open MParser
let rexp = MParser_PCRE.make_regexp "[a-zA-Z][_0-9a-zA-Z]*[']?" 
let parse_ident st = (MParser_PCRE.regexp rexp << spaces <?> "Identifier") st

let handle_reply reply =
  match reply with
  | Success res -> res
  | Failed(msg,_) -> prerr_endline msg ; assert false

let runtest name tst =
  let bt = Printexc.backtrace_status () in
  let () = Printexc.record_backtrace true in
  try
    tst ()
  with 
  | Assert_failure(_, line, _) ->
    begin
      Printf.eprintf "Test `%s' failed, line %d.\n" name line ;
      exit (-1)
    end
  | exn ->
    begin
      let sep = String.make 72 '=' in
      Printf.eprintf 
        "Test `%s' threw exception `%s'.\nBacktrace:\n%s\n%s%s\n" 
        name  (Printexc.to_string exn) sep (Printexc.get_backtrace ()) sep;
      let () = exit (-1) in ()
    end ;
  Printexc.record_backtrace bt

    
let mk_of_string parse =
  fun s -> handle_reply (MParser.parse_string parse s ())
  
