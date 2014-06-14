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
let rexp = MParser.make_regexp "[a-zA-Z][_0-9a-zA-Z]*[']?" 
let parse_ident st = (regexp rexp << spaces <?> "Identifier") st

let handle_reply reply = 
  match reply with
  | Success res -> res 
  | Failed(msg,_) -> prerr_endline msg ; assert false

