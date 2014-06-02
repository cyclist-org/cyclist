open Lib
open Symheap

(* goal: also allow checking consistency of a predicate given by the user  *)
(* let cl_predicate = ref ""                                               *)
let defs_path = ref "examples/sl.defs"

module Parser = Sl_parser
module Lexer = Sl_lexer
module F = Slsat_frontend


let defs_of_channel c =
	let lexbuf = Lexing.from_channel c in
	try
		Parser.ind_def_set Lexer.token lexbuf
	with
	| Lexer.Error msg -> print_endline msg ; assert false
	| Parser.Error -> Printf.fprintf stderr "At offset %d: syntax error.\n%!" (Lexing.lexeme_start lexbuf) ; assert false

let () = F.usage := !F.usage ^ " [-D <file>]" 

let () = F.speclist := !F.speclist @ [
       ("-D", Arg.Set_string defs_path,
           ": read inductive definitions from <file>, default is " ^ !defs_path);
	]


let () =
  gc_setup () ;
	Arg.parse !F.speclist (fun _ -> raise (Arg.Bad "Stray argument found.")) !F.usage ;
	let res = F.check_consistency (defs_of_channel (open_in !defs_path)) in
  let () = print_endline ("Exit code: " ^ (string_of_int res)) in 
	exit res


