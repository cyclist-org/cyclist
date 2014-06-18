open Lib
open Symheap

let defs_path = ref "examples/sl.defs"

module F = Slsat_frontend

let () = F.usage := !F.usage ^ " [-D <file>]" 

let () = F.speclist := !F.speclist @ [
       ("-D", Arg.Set_string defs_path,
           ": read inductive definitions from <file>, default is " ^ !defs_path);
	]

let () =
  gc_setup () ;
	Arg.parse !F.speclist (fun _ -> raise (Arg.Bad "Stray argument found.")) !F.usage ;
	let res = F.check_consistency (Sl_defs.of_channel (open_in !defs_path)) in
  let () = print_endline ("Exit code: " ^ (string_of_int res)) in 
	exit res


