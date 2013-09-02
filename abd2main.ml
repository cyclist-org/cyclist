open Lib
open Util

let show_proof = ref false 
let show_defs = ref false
let simpl_defs = ref false
(*let defs_path = ref "examples/sl.defs"*)
let prog_path = ref ""
let latex_path = ref ""
let latex_defs = ref false
let timeout = ref 30
let rec_defs_path = ref "/tmp/recdefs/"
let gen_defs = ref false

module Seq = While_program.Seq
module Parser = While_parser
module Lexer = While_lexer

let program_of_channel c =
  let lexbuf = Lexing.from_channel c in
  try
    Parser.program Lexer.token lexbuf
  with
    | Lexer.Error msg -> print_endline msg ; assert false
    | Parser.Error ->
      begin
        let curr = lexbuf.Lexing.lex_curr_p in
        let line = curr.Lexing.pos_lnum in
        let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
        let tok = Lexing.lexeme lexbuf in
        Printf.fprintf stderr "Syntax error at line %d, column %d: token '%s'.\n%!" line cnum tok ; assert false
      end

let defs_count = ref 0

let record_defs defs =
  let fn = Filename.chop_extension (Filename.basename !prog_path) in
  let ext = (Printf.sprintf "%.5d" !defs_count) ^ ".defs" in
  let path_fn = Filename.concat !rec_defs_path (fn ^ ext) in
  let () = incr defs_count in
  let ch = open_out path_fn in
  let () = output_string ch (While_program.Defs.to_string (While_abdprover.empify defs)) in
  let () = close_out ch in
	if !defs_count>50000 then exit 0 else false 

let prove_prog seq =
  (* seq_to_prove := Some seq ; *)
  Stats.reset () ;
  Stats.Gen.call () ; 
  let res =  
    w_timeout
      (fun () ->
        While_abdprover.abduce seq [] While_abdprover.mk_rules 
        (if !gen_defs then record_defs else While_abdprover.is_possibly_consistent)) 
      !timeout
      in
  Stats.Gen.end_call () ;
  if !Stats.do_statistics then Stats.gen_print ();
  if Option.is_none res then
    (print_endline ("NOT proved: " ^ (Seq.to_string seq) ^ " [TIMEOUT]") ; 2) else
  let res = Option.get res in
  if Option.is_none res then
    (print_endline ("NOT proved: " ^ (Seq.to_string seq)) ; 1) else
  let (proof, defs) = Option.get res in
  if !Stats.do_statistics then While_abdprover.print_proof_stats proof ;
  if !show_proof then
    While_abdprover.print_proof proof
  else
    print_endline ("Proved: " ^ (While_program.Seq.to_string seq)) ;
  if !show_defs || !simpl_defs then  
    print_endline (While_program.Defs.to_string  
      (( if !simpl_defs then While_abdprover.simplify_defs else While_abdprover.empify ) defs));
  if !latex_path<>"" then 
  begin
    let ch = open_out_gen [Open_creat; Open_wronly; Open_trunc] 402 !latex_path in
    While_abdprover.melt_proof ch proof ; close_out ch
  end ;
  if !latex_defs then 
    ignore (Latex.to_channel ~mode:Latex.M stdout (Symheap.Defs.to_melt (While_abdprover.simplify_defs defs)));
  0


let usage = 
  "usage: " ^ Sys.argv.(0) ^ " [-g] [-p] [-d] [-l <file>] [-P <file>]"

let () = While_abdprover.maxbound := 20 

let speclist = [
    ("-m", Arg.Set_int While_abdprover.minbound, 
      (": set starting depth for IDFS to <int>, default is " ^ (string_of_int !While_abdprover.minbound)));
    ("-M", Arg.Set_int While_abdprover.maxbound, 
      (": set maximum depth for IDFS/BFS to <int>, default is " ^ (string_of_int !While_abdprover.maxbound)));
    ("-L", Arg.Int (fun n -> While_abdprover.minbound := n ; While_abdprover.maxbound := n), ": set both depths to <int>.");
    ("-p", Arg.Set show_proof,": show proof");
    ("-pd", Arg.Set show_defs,": show abduced definitions");
    ("-sd", Arg.Set simpl_defs,": show simlpified abduced definitions");
    ("-d", Arg.Set do_debug,": print debug messages");
    ("-l", Arg.Set_string latex_path, ": write latex proofs to <file>");
    ("-ld", Arg.Set latex_defs, ": output latex defs, default is " ^ (string_of_bool !latex_defs));
    ("-s", Arg.Set Stats.do_statistics,": print statistics");
(*    ("-D", Arg.Set_string defs_path,                                        *)
(*      ": read inductive definitions from <file>, default is " ^ !defs_path);*)
    ("-P", Arg.Set_string prog_path, ": prove termination of program <file>");
    ("-t", Arg.Set_int timeout, 
      (": set timeout in seconds to <int>, 0 disables it, default is " ^ 
      (string_of_int !timeout)));
    ("-g", Arg.Set gen_defs, ": fail and record all definitions.");
    ("-T", Arg.Set While_program.termination, ": also prove termination, default is " ^ 
		  (string_of_bool !While_program.termination));
		
  ]

let die msg =
  print_endline msg ;
  print_endline (Arg.usage_string speclist usage) ;
  exit 1

let () =
  Format.set_margin 300 ;
  Arg.parse speclist (fun _ -> raise (Arg.Bad "Stray argument found.")) usage ;
  if !prog_path="" then die "-P must be specified." ;
  let ((f, cmd) as seq) = program_of_channel (open_in !prog_path) in
  While_program.set_program cmd ; 
  (* Safety_prover.setup [] ;  *)
  While_abdprover.setup () ;
  exit (prove_prog seq)
    


