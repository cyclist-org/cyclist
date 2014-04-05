open Lib
open Util

let show_proof = ref false 
let show_defs = ref false
(*let defs_path = ref "examples/sl.defs"*)
let prog_path = ref ""
let latex_path = ref ""
let latex_defs = ref false
let timeout = ref 30
let minbound = ref 1
let maxbound = ref 20

module Seq = Goto_program.Seq
module Parser = Sl_parser
module Lexer = Sl_lexer
module Abducer = Abducer.Make(Goto_program.Seq)(Goto_program.Defs)

let program_of_channel c =
  let lexbuf = Lexing.from_channel c in
  try
    Sl_parser.program Sl_lexer.token lexbuf
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

(* let defs_found : (Program.Defs.t * int) Stack.t = Stack.create () *)
(* let seq_to_prove = ref None                                       *)

(* let record_defs defs =                                                                 *)
(*   let defs = Goto_abdrules.simplify_defs defs in                                           *)
(*   if not (Goto_abdrules.is_possibly_consistent defs) then false else                       *)
(*   let () = Prprover.setup defs (Option.get !seq_to_prove) in                           *)
(*   let res = w_timeout (fun () -> Prprover.idfs (Option.get !seq_to_prove)) !timeout in *)
(*   if Option.is_none res then                                                           *)
(*     (Stack.push (defs, -100) defs_found; false) else                                   *)
(*   let res = Option.get res in                                                          *)
(*   if Option.is_none res then                                                           *)
(*     (Stack.push (defs, -50) defs_found ; false) else                                   *)
(*   let prf = Option.get res in                                                          *)
(*   let c = Prprover.coverage prf in                                                     *)
(*   Stack.push (defs, c) defs_found ; c = 100                                            *)


let prove_prog seq =
  (* seq_to_prove := Some seq ; *)
  Stats.reset () ;
  Stats.Gen.call () ; 
  let res =  
    w_timeout
      (fun () ->
        Abducer.bfs !minbound !maxbound Goto_abdrules.ruleset seq [] Goto_abdrules.is_possibly_consistent) 
      !timeout
      in
  Stats.Gen.end_call () ;
  (* Stack.iter                                                                     *)
  (*   begin fun (defs, cov) ->                                                     *)
  (*     print_endline (">>> Definitions found, coverage=" ^ (string_of_int cov)) ; *)
  (*     print_endline (Program.Defs.to_string (Goto_abdrules.simplify_defs defs));     *)
  (*     print_endline "" ;                                                         *)
  (*   end                                                                          *)
  (*   defs_found ;                                                                 *)
  if Option.is_none res then
    (print_endline ("NOT proved: " ^ (Seq.to_string seq) ^ " [TIMEOUT]") ; 2) else
  let res = Option.get res in
  if Option.is_none res then
    (print_endline ("NOT proved: " ^ (Seq.to_string seq)) ; 1) else
  let (proof, defs) = Option.get res in
  if !show_proof then
    print_endline (Abducer.Proof.to_string proof)
  else
    print_endline ("Proved: " ^ (Goto_program.Seq.to_string seq)) ;
  if !show_defs then  
    print_endline (Goto_program.Defs.to_string 
      (Goto_abdrules.simplify_defs
      defs
      )
      );
  if !Stats.do_statistics then 
  begin
    Stats.gen_print ();
    Abducer.print_proof_stats proof
  end ;
  if !latex_path<>"" then 
  begin
    let ch = open_out_gen [Open_creat; Open_wronly; Open_trunc] 402 !latex_path in
    Abducer.melt_proof ch proof ; close_out ch
  end ;
  if !latex_defs then 
    ignore (Latex.to_channel ~mode:Latex.M stdout (Symheap.Defs.to_melt (Goto_abdrules.simplify_defs defs)));
  (* Prprover.setup (Goto_abdrules.simplify_defs defs) seq;                                      *)
  (* let res = w_timeout (fun () -> Prprover.idfs seq) !timeout in                           *)
  (* if Option.is_none res then                                                              *)
  (*   (print_endline ("NOT verified: " ^ (Seq.to_string seq) ^ " [TIMEOUT]") ; 2) else      *)
  (* let res = Option.get res in                                                             *)
  (* if Option.is_none res then                                                              *)
  (*   (print_endline ("NOT verified: " ^ (Seq.to_string seq)) ; 1) else                     *)
  (* let prf = Option.get res in                                                             *)
  (* print_endline ("Verified: coverage=" ^ (string_of_int (Prprover.coverage prf)) ^ "%") ; *)
  0


let usage = 
  "usage: " ^ Sys.argv.(0) ^ " [-p] [-d] [-l <file>] [-P <file>]"

let speclist = [
    ("-m", Arg.Set_int minbound, 
      (": set starting depth for IDFS to <int>, default is " ^ (string_of_int !minbound)));
    ("-M", Arg.Set_int maxbound, 
      (": set maximum depth for IDFS/BFS to <int>, default is " ^ (string_of_int !maxbound)));
    ("-L", Arg.Int (fun n -> minbound := n ; maxbound := n), ": set both depths to <int>.");
    ("-p", Arg.Set show_proof,": show proof");
    ("-pd", Arg.Set show_defs,": show abduced definitions");
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
   
  ]

let die msg =
  print_endline msg ;
  print_endline (Arg.usage_string speclist usage) ;
  exit 1

let () =
  Format.set_margin 300 ;
  Arg.parse speclist (fun _ -> raise (Arg.Bad "Stray argument found.")) usage ;
  if !prog_path="" then die "-P must be specified." ;
  let (seq, prog) = program_of_channel (open_in !prog_path) in
  Goto_program.set_program prog ; 
  (* Prprover.setup [] seq ;  *)
  exit (prove_prog seq)
    


