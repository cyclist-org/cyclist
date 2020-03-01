open Lib
open Generic
open Seplog
open While

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

let maxbound = ref 20

module Seq = Program.Seq
module Abducer = Abducer.Make (Program.Seq) (Defs)

let defs_count = ref 0

let record_defs defs =
  let fn = Filename.chop_extension (Filename.basename !prog_path) in
  let ext = Printf.sprintf "%.5d" !defs_count ^ ".defs" in
  let path_fn = Filename.concat !rec_defs_path (fn ^ ext) in
  let () = incr defs_count in
  let ch = open_out path_fn in
  let () = output_string ch (Defs.to_string (Abdrules.empify defs)) in
  let () = close_out ch in
  if Int.( > ) !defs_count 50000 then exit 0 else false

let prove_prog seq =
  (* seq_to_prove := Some seq ; *)
  Stats.reset () ;
  Stats.Gen.call () ;
  let res =
    w_timeout
      (fun () ->
        Abducer.bfs !maxbound Abdrules.rules seq Defs.empty
          (if !gen_defs then record_defs else Abdrules.is_sat) )
      !timeout
  in
  Stats.Gen.end_call () ;
  if !Stats.do_statistics then Stats.gen_print () ;
  if Option.is_none res then (
    print_endline ("NOT proved: " ^ Seq.to_string seq ^ " [TIMEOUT]") ;
    2 )
  else
    let res = Option.get res in
    if Option.is_none res then (
      print_endline ("NOT proved: " ^ Seq.to_string seq) ;
      1 )
    else
      let proof, defs = Option.get res in
      if !Stats.do_statistics then Abducer.print_proof_stats proof ;
      if !show_proof then print_endline (Abducer.Proof.to_string proof)
      else print_endline ("Proved: " ^ Program.Seq.to_string seq) ;
      if !show_defs || !simpl_defs then
        print_endline
          (Defs.to_string
             (( if !simpl_defs then Abdrules.simplify_defs
              else Abdrules.empify )
                defs)) ;
      0

let usage =
  "usage: " ^ Sys.argv.(0) ^ " [-g] [-p] [-d] [-l <file>] [-P <file>]"

let speclist =
  [ ( "-M"
    , Arg.Set_int maxbound
    , ": set maximum depth for BFS to <int>, default is "
      ^ string_of_int !maxbound )
  ; ("-p", Arg.Set show_proof, ": show proof")
  ; ("-pd", Arg.Set show_defs, ": show abduced definitions")
  ; ("-sd", Arg.Set simpl_defs, ": show simlpified abduced definitions")
  ; ("-d", Arg.Set do_debug, ": print debug messages")
  ; ("-l", Arg.Set_string latex_path, ": write latex proofs to <file>")
  ; ( "-ld"
    , Arg.Set latex_defs
    , ": output latex defs, default is " ^ string_of_bool !latex_defs )
  ; ("-s", Arg.Set Stats.do_statistics, ": print statistics")
  ; (*    ("-D", Arg.Set_string defs_path,                                        *)
    (*      ": read inductive definitions from <file>, default is " ^ !defs_path);*)
    ("-P", Arg.Set_string prog_path, ": prove termination of program <file>")
  ; ( "-t"
    , Arg.Set_int timeout
    , ": set timeout in seconds to <int>, 0 disables it, default is "
      ^ string_of_int !timeout )
  ; ("-g", Arg.Set gen_defs, ": fail and record all definitions.")
  ; ( "-T"
    , Arg.Set Program.termination
    , ": also prove termination, default is "
      ^ string_of_bool !Program.termination ) ]

let die msg =
  print_endline msg ;
  print_endline (Arg.usage_string speclist usage) ;
  exit 1

let () =
  Format.set_margin 300 ;
  Arg.parse speclist (fun _ -> raise (Arg.Bad "Stray argument found.")) usage ;
  if String.equal !prog_path "" then die "-P must be specified." ;
  let ((f, cmd) as seq) = Program.of_channel (open_in !prog_path) in
  Program.set_program cmd ;
  (* Safety_prover.setup [] ;  *)
  exit (prove_prog seq)
