open Lib

module Make(Prover : Sigs.PROVER) = 
  struct
    module Seq = Prover.Seq
    
    let show_proof = ref false 
    let latex_path = ref ""
    let timeout = ref 30
    let minbound = ref 1
    let maxbound = ref 11

    let speclist = ref [
      ("-m", Arg.Set_int minbound, 
        (": set starting depth for IDFS to <int>, default is " ^ 
          (string_of_int !minbound)));
      ("-M", Arg.Set_int maxbound, 
        (": set maximum depth for IDFS to <int>, 0 disables it, default is " ^ 
          (string_of_int !maxbound)));
      ("-L", Arg.Int 
        (fun n -> minbound := n ; maxbound := n), 
        ": set both depths to <int>.");
      ("-p", Arg.Set show_proof,": show proof");
      ("-d", Arg.Set do_debug,": print debug messages");
      ("-s", Arg.Set Stats.do_statistics,": print statistics");
      ("-l", Arg.Set_string latex_path, ": write proofs to <file>");
      ("-t", Arg.Set_int timeout, 
        (": set timeout in seconds to <int>, 0 disables it, default is " ^ 
          (string_of_int !timeout)));
    ]

    let usage = 
      ref ("usage: " ^ Sys.argv.(0) ^ " [-p/d/s] [-l <file>] [-t/m/M/L <int>] <sequent>")

    let die msg =
      print_endline msg ;
      print_endline (Arg.usage_string !speclist !usage) ;
      exit 1

    let prove_seq ax r seq =
      Format.set_margin (Sys.command "exit $(tput cols)") ;
      Stats.reset () ;
      Stats.Gen.call () ;
      let maxbound = if !maxbound < 1 then max_int else !maxbound in
      let call () = Prover.idfs !minbound maxbound ax r seq in
      let res = if !timeout<>0 then
				w_timeout call  !timeout
			else
				Some (call ()) in
      Stats.Gen.end_call () ;
      if !Stats.do_statistics then Stats.gen_print ();
      if Option.is_none res then
        (print_endline ("NOT proved: " ^ (Seq.to_string seq) ^ " [TIMEOUT]") ; 2) else
      let res = Option.get res in
      if Option.is_none res then
        (print_endline ("NOT proved: " ^ (Seq.to_string seq)) ; 1) else
      let proof = Option.get res in
      if !show_proof then
        Prover.Proof.pp Format.std_formatter proof
      else
        print_endline ("Proved: " ^ (Seq.to_string seq)) ;
      if !Stats.do_statistics then Prover.print_proof_stats proof ;
      if !latex_path<>"" then
      begin
        let ch =
          open_out_gen [Open_creat; Open_wronly; Open_trunc] 402 !latex_path in
        Prover.melt_proof ch proof ; close_out ch
      end ;
      0
  end
