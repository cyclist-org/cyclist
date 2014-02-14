open Lib

module Make(Prover: Sigs.P)(Seq: Sigs.S with type t = Prover.sequent) = 
  struct
    let show_proof = ref false 
    let latex_path = ref ""
    let timeout = ref 30

    let speclist = ref [
        ("-m", Arg.Set_int Prover.minbound, 
          (": set starting depth for IDFS to <int>, default is " ^ 
            (string_of_int !Prover.minbound)));
        ("-M", Arg.Set_int Prover.maxbound, 
          (": set maximum depth for IDFS to <int>, default is " ^ 
            (string_of_int !Prover.maxbound)));
        ("-L", Arg.Int 
          (fun n -> Prover.minbound := n ; Prover.maxbound := n), 
          ": set both depths to <int>.");
        ("-p", Arg.Set show_proof,": show proof");
        ("-d", Arg.Set do_debug,": print debug messages");
        ("-s", Arg.Set Stats.do_statistics,": print statistics");
        ("-l", Arg.Set_string latex_path, ": write proofs to <file>");
        ("-a", Arg.Set Prover.ancestral_links_only,": only create backlinks to ancestral nodes, default is " ^ (string_of_bool !Prover.ancestral_links_only));
        ("-z", Arg.Set Prover.lazy_soundness_check,": check backlink soundness lazily, default is " ^ (string_of_bool !Prover.lazy_soundness_check));
        ("-t", Arg.Set_int timeout, 
          (": set timeout in seconds to <int>, 0 disables it, default is " ^ 
            (string_of_int !timeout)));
        ("-e", Arg.Set Prover.expand_proof,": expand compound proof steps, default is " ^ (string_of_bool !Prover.expand_proof));
      ]

    let usage = ref ("usage: " ^ Sys.argv.(0) ^ " [-p/e/d/s/a/z] [-m/M/L/t <int>] [-l <file>]")

    let die msg =
      print_endline msg ;
      print_endline (Arg.usage_string !speclist !usage) ;
      exit 1

    let prove_seq seq =
      Format.set_margin (Sys.command "exit $(tput cols)") ;
      Stats.reset () ;
      Stats.Gen.call () ;
      let res = if !timeout<>0 then 
				w_timeout (fun () -> Prover.idfs seq) !timeout
			else
				Some (Prover.idfs seq) in
      Stats.Gen.end_call () ;
      if Option.is_none res then
        (print_endline ("NOT proved: " ^ (Seq.to_string seq) ^ " [TIMEOUT]") ; 2) else
      let res = Option.get res in
      if Option.is_none res then
        (print_endline ("NOT proved: " ^ (Seq.to_string seq)) ; 1) else
      let proof = Option.get res in 
      if !show_proof then 
        Prover.pp_proof Format.std_formatter proof
      else
        print_endline ("Proved: " ^ (Seq.to_string seq)) ;
      if !Stats.do_statistics then 
      begin
        Stats.gen_print ();
        Prover.print_proof_stats proof
      end ;
      if !latex_path<>"" then 
      begin
        let ch = 
          open_out_gen [Open_creat; Open_wronly; Open_trunc] 402 !latex_path in
        Prover.melt_proof ch proof ; close_out ch
      end ;
      0
  end
