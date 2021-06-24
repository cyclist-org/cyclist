open Lib

module Make (Prover : Prover.S) = struct
  module Seq = Prover.Seq

  type result_t = TIMEOUT | NOT_FOUND | SUCCESS of Prover.Proof.t

  let show_proof = ref false

  let latex_path = ref ""

  let open_file_for_append = ref false

  let timeout = ref 30

  let minbound = ref 1

  let maxbound = ref 11

  let speclist =
    ref (fun () ->
        [ ( "-m"
          , Arg.Set_int minbound
          , ": set starting depth for IDFS to <int>, default is "
            ^ string_of_int !minbound )
        ; ( "-M"
          , Arg.Set_int maxbound
          , ": set maximum depth for IDFS to <int>, 0 disables it, default is "
            ^ string_of_int !maxbound )
        ; ( "-L"
          , Arg.Int
              (fun n ->
                minbound := n ;
                maxbound := n )
          , ": set both depths to <int>." )
        ; ("-spot", Arg.Set Soundcheck.use_spot, ": use the spot model checker to verify the cyclic trace condition")
        ; ("-ext", Arg.Set Soundcheck.use_external, ": use external C++ code to verify the cyclic trace condition")
        ; ("-p", Arg.Set show_proof, ": show proof")
        ; ("-d", Arg.Set do_debug, ": print debug messages")
        ; ("-s", Arg.Set Stats.do_statistics, ": print statistics")
        ; ("-l", Arg.Set_string latex_path, ": write proofs to <file>")
        ; ( "-t"
          , Arg.Set_int timeout
          , ": set timeout in seconds to <int>, 0 disables it, default is "
            ^ string_of_int !timeout ) ] )

  let usage =
    ref ("usage: " ^ Sys.argv.(0) ^ " [-p/d/s] [-l <file>] [-t/m/M/L <int>] [-spot|-ext]")

  let die msg spec_list usage =
    print_endline msg ;
    print_endline (Arg.usage_string spec_list usage) ;
    exit 1

  let exit = function
    | TIMEOUT -> exit 2
    | NOT_FOUND -> exit 1
    | SUCCESS _ -> exit 0

  let gather_stats call =
    Stats.reset () ;
    Stats.Gen.call () ;
    let res =
      if not (Int.equal !timeout 0) then w_timeout call !timeout
      else Some (call ())
    in
    Stats.Gen.end_call () ;
    if !Stats.do_statistics then Stats.gen_print () ;
    res

  let process_result output seq res =
    if Option.is_none res then (
      if output then
        print_endline ("NOT proved: " ^ Seq.to_string seq ^ " [TIMEOUT]") ;
      TIMEOUT )
    else
      let res = Option.get res in
      if Option.is_none res then (
        if output then print_endline ("NOT proved: " ^ Seq.to_string seq) ;
        NOT_FOUND )
      else
        let proof = Option.get res in
        if !show_proof then Prover.Proof.pp Format.std_formatter proof
        else if output then print_endline ("Proved: " ^ Seq.to_string seq) ;
        if !Stats.do_statistics then Prover.print_proof_stats proof ;
        SUCCESS proof

  let idfs ax r seq =
    let maxbound = if Int.( < ) !maxbound 1 then max_int else !maxbound in
    Prover.idfs !minbound maxbound ax r seq

  let prove_seq ax r seq =
    Format.set_margin (Sys.command "exit $(tput cols)") ;
    let res = gather_stats (fun () -> idfs ax r seq) in
    process_result true seq res
end
