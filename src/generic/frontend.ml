open Lib

(* Exit codes common to the prover commands. [Cmdliner.Cmd.Exit.defaults] is
   not reused because it documents 123 (some_error), which no command returns,
   and omits the 1 and 2 that the provers do return. *)
let exits =
  let open Cmdliner.Cmd.Exit in
  [
    info ok ~doc:"on a successful proof.";
    info 1 ~doc:"if no proof was found.";
    info 2 ~doc:"on timeout.";
    info cli_error ~doc:"on command line parsing errors.";
    info internal_error ~doc:"on unexpected internal errors.";
  ]

(* The output options that every command has, whether or not it drives the
   iterative-deepening prover. *)
let debug_term =
  let open Cmdliner in
  let d =
    Arg.(value & flag & info [ "d"; "debug" ] ~doc:"Print debug messages.")
  in
  let s = Arg.(value & flag & info [ "s"; "stats" ] ~doc:"Print statistics.") in
  let id =
    Arg.(
      value & opt string !run_identifier
      & info [ "id" ] ~docv:"ID"
          ~doc:"Identifier for this execution, used in debug output.")
  in
  let apply d s id =
    if d then do_debug := true;
    if s then Stats.do_statistics := true;
    run_identifier := id
  in
  Term.(const apply $ d $ s $ id)

module Make (Prover : Prover.S) = struct
  module Seq = Prover.Seq

  type result_t = TIMEOUT | NOT_FOUND | SUCCESS of Prover.Proof.t

  let show_proof = ref false
  let use_dot = ref false
  let timeout = ref 30
  let minbound = ref 1
  let maxbound = ref 11

  (* The search and output options common to every prover command. The
     defaults are parameters rather than pre-set mutations of the refs above,
     so that a command's choice of search bounds is visible in its own
     `--help` output. *)
  let term ?(min_depth = !minbound) ?(max_depth = !maxbound)
      ?(timeout_secs = !timeout) () =
    let open Cmdliner in
    let m =
      Arg.(
        value & opt int min_depth
        & info [ "m"; "min-depth" ] ~docv:"INT"
            ~doc:"Starting depth for iterative-deepening search.")
    in
    let mm =
      Arg.(
        value & opt int max_depth
        & info [ "M"; "max-depth" ] ~docv:"INT"
            ~doc:
              "Maximum depth for iterative-deepening search. 0 disables the \
               bound.")
    in
    let l =
      Arg.(
        value
        & opt (some int) None
        & info [ "L"; "depth" ] ~docv:"INT"
            ~doc:"Set both the starting and the maximum search depth.")
    in
    let p =
      Arg.(value & flag & info [ "p"; "show-proof" ] ~doc:"Show the proof.")
    in
    let dot =
      Arg.(value & flag & info [ "dot" ] ~doc:"Use DOT format for proofs.")
    in
    let t =
      Arg.(
        value & opt int timeout_secs
        & info [ "t"; "timeout" ] ~docv:"SECONDS"
            ~doc:"Timeout in seconds. 0 disables it.")
    in
    let apply () m' mm' l' p' dot' t' =
      minbound := m';
      maxbound := mm';
      Option.iter
        (fun n ->
          minbound := n;
          maxbound := n)
        l';
      show_proof := p';
      use_dot := dot';
      timeout := t'
    in
    Term.(const apply $ debug_term $ m $ mm $ l $ p $ dot $ t)

  (* The common options of every prover command: the search and output
     settings above, together with the infinite descent check settings. *)
  let common_term ?min_depth ?max_depth ?timeout_secs () =
    Cmdliner.Term.(
      const (fun () () -> ())
      $ term ?min_depth ?max_depth ?timeout_secs ()
      $ Soundcheck.term)

  let exit = function
    | TIMEOUT -> exit 2
    | NOT_FOUND -> exit 1
    | SUCCESS _ -> exit 0

  let gather_stats call =
    Stats.reset ();
    Stats.Gen.call ();
    let res =
      if not (Int.equal !timeout 0) then w_timeout call !timeout
      else Some (call ())
    in
    Stats.Gen.end_call ();
    if !Stats.do_statistics then Stats.gen_print ();
    res

  let process_result output seq res =
    match res with
    | None ->
        if output then
          print_endline ("NOT proved: " ^ Seq.to_string seq ^ " [TIMEOUT]");
        TIMEOUT
    | Some None ->
        if output then print_endline ("NOT proved: " ^ Seq.to_string seq);
        NOT_FOUND
    | Some (Some proof) ->
        if !show_proof then
          let pp = if !use_dot then Prover.Proof.pp_dot else Prover.Proof.pp in
          pp Format.std_formatter proof
        else if output then print_endline ("Proved: " ^ Seq.to_string seq);
        if !Stats.do_statistics then Prover.print_proof_stats proof;
        SUCCESS proof

  let idfs ax r seq =
    let maxbound = if Int.( < ) !maxbound 1 then max_int else !maxbound in
    Prover.idfs !minbound maxbound ax r seq

  let prove_seq ax r seq =
    let res = gather_stats (fun () -> idfs ax r seq) in
    process_result true seq res
end
