open Lib
open Generic

let default_defs_path = "examples/sl.defs"

let run defs_path show_proof timeout partition_strengthening cl_sequent () =
  gc_setup ();
  if partition_strengthening then Invalid.partition_strengthening := true;
  let seq = Seq.of_string cl_sequent in
  let defs = Defs.of_channel (open_in defs_path) in
  Rules.setup defs;
  Stats.reset ();
  Stats.Gen.call ();
  let call () = Invalid.invalidity_witness defs seq in
  let res =
    if not (Int.equal timeout 0) then w_timeout call timeout else Some (call ())
  in
  Stats.Gen.end_call ();
  if !Stats.do_statistics then Stats.gen_print ();
  let exit_code =
    match res with
    | None ->
        print_endline ("UNKNOWN: " ^ Seq.to_string seq ^ " [TIMEOUT]");
        2
    | Some (Some bp) ->
        print_endline ("INVALID: " ^ Seq.to_string seq);
        if show_proof then Format.printf "INVALID witness: %a\n" Basepair.pp bp;
        255
    | Some None ->
        print_endline ("UNKNOWN: " ^ Seq.to_string seq);
        1
  in
  exit exit_code

let cmd =
  let open Cmdliner in
  let defs =
    Arg.(
      value & opt file default_defs_path
      & info [ "D"; "defs" ] ~docv:"FILE"
          ~doc:"Read inductive definitions from $(docv).")
  in
  let show_proof =
    Arg.(
      value & flag
      & info [ "p"; "show-proof" ] ~doc:"Show the invalidity witness.")
  in
  let timeout =
    Arg.(
      value & opt int 60
      & info [ "t"; "timeout" ] ~docv:"SECONDS"
          ~doc:"Timeout in seconds. 0 disables it.")
  in
  let partition_strengthening =
    Arg.(
      value & flag
      & info
          [ "partition-strengthening" ]
          ~doc:"Use partition strengthening in the invalidity heuristic.")
  in
  let sequent =
    Arg.(
      required
      & opt (some string) None
      & info [ "S"; "sequent" ] ~docv:"SEQUENT"
          ~doc:"Disprove the separation logic sequent provided in $(docv).")
  in
  Cmd.v
    (Cmd.info "disprove"
       ~doc:"Show a separation logic entailment to be invalid."
       ~exits:
         (let open Cmd.Exit in
          [
            info ok ~doc:"never; the command reports 1, 2 or 255.";
            info 1 ~doc:"if the sequent could not be shown invalid.";
            info 2 ~doc:"on timeout.";
            info 255 ~doc:"if the sequent was shown to be invalid.";
            info cli_error ~doc:"on command line parsing errors.";
            info internal_error ~doc:"on unexpected internal errors.";
          ]))
    Term.(
      const run $ defs $ show_proof $ timeout $ partition_strengthening
      $ sequent $ Frontend.debug_term)
