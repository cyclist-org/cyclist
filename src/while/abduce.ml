open Lib
open Generic
module Seq = Program.Seq
module Abducer = Abducer.Make (Program.Seq) (Seplog.Defs)

(* Abduced definitions are dumped one file per candidate when [--gen-defs] is
   set, so the counter is state of the run rather than of the command. *)
let defs_count = ref 0

let record_defs rec_defs_path prog_path defs =
  let fn = Filename.chop_extension (Filename.basename prog_path) in
  let ext = Printf.sprintf "%.5d" !defs_count ^ ".defs" in
  let path_fn = Filename.concat rec_defs_path (fn ^ ext) in
  let () = incr defs_count in
  let ch = open_out path_fn in
  let () = output_string ch (Seplog.Defs.to_string (Abdrules.empify defs)) in
  let () = close_out ch in
  if Int.( > ) !defs_count 50000 then exit 0 else false

let prove_prog ~maxbound ~timeout ~show_proof ~show_defs ~simpl_defs ~is_sat seq
    =
  Stats.reset ();
  Stats.Gen.call ();
  let res =
    w_timeout
      (fun () ->
        Abducer.bfs maxbound Abdrules.rules seq Seplog.Defs.empty is_sat)
      timeout
  in
  Stats.Gen.end_call ();
  if !Stats.do_statistics then Stats.gen_print ();
  if Option.is_none res then (
    print_endline ("NOT proved: " ^ Seq.to_string seq ^ " [TIMEOUT]");
    2)
  else
    let res = Option.get res in
    if Option.is_none res then (
      print_endline ("NOT proved: " ^ Seq.to_string seq);
      1)
    else
      let proof, defs = Option.get res in
      if !Stats.do_statistics then Abducer.print_proof_stats proof;
      if show_proof then print_endline (Abducer.Proof.to_string proof)
      else print_endline ("Proved: " ^ Program.Seq.to_string seq);
      if show_defs || simpl_defs then
        print_endline
          (Seplog.Defs.to_string
             ((if simpl_defs then Abdrules.simplify_defs else Abdrules.empify)
                defs));
      0

let run maxbound show_proof show_defs simpl_defs prog_path timeout gen_defs
    rec_defs_path termination () =
  if termination then Program.termination := true;
  Format.set_margin 300;
  let ((_f, cmd) as seq) = Program.of_channel (open_in prog_path) in
  Program.set_program cmd;
  let is_sat =
    if gen_defs then record_defs rec_defs_path prog_path else Abdrules.is_sat
  in
  exit
    (prove_prog ~maxbound ~timeout ~show_proof ~show_defs ~simpl_defs ~is_sat
       seq)

let cmd =
  let open Cmdliner in
  let maxbound =
    Arg.(
      value & opt int 20
      & info [ "M"; "max-depth" ] ~docv:"INT"
          ~doc:"Maximum depth for the breadth-first search.")
  in
  let show_proof =
    Arg.(value & flag & info [ "p"; "show-proof" ] ~doc:"Show the proof.")
  in
  let show_defs =
    Arg.(
      value & flag & info [ "show-defs" ] ~doc:"Show the abduced definitions.")
  in
  let simpl_defs =
    Arg.(
      value & flag
      & info [ "show-simplified-defs" ]
          ~doc:"Show the simplified abduced definitions.")
  in
  let prog =
    Arg.(
      required
      & opt (some file) None
      & info [ "P"; "program" ] ~docv:"FILE"
          ~doc:"Abduce definitions for the program in $(docv).")
  in
  let timeout =
    Arg.(
      value & opt int 30
      & info [ "t"; "timeout" ] ~docv:"SECONDS"
          ~doc:"Timeout in seconds. 0 disables it.")
  in
  let gen_defs =
    Arg.(
      value & flag
      & info [ "g"; "gen-defs" ]
          ~doc:"Fail, and record every candidate set of definitions.")
  in
  let rec_defs_path =
    Arg.(
      value & opt dir "/tmp/recdefs/"
      & info [ "gen-defs-dir" ] ~docv:"DIR"
          ~doc:"Where $(b,--gen-defs) writes the recorded definitions.")
  in
  let termination =
    Arg.(
      value & flag & info [ "T"; "termination" ] ~doc:"Also prove termination.")
  in
  Cmd.v
    (Cmd.info "abduce" ~doc:"Abduce inductive definitions for a while program."
       ~exits:Frontend.exits)
    Term.(
      const run $ maxbound $ show_proof $ show_defs $ simpl_defs $ prog
      $ timeout $ gen_defs $ rec_defs_path $ termination $ Frontend.debug_term)
