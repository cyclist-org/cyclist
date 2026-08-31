open Generic
open Asl
module Prover = Prover.Make (Asl_while_program.Seq)
module F = Frontend.Make (Prover)

let run prog_path z3_path () =
  Option.iter (fun p -> Asl_sat.z3_path := p) z3_path;
  let seq, prog = Asl_while_program.of_channel (open_in prog_path) in
  let prog = Asl_while_program.Cmd.number prog in
  Asl_while_program.set_program prog;
  Asl_while_rules.setup ();
  let start = Unix.gettimeofday () in
  let res =
    F.prove_seq !Asl_while_rules.axioms !Asl_while_rules.rules (seq, prog)
  in
  let stop = Unix.gettimeofday () in
  Printf.printf "Execution time: %fs\n%!" (stop -. start);
  Printf.printf "z3 called %i times.\n" !Asl_sat.times;
  F.exit res

let cmd =
  let open Cmdliner in
  let prog =
    Arg.(
      required
      & opt (some file) None
      & info [ "P"; "program" ] ~docv:"FILE"
          ~doc:"Prove memory safety of the program in $(docv).")
  in
  let z3 =
    Arg.(
      value
      & opt (some string) None
      & info [ "z3" ] ~docv:"FILE"
          ~doc:
            ("Use $(docv) as the z3 executable, default is " ^ !Asl_sat.z3_path
           ^ "."))
  in
  Cmd.v
    (Cmd.info "prove"
       ~doc:
         "Prove memory safety of a while program over array separation logic."
       ~exits:Frontend.exits)
    Term.(const run $ prog $ z3 $ F.common_term ~max_depth:18 ())
