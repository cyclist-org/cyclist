open Generic
open Asl
open Asl_while
open Asl_while_program

let prog_path = ref ""

module Prover = Prover.Make (Asl_while_program.Seq)
module F = Frontend.Make (Prover)

let () = F.maxbound := 18
let () = F.usage := !F.usage ^ " -P <file>"

let () =
  let old_spec_thunk = !F.speclist in
  F.speclist :=
    fun () ->
      old_spec_thunk ()
      @ [
          ( "-P",
            Arg.Set_string prog_path,
            ": prove memory safety of program <file>" );
          ( "-z3",
            Arg.Set_string Asl_sat.z3_path,
            ": use <file> as the z3 executable, default is " ^ !Asl_sat.z3_path
          );
        ]

let () =
  let spec_list = !F.speclist () in
  Arg.parse spec_list
    (fun _ -> raise (Arg.Bad "Stray argument found."))
    !F.usage;
  if String.equal !prog_path "" then
    F.die "-P must be specified." spec_list !F.usage;
  let seq, prog = Asl_while_program.of_channel (open_in !prog_path) in
  let prog = Cmd.number prog in
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
