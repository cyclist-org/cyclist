open Generic
open Seplog_ltl

let defs_path = ref "examples/sl.defs"
let prog_path = ref ""

module Prover = Prover.Make(Program.Seq)
module F = Frontend.Make(Prover)

let () = F.usage := !F.usage ^ " [-D <file] [-P <file>] [-IT]"

let () =
  let old_spec_thunk = !F.speclist in
  F.speclist :=
    (fun () -> old_spec_thunk() @ [
      ("-D", Arg.Set_string defs_path,
        ": read inductive definitions from <file>, default is " ^ !defs_path);
      ("-P", Arg.Set_string prog_path, ": prove temporal property of program <file>");
      ("-IT", Arg.Set Seplog.Rules.use_invalidity_heuristic,
      ": run invalidity heuristic during check, default is " ^
        (string_of_bool !Seplog.Rules.use_invalidity_heuristic));
    ])

let () =
  let spec_list = !F.speclist() in
  Arg.parse spec_list (fun _ -> raise (Arg.Bad "Stray argument found.")) !F.usage ;
  if !prog_path="" then F.die "-P must be specified." spec_list !F.usage ;
  let (seq, prog, tfext) = Program.of_channel (open_in !prog_path) in
  let prog = Program.Cmd.number prog in
  Program.set_program prog ;
  Rules.setup (Seplog.Defs.of_channel (open_in !defs_path));
  Program.Seq.pp Format.std_formatter (seq, prog, tfext) ;
  F.exit (F.prove_seq !Rules.axioms !Rules.rules (seq, prog, tfext))


