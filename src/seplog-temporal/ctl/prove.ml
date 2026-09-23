open Generic
open Seplog_ctl

let defs_path = ref "examples/sl.defs"
let prog_path = ref ""

module Prover = Prover.Make(Seq)
module F = Frontend.Make(Prover)

let () = F.usage := !F.usage ^ " [-D <file>] [-Lem <int>] [-IT] -P <file>"

let () =
  let old_spec_thunk = !F.speclist in
  F.speclist :=
    (fun () -> old_spec_thunk() @ [
      ("-D", Arg.Set_string defs_path,
        ": read inductive definitions from <file>, default is " ^ !defs_path);
      ("-Lem", Arg.Int Seplog.Rules.set_lemma_level,
        ": specify the permissiveness of the lemma application strategy\n" ^
          Seplog.Rules.lemma_option_descr_str());
      ("-IT", Arg.Set Seplog.Rules.use_invalidity_heuristic,
       ": run invalidity heuristic during check, default is " ^
         (string_of_bool !Seplog.Rules.use_invalidity_heuristic));
      ("-P", Arg.Set_string prog_path, ": prove temporal property of program <file>");
    ])

let () =
  let spec_list = !F.speclist() in
  Arg.parse spec_list (fun _ -> raise (Arg.Bad "Stray argument found.")) !F.usage ;
  if !prog_path="" then F.die "-P must be specified." spec_list !F.usage ;
  let (seq, prog, tfext) = Program.of_channel (open_in !prog_path) in
  let prog = While.Program.Cmd.number prog in
  Program.set_program prog ;
  Rules.setup (Seplog.Defs.of_channel (open_in !defs_path));
  F.exit (F.prove_seq !Rules.axioms !Rules.rules (seq, prog, tfext))
