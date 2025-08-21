open Lib
open Generic
open Seplog

let cl_sequent = ref ""

let defs_path = ref "examples/sl.defs"

let parse_null_as_emp = ref false

(* switches controlling invalidity heuristic *)
let invalidity_check = ref false

let slcomp = ref ""

module Prover = Prover.Make (Seq) (Strng)
module F = Frontend.Make (Seq) (Strng)

let () =
  F.usage :=
    !F.usage
    ^ " [-D <file>] [-emp] [-Bl <int>] [-Lem <int>] [-IC] [-IT] [-IP] \
        [-SLCOMP file] [-S <string>]"

let () =
  F.speclist :=
    let old_spec_thunk = !F.speclist in
    fun () ->
      old_spec_thunk ()
      @ [ ( "-D"
          , Arg.Set_string defs_path
          , ": read inductive definitions from <file>, default is "
            ^ !defs_path )
        ; ( "-emp"
          , Arg.Set parse_null_as_emp
          , "parse the empty string as the formula [emp] rather than [False], "
            ^ "default is "
            ^ string_of_bool !parse_null_as_emp )
        ; ("-Bl"
          , Arg.Int Rules.set_default_select_f
          , ": specify which proof nodes are considered for backlinks"
            ^ "\n"
            ^ Rules.default_select_f_descr ())
        ; ( "-Lem"
          , Arg.Int Rules.set_lemma_level
          , ": specify the permissiveness of the lemma application strategy"
            ^ "\n"
            ^ Rules.lemma_option_descr_str () )
        ; ( "-IC"
          , Arg.Set invalidity_check
          , ": run invalidity heuristic before search, default is "
            ^ string_of_bool !invalidity_check )
        ; ( "-IT"
          , Arg.Set Rules.use_invalidity_heuristic
          , ": run invalidity heuristic during search, default is "
            ^ string_of_bool !Rules.use_invalidity_heuristic )
        ; ( "-IP"
          , Arg.Set Invalid.partition_strengthening
          , ": use partition strengthening in invalidity heuristic, default is "
            ^ string_of_bool !Invalid.partition_strengthening )
        ; ( "-SLCOMP"
          , Arg.Set_string slcomp
          , ": change input to SMTLIB <file> and output to sat/unsat/unknown \
             for SLCOMP " ^ !slcomp )
        ; ( "-S"
          , Arg.Set_string cl_sequent
          , ": prove the SL sequent provided in <string>" ) ]

let do_check defs seq =
  if !invalidity_check && Invalid.check defs seq
    then None
    else Some (F.prove_seq !Rules.axioms !Rules.rules seq)

let process_result slcomp_mode seq (res, stats) =
  match res with
  | `TIMEOUT ->
    let result = F.process_result seq (`TIMEOUT, stats) in
    let () =
      if slcomp_mode
        then print_endline "unknown"
        else F.pp_result Format.std_formatter result in
    F.exit result
  | `RESULT None ->
    if slcomp_mode then
      print_endline "sat"
    else
      begin
        print_endline ("NOT proved: " ^ Seq.to_string seq ^ " [invalid]" );
        if !Stats.do_statistics
          then Stats.pp_stats Format.std_formatter stats ;
      end ;
    exit 255
  | `RESULT (Some res)->
    let result = F.process_result seq (`RESULT res, stats) in
    let () =
      if slcomp_mode
        then print_endline "unsat"
        else F.pp_result Format.std_formatter result in
    F.exit result

let () =
  gc_setup () ;
  let spec_list = !F.speclist () in
  Arg.parse spec_list
    (fun _ -> raise (Arg.Bad "Stray argument found."))
    !F.usage ;
  let slcomp_mode = not (String.equal !slcomp "") in
  if (not slcomp_mode) && String.equal !cl_sequent "" then
    F.die "-S must be specified." spec_list !F.usage ;
  let seq, defs =
    if slcomp_mode then Smtlib.of_channel (open_in !slcomp)
    else
      ( Seq.of_string ~null_is_emp:!parse_null_as_emp !cl_sequent
      , Defs.of_channel (open_in !defs_path) )
  in
  Rules.setup defs ;
  Stats.gather
    (!F.timeout)
    (fun () -> do_check defs seq)
    (process_result slcomp_mode seq)
