open Lib
open Generic
open Seplog

let cl_sequent = ref ""

let defs_path = ref "examples/sl.defs"

let parse_null_as_emp = ref false

(* switches controlling invalidity heuristic *)
let invalidity_check = ref false

let slcomp = ref ""

module Prover = Prover.Make (Sl_seq)
module F = Frontend.Make (Prover)

let () = F.maxbound := 0

let () =
  F.usage :=
    !F.usage
    ^ " [-D <file>] [-emp] [-Lem <int>] [-IC] [-IT] [-IP] [-SLCOMP file] [-S \
       <string>]"

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
        ; ( "-Lem"
          , Arg.Int Sl_rules.set_lemma_level
          , ": specify the permissiveness of the lemma application strategy"
            ^ "\n"
            ^ Sl_rules.lemma_option_descr_str () )
        ; ( "-IC"
          , Arg.Set invalidity_check
          , ": run invalidity heuristic before search, default is "
            ^ string_of_bool !invalidity_check )
        ; ( "-IT"
          , Arg.Set Sl_rules.use_invalidity_heuristic
          , ": run invalidity heuristic during search, default is "
            ^ string_of_bool !Sl_rules.use_invalidity_heuristic )
        ; ( "-IP"
          , Arg.Set Sl_invalid.partition_strengthening
          , ": use partition strengthening in invalidity heuristic, default is "
            ^ string_of_bool !Sl_invalid.partition_strengthening )
        ; ( "-SLCOMP"
          , Arg.Set_string slcomp
          , ": change input to SMTLIB <file> and output to sat/unsat/unknown \
             for SLCOMP " ^ !slcomp )
        ; ( "-S"
          , Arg.Set_string cl_sequent
          , ": prove the SL sequent provided in <string>" ) ]

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
    if slcomp_mode then Sl_smtlib.of_channel (open_in !slcomp)
    else
      ( Sl_seq.of_string ~null_is_emp:!parse_null_as_emp !cl_sequent
      , Sl_defs.of_channel (open_in !defs_path) )
  in
  Sl_rules.setup defs ;
  let res =
    F.gather_stats (fun () ->
        if !invalidity_check && Sl_invalid.check defs seq then None
        else Some (F.idfs !Sl_rules.axioms !Sl_rules.rules seq) )
  in
  match res with
  | Some None ->
      print_endline
        ( if slcomp_mode then "sat"
        else "NOT proved: " ^ Sl_seq.to_string seq ^ " [invalid]" ) ;
      exit 255
  | _ ->
      let res = Option.flatten res in
      if slcomp_mode then
        match res with
        | Some (Some _) -> print_endline "unsat"
        | _ -> print_endline "unknown"
      else F.exit (F.process_result (not slcomp_mode) seq res)
