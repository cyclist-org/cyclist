open Lib

let cl_sequent = ref ""
let defs_path = ref "examples/sl.defs"

let parse_null_as_emp = ref false

(* switches controlling invalidity heuristic *)
let invalidity_check = ref false

let slcomp_mode = ref false

module Prover = Prover.Make(Sl_seq)
module F = Frontend.Make(Prover)

let () = F.usage := !F.usage ^ " [-D <file>] [-S <string>] [-IC] [-IT] [-IP]"

let () = F.speclist := !F.speclist @ [
    ("-D", Arg.Set_string defs_path, 
      ": read inductive definitions from <file>, default is " ^ !defs_path);
    ("-S", Arg.Set_string cl_sequent, ": prove the SL sequent provided in <string>");
    ("-emp", Arg.Set parse_null_as_emp, 
      "parse the empty string as the formula [emp] rather than [False], " ^
      "default is " ^ (string_of_bool !parse_null_as_emp));
    ("-IC", Arg.Set invalidity_check, 
      ": run invalidity heuristic before search, default is " ^ 
      (string_of_bool !invalidity_check));
    ("-IT", Arg.Set Sl_rules.use_invalidity_heuristic, 
      ": run invalidity heuristic during search, default is " ^ 
      (string_of_bool !Sl_rules.use_invalidity_heuristic));
    ("-IP", Arg.Set Sl_invalid.partition_strengthening, 
      ": use partition strengthening in invalidity heuristic, default is " ^ 
      (string_of_bool !Sl_invalid.partition_strengthening));
    ("-Lem", Arg.Int Sl_rules.set_lemma_level,
      ": specify the permissiveness of the lemma application strategy" ^ "\n\t" ^ Sl_rules.lemma_option_descr_str);
    ("--permissive-lemma-application", Arg.Set Sl_rules.permissive_lemma_application, 
      ": Allow lemma application requiring existential introduction in the succedent");
    ("-SLCOMP", Arg.Set slcomp_mode, ": change output to sat/unsat/unknown for SLCOMP");
  ]

type search_result =
  | Valid of Prover.Proof.t
  | Invalid
  | Unknown

let () =
  gc_setup () ;
  Format.set_margin (Sys.command "exit $(tput cols)") ;
  Arg.parse !F.speclist (fun _ -> raise (Arg.Bad "Stray argument found.")) !F.usage ;
  if !cl_sequent="" then F.die "-S must be specified." ;
  let seq = Sl_seq.of_string ~null_is_emp:!parse_null_as_emp !cl_sequent in
  let defs = Sl_defs.of_channel (open_in !defs_path) in
  Sl_rules.setup defs ;
  Stats.reset () ;
  Stats.Gen.call () ;
  let call () =
    if !invalidity_check && Sl_invalid.check defs seq then Invalid else
    match 
      let maxbound = if !F.maxbound < 1 then max_int else !F.maxbound in
      Prover.idfs !F.minbound maxbound !Sl_rules.axioms !Sl_rules.rules seq
    with
    | None -> Unknown
    | Some p -> Valid p 
    in
  let res = 
    if !F.timeout<>0 then w_timeout call !F.timeout else Some (call ()) in
  Stats.Gen.end_call () ;
  let exit_code = match res with
  | None -> 
    begin 
      print_endline 
        (if !slcomp_mode then 
          "unknown" 
        else 
         "NOT proved: " ^ (Sl_seq.to_string seq) ^ " [TIMEOUT]") ; 
      2
    end
  | Some Invalid ->
    begin
      print_endline 
        (if !slcomp_mode then
          "sat"
        else
          "NOT proved: " ^ (Sl_seq.to_string seq) ^ " [invalid]") ; 
      255
    end ;
  | Some Unknown ->
    begin 
      print_endline 
        (if !slcomp_mode then
          "unknown"
        else
          "NOT proved: " ^ (Sl_seq.to_string seq)) ; 
      1
    end
  | Some (Valid proof) ->
    begin
      if !F.show_proof then
        Prover.Proof.pp Format.std_formatter proof
      else
        print_endline 
          (if !slcomp_mode then
            "unsat"
          else
            "Proved: " ^ (Sl_seq.to_string seq)) ;
      if !Stats.do_statistics then Prover.print_proof_stats proof ;
      if !F.latex_path<>"" then
      begin
        let ch =
          open_out_gen [Open_creat; Open_wronly; Open_trunc] 402 !F.latex_path in
        Prover.melt_proof ch proof ; close_out ch
      end ;
      0
    end in
  if !Stats.do_statistics then Stats.gen_print ();
  let exit_code = if !slcomp_mode then 0 else exit_code in
  exit exit_code
    


