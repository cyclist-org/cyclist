open Lib

let cl_sequent = ref ""
let defs_path = ref "examples/sl.defs"

(* switches controlling invalidity heuristic *)
let invalidity_check = ref false

(* sequent from file *)
let cl_sequent_fname = ref ("" : string)
let cl_f_sequents = ref ([]: string list)

module Prover = Prover.Make(Sld_seq)
module F = Frontend.Make(Prover)


let get_seq_fname arg=
  let _ = cl_sequent_fname := arg in
  ()

let () = F.usage := !F.usage ^ " [-D <file>] [-S <string>] [-IC] [-IT] [-IP]"

let () = F.speclist := !F.speclist @ [
    ("-D", Arg.Set_string defs_path, 
      ": read inductive definitions from <file>, default is " ^ !defs_path);
    ("-S", Arg.Set_string cl_sequent, ": prove the SL sequent provided in <string>");
    ("-IC", Arg.Set invalidity_check, 
      ": run invalidity heuristic before search, default is " ^ 
      (string_of_bool !invalidity_check));
    ("-IT", Arg.Set Sld_rules.use_invalidity_heuristic, 
      ": run invalidity heuristic during search, default is " ^ 
      (string_of_bool !Sld_rules.use_invalidity_heuristic));
    ("-IP", Arg.Set Sld_invalid.partition_strengthening, 
      ": use partition strengthening in invalidity heuristic, default is " ^ 
      (string_of_bool !Sld_invalid.partition_strengthening));
  ]

type search_result =
  | Valid of Prover.Proof.t
  | Invalid
  | Unknown

let () =
  gc_setup () ;
  Format.set_margin (Sys.command "exit $(tput cols)") ;
  Arg.parse !F.speclist get_seq_fname !F.usage ;
  cl_f_sequents := string_lines_of_file !cl_sequent_fname;
  let () = if !cl_sequent="" then
    match !cl_f_sequents with
      | [] -> F.die "-S must be specified."
      | s::_ -> cl_sequent := s (*now only process the first one. to support all*)
  in
  let seq = Sld_seq.of_string !cl_sequent in
  let defs = Sld_defs.of_channel (open_in !defs_path) in
  Sld_rules.setup defs ;
  Stats.reset () ;
  Stats.Gen.call () ;
  let call () =
    if !invalidity_check && Sld_invalid.check defs seq then Invalid else
    match 
      Prover.idfs !F.minbound !F.maxbound !Sld_rules.axioms !Sld_rules.rules seq
    with
    | None -> Unknown
    | Some p -> Valid p 
    in
  let res = 
    if !F.timeout<>0 then w_timeout call !F.timeout else Some (call ()) in
  Stats.Gen.end_call () ;
  if !Stats.do_statistics then Stats.gen_print ();
  let exit_code = match res with
  | None -> 
    begin 
      print_endline ("NOT proved: " ^ (Sld_seq.to_string seq) ^ " [TIMEOUT]") ; 
      2
    end
  | Some Invalid ->
    begin
      print_endline ("NOT proved: " ^ (Sld_seq.to_string seq) ^ " [invalid]") ; 
      255
    end ;
  | Some Unknown ->
    begin 
      print_endline ("NOT proved: " ^ (Sld_seq.to_string seq)) ; 
      1
    end
  | Some (Valid proof) ->
    begin
      if !F.show_proof then
        Prover.Proof.pp Format.std_formatter proof
      else
        print_endline ("Proved: " ^ (Sld_seq.to_string seq)) ;
      if !Stats.do_statistics then Prover.print_proof_stats proof ;
      if !F.latex_path<>"" then
      begin
        let ch =
          open_out_gen [Open_creat; Open_wronly; Open_trunc] 402 !F.latex_path in
        Prover.melt_proof ch proof ; close_out ch
      end ;
      0
    end in
    exit exit_code
    


