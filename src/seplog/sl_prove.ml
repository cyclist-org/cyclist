open Lib

let cl_sequent = ref ""
let defs_path = ref "examples/sl.defs"

module Prover = Prover.Make(Sl_seq)
module F = Frontend.Make(Prover)

let () = F.usage := !F.usage ^ " [-D <file>] [-S <string>]"

let () = F.speclist := !F.speclist @ [
    ("-D", Arg.Set_string defs_path, 
      ": read inductive definitions from <file>, default is " ^ !defs_path);
    ("-S", Arg.Set_string cl_sequent, ": prove the SL sequent provided in <string>");
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
  let seq = Sl_seq.of_string !cl_sequent in
  let defs = Sl_defs.of_channel (open_in !defs_path) in
  Sl_rules.setup defs ;
  Stats.reset () ;
  Stats.Gen.call () ;
  let call () =
    if Sl_seq.invalid defs seq then Invalid else
    match 
      Prover.idfs !F.minbound !F.maxbound !Sl_rules.axioms !Sl_rules.rules seq
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
      print_endline ("NOT proved: " ^ (Sl_seq.to_string seq) ^ " [TIMEOUT]") ; 
      2
    end
  | Some Invalid ->
    begin
      print_endline ("NOT proved: " ^ (Sl_seq.to_string seq) ^ " [invalid]") ; 
      255
    end ;
  | Some Unknown ->
    begin 
      print_endline ("NOT proved: " ^ (Sl_seq.to_string seq)) ; 
      1
    end
  | Some (Valid proof) ->
    begin
      if !F.show_proof then
        Prover.Proof.pp Format.std_formatter proof
      else
        print_endline ("Proved: " ^ (Sl_seq.to_string seq)) ;
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
    


