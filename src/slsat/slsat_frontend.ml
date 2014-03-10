(* Handles command-line parameters, etc. for the predicate consistency checker. *)
(* Similar to cyclist/generic/frontend.ml, which is for instantiations of the *)
(* generic cyclic theorem prover. *)

open Lib
open Symheap

    let show_proof = ref false
    let latex_path = ref ""
    let timeout = ref 30

    let speclist = ref [
        ("-p", Arg.Set show_proof,": show proof");
        ("-d", Arg.Set do_debug,": print debug messages");
        ("-s", Arg.Set Stats.do_statistics,": print statistics");
        ("-l", Arg.Set_string latex_path, ": write proofs to <file>");
        ("-t", Arg.Set_int timeout, 
          (": set timeout in seconds to <int>, 0 disables it, default is " ^ 
            (string_of_int !timeout)));
      ]

    let usage = ref ("usage: " ^ Sys.argv.(0) ^ " [-p/d/s] [-t <int>] [-l <file>]")

    let die msg =
      print_endline msg ;
      print_endline (Arg.usage_string !speclist !usage) ;
      exit 1

    let check_consistency defs =
      let exit_code = ref 0 in
      Format.set_margin (Sys.command "exit $(tput cols)") ;
      Stats.reset () ;
      Stats.Gen.call () ;
      let consistency_check =
        if !show_proof then
          fun () -> Defs.consistent_plus_output defs
        else
          (fun () -> (Defs.consistent defs, "")) in
      let res = w_timeout consistency_check !timeout in
      Stats.Gen.end_call () ;
      begin
        if Option.is_none res then
        begin
          print_endline ("NOT proved: [TIMEOUT]") ;
          exit_code := 2
        end
        else
          let (is_consistent, bases) = Option.get res in
          if is_consistent == false then
          begin
            print_endline ("NOT proved: Some predicate has an empty base.") ;
            if !show_proof then
              print_endline ("Mapping from inductive rules to bases:\n" ^ bases)
            else () ;
            exit_code := 1
          end
          else
            begin
              print_endline ("Proved: All predicates have non-empty bases.") ;
              if !show_proof then
                print_endline ("Mapping from inductive rules to bases:\n" ^ bases)
              else ()
            end
      end ;
      if !Stats.do_statistics then 
      begin
        Stats.gen_print ();
      end ;
      (*
      if !latex_path<>"" then 
      begin
        let ch = 
          open_out_gen [Open_creat; Open_wronly; Open_trunc] 402 !latex_path in
        Prover.latex_proof ch proof ; close_out ch
      end ;
      *)
      !exit_code
