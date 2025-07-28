open Lib

module type S = sig

  type seq_t

  type infrule_t

  type proofrule_t

  type node_t

  module Proof : Proof.S with type seq_t := seq_t
                          and type infrule_t := infrule_t
                          and type node_t := node_t

  type result_t = TIMEOUT | NOT_FOUND | SUCCESS of Proof.t

  val show_proof : bool ref
  val use_dot : bool ref
  val timeout : int ref
  val minbound : int ref
  val maxbound : int ref
  val speclist : (unit -> (string * Arg.spec * string) list) ref
  val usage : string ref
  val die : string -> (string * Arg.spec * string) list -> string -> 'a
  val exit : result_t -> 'a
  val gather_stats : (unit -> 'a) -> 'a option
  val idfs : proofrule_t -> proofrule_t -> seq_t -> Proof.t option
  val process_result : bool -> seq_t -> Proof.t option option -> result_t

  val prove_seq : proofrule_t -> proofrule_t -> seq_t -> result_t

end

module Make (Seq : Sequent.S) (Infrule : Infrule.S) = struct

  module Proof = Proof.Make(Seq)(Infrule)

  module Prover = Prover.Make(Seq)(Infrule)

  type result_t = TIMEOUT | NOT_FOUND | SUCCESS of Proof.t

  let show_proof = ref false

  let use_dot = ref false

  let latex_path = ref ""

  let timeout = ref 30

  let minbound = ref 1

  let maxbound = ref 11

  let speclist =
    ref (fun () ->
        [ ( "-m", Arg.Set_int minbound,
              ": set starting depth for IDFS to <int>, default is "
                ^ string_of_int !minbound )
        ; ( "-M", Arg.Set_int maxbound,
              ": set maximum depth for IDFS to <int>, 0 disables it,
              default is " ^ string_of_int !maxbound )
        ; ( "-L",
              Arg.Int (fun n -> minbound := n ; maxbound := n ),
              ": set both depths to <int>." )
        ; ("-p", Arg.Set show_proof, ": show proof")
        ; ("--dot", Arg.Set use_dot, ": use DOT format for proofs")
        ; ("-d", Arg.Set do_debug, ": print debug messages")
        ; ("--id", Arg.Set_string run_identifier,
            ": identifier for the execution, used in debug output" )
        ; ("-s", Arg.Set Stats.do_statistics, ": print statistics")
        ; ("-l", Arg.Set_string latex_path, ": write proofs to <file>")
        ; ( "-t", Arg.Set_int timeout,
              ": set timeout in seconds to <int>, 0 disables it, default is "
                ^ string_of_int !timeout )
        ]
        @ Soundcheck.arg_opts )

  let usage =
    ref
      ("usage: " ^
        Sys.argv.(0) ^
        " [-p/d/s] [-l <file>] [-t/m/M/L <int>] \
        [--inf-desc ( vla | sla | fwk-full | fwk-or | cyclone )]")

  let die msg spec_list usage =
    print_endline msg ;
    print_endline (Arg.usage_string spec_list usage) ;
    exit 1

  let exit = function
    | TIMEOUT -> exit 2
    | NOT_FOUND -> exit 1
    | SUCCESS _ -> exit 0

  let gather_stats call =
    Stats.reset () ;
    Stats.Gen.call () ;
    let res =
      if not (Int.equal !timeout 0) then w_timeout call !timeout
      else Some (call ())
    in
    Stats.Gen.end_call () ;
    if !Stats.do_statistics then Stats.gen_print () ;
    res

  let process_result output seq res =
    if Option.is_none res then (
      if output then
        print_endline ("NOT proved: " ^ Seq.to_string seq ^ " [TIMEOUT]") ;
      TIMEOUT )
    else
      let res = Option.get res in
      if Option.is_none res then (
        if output then print_endline ("NOT proved: " ^ Seq.to_string seq) ;
        NOT_FOUND )
      else
        let proof = Option.get res in
        if !show_proof then
          let pp = if !use_dot then Proof.pp_dot else Proof.pp in
          pp Format.std_formatter proof
        else if output then print_endline ("Proved: " ^ Seq.to_string seq) ;
        if !Stats.do_statistics then Prover.print_proof_stats proof ;
        SUCCESS proof

  let idfs ax r seq =
    let maxbound = if Int.( < ) !maxbound 1 then max_int else !maxbound in
    Prover.idfs !minbound maxbound ax r seq

  let prove_seq ax r seq =
    Format.set_margin (Sys.command "exit $(tput cols)") ;
    let res = gather_stats (fun () -> idfs ax r seq) in
    process_result true seq res
end
