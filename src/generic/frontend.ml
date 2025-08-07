open Lib

module type S = sig

  type seq_t

  type infrule_t

  type proofrule_t

  type node_t

  module Proof : Proof.S with type seq_t := seq_t
                          and type infrule_t := infrule_t
                          and type node_t := node_t

  module Prover : Prover.S with type seq_t := seq_t
                          and type rule_t := proofrule_t
                          and type proof_t := Proof.t

  type result_t =
    seq_t * [ `TIMEOUT | `NOT_FOUND | `SUCCESS of Proof.t ] * Stats.t * int

  val show_proof : bool ref
  val use_dot : bool ref
  val timeout : int ref
  val minbound : int ref
  val maxbound : int option ref
  val speclist : (unit -> (string * Arg.spec * string) list) ref
  val usage : string ref
  val die : string -> (string * Arg.spec * string) list -> string -> 'a
  val exit : result_t -> 'a

  val prove_seq : proofrule_t -> proofrule_t -> seq_t -> result_t
  val pp_result : Format.formatter -> result_t -> unit
  val print_result : result_t -> unit

end

module Make (Seq : Sequent.S) (Infrule : Infrule.S) = struct

  module Proof = Proof.Make(Seq)(Infrule)

  module Prover = Prover.Make(Seq)(Infrule)

  type result_t =
    Seq.t * [ `TIMEOUT | `NOT_FOUND | `SUCCESS of Proof.t ] * Stats.t * int

  let show_proof = ref false

  let use_dot = ref false

  let latex_path = ref ""

  let timeout = ref 30

  let minbound = ref 1

  let maxbound = ref None

  let speclist =
    ref (fun () ->
        [ ( "-m", Arg.Set_int minbound,
              ": set starting depth for IDFS to <int>, default is "
                ^ string_of_int !minbound )
        ; ( "-M", Arg.Int (fun i -> maxbound := Some i),
              ": set maximum depth for IDFS to <int>, default is unbounded" )
        ; ( "-L",
              Arg.Int (fun n -> minbound := n ; maxbound := Some n ),
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

  let exit (_, res, _, _) =
    match res with
    | `TIMEOUT ->
      exit 2
    | `NOT_FOUND ->
      exit 1
    | `SUCCESS _ ->
      exit 0

  let gather_stats call =
    Stats.reset () ;
    Stats.Gen.call () ;
    let res =
      if not (Int.equal !timeout 0) then w_timeout call !timeout
      else Some (call ())
    in
    Stats.Gen.end_call () ;
    res

  let idfs ax r seq =
    let maxbound =
      match !maxbound with
      | None ->
        max_int
      | Some i ->
        if Int.( < ) i !minbound then !minbound else i in
    Prover.idfs !minbound maxbound ax r seq

  let prove_seq ax r seq =
    Format.set_margin (Sys.command "exit $(tput cols)") ;
    let res =
      match (gather_stats (fun () -> idfs ax r seq)) with
      | None ->
        `TIMEOUT
      | Some None ->
        `NOT_FOUND
      | Some (Some proof) ->
        `SUCCESS proof in
    (seq, res, Stats.get_stats (), !Prover.last_search_depth)

  let pp_result fmt (seq, res, stats, depth) =
    if !Stats.do_statistics then Stats.pp_stats fmt stats ;
    match res with
    | `TIMEOUT ->
      Format.fprintf fmt "NOT proved: %a [TIMEOUT]@." Seq.pp seq
    | `NOT_FOUND ->
      Format.fprintf fmt "NOT proved: %a." Seq.pp seq ;
      if !Stats.do_statistics then
        Format.fprintf fmt "Search depth was %i@." depth
    | `SUCCESS proof ->
      if !show_proof then
        let pp = if !use_dot then Proof.pp_dot else Proof.pp in
        pp fmt proof
      else
        Format.fprintf fmt "Proved: %a@." Seq.pp seq ;
      if !Stats.do_statistics then
        begin
          Prover.pp_proof_stats fmt proof ;
          Format.fprintf fmt "Required search depth was %i@." depth ;
        end

  let print_result res =
    pp_result Format.std_formatter res
end
