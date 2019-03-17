open Lib
open Symbols

(** Proof node signature. *)
module type S = sig
  type t

  (** Sequent type used for building proof nodes. *)
  type seq_t

  (** Constructors. *)

  val mk_open : seq_t -> t
  (** [mk_open seq] creates an open Proof.t node labelled by [seq]. *)

  val mk_axiom : seq_t -> string -> t
  (** [mk_axiom seq descr] creates an axiom node labelled by
      sequent [seq] and description [descr].*)

  val mk_backlink : seq_t -> string -> int -> Tagpairs.t -> t
  (** [mk_backlink seq descr target vtts] creates a back-link node labelled by
      sequent [seq], description [descr], target index [target] and set of
      valid tag transitions (as pairs) [vtts].*)

  val mk_inf :
    seq_t -> string -> int list -> (Tagpairs.t * Tagpairs.t) list -> t
  (** [mk_inf seq descr subgoals back] creates an inference node labelled by
      sequent [seq], description [descr], a list of triples consisting of
      subgoal index, valid tag transitions and progressing tag transitions
      [subgoals].*)

  (** Destructors. *)

  val dest : t -> seq_t * string
  (** [dest n] returns (sequent, description). This works with all Proof.t nodes. *)

  val dest_backlink : t -> seq_t * string * int * Tagpairs.t
  (** [dest_backlink n] destroys a back-link node [n], otherwise raises [Invalid_arg].*)

  val dest_inf :
    t -> seq_t * string * int list * (Tagpairs.t * Tagpairs.t) list
  (** [dest_inf n] destroys an inference node [n], otherwise raises [Invalid_arg].*)

  (** Functions for checking the sort of a node. *)

  val is_open : t -> bool

  val is_axiom : t -> bool

  val is_backlink : t -> bool

  val is_inf : t -> bool

  (** Auxiliary functions for getting information from all nodes. *)

  val get_seq : t -> seq_t
  (** Get sequent labelling the node. *)

  val get_succs : t -> int list
  (** Get the successor node indices of this node. *)

  val to_abstract_node : t -> Soundcheck.abstract_node
  (** Convert Proof.t node to abstract node as in {!Soundcheck}. *)

  (** Pretty printing and Latex conversion. *)

  val pp : Format.formatter -> t -> unit
end

module Make (Seq : Sequent.S) = struct
  module Seq = Seq

  type seq_t = Seq.t

  type t =
    | OpenNode of {seq: Seq.t; descr: string}
    | AxiomNode of {seq: Seq.t; descr: string}
    | InfNode of
        { seq: Seq.t
        ; descr: string
        ; succs: int list
        ; tagpairs2: (Tagpairs.t * Tagpairs.t) list }
    | BackNode of {seq: Seq.t; descr: string; succ: int; tagpairs: Tagpairs.t}

  let get_seq = function
    | OpenNode {seq} | AxiomNode {seq} | InfNode {seq} | BackNode {seq} -> seq

  let get_succs = function
    | AxiomNode _ | OpenNode _ -> []
    | BackNode {succ= s} -> [s]
    | InfNode {succs} -> succs

  let dest = function
    | OpenNode {seq; descr}
     |AxiomNode {seq; descr}
     |InfNode {seq; descr}
     |BackNode {seq; descr} ->
        (seq, descr)

  let dest_backlink = function
    | BackNode {seq; descr; succ; tagpairs} -> (seq, descr, succ, tagpairs)
    | _ -> invalid_arg "dest_backlink"

  let dest_inf = function
    | InfNode {seq; descr; succs; tagpairs2} -> (seq, descr, succs, tagpairs2)
    | _ -> invalid_arg "dest_inf"

  let is_open = function OpenNode _ -> true | _ -> false

  let is_backlink = function BackNode _ -> true | _ -> false

  let is_axiom = function AxiomNode _ -> true | _ -> false

  let is_inf = function InfNode _ -> true | _ -> false

  let mk_open seq = OpenNode {seq; descr= "(Open)"}

  let mk_axiom seq descr = AxiomNode {seq; descr}

  let mk_inf seq descr succs tagpairs2 = InfNode {seq; descr; succs; tagpairs2}

  let mk_backlink seq descr succ tagpairs =
    BackNode {seq; succ; tagpairs; descr}

  let to_abstract_node = function
    | OpenNode {seq} | AxiomNode {seq} ->
        Soundcheck.mk_abs_node (Seq.tags seq) [] []
    | InfNode {seq; succs; tagpairs2} ->
        Soundcheck.mk_abs_node (Seq.tags seq) succs tagpairs2
    | BackNode {seq; succ; tagpairs} ->
        Soundcheck.mk_abs_node (Seq.tags seq) [succ]
          [(tagpairs, Tagpairs.empty)]

  let pp fmt = function
    | OpenNode {seq} -> Format.fprintf fmt "@[%a (Open)@]" Seq.pp seq
    | AxiomNode {seq; descr} ->
        Format.fprintf fmt "@[%a (%s)@]" Seq.pp seq descr
    | BackNode {seq; descr; succ; tagpairs} ->
        Format.fprintf fmt "@[%a (%s) [%i] <pre=%a>@]" Seq.pp seq descr succ
          Tagpairs.pp tagpairs
    | InfNode {seq; descr; succs; tagpairs2} ->
        Format.fprintf fmt "@[%a (%s) [%a]@]" Seq.pp seq descr
          (Blist.pp pp_commasp (fun fmt (i, (pres, prog)) ->
               (* Format.fprintf fmt "%i" i)) p *)
               Format.fprintf fmt "@[%i <%a/%a>@]" i Tagpairs.pp
                 (Tagpairs.diff pres prog) Tagpairs.pp prog ))
          (List.combine succs tagpairs2)
end
