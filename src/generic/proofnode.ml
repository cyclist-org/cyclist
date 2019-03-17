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

  val mk_inf : seq_t -> string -> (int * Tagpairs.t * Tagpairs.t) list -> t
  (** [mk_inf seq descr subgoals back] creates an inference node labelled by
      sequent [seq], description [descr], a list of triples consisting of
      subgoal index, valid tag transitions and progressing tag transitions
      [subgoals].*)

  (** Destructors. *)

  val dest : t -> seq_t * string
  (** [dest n] returns (sequent, description). This works with all Proof.t nodes. *)

  val dest_backlink : t -> seq_t * string * int * Tagpairs.t
  (** [dest_backlink n] destroys a back-link node [n], otherwise raises [Invalid_arg].*)

  val dest_inf : t -> seq_t * string * (int * Tagpairs.t * Tagpairs.t) list
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

  type proof_subnode =
    | OpenNode
    | AxiomNode
    | InfNode of (int * Tagpairs.t * Tagpairs.t) list
    | BackNode of int * Tagpairs.t

  type t = {seq: Seq.t; descr: string; node: proof_subnode}

  let get_seq n = n.seq

  let get_succs n =
    match n.node with
    | AxiomNode | OpenNode -> []
    | BackNode (s, _) -> [s]
    | InfNode ss ->
        let ss', _, _ = Blist.unzip3 ss in
        ss'

  let dest n = (n.seq, n.descr)

  let dest_backlink n =
    match n.node with
    | BackNode (child, vtts) -> (n.seq, n.descr, child, vtts)
    | _ -> invalid_arg "dest_backlink"

  let dest_inf n =
    match n.node with
    | InfNode subgs -> (n.seq, n.descr, subgs)
    | _ -> invalid_arg "dest_inf"

  let is_open n = match n.node with OpenNode -> true | _ -> false

  let is_backlink n = match n.node with BackNode _ -> true | _ -> false

  let is_axiom n = match n.node with AxiomNode -> true | _ -> false

  let is_inf n = match n.node with InfNode _ -> true | _ -> false

  let mk seq node descr =
    assert (not (String.equal descr "")) ;
    {seq; node; descr}

  let mk_open seq = mk seq OpenNode "(Open)"

  let mk_axiom seq descr = mk seq AxiomNode descr

  let mk_inf seq descr subgoals = mk seq (InfNode subgoals) descr

  let mk_backlink seq descr child vtts = mk seq (BackNode (child, vtts)) descr

  let to_abstract_node n =
    match n.node with
    | OpenNode | AxiomNode -> Soundcheck.mk_abs_node (Seq.tags n.seq) []
    | InfNode subg -> Soundcheck.mk_abs_node (Seq.tags n.seq) subg
    | BackNode (child, tv) ->
        Soundcheck.mk_abs_node (Seq.tags n.seq) [(child, tv, Tagpairs.empty)]

  let pp fmt n =
    match n.node with
    | OpenNode -> Format.fprintf fmt "@[%a (Open)@]" Seq.pp n.seq
    | AxiomNode -> Format.fprintf fmt "@[%a (%s)@]" Seq.pp n.seq n.descr
    | BackNode (i, tps) ->
        Format.fprintf fmt "@[%a (%s) [%i] <pre=%a>@]" Seq.pp n.seq n.descr i
          Tagpairs.pp tps
    | InfNode p ->
        Format.fprintf fmt "@[%a (%s) [%a]@]" Seq.pp n.seq n.descr
          (Blist.pp pp_commasp (fun fmt (i, pres, prog) ->
               (* Format.fprintf fmt "%i" i)) p *)
               Format.fprintf fmt "@[%i <%a/%a>@]" i Tagpairs.pp
                 (Tagpairs.diff pres prog) Tagpairs.pp prog ))
          p
end
