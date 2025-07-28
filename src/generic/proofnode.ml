open Lib

(** Proof node signature. *)
module type S = sig
  type t

  type seq_t

  type infrule_t

  val mk_open : seq_t -> t

  val mk_axiom : seq_t -> infrule_t -> t

  val mk_backlink : seq_t -> int -> Tagpairs.t -> t

  val mk_inf :
    seq_t -> infrule_t -> int list -> (Tagpairs.t * Tagpairs.t) list -> t

  val dest : t -> seq_t * infrule_t option

  val dest_backlink : t -> seq_t * int * Tagpairs.t

  val dest_axiom : t -> seq_t * infrule_t

  val dest_inf :
    t -> seq_t * infrule_t * int list * (Tagpairs.t * Tagpairs.t) list

  val is_open : t -> bool

  val is_axiom : t -> bool

  val is_backlink : t -> bool

  val is_inf : t -> bool

  val get_seq : t -> seq_t

  val get_succs : t -> int list

  val to_abstract_node : t -> Soundcheck.abstract_node

  val pp : Format.formatter -> t -> unit
end

module Make
    (Seq : Sequent.S)
    (Infrule : Infrule.S) =
struct

  type t =
    | OpenNode of {seq: Seq.t;}
    | AxiomNode of {seq: Seq.t; descr: Infrule.t}
    | InfNode of
        { seq: Seq.t
        ; descr: Infrule.t
        ; succs: int list
        ; tagpairs2: (Tagpairs.t * Tagpairs.t) list }
    | BackNode of {seq: Seq.t; succ: int; tagpairs: Tagpairs.t}

  let get_seq = function
    | OpenNode {seq} | AxiomNode {seq} | InfNode {seq} | BackNode {seq} -> seq

  let get_succs = function
    | AxiomNode _ | OpenNode _ -> []
    | BackNode {succ= s} -> [s]
    | InfNode {succs} -> succs

  let dest = function
    | OpenNode {seq;}
    | BackNode {seq;} ->
      (seq, None)
    | AxiomNode {seq; descr}
    | InfNode {seq; descr} ->
      (seq, Some descr)

  let dest_backlink = function
    | BackNode {seq; succ; tagpairs} -> (seq, succ, tagpairs)
    | _ -> invalid_arg "dest_backlink"

  let dest_axiom = function
    | AxiomNode {seq; descr;} ->
      (seq, descr)
    | _ -> invalid_arg "dest_axiom"

  let dest_inf = function
    | InfNode {seq; descr; succs; tagpairs2} -> (seq, descr, succs, tagpairs2)
    | _ -> invalid_arg "dest_inf"

  let is_open = function OpenNode _ -> true | _ -> false

  let is_backlink = function BackNode _ -> true | _ -> false

  let is_axiom = function AxiomNode _ -> true | _ -> false

  let is_inf = function InfNode _ -> true | _ -> false

  let mk_open seq = OpenNode {seq;}

  let mk_axiom seq descr = AxiomNode {seq; descr}

  let mk_inf seq descr succs tagpairs2 = InfNode {seq; descr; succs; tagpairs2}

  let mk_backlink seq succ tagpairs =
    BackNode {seq; succ; tagpairs;}

  let to_abstract_node = function
    | OpenNode {seq} | AxiomNode {seq} ->
        Soundcheck.mk_abs_node (Seq.tags seq) [] []
    | InfNode {seq; succs; tagpairs2} ->
        Soundcheck.mk_abs_node (Seq.tags seq) succs tagpairs2
    | BackNode {seq; succ; tagpairs} ->
        Soundcheck.mk_abs_node ~bud:true (Seq.tags seq) [succ]
          [(tagpairs, Tagpairs.empty)]

  let pp fmt = function
    | OpenNode {seq} -> Format.fprintf fmt "@[%a (Open)@]" Seq.pp seq
    | AxiomNode {seq; descr} ->
        Format.fprintf fmt "@[%a (%a)@]" Seq.pp seq Infrule.pp descr
    | BackNode {seq; succ; tagpairs} ->
        Format.fprintf fmt "@[%a (Backl) [%i] <pre=%a>@]" Seq.pp seq succ
          Tagpairs.pp tagpairs
    | InfNode {seq; descr; succs; tagpairs2} ->
        Format.fprintf fmt "@[%a (%a) [%a]@]" Seq.pp seq Infrule.pp descr
          (Blist.pp pp_commasp (fun fmt (i, (pres, prog)) ->
               (* Format.fprintf fmt "%i" i)) p *)
               Format.fprintf fmt "@[%i <%a/%a>@]" i Tagpairs.pp
                 (Tagpairs.diff pres prog) Tagpairs.pp prog ))
          (List.combine succs tagpairs2)
end
