open Lib
open Symbols

module Make (Seq : Sigs.SEQUENT) = struct
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
