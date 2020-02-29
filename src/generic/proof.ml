open Lib

(** Proof signature. *)
module type S = sig
  (** Proof type. Invariants are:
      - Graph (all indices point to existing nodes).
      - Non-empty.
      - Rooted at 0.
      - Connected.
      - Leaves are open nodes, axioms or backlinks. *)
  type t

  type seq_t

  type node_t

  val mk : seq_t -> t
  (** Constructor.  Takes a sequent and makes an open node at the root (0).*)

  (** Other constructors, which return the indices of new
      subgoals, if any, which are new, open nodes, and the new proof.

      All of these take an index to an open node, and a description plus
      more arguments appropriate to the type of constructor.  The open
      node will be replaced by another node with the same index, and its
      description set to the parameter value.

      Back-links *must* have equal sequents to their targets, otherwise
      an exception will be thrown.`
      *)

  val add_axiom : int -> string -> t -> t

  val add_backlink : int -> string -> int -> Tagpairs.t -> t -> t

  val add_inf :
       int
    -> string
    -> (seq_t * Tagpairs.t * Tagpairs.t) list
    -> t
    -> int list * t

  val add_subprf : t -> int -> t -> t
  (** [add_subprf p idx p'] replaces the node [idx] in [p'] with a copy of [p].
      N.B. The following should hold, otherwise exceptions will be raised:
        - [idx] should be an open node;
        - the sequent of [idx] should be identical to the root sequent of [p]. *)

  val extract_subproof : int -> t -> t
  (** [extract_subproof idx prf] returns [prf] rearranged such that [idx] is
      the root node. *)

  (** Accessor functions. *)

  val find : int -> t -> node_t

  val get_seq : int -> t -> seq_t

  val size : t -> int

  val num_backlinks : t -> int

  (* val mem : int -> t -> bool *)
  val fresh_idx : t -> int

  val fresh_idxs : 'a list -> t -> int list

  val get_ancestry : int -> t -> (int * node_t) list

  val is_closed_at : t -> int -> bool

  val check : t -> bool
  (** Check soundness. Proof does not need to be closed. *)

  val is_closed : t -> bool
  (** Are all nodes not open? *)

  val to_list : t -> (int * node_t) list

  (** Output functions. *)

  val pp : Format.formatter -> t -> unit

  val to_string : t -> string
end

module Make (Seq : Sequent.S) = struct
  module Node = Proofnode.Make (Seq)
  module P = Int.Map

  type t = (int * Node.t) P.t

  type seq_t = Seq.t

  type node_t = Node.t

  let get idx prf = P.find idx prf

  let find idx prf = snd (get idx prf)

  let get_seq idx prf = Node.get_seq (find idx prf)

  let fresh_idx prf = 1 + fst (P.max_binding prf)

  let fresh_idxs xs prf = Blist.range (fresh_idx prf) xs

  let to_list m = Blist.map (fun (i, (_, n)) -> (i, n)) (P.bindings m)

  let size prf = P.cardinal prf

  let num_backlinks prf =
    Blist.length
      (Blist.filter (fun (_, n) -> Node.is_backlink n) (to_list prf))

  let pp fmt prf =
    let rec pp_aux fmt idx =
      let n = find idx prf in
      Format.pp_open_vbox fmt (if Node.is_inf n then 2 else 0) ;
      Format.fprintf fmt "@[%i:@ %a@]" idx Node.pp n ;
      if Node.is_inf n then
        Format.fprintf fmt "@;%a"
          (Blist.pp Format.pp_print_cut pp_aux)
          (Node.get_succs n) ;
      Format.pp_close_box fmt ()
    in
    Format.fprintf fmt "@[%a@]@." pp_aux 0

  let to_string prf = mk_to_string pp prf

  let is_closed prf = P.for_all (fun _ (_, n) -> not (Node.is_open n)) prf

  let check p =
    let () = debug (fun _ -> "Checking global soundness") in
    let () = debug (fun _ -> to_string p) in
    Soundcheck.check_proof (P.map (fun (_, n) -> Node.to_abstract_node n) p)

  let mk seq = P.add 0 (0, Node.mk_open seq) P.empty

  let replace idx n prf = P.add idx (fst (get idx prf), n) prf

  let ensure_add idx n prf =
    let n' = find idx prf in
    assert (Node.is_open n' && Seq.equal (Node.get_seq n) (Node.get_seq n'))

  let add_axiom idx descr prf =
    let n = Node.mk_axiom (get_seq idx prf) descr in
    ensure_add idx n prf ; replace idx n prf

  let add_backlink idx descr target vtts prf =
    let seq = get_seq idx prf in
    let n = Node.mk_backlink seq descr target vtts in
    ensure_add idx n prf ;
    assert (Seq.equal_upto_tags seq (get_seq target prf)) ;
    replace idx n prf

  let add_inf idx descr subgoals prf =
    let subidxs = Blist.range (fresh_idx prf) subgoals in
    let subnodes =
      Blist.map2 (fun i (seq, _, _) -> (i, Node.mk_open seq)) subidxs subgoals
    in
    let tagpairs2 = Blist.map (fun (_, vtts, ptts) -> (vtts, ptts)) subgoals in
    let n = Node.mk_inf (get_seq idx prf) descr subidxs tagpairs2 in
    ensure_add idx n prf ;
    let prf' =
      Blist.foldl
        (fun prf' (ci, cn) -> P.add ci (idx, cn) prf')
        (replace idx n prf) subnodes
    in
    (subidxs, prf')

  let add_subprf prf idx prf' =
    let rec _add (prf', (node_map, bls)) idx idx' =
      let n = find idx prf in
      if Node.is_open n then (prf', (node_map, bls))
      else if Node.is_axiom n then
        let _, descr = Node.dest n in
        (add_axiom idx' descr prf', (node_map, bls))
      else if Node.is_backlink n then (prf', (node_map, (idx, idx') :: bls))
      else if Node.is_inf n then
        let _, descr, premises, tagpairs2 = Node.dest_inf n in
        let premises' =
          Blist.map2
            (fun pidx (vts, pts) -> (Node.get_seq (find pidx prf), vts, pts))
            premises tagpairs2
        in
        let premises', prf' = add_inf idx' descr premises' prf' in
        let node_map =
          Blist.fold_right2 Int.Map.add premises premises' node_map
        in
        Blist.fold_left2 _add (prf', (node_map, bls)) premises premises'
      else invalid_arg "Unrecognised node type!"
    in
    assert (Node.is_open (find idx prf')) ;
    assert (
      Seq.equal (Node.get_seq (find 0 prf)) (Node.get_seq (find idx prf')) ) ;
    let prf', (node_map, bls) =
      _add (prf', (Int.Map.singleton 0 idx, [])) 0 idx
    in
    let fix_bl prf' (idx, idx') =
      let _, descr, target, vtts = Node.dest_backlink (find idx prf) in
      add_backlink idx' descr (Int.Map.find target node_map) vtts prf'
    in
    Blist.fold_left fix_bl prf' bls

  let extract_subproof idx prf =
    let rec _extract (prf', node_map) idx idx' =
      if Int.Map.mem idx node_map then
        let prf' =
          add_backlink idx' "Backl"
            (Int.Map.find idx node_map)
            (Tagpairs.mk (Seq.tags (get_seq idx prf)))
            prf'
        in
        (prf', node_map)
      else
        let node_map = Int.Map.add idx idx' node_map in
        let n = find idx prf in
        if Node.is_open n then (prf', node_map)
        else if Node.is_axiom n then
          let _, descr = Node.dest n in
          (add_axiom idx' descr prf', node_map)
        else if Node.is_backlink n then
          let _, descr, target, vtts = Node.dest_backlink n in
          if Int.Map.mem target node_map then
            ( add_backlink idx' descr (Int.Map.find target node_map) vtts prf'
            , node_map )
          else _extract (prf', node_map) target idx'
        else if Node.is_inf n then
          let _, descr, premises, tagpairs2 = Node.dest_inf n in
          let premises' =
            Blist.map2
              (fun pidx (vts, pts) -> (Node.get_seq (find pidx prf), vts, pts))
              premises tagpairs2
          in
          let premises', prf' = add_inf idx' descr premises' prf' in
          Blist.fold_left2 _extract (prf', node_map) premises premises'
        else invalid_arg "Unrecognised node type!"
    in
    fst (_extract (mk (get_seq idx prf), Int.Map.empty) idx 0)

  let get_ancestry idx prf =
    let rec aux acc idx (par_idx, n) =
      let parent = get par_idx prf in
      let acc = (idx, n) :: acc in
      if Int.equal par_idx idx then acc else aux acc par_idx parent
    in
    aux [] idx (get idx prf)

  let rec is_closed_at prf idx =
    let n = find idx prf in
    if Node.is_axiom n then true
    else if Node.is_open n then false
    else if Node.is_backlink n then true
    else Blist.for_all (is_closed_at prf) (Node.get_succs n)
end
