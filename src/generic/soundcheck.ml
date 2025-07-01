open Lib
open Parsers

open MParser
open MParser_RE

type soundness_method =
  | RELATIONAL_OCAML
  | SPOT_LEGACY
  | FWK_CPP
  | ORTL_CPP
  | VLA
  | SLA
  | CYCLONE

let soundness_method = ref ORTL_CPP
let use_legacy () =
  soundness_method := SPOT_LEGACY
let use_ocaml () =
  soundness_method := RELATIONAL_OCAML
let use_fwk_order_reduced () =
  soundness_method := ORTL_CPP
let use_fwk_full () =
  soundness_method := FWK_CPP
let use_vla () =
  soundness_method := VLA
let use_sla () =
  soundness_method := SLA
let use_cyclone () =
  soundness_method := CYCLONE
let inf_desc_opts =
  let options = [
    ("ocaml", RELATIONAL_OCAML) ;
    ("legacy", SPOT_LEGACY) ;
    ("fwk-full", FWK_CPP) ;
    ("fwk-or", ORTL_CPP) ;
    ("vla", VLA) ;
    ("sla", SLA) ;
    ("cyclone", CYCLONE) ;
  ] in
  let select opt =
    let opt = List.assoc opt options in
    soundness_method := opt
  in
  Arg.Symbol (List.map fst options, select)


let node_order = ref 0
let set_node_order order =
  (* Whatever bounds checking is performed here should be consistent with the
     enumeration values defined in the C++ code: check heighted_graph.hpp *)
  if (order < 0 || order > 2) then
    prerr_endline "Invalid node order specified!"
  else
    node_order := order

module IntPair = struct
  include Pair.Make(Int)(Int)
  include Containers.Make(Pair.Make(Int)(Int))
end

(* computes the composition of two sets of pairs *)
let compose t1 t2 =
  IntPair.Set.fold
    (fun (x, y) a ->
      IntPair.Set.fold
        (fun (w, z) b -> if Int.equal y w then IntPair.Set.add (x, z) b else b)
        t2 a )
    t1 IntPair.Set.empty

(* An abstract node consists of:
     - a flag indicating whether the node is a bud
     - a set of tags
     - a list of successor node IDs along with, for each successor,
        a list of the tag pairs and strictly progressing tag pairs
*)
type abstract_node = bool * Int.Set.t * (int * IntPair.Set.t * IntPair.Set.t) list

(* An abstract proof is a map from node IDs to abstract nodes *)
type t = abstract_node Int.Map.t

let get_tags (_, tags, _) = tags

let get_subg (_, _, children) = children

let is_leaf n = Blist.is_empty (get_subg n)

let in_children child n =
  Blist.exists (fun (i, _, _) -> Int.equal i child) (get_subg n)

let index_of_child child n =
  Blist.find_index (fun (i, _, _) -> Int.equal i child) (get_subg n)

let mk_abs_node ?(bud=false) tags succs tps_pair =
  let tags = Tags.to_ints tags in
  let subg =
    List.map2
      (fun i (tps, tps') ->
        let tps, tps' =
          Pair.map
            (Tagpairs.map_to IntPair.Set.add IntPair.Set.empty
               (Pair.map Tags.Elt.to_int))
            (tps, tps')
        in
        (i, tps, tps') )
      succs tps_pair
  in
  (bud, tags, subg)

let build_proof nodes =
  Int.Map.of_list
    (List.rev_map
       (fun (id, tags, premises) ->
         let premises =
           List.rev_map
             (fun (target, allpairs, progpairs) ->
               ( target
               , IntPair.Set.of_list allpairs
               , IntPair.Set.of_list progpairs ) )
             premises
         in
         (id, (false, Int.Set.of_list tags, List.rev premises)) )
       nodes)

(* has one child and is not a self loop *)
let is_single_node idx n =
  match get_subg n with
  | [(idx', _, _)] -> not (Int.equal idx' idx)
  | _ -> false

let fathers_grandchild prf idx n =
  match get_subg n with
  | [(grandchild, _, _)] ->
      if Int.equal idx grandchild then invalid_arg "fathers_grandchild1"
      else
        Int.Map.exists
          (fun idx' par_node ->
            in_children idx par_node && in_children grandchild par_node )
          prf
  | _ -> invalid_arg "fathers_grandchild2"

let remove_dead_nodes prf' =
  let prf = ref prf' in
  let process_node child par_idx n =
    if not (in_children child n) then ()
    else
      let newparent =
        let bud, tags, subg = n in
        match subg with
        | [_] ->
          (* If we have removed the only child of this node, it can no longer be a bud *)
          (false, tags, [])
        | _ ->
          (bud, tags, Blist.remove_nth (index_of_child child n) subg)
      in
      prf := Int.Map.add par_idx newparent !prf
  in
  let remove_dead_node idx n =
    let () = prf := Int.Map.remove idx !prf in
    Int.Map.iter (fun p n -> process_node idx p n) !prf
  in
  let cont = ref true in
  while !cont do
    match
      Int.Map.find_map (fun idx n -> (not (Int.equal idx 0)) && is_leaf n) !prf
    with
    | Some (idx, n) -> remove_dead_node idx n
    | None -> cont := false
  done ;
  !prf

let fuse_single_nodes prf' init =
  let prf = ref prf' in
  let process_node child grand_child tv tp par_idx n =
    if not (in_children child n) then ()
    else
      let bud, par_tags, par_subg = n in
      let pos = index_of_child child n in
      let _, par_tv, par_tp = List.nth par_subg pos in
      let newsubg =
        Blist.replace_nth
          ( grand_child
          , compose par_tv tv
          , IntPair.Set.union_of_list
              [compose par_tp tp; compose par_tv tp; compose par_tp tv] )
          pos par_subg
      in
      prf := Int.Map.add par_idx (bud, par_tags, newsubg) !prf
  in
  let fuse_node idx = function
    | false, tags, [(child, tv, tp)] ->
        Int.Map.iter (fun p n -> process_node idx child tv tp p n) !prf ;
        prf := Int.Map.remove idx !prf
    | _ -> invalid_arg "fuse_node"
  in
  let cont = ref true in
  (* if a parent points to the child of the node to be fused then *)
  (* we would run into difficulties when updating that parent to point *)
  (* directly to the grandchild, so we avoid that altogether *)
  let p idx ((bud, _, _) as n) =
    (not (Int.equal idx init))
    && not bud
    && is_single_node idx n
    && not (fathers_grandchild !prf idx n)
  in
  while !cont do
    match Int.Map.find_map p !prf with
    | Some (idx, n) -> fuse_node idx n
    | None -> cont := false
  done ;
  !prf

let minimize_abs_proof prf init = fuse_single_nodes (remove_dead_nodes prf) init

let valid prf init =
  let projectl = IntPair.Set.map_to Int.Set.add Int.Set.empty Pair.left in
  let projectr = IntPair.Set.map_to Int.Set.add Int.Set.empty Pair.right in
  (* init is a node in the proof *)
  if (not (Int.Map.mem init prf)) then
    let () =
      prerr_endline
        (Printf.sprintf "Initial node %i not in proof" init) in
    false
  else
  (* For all nodes n in the proof *)
  Int.Map.for_all
    (fun n_idx n ->
      (* For all premises i of n *)
      Blist.for_all
        (fun (i, tv, tp) ->
          (* i is a node in the proof *)
          if (not (Int.Map.mem i prf)) then
            let () =
              prerr_endline
                (Printf.sprintf "Goal %i for node %i not in proof" i n_idx) in
            false
          (* progressing tag pairs of i are a subset of all tagpairs of i *)
          else if (not (IntPair.Set.subset tp tv)) then
            let () =
              prerr_endline
                (Printf.sprintf "Prog pairs for goal %i of node %i not contained in all pairs" i n_idx) in
            false
          (* The left-hand components of all tagpairs are in the tagset of n *)
          else if (not (Int.Set.subset (projectl tv) (get_tags n))) then
            let () =
              prerr_endline
                (Printf.sprintf "Source tags of tagpairs for goal %i of node %i not contained in tags of node" i n_idx) in
            false
          (* The right-hand components of all tagpairs are in the tagset of i *)
          else if (not (Int.Set.subset (projectr tv) (get_tags (Int.Map.find i prf)))) then
            let () =
              prerr_endline
                (Printf.sprintf "Target tags of tagpairs for goal %i of node %i not contained in tags of target" i n_idx) in
            false
          else
            true)
        (get_subg n))
    prf

let size prf =
  Int.Map.cardinal prf
let trace_width prf =
  Int.Map.fold
    (fun _ (_, tags, _) acc -> Int.max acc (Int.Set.cardinal tags))
    (prf)
    (0)
let num_buds prf =
  Int.Map.fold
    (fun _ (bud, _, _) acc -> if bud then acc + 1 else acc)
    (prf)
    (0)

(* Module type for concrete representations of abstract proofs *)
module type Representation = sig
  val pp : Format.formatter -> t -> unit
  val to_string : t -> string
  val parse : (t, 's) MParser.parser
end

let parse_tagpairs s =
  (Tokens.braces
    (Tokens.comma_sep
      (Tokens.parens
        (Tokens.decimal >>= (fun t ->
         Tokens.comma >>
         Tokens.decimal |>> (fun t' ->
         (t, t'))))))) s

module NodeList = struct

  let pp_proof_node fmt n =
    let aux fmt (bud, tags, subg) =
      Format.fprintf fmt "tags=%a " Int.Set.pp tags ;
      if Blist.is_empty subg then Format.pp_print_string fmt "leaf"
      else
        if bud then Format.pp_print_string fmt "bud " ;
        Blist.pp pp_commasp
          (fun fmt (i, tv, tp) ->
            Format.fprintf fmt
              "(goal=%a, valid=%a, prog=%a)"
                Format.pp_print_int i
                IntPair.Set.pp tv
                IntPair.Set.pp tp)
          fmt subg
    in
    Format.fprintf fmt "@[%a@]" aux n

  let pp fmt prf =
    Format.open_vbox 0 ;
    Int.Map.iter
      (fun idx n -> Format.fprintf fmt "%i: %a@\n" idx pp_proof_node n)
      prf ;
    Format.close_box ()

  let to_string = mk_to_string pp

  let parse_fieldname name s =
    (optional (Tokens.symbol name >> Tokens.symbol "=")) s
  let parse_tags s =
    (parse_fieldname "tags" >>
     Tokens.braces (Tokens.comma_sep Tokens.decimal) |>> Int.Set.of_list) s
  let parse_subg s =
    (Tokens.parens (
      (parse_fieldname "goal" >>
       Tokens.decimal << Tokens.comma >>= (fun id ->
       parse_fieldname "valid" >>
       parse_tagpairs << Tokens.comma >>= (fun valid ->
       parse_fieldname "prog" >>
       parse_tagpairs |>> (fun prog ->
       (id, IntPair.Set.of_list valid, IntPair.Set.of_list prog)))))
    )) s
  let parse_line s =
    (
      Tokens.decimal >>= (fun id ->
      Tokens.colon >>
      parse_tags >>= (fun tags ->
      (Tokens.symbol "leaf" >>$ (id, (false, tags, [])))
        <|>
      (Tokens.symbol "bud" >> parse_subg |>> (fun subg ->
       (id, (true, tags, [subg]))))
        <|>
      (Tokens.comma_sep parse_subg |>> (fun subg ->
       (id, (false, tags, subg))) )
      ))
    ) s
  let parse s =
    (many1 parse_line |>> Int.Map.of_list) s

end

include (NodeList : Representation)

module EdgeList = struct

  let pp_line fmt (src, dst, backlink, prog, nonprog) =
    Format.fprintf fmt "@[%i %s %i : %a, %a@]"
      src
      (if backlink then "-->" else "->")
      dst
      IntPair.Set.pp prog
      IntPair.Set.pp nonprog

  let pp fmt prf =
    Format.open_vbox 0 ;
    Int.Map.iter
      (fun idx (bud, _, subg) ->
        List.iter
          (fun (dst, tv, tp) ->
            Format.fprintf fmt "%a@\n"
              pp_line
              (idx, dst, bud, tp, IntPair.Set.diff tv tp))
          subg)
      prf ;
    Format.close_box ()

  let to_string = mk_to_string pp

  let parse_line s =
    (
      Tokens.decimal >>= (fun src ->
      ((attempt (Tokens.symbol "->") >>$ false)
            <|> (Tokens.symbol "-->" >>$ true)) >>= (fun backlink ->
      Tokens.decimal << Tokens.colon >>= (fun dest ->
      parse_tagpairs << Tokens.comma >>= (fun prog ->
      parse_tagpairs |>> (fun nonprog ->
      let prog = IntPair.Set.of_list prog in
      let nonprog = IntPair.Set.of_list nonprog in
      (src, dest, backlink, prog, nonprog))))))
    ) s

    let mk_prf lines =
      let open Int.Map in
      let add_entry nodemap (src, dest, backlink, prog, nonprog) =
        let all = IntPair.Set.union prog nonprog in
        let src_tags =
          IntPair.Set.fold (fun (t,_) ts -> Int.Set.add t ts) all Int.Set.empty in
        let dest_tags =
          IntPair.Set.fold (fun (_,t) ts -> Int.Set.add t ts) all Int.Set.empty in
        let () =
          assert (
            match (find_opt src nodemap) with
            | None ->
              true
            | Some (_, _, subg) ->
              not (List.exists (fun (dest', _, _) -> Int.equal dest dest') subg)
          ) in
        let nodemap =
          update
            src
            (function
              | None ->
                Some (backlink, src_tags, [(dest, all, prog)])
              | Some (bud, heights, subg) ->
                let heights = Int.Set.union heights src_tags in
                Some (bud || backlink, heights, List.rev ((dest, all, prog) :: subg)))
            nodemap in
        let nodemap =
          update
            dest
            (function
              | None ->
                Some (false, dest_tags, [])
              | Some (bud, heights, subg) ->
                let heights = Int.Set.union heights dest_tags in
                Some (bud, heights, subg))
            nodemap in
        nodemap in
      List.fold_left add_entry empty lines

    let parse s =
      (many1 parse_line |>> mk_prf) s

end

module JSON = struct

  let pp_heights fmt heights =
    Format.fprintf fmt "\"Height\" : @[[%a]@]"
      (Blist.pp pp_commasp Int.pp) (Int.Set.to_list heights)

  let pp_buds fmt buds =
    Format.fprintf fmt "\"Bud\" : @[[%a]@]"
      (Blist.pp pp_commasp Int.pp) buds

  let pp_nodes fmt nodes =
    let pp_node fmt (idx, heights) =
      Format.fprintf fmt "[%i, @[[%a]@]]"
        idx
        (Blist.pp pp_commasp Int.pp) (Int.Set.to_list heights) in
    let aux fmt =
      function
      | []    ->
        Format.fprintf fmt "@]"
      | nodes ->
        Format.fprintf fmt "@;" ;
        Blist.pp (fun fmt () -> Format.fprintf fmt ",@;") pp_node fmt nodes ;
        Format.fprintf fmt "@]@;" in
    Format.fprintf fmt "@[<v 4>\"Node\" : [%a]" aux nodes

  let pp_edges fmt edges =
    let pp_slope fmt (src, dst, slope) =
      Format.fprintf fmt "[%i,%i,%i]" src dst slope in
    let pp_edge fmt (src, dst, slopes) =
      Format.fprintf fmt "[[%i,%i],[@[%a@]]]"
        src
        dst
        (Blist.pp pp_commasp pp_slope) slopes in
    let aux fmt =
      function
      | []    ->
        Format.fprintf fmt "@]"
      | edges ->
        Format.fprintf fmt "@;" ;
        Blist.pp (fun fmt () -> Format.fprintf fmt ",@;") pp_edge fmt edges ;
        Format.fprintf fmt "@]@;" in
    Format.fprintf fmt "@[<v 4>\"Edge\" : [%a]" aux edges

  let pp fmt prf =
    let nodes, buds =
      Int.Map.fold
        (fun idx (bud, heights, _) (nodes, buds) ->
          let nodes = (idx, heights) :: nodes in
          let buds = if bud then idx :: buds else buds in
          nodes, buds)
        prf
        (Blist.empty, Blist.empty) in
    let nodes = List.rev nodes in
    let buds = List.rev buds in
    let heights =
      List.fold_left
        (fun all_heights (_, heights) -> Int.Set.union heights all_heights)
        Int.Set.empty
        nodes in
    let edges =
      List.concat
        (List.rev
          (Int.Map.fold
            (fun src (_, _, subg) all_edges ->
              let edges =
                List.fold_left
                  (fun edges (dst, tv, tp) ->
                    (* TODO: get rid of the magic "slope" numbers '0' and '1' *)
                    let prog =
                      IntPair.Set.map_to_list (fun (t, t') -> (t, t', 1)) tp in
                    let nonprog =
                      let nonprog = IntPair.Set.diff tv tp in
                      IntPair.Set.map_to_list (fun (t, t') -> (t, t', 0)) nonprog in
                    (src, dst, nonprog @ prog) :: edges)
                  Blist.empty
                  subg
              in
              (List.rev edges) :: all_edges)
            prf
            Blist.empty))
    in
    Format.fprintf fmt "@[<v 4>{@;%a,@;%a,@;%a,@;%a@.}@]"
      pp_nodes nodes
      pp_buds buds
      pp_edges edges
      pp_heights heights

  let to_string = mk_to_string pp

  let parse_element_id s =
    (((Tokens.symbol "Node") >>$ `Nodes)
      <|>
     ((Tokens.symbol "Bud") >>$ `Buds)
      <|>
     ((Tokens.symbol "Edge") >>$ `Edges)
      <|>
     ((Tokens.symbol "Height") >>$ `Heights)) s

  let parse_element s =
    let parse_int_list s =
      (Tokens.comma_sep Tokens.decimal) s in
    let parse_int_pair s =
      (
        Tokens.squares (
        Tokens.decimal << Tokens.comma >>= (fun x ->
        Tokens.decimal |>> (fun y ->
        (x, y))))
      ) s in
    let parse_slope_triple s =
      (
        Tokens.squares (
        Tokens.decimal << Tokens.comma >>= (fun x ->
        Tokens.decimal << Tokens.comma >>= (fun y ->
        Tokens.decimal |>> (fun z ->
        (x, y, z)))))
      ) s in
    let parse_node s =
      (
        Tokens.squares (
        Tokens.decimal << Tokens.comma >>= (fun node_id ->
        (Tokens.squares parse_int_list) |>> (fun heights ->
        (node_id, Int.Set.of_list heights))))
      ) s in
    let parse_edge s =
      (
        Tokens.squares (
        parse_int_pair << Tokens.comma >>= (fun edge ->
        Tokens.squares (Tokens.comma_sep (parse_slope_triple)) |>> (fun slopes ->
        (edge, slopes))))
      ) s in
    ((between (char '"') (char '"') parse_element_id <?> "Node | Bud | Edge | Height")
        << spaces << Tokens.colon >>= (fun id ->
     Tokens.squares
      (match id with
       | `Nodes ->
          Tokens.comma_sep (spaces >> parse_node << spaces)
          |>> (fun nodes -> `Nodes nodes)
       | `Buds ->
         parse_int_list
         |>> (fun buds -> `Buds buds)
       | `Edges ->
         Tokens.comma_sep (spaces >> parse_edge << spaces)
         |>> (fun edges -> `Edges edges)
       | `Heights ->
         parse_int_list
         |>> (fun heights -> `Heights heights))
    )) s

  let parse s =
    let mk_prf nodes buds edges =
      (* Create bindings for all nodes, with their heights, and empty lists for:
            - child node IDs
            - tag pairs for corresponding edge *)
      let abs_nodes =
        List.fold_left
          (fun abs_nodes (node, heights) ->
            if Int.Map.mem node abs_nodes then
              let () =
                prerr_endline
                  (Format.sprintf
                    "Warning: ignoring duplicate node declaration: %i" node) in
              abs_nodes
            else
              let bud = List.mem node buds in
              Int.Map.add node (bud, heights, [], []) abs_nodes)
          (Int.Map.empty)
          (nodes) in
      (* Now iterate over edges to add information to the map *)
      let abs_nodes =
        List.fold_left
          (fun abs_nodes ((src, dest), slopes) ->
            Int.Map.update
              (src)
              (Option.map
                (fun ((bud, heights, succs, slopes') as binding) ->
                  (* Duplicates in this list are not handled by the
                     proof minimisation procedure *)
                  if List.mem dest succs then
                    let () =
                      prerr_endline
                        (Format.sprintf
                          "Warning: ignoring duplicate edge declaration (%i, %i)"
                          src dest) in
                    binding
                  else
                    let tps, prog_tps =
                      List.fold_left
                        (fun (tps, prog_tps) (src_height, dst_height, slope) ->
                          let tp = (src_height, dst_height) in
                          let tps = tp :: tps in
                          (* TODO: get rid of this magic number '1' *)
                          if Int.equal slope 1
                            then (tps, tp::prog_tps)
                            else (tps, prog_tps))
                        ([], [])
                        (slopes) in
                    let tps = IntPair.Set.of_list tps in
                    let prog_tps = IntPair.Set.of_list prog_tps in
                    (bud, heights, dest::succs, (tps, prog_tps)::slopes')))
              (abs_nodes))
          (abs_nodes)
          (edges) in
      (* Now transform the bindings into the right shape *)
      Int.Map.map
        (fun (bud, heights, succs, slopes) ->
          let subg =
            List.map2
              (fun subg (all_tps, prog_tps) -> (subg, all_tps, prog_tps))
              succs
              slopes in
          (bud, heights, subg))
        (abs_nodes) in
    let mk_prf =
      function
      | (_, Some nodes, buds, Some edges) ->
        let buds = Option.dest Blist.empty Fun.id buds in
        mk_prf nodes buds edges
      | _ ->
        invalid_arg "Missing information!" in
    let fold_elem (heights, nodes, buds, edges) =
      function
      | `Heights heights ->
        (Some heights, nodes, buds, edges)
      | `Nodes nodes ->
        (heights, Some nodes, buds, edges)
      | `Buds buds ->
        (heights, nodes, Some buds, edges)
      | `Edges edges ->
        (heights, nodes, buds, Some edges) in
    (Tokens.braces (Tokens.comma_sep parse_element) |>> (fun elems ->
     mk_prf (List.fold_left fold_elem (None, None, None, None) elems))) s

end

(* Which concrete representation to use for abstract proofs *)
type repr_t =
| NODE_LIST
| EDGE_LIST
| JSON

let repr = ref NODE_LIST

let repr_opts =
  let options = [
    ("node", NODE_LIST) ;
    ("edge", EDGE_LIST) ;
    ("json", JSON) ;
  ] in
  let select opt =
    let opt = List.assoc opt options in
    repr := opt
  in
  Arg.Symbol (List.map fst options, select)

let pp fmt prf =
  match !repr with
  | NODE_LIST ->
    NodeList.pp fmt prf
  | EDGE_LIST ->
    EdgeList.pp fmt prf
  | JSON ->
    JSON.pp fmt prf

let to_string prf =
  match !repr with
  | NODE_LIST ->
    NodeList.to_string prf
  | EDGE_LIST ->
    EdgeList.to_string prf
  | JSON ->
    JSON.to_string prf

let parse s =
  match !repr with
  | NODE_LIST ->
    NodeList.parse s
  | EDGE_LIST ->
    EdgeList.parse s
  | JSON ->
    JSON.parse s

let representation () =
  match !repr with
  | NODE_LIST ->
    (module NodeList : Representation)
  | EDGE_LIST ->
    (module EdgeList : Representation)
  | JSON ->
    (module JSON : Representation)

let repr_suf () =
  match !repr with
  | NODE_LIST ->
    ".node"
  | EDGE_LIST ->
    ".edge"
  | JSON ->
    ".json"


(* **************** *)
(* Soundness checks *)
(* **************** *)

module LegacyCheck = struct

  external create_aut : int -> unit = "create_aut"
  external destroy_aut : unit -> unit = "destroy_aut"
  external create_vertex : int -> unit = "create_vertex"
  external tag_vertex : int -> int -> unit = "tag_vertex"
  external set_successor : int -> int -> unit = "set_successor"
  external set_trace_pair : int -> int -> int -> int -> unit = "set_trace_pair"
  external set_progress_pair : int -> int -> int -> int -> unit
    = "set_progress_pair"
  external check_soundness : unit -> bool = "check_soundness_buchi"
  external get_paut_hoa : unit -> string = "get_proof_aut_hoa"
  external get_taut_hoa : unit -> string = "get_trace_aut_hoa"
  external set_initial_vertex : int -> unit = "set_initial_vertex"

  let print_paut = ref false
  let print_taut = ref false

    (* check global soundness condition on proof *)
  let check_proof ?(init=0) p =
    Stats.MC.call () ;
    let create_tags i n = Int.Set.iter (tag_vertex i) (get_tags n) in
    let create_succs i (_, _, l) =
      Blist.iter (fun (j, _, _) -> set_successor i j) l
    in
    let create_trace_pairs i (_, _, l) =
      let do_tag_transitions (j, tvs, tps) =
        IntPair.Set.iter (fun (k, m) -> set_trace_pair i j k m) tvs ;
        IntPair.Set.iter (fun (k, m) -> set_progress_pair i j k m) tps
      in
      Blist.iter do_tag_transitions l
    in
    let size = Int.Map.cardinal p in
    let log2size =
      1 + int_of_float (ceil (log (float_of_int size) /. log 2.0))
    in
    debug (fun () -> "Checking soundness starts...") ;
    create_aut log2size ;
    Int.Map.iter (fun i _ -> create_vertex i) p ;
    Int.Map.iter create_tags p ;
    Int.Map.iter create_succs p ;
    set_initial_vertex init ;
    Int.Map.iter create_trace_pairs p ;
    if !print_paut then begin
      print_endline "Proof Automaton" ;
      print_endline (get_paut_hoa ()) ;
    end ;
    if !print_taut then begin
      print_endline "Trace Automaton" ;
      print_endline (get_taut_hoa ()) ;
    end ;
    let retval = check_soundness () in
    if retval then Stats.MC.accept () else Stats.MC.reject () ;
    destroy_aut () ;
    debug (fun () ->
        "Checking soundness ends, result=" ^ if retval then "OK" else "NOT OK" ) ;
    retval
end

module RelationalCheck = struct

  module Slope = struct

    type t =
      (* | Unknown *)
      | Stay
      | Decrease

    let equal h h' =
      match (h, h') with
      | Decrease, Decrease
      | Stay, Stay
      (* | Unknown, Unknown  *)
        ->
        true
      | _ ->
        false

    let ( < ) h h' =
      match (h, h') with
      (* | Unknown, Stay
      | Unknown, Decrease *)
      | Stay, Decrease ->
        true
      | _, _ ->
        false

    let compare h h' =
      match (h, h') with
      | Stay, Decrease ->
        -1
      | Decrease, Stay ->
        1
      | _ ->
        0

    let max h h' =
      match (h, h') with
      | Decrease, _
      | _, Decrease ->
        Decrease
      (* | Stay, _
      | _, Stay ->
        Stay *)
      | _, _ ->
        (* Unknown *)
        Stay

    let pp fmt s =
      Format.fprintf fmt "%s"
        begin match s with
        (* | Unknown -> "Unknown" *)
        | Stay ->
          "Stay"
        | Decrease ->
          "Decrease"
        end

    let hash s = Hashtbl.hash s

  end

  module SlopedRel = struct

    module HashedKernel = struct
      let collect eq_key s =
        let rec collect prev acc s =
          match s () with
          | Seq.Nil ->
            begin match acc with
            | [] ->
              []
            | (k, vs)::acc ->
              List.rev ((k, List.rev vs)::acc)
            end
          | Seq.Cons ((k, v), s') ->
            begin match prev, acc with
            | None, _ ->
              collect (Some k) ((k, [v])::acc) s'
            | Some k', (_, vs)::acc' ->
              if (eq_key k k') then
                collect (Some k) ((k, v::vs)::acc') s'
              else
                collect (Some k) ((k, [v])::acc) s'
            | _ ->
              failwith "SlopedRel.HashedKernel.collect"
            end in
        collect None [] s
      let sort_bindings cmp_key s =
        List.stable_sort (fun (k, _) (k', _) -> cmp_key k k') (List.of_seq s)
      let compare cmp_key cmp_val kvs kvs' =
        let rec compare kvs kvs' =
          match kvs, kvs' with
          | [], [] ->
            0
          | [], _ ->
            -1
          | _, [] ->
            1
          | (k, v)::kvs, (k', v')::kvs' ->
            let k_cmp = cmp_key k k' in
            if Int.(k_cmp <> 0) then
              k_cmp
            else
              let v_cmp = cmp_val v v' in
              if Int.(v_cmp <> 0) then
                v_cmp
              else
                compare kvs kvs' in
        compare kvs kvs'
      type domain_map = Int.Hashset.t Int.Hashmap.t
      type slope_map = Slope.t IntPair.Hashmap.t
      (* domain-codomain mapping, codomain-domain mapping, slopes *)
      type t = domain_map * domain_map * slope_map
      let equal (_, _, p) (_, _, q) =
        if not (Int.equal (IntPair.Hashmap.length p) (IntPair.Hashmap.length p))
          then false
          else
            List.for_all
              (fun (((h, h') as k), ss) ->
                let ss' = IntPair.Hashmap.find_all q k in
                if not (Int.equal (List.length ss) (List.length ss'))
                  then false
                  else
                    List.for_all2 Slope.equal ss ss')
              (collect IntPair.equal (IntPair.Hashmap.to_seq p))
      let compare (_, _, p) (_, _, q) =
        let p_kvs, q_kvs =
          Pair.map
            (fun x -> sort_bindings IntPair.compare (IntPair.Hashmap.to_seq x))
            (p, q) in
        Pair.apply (compare IntPair.compare Slope.compare) (p_kvs, q_kvs)
      let hash (_, _, p) =
        let bindings =
          sort_bindings IntPair.compare (IntPair.Hashmap.to_seq p) in
        List.fold_left
          (fun a (k, v) -> genhash a (genhash (IntPair.hash k) (Slope.hash v)))
          0x9e3779b9
          bindings
      include HashtablePrinter.Make(IntPair.Hashmap)
      let pp fmt (_, _, p) =
        pp IntPair.pp Slope.pp fmt p
      let to_string (_, _, p) =
        to_string IntPair.pp Slope.pp p
    end

    include (HashedKernel : BasicType with type t = HashedKernel.t)

    let copy (fd, bk, slopes) =
      let fd' = Int.Hashmap.create (Int.Hashmap.length fd) in
      let () =
        Int.Hashmap.iter
          (fun i hs -> Int.Hashmap.add fd' i (Int.Hashset.copy hs))
          (fd) in
      let bk' = Int.Hashmap.create (Int.Hashmap.length bk) in
      let () =
        Int.Hashmap.iter
          (fun i hs -> Int.Hashmap.add bk' i (Int.Hashset.copy hs))
          (bk) in
      let slopes' = IntPair.Hashmap.copy slopes in
      (fd', bk', slopes')

    include Containers.Make(HashedKernel)

    let empty () =
      let fd = Int.Hashmap.create 1 in
      let bd = Int.Hashmap.create 1 in
      let slopes = IntPair.Hashmap.create 1 in
      (fd, bd, slopes)

    let add (h, h', s) (fd, bk, slopes) =
      let upd x =
        function
        | None ->
          Int.Hashset.singleton x
        | Some s ->
          let () = Int.Hashset.add s x in
          s in
      Int.Hashmap.replace fd h (upd h' (Int.Hashmap.find_opt fd h)) ;
      Int.Hashmap.replace bk h' (upd h (Int.Hashmap.find_opt bk h')) ;
      IntPair.Hashmap.replace
        slopes
        (h, h')
        (match IntPair.Hashmap.find_opt slopes (h, h')
          with
          | None -> s
          | Some s' -> Slope.max s s')

    let has_decreasing_self_loop (_, _, slopes) =
      List.exists
        (fun ((n, n'), s) ->
          if Int.(n <> n') then
            false
          else
            match s with
            | Slope.Decrease ->
              true
            | _ ->
              false)
        (List.of_seq (IntPair.Hashmap.to_seq slopes))

    let compose (p_fd, _, p_sl) (_, q_bk, q_sl) =
      let result = empty () in
      let () =
        Int.Hashmap.iter
          (fun h hs ->
            Int.Hashmap.iter
              (fun h' hs' ->
                let s =
                  Int.Hashset.fold
                  (fun h'' s ->
                    let s' =
                      Slope.max
                        (IntPair.Hashmap.find p_sl (h, h''))
                        (IntPair.Hashmap.find q_sl (h'', h')) in
                    match s with
                    | None   -> Some s'
                    | Some s -> Some (Slope.max s s'))
                  (Int.Hashset.inter hs hs')
                  None in
                match s with
                | None   -> ()
                | Some s -> add (h, h', s) result)
              q_bk)
          p_fd in
      result

    (* Repeat code for relational composition, so as to inline check for whether
       we have reached the fixed point - slightly more efficient than comparing
       new relation for equality with the old one at each iteration. *)
    let transitive_closure ((p_fd, p_bk, p_sl) as p) =
      let rec transitive_closure ((q_fd, q_bk, q_sl) as q) =
        let result = copy q in
        let continue =
          Int.Hashmap.fold
            (fun h hs ->
              Int.Hashmap.fold
                (fun h' hs' continue ->
                  let s =
                    Int.Hashset.fold
                    (fun h'' s ->
                      let s' =
                        Slope.max
                          (IntPair.Hashmap.find p_sl (h, h''))
                          (IntPair.Hashmap.find q_sl (h'', h')) in
                        match s with
                          | None   -> Some s'
                          | Some s -> Some (Slope.max s s'))
                    (Int.Hashset.inter hs hs')
                    None in
                  let () =
                    match s with
                    | None   -> ()
                    | Some s -> add (h, h', s) result in
                  match s with
                  | None ->
                    continue
                  | Some s' ->
                    continue
                      ||
                    not (IntPair.Hashmap.mem q_sl (h, h'))
                      ||
                    Slope.((IntPair.Hashmap.find q_sl (h, h')) < s'))
              q_bk)
            p_fd
            false in
        if continue then transitive_closure result else result in
      transitive_closure p

  end

  (* A height graph is a set of nodes and sloped relation for each edge *)
  type height_graph = Int.Set.t * SlopedRel.t IntPair.Map.t

  let pp_height_graph fmt (nodes, slopes) =
    Format.fprintf fmt "@[Nodes: %a@.Slopes: %a@]"
      Int.Set.pp nodes
      (IntPair.Map.pp SlopedRel.pp) slopes

  (* Compute the composition closure of the given height graph *)
  let comp_closure (ns, slopes) =
    let init =
      Int.Set.fold
        (fun n ccl ->
          Int.Set.fold
            (fun n' ccl ->
              let edge = (n, n') in
              let s = SlopedRel.Hashset.create 1 in
              let () =
                match (IntPair.Map.find_opt edge slopes) with
                | None ->
                  ()
                | Some p ->
                  SlopedRel.Hashset.add s p in
              IntPair.Map.add edge s ccl)
            ns
            ccl)
        ns
        IntPair.Map.empty in
    let rec closure ccl =
      let ccl', cont =
        Int.Set.fold (fun n ->
        Int.Set.fold (fun n' (acc, cont) ->
          let old_slopes = IntPair.Map.find (n, n') ccl in
          let new_slopes = SlopedRel.Hashset.copy old_slopes in
          let cont =
            Int.Set.fold (fun n'' ->
            SlopedRel.Hashset.fold (fun p ->
            SlopedRel.Hashset.fold (fun q cont ->
              let r = SlopedRel.compose p q in
              let () = SlopedRel.Hashset.add new_slopes r in
              cont || not (SlopedRel.Hashset.mem old_slopes r)
            ) (IntPair.Map.find (n'', n') ccl)
            ) (IntPair.Map.find (n, n'') ccl)
            ) ns cont in
          (IntPair.Map.add (n, n') new_slopes acc, cont)
        ) ns) ns (IntPair.Map.empty, false) in
      if cont then closure ccl' else ccl in
    closure init

  let pp_closure fmt ccl =
    let pp fmt s =
      let () = Format.fprintf fmt "@[[" in
      let first = ref true in
      let () =
        SlopedRel.Hashset.iter
          (fun p ->
            let () = if not !first then Format.fprintf fmt ", " in
            let () = first := false in
            let () = Format.fprintf fmt "%a" SlopedRel.pp p in
            ())
          s in
      let () = Format.fprintf fmt "]@]" in
      () in
    IntPair.Map.pp pp fmt ccl

  let check_proof p =
    Stats.MC.call () ;
    let to_height_graph p =
      Int.Map.fold
        (fun n (_, _, succs) (nodes, slopes) ->
          let nodes = Int.Set.add n nodes in
          let slopes =
            List.fold_left
              (fun slopes (n', all_tags, prog_tags) ->
                let hs = SlopedRel.empty () in
                let () =
                  IntPair.Set.iter
                    (fun (h, h') -> SlopedRel.add (h, h', Slope.Stay) hs)
                    all_tags in
                let () =
                  IntPair.Set.iter
                    (fun (h, h') -> SlopedRel.add (h, h', Slope.Decrease) hs)
                    prog_tags in
                IntPair.Map.add (n, n') hs slopes)
              slopes
              succs in
          (nodes, slopes))
        p
        (Int.Set.empty, IntPair.Map.empty) in
    let () = debug (fun () -> "Checking soundness starts...") in
    let ((nodes, _) as g) = to_height_graph p in
    let () = debug (fun () -> "Height Graph:\n" ^ mk_to_string pp_height_graph g) in
    let ccl = comp_closure g in
    let () = debug (fun () -> "Composition Closure:\n" ^ mk_to_string pp_closure ccl) in
    let retval =
      try
        let () =
          Int.Set.iter (fun n ->
          SlopedRel.Hashset.iter (fun p ->
            let () = debug (fun () -> "Checking " ^ mk_to_string SlopedRel.pp p) in
            let r = SlopedRel.transitive_closure p in
            let () = debug (fun () -> "Transitive closure: " ^ mk_to_string SlopedRel.pp r) in
            if not (SlopedRel.has_decreasing_self_loop r)
              then raise Not_found
          ) (IntPair.Map.find (n, n) ccl)
          ) nodes in
        true
      with Not_found ->
        false in
    if retval then Stats.MC.accept () else Stats.MC.reject () ;
    debug (fun () ->
        "Checking soundness ends, result=" ^ if retval then "OK" else "NOT OK" ) ;
    retval

  (* Call out to C++ library for relational check *)
  module Ext = struct

    external create_hgraph : int -> unit = "create_hgraph"
    external destroy_hgraph : unit -> unit = "destroy_hgraph"
    external add_node : int -> bool -> unit = "add_node"
    external add_height : int -> int -> unit = "add_height"
    external add_edge : int -> int -> unit = "add_edge"
    external add_stay : int -> int -> int -> int -> unit = "add_stay"
    external add_decrease : int -> int -> int -> int -> unit = "add_decr"
    external order_reduced_check : int -> int -> bool = "order_reduced_check"
    external fwk_check : int -> bool = "fwk_check"
    external sla_automata_check : unit -> bool = "sla_automata_check"
    external vla_automata_check : unit -> bool = "vla_automata_check"
    external cyclone_check: int -> int -> bool = "cyclone_check"

    external print_stats : unit -> unit = "print_statistics"

    (* Flags for applying different optimisations in the C++ code
       These values MUST match the corresponding constants in the C++ code
       Check heighted_graph.c
    *)
    let flag_fail_fast = 0b0001
    let flag_use_minimality = 0b1000
    (* Invariant: flag_transitive_loop_check XOR flag_idempotent_loop_check *)
    let flag_transitive_loop_check = 0b0010
    let flag_idempotent_loop_check = 0b0100

    let opts =
      ref
        (flag_fail_fast lor flag_transitive_loop_check lor flag_use_minimality)
    let do_stats = ref false

    let fail_fast b =
      if b then
        opts := !opts lor flag_fail_fast
      else
        opts := !opts land (lnot flag_fail_fast)
    let use_minimality b =
      if (not b) then
        opts := !opts land (lnot flag_use_minimality)
      else if (Int.equal (!opts land flag_idempotent_loop_check) 0) then
        invalid_arg
          "Cannot use minimality optimisation with idempotent loop check!"
      else
        opts := !opts lor flag_use_minimality
    let use_transitive_loop_check () =
      (* If idempotent looping flag is active,
         replace it with transitive looping and set minimality flag *)
      if (Int.equal (!opts land flag_idempotent_loop_check) 0) then
        begin
          opts := !opts land (lnot flag_idempotent_loop_check) ;
          opts := !opts lor flag_transitive_loop_check lor flag_use_minimality
        end
      (* Otherwise, no need to do anything as transitive looping flag must
         already have been set; we leave other flags as they are. *)
    let use_idempotent_loop_check () =
      begin
        (* Unset transitive loop check and minimality flags *)
        let to_unset = flag_transitive_loop_check lor flag_use_minimality in
        opts := !opts land (lnot to_unset) ;
        (* Set the idempotent loop check flag *)
        opts := !opts lor flag_idempotent_loop_check
      end

    let check_proof p =
      let process_succ n (n', tps, prog) =
        add_edge n n' ;
        IntPair.Set.iter
          (fun ((h, h') as p) ->
            if IntPair.Set.mem p prog
              then add_decrease n h n' h'
              else add_stay n h n' h')
          (tps) ;
        in
      let add_node_ n (is_bud, _, _) =
        add_node n is_bud in
      let add_heights n (_, tags, _) =
        Int.Set.iter (add_height n) tags in
      let add_edges n (_, _, succs) =
        List.iter (process_succ n) succs in
      Stats.MC.call () ;
      debug (fun () -> "Checking soundness starts...") ;
      create_hgraph (size p) ;
      Int.Map.iter add_node_ p ;
      Int.Map.iter add_heights p ;
      Int.Map.iter add_edges p ;
      let retval =
        match !soundness_method with
        | ORTL_CPP ->
          order_reduced_check !node_order !opts
        | FWK_CPP ->
          fwk_check !opts
        | SLA ->
          sla_automata_check ()
        | VLA ->
          vla_automata_check ()
        | CYCLONE ->
          cyclone_check !node_order !opts
        | _ ->
          failwith "Unexpected soundness check method!" in
      if retval then Stats.MC.accept () else Stats.MC.reject () ;
      if !do_stats then print_stats ();
      destroy_hgraph () ;
      debug (fun () ->
          "Checking soundness ends, result=" ^ if retval then "OK" else "NOT OK" ) ;
      retval

  end

end

include (RelationalCheck.Ext : sig
  val fail_fast : bool -> unit
  val use_minimality : bool -> unit
  val use_transitive_loop_check : unit -> unit
  val use_idempotent_loop_check : unit -> unit
  val do_stats : bool ref
end)

(* Config for dumping abstract proof graphs to file *)
let dump_graphs = ref false
let graph_dump_dir = ref Filename.current_dir_name
let next_graph_id =
  let graph_id = ref 0 in
  fun () ->
    let id = !graph_id in
    let () = graph_id := !graph_id + 1 in
    id
let mk_graph_name id minimised =
  let base = !Lib.run_identifier in
  let base =
    if not (String.equal base String.empty) then
      base ^ ".graph"
    else
      base ^ "graph" in
  let base = Format.sprintf "%s-%010i" base id in
  let base =
    Format.sprintf "%s%s"
      base
      (if minimised then ".minimised" else String.empty) in
  base

(* Config for whether to minimise proof graphs *)
let minimize_proofs = ref true

let arg_opts =
  [
    ("--inf-desc", inf_desc_opts,
      ": specify the method to be used for the Infinite Descent validity check \
      (default is order reduced Floyd-Warshall-Kleene)") ;
    ("-ord", Arg.Int set_node_order,
      "<int>: specify which node order to use in the order-reduced relational \
      check.\n\
      \t0 - Natural Ordering\n\
      \t1 - Out-degree, In-degree (lexicographically) ascending\n\
      \t2 - Out-degree, In-degree (lexicographically) descending") ;
    ("--no-fast-fail", Arg.Unit (fun () -> fail_fast false),
      ": do not use fast fail in relation-based infinite desc checks") ;
    ("--no-minimality", Arg.Unit (fun () -> use_minimality false),
      ": do not use minimality optimisation in relation-based infinite descent \
      checks") ;
    ("--idempotent-loop-check", Arg.Unit use_idempotent_loop_check,
      ": use idempotent loop check instead of transitive loop check in \
      relation-based infinite descent checks") ;
    ("--unminimized-proofs", Arg.Clear minimize_proofs,
      ": keep proofs unminimized") ;
    ("--rel-stats", Arg.Set do_stats,
      ": print out profiling stats for the relation-based validity check") ;
    ("--print-paut", Arg.Set LegacyCheck.print_paut,
      ": print the proof automaton in HOA format \
      (only when using the legacy infinite descent check)" ) ;
    ("--print-taut", Arg.Set LegacyCheck.print_taut,
      ": print the trace automaton in HOA format \
      (only when using the legacy infinite descent check)" ) ;
    ("--dump-graphs", Arg.Set dump_graphs,
      ": dump abstract proof graphs to file") ;
    ("--graph-dir", Arg.Set_string graph_dump_dir,
      ": directory for storing abstract proof graphs (default: current directory)") ;
    ("-R", repr_opts,
      ": the concrete representation to use for abstract proof graphs \
      (node list [default], edge list, or JSON)") ;
  ]

module CheckCache = Hashtbl

let ccache = CheckCache.create 1000
(* let limit = ref 1 *)


let check_proof ?(init=0) ?(minimize) prf =
  let prf_id = next_graph_id () in
  let debug_output prf =
    begin
      debug (fun _ -> to_string prf) ;
      debug (fun _ -> Format.sprintf "Number of proof nodes: %i" (size prf)) ;
      debug (fun _ -> Format.sprintf "Number of buds: %i" (num_buds prf)) ;
      debug (fun _ -> Format.sprintf "Trace width: %i" (trace_width prf))
    end in
  let debug_stats prf minimised is_valid =
    debug (fun _ ->
      Format.sprintf "Proof summary: %s, %i, %i, %i, %s"
        (mk_graph_name prf_id minimised)
        (size prf)
        (num_buds prf)
        (trace_width prf)
        (if is_valid then "VALID" else "INVALID")) in
  let () = debug (fun _ -> Format.sprintf "Proof ID: %i" prf_id) in
  let () = debug_output prf in
  if (Int.Map.is_empty prf) then
    true
  else
    let () =
      if not (valid prf init) then (
        pp Format.std_formatter prf ;
        assert false )
    in
    let minimize =
      match minimize with
      | None ->
        !minimize_proofs
      | Some b ->
        let () =
          if b && (not !minimize_proofs) then
            debug
              (fun _ ->
                "Command line specification to keep proof graphs unminimised \
                is being ignored!") in
        b in
    let prf' =
      if minimize
        then
          let () = Stats.Minimization.call() in
          let prf = minimize_abs_proof prf init in
          let () = Stats.Minimization.end_call() in
          let () = debug (fun _ -> "Minimized proof:") in
          let () = debug_output prf in
          prf
        else
          prf in
    if (Int.Map.is_empty prf') then
      true
    else
      (* TODO: We should extract the strongly connected components of the proof
               and test/cache these individually *)
      let () =
        if not (valid prf' init) then (
          pp Format.std_formatter prf' ;
          assert false ) in
      let () =
        if !dump_graphs && minimize then
          (* Output original proof to file *)
          let out_file_basename =
            Filename.concat !graph_dump_dir (mk_graph_name prf_id false) in
          let c = open_out (out_file_basename ^ (repr_suf ())) in
          begin
            output_string c (to_string prf) ;
            close_out_noerr c ;
          end in
      try
        Stats.MCCache.call () ;
        let (prf_id', is_valid) = CheckCache.find ccache prf' in
        Stats.MCCache.end_call () ;
        Stats.MCCache.hit () ;
        let () =
          debug (fun _ ->
            Format.sprintf
              "Found soundness result in the cache (Proof ID: %i): %s"
                (prf_id')
                (if is_valid then "OK" else "NOT OK")) in
        let () = if minimize then debug_stats prf false is_valid in
        is_valid
      with Not_found ->
        Stats.MCCache.end_call () ;
        Stats.MCCache.miss () ;
        let is_valid =
          match !soundness_method with
          | SPOT_LEGACY ->
            LegacyCheck.check_proof ~init prf'
          | RELATIONAL_OCAML ->
            RelationalCheck.check_proof prf'
          | _ ->
            RelationalCheck.Ext.check_proof prf' in
        Stats.MCCache.call () ;
        CheckCache.add ccache prf' (prf_id, is_valid) ;
        Stats.MCCache.end_call () ;
        (* if CheckCache.length ccache > !limit then                                          *)
        (*   begin                                                                            *)
        (*     debug (fun () -> "Soundness cache passed limit: " ^ (string_of_int !limit)) ;  *)
        (*     limit := 10 * !limit                                                           *)
        (*   end ;                                                                            *)
        let () = debug_stats prf false is_valid in
        let () = if minimize then debug_stats prf' true is_valid in
        let () =
          if (!dump_graphs) then
            let out_file_basename =
              Filename.concat !graph_dump_dir
                              (mk_graph_name prf_id minimize) in
            let c = open_out (out_file_basename ^ (repr_suf ())) in
            begin
              output_string c (to_string prf') ;
              close_out_noerr c ;
            end in
        is_valid
