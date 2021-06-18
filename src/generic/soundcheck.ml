open Lib
open Parsers

external create_aut : int -> unit = "create_aut"

external destroy_aut : unit -> unit = "destroy_aut"

external create_vertex : int -> unit = "create_vertex"

external tag_vertex : int -> int -> unit = "tag_vertex"

external set_successor : int -> int -> unit = "set_successor"

external set_trace_pair : int -> int -> int -> int -> unit = "set_trace_pair"

external set_progress_pair :
  int -> int -> int -> int -> unit
  = "set_progress_pair"

external check_soundness : unit -> bool = "check_soundness"

external set_initial_vertex : int -> unit = "set_initial_vertex"

module IntPairSet = Treeset.Make (Pair.Make (Int) (Int))

(* computes the composition of two sets of pairs *)
let compose t1 t2 =
  IntPairSet.fold
    (fun (x, y) a ->
      IntPairSet.fold
        (fun (w, z) b -> if Int.equal y w then IntPairSet.add (x, z) b else b)
        t2 a )
    t1 IntPairSet.empty

(* An abstract node is a set of tags, and a list of successor node IDs along
   with, for each successor, a list of the tag pairs and strictly progressing
   tag pairs *)
type abstract_node = Int.Set.t * (int * IntPairSet.t * IntPairSet.t) list

(* An abstract proof is a map from node IDs to abstract nodes *)
type t = abstract_node Int.Map.t

let get_tags n = fst n

let get_subg n = snd n

let is_leaf n = Blist.is_empty (get_subg n)

let in_children child n =
  Blist.exists (fun (i, _, _) -> Int.equal i child) (get_subg n)

let index_of_child child (_, subg) =
  Blist.find_index (fun (i, _, _) -> Int.equal i child) subg

let mk_abs_node tags succs tps_pair =
  let tags = Tags.to_ints tags in
  let subg =
    List.map2
      (fun i (tps, tps') ->
        let tps, tps' =
          Pair.map
            (Tagpairs.map_to IntPairSet.add IntPairSet.empty
               (Pair.map Tags.Elt.to_int))
            (tps, tps')
        in
        (i, tps, tps') )
      succs tps_pair
  in
  (tags, subg)

let build_proof nodes =
  Int.Map.of_list
    (List.rev_map
       (fun (id, tags, premises) ->
         let premises =
           List.rev_map
             (fun (target, allpairs, progpairs) ->
               ( target
               , IntPairSet.of_list allpairs
               , IntPairSet.of_list progpairs ) )
             premises
         in
         (id, (Int.Set.of_list tags, List.rev premises)) )
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

let pp_proof_node fmt n =
  let aux fmt (tags, subg) =
    Format.fprintf fmt "tags=%a " Int.Set.pp tags ;
    if Blist.is_empty subg then Format.pp_print_string fmt "leaf"
    else
      Blist.pp pp_semicolonsp
        (fun fmt (i, tv, tp) ->
          Format.fprintf fmt "(goal=%a, valid=%a, prog=%a)" Format.pp_print_int
            i IntPairSet.pp tv IntPairSet.pp tp )
        fmt subg
  in
  Format.fprintf fmt "@[%a@]" aux n

let pp fmt prf =
  Format.open_vbox 0 ;
  Int.Map.iter
    (fun idx n -> Format.fprintf fmt "%i: %a@\n" idx pp_proof_node n)
    prf ;
  Format.close_box ()

let remove_dead_nodes prf' =
  let prf = ref prf' in
  let process_node child par_idx n =
    if not (in_children child n) then ()
    else
      let newparent =
        let tags, subg = n in
        match subg with
        | [_] -> (tags, [])
        | _ -> (tags, Blist.remove_nth (index_of_child child n) subg)
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
      let par_tags, par_subg = n in
      let pos = index_of_child child n in
      let _, par_tv, par_tp = List.nth par_subg pos in
      let newsubg =
        Blist.replace_nth
          ( grand_child
          , compose par_tv tv
          , IntPairSet.union_of_list
              [compose par_tp tp; compose par_tv tp; compose par_tp tv] )
          pos par_subg
      in
      prf := Int.Map.add par_idx (par_tags, newsubg) !prf
  in
  let fuse_node idx = function
    | tags, [(child, tv, tp)] ->
        Int.Map.iter (fun p n -> process_node idx child tv tp p n) !prf ;
        prf := Int.Map.remove idx !prf
    | _ -> invalid_arg "fuse_node"
  in
  let cont = ref true in
  (* if a parent points to the child of the node to be fused then *)
  (* we would run into difficulties when updating that parent to point *)
  (* directly to the grandchild, so we avoid that altogether *)
  let p idx n =
    (not (Int.equal idx init))
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
  let projectl = IntPairSet.map_to Int.Set.add Int.Set.empty Pair.left in
  let projectr = IntPairSet.map_to Int.Set.add Int.Set.empty Pair.right in
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
          else if (not (IntPairSet.subset tp tv)) then
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

module AutomatonCheck = struct
  (* check global soundness condition on proof *)
  let check_proof ?(init=0) p =
    Stats.MC.call () ;
    let create_tags i n = Int.Set.iter (tag_vertex i) (get_tags n) in
    let create_succs i (_, l) =
      Blist.iter (fun (j, _, _) -> set_successor i j) l
    in
    let create_trace_pairs i (_, l) =
      let do_tag_transitions (j, tvs, tps) =
        IntPairSet.iter (fun (k, m) -> set_trace_pair i j k m) tvs ;
        IntPairSet.iter (fun (k, m) -> set_progress_pair i j k m) tps
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
    let retval = check_soundness () in
    destroy_aut () ;
    if retval then Stats.MC.accept () else Stats.MC.reject () ;
    debug (fun () ->
        "Checking soundness ends, result=" ^ if retval then "OK" else "NOT OK" ) ;
    retval
end

module RelationalCheck = struct

  module IntPairMap = Lib.Treemap.Make(Pair.Make(Int)(Int))

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
      let s = match s with
      (* | Unknown -> "Unknown" *)
      | Stay -> "Stay"
      | Decrease -> "Decrease" in
      Format.fprintf fmt "%s" s
    
    let hash s = Hashtbl.hash s
    
  end

  module SlopedRel = struct

    module HashedKernel = struct
      (* domain-codomain mapping, codomain-domain mapping, slopes *)
      type t = 
        Int.Set.t Int.Map.t * Int.Set.t Int.Map.t * Slope.t IntPairMap.t
      let equal (_, _, p) (_, _, q) =
        IntPairMap.equal Slope.equal p q
      let pp fmt (_, _, p) =
        IntPairMap.pp Slope.pp fmt p
      let hash (_, _, p) =
        IntPairMap.hash Slope.hash p
    end

    module Set = struct
      include Hashset.Make(HashedKernel)
      let equal s s' =
        if Int.(cardinal s <> cardinal s') then
          false
        else
          try
            let () =
              iter (fun p -> if not (mem s' p) then raise Not_found) s in
            true
          with Not_found -> false
      let pp fmt s =
        let () = Format.fprintf fmt "@[[" in
        let first = ref true in
        let () =
          iter
            (fun p ->
              let () = if not !first then Format.fprintf fmt ", " in
              let () = first := false in
              let () = Format.fprintf fmt "%a" HashedKernel.pp p in
              ())
            s in
        let () = Format.fprintf fmt "]@]" in
        ()
    end
    
    include HashedKernel

    let empty = (Int.Map.empty, Int.Map.empty, IntPairMap.empty)

    let add (h, h', s) (fd, bk, slopes) =
      let upd x = function
      | None -> Some (Int.Set.singleton x)
      | Some s -> Some (Int.Set.add x s) in
      (Int.Map.update h (upd h') fd,
       Int.Map.update h' (upd h) bk,
       IntPairMap.update (h, h') 
        (function
          | None -> Some s
          | Some s' -> Some (Slope.max s s'))
        slopes)

    let has_decreasing_self_loop (_, _, slopes) =
      IntPairMap.exists
        (fun (n, n') s ->
          if Int.(n <> n') then
            false
          else
            match s with
            | Slope.Decrease ->
              true
            | _ ->
              false)
        slopes

    let compose (p_fd, p_bk, p_sl) (q_fd, q_bk, q_sl) = 
      Int.Map.fold
        (fun h hs ->
          Int.Map.fold
            (fun h' hs' result ->
              let s = 
                Int.Set.fold
                (fun h'' s ->
                  let s' =
                    Slope.max
                      (IntPairMap.find (h, h'') p_sl)
                      (IntPairMap.find (h'', h') q_sl) in
                  match s with
                  | None   -> Some s'
                  | Some s -> Some (Slope.max s s'))
                (Int.Set.inter hs hs')
                None in
              match s with
              | None   -> result
              | Some s -> add (h, h', s) result)
           q_bk)
        p_fd
        empty
    
    (* Repeat code for relational composition, so as to inline check for whether
       we have reached the fixed point - slightly more efficient than comparing
       new relation for equality with the old one at each iteration. *)
    let transitive_closure ((p_fd, p_bk, p_sl) as p) =
      let rec transitive_closure ((q_fd, q_bk, q_sl) as q) =
        let result, continue =
          Int.Map.fold
            (fun h hs ->
              Int.Map.fold
                (fun h' hs' (result, continue) ->
                  let s = 
                    Int.Set.fold
                    (fun h'' s ->
                      let s' =
                        Slope.max
                          (IntPairMap.find (h, h'') p_sl)
                          (IntPairMap.find (h'', h') q_sl) in
                        match s with
                          | None   -> Some s'
                          | Some s -> Some (Slope.max s s'))
                    (Int.Set.inter hs hs')
                    None in
                  let result =
                    match s with
                    | None   -> result
                    | Some s -> add (h, h', s) result in
                  let continue =
                    if Option.is_none s then
                      continue
                    else 
                      continue
                        ||
                      not (IntPairMap.mem (h, h') q_sl)
                        ||
                      Slope.((IntPairMap.find (h, h') q_sl) < (Option.get s)) in
                  result, continue)
              q_bk)
            p_fd
            (q, false) in
        if continue then transitive_closure result else result in
      transitive_closure p

  end

  (* A height graph is a set of nodes and sloped relation for each edge *)
  type height_graph = Int.Set.t * SlopedRel.t IntPairMap.t

  let pp_height_graph fmt (nodes, slopes) =
    Format.fprintf fmt "@[Nodes: %a@.Slopes: %a@]"
      Int.Set.pp nodes
      (IntPairMap.pp SlopedRel.pp) slopes

  (* Compute the composition closure of the given height graph *)
  let comp_closure (ns, slopes) =
    let init = 
      Int.Set.fold
        (fun n ccl ->
          Int.Set.fold
            (fun n' ccl ->
              let edge = (n, n') in
              let s = SlopedRel.Set.create 1 in
              let () =
                match (IntPairMap.find_opt edge slopes) with
                | None ->
                  ()
                | Some p ->
                  SlopedRel.Set.add s p in
              IntPairMap.add edge s ccl)
            ns
            ccl)
        ns
        IntPairMap.empty in
    let rec closure ccl =
      let ccl', cont =
        Int.Set.fold (fun n ->
        Int.Set.fold (fun n' (acc, cont) ->
          let old_slopes = IntPairMap.find (n, n') ccl in
          let new_slopes = SlopedRel.Set.copy old_slopes in
          let cont =
            Int.Set.fold (fun n'' ->
            SlopedRel.Set.fold (fun p ->
            SlopedRel.Set.fold (fun q cont ->
              let r = SlopedRel.compose p q in
              let () = SlopedRel.Set.add new_slopes r in
              cont || not (SlopedRel.Set.mem old_slopes r)
            ) (IntPairMap.find (n'', n') ccl)
            ) (IntPairMap.find (n, n'') ccl)
            ) ns cont in
          (IntPairMap.add (n, n') new_slopes acc, cont)
        ) ns) ns (IntPairMap.empty, false) in
      if cont then closure ccl' else ccl in
    closure init

  let pp_closure fmt ccl =
    IntPairMap.pp SlopedRel.Set.pp fmt ccl

  let check_proof p =
    Stats.MC.call () ;
    let to_height_graph p =
      Int.Map.fold
        (fun n (_, succs) (nodes, slopes) ->
          let nodes = Int.Set.add n nodes in
          let slopes = 
            List.fold_left
              (fun slopes (n', all_tags, prog_tags) ->
                let hs =
                  IntPairSet.fold
                    (fun (h, h') -> SlopedRel.add (h, h', Slope.Stay))
                    all_tags
                    SlopedRel.empty in
                let hs =
                  IntPairSet.fold
                    (fun (h, h') -> SlopedRel.add (h, h', Slope.Decrease))
                    prog_tags
                    hs in
                IntPairMap.add (n, n') hs slopes)
              slopes
              succs in
          (nodes, slopes))
        p
        (Int.Set.empty, IntPairMap.empty) in
    let () = debug (fun () -> "Checking soundness starts...") in
    let ((nodes, _) as g) = to_height_graph p in
    let () = debug (fun () -> "Height Graph:\n" ^ mk_to_string pp_height_graph g) in
    let ccl = comp_closure g in
    let () = debug (fun () -> "Composition Closure:\n" ^ mk_to_string pp_closure ccl) in
    let retval =
      try
        let () = 
          Int.Set.iter (fun n ->
          SlopedRel.Set.iter (fun p ->
            let () = debug (fun () -> "Checking " ^ mk_to_string SlopedRel.pp p) in
            let r = SlopedRel.transitive_closure p in
            let () = debug (fun () -> "Transitive closure: " ^ mk_to_string SlopedRel.pp r) in
            if not (SlopedRel.has_decreasing_self_loop r)
              then raise Not_found
          ) (IntPairMap.find (n, n) ccl)
          ) nodes in
        true
      with Not_found ->
        false in
    if retval then Stats.MC.accept () else Stats.MC.reject () ;
    debug (fun () ->
        "Checking soundness ends, result=" ^ if retval then "OK" else "NOT OK" ) ;
    retval
        

end

let use_spot = ref false

module CheckCache = Hashtbl

let ccache = CheckCache.create 1000
(* let limit = ref 1 *)

let check_proof ?(init=0) ?(minimize=true) prf =
  if (Int.Map.is_empty prf) then
    true
  else
    let () =
      if not (valid prf init) then (
        pp Format.std_formatter prf ;
        assert false )
    in
    let aprf =
      if minimize
        then minimize_abs_proof prf init
        else prf in
    if (Int.Map.is_empty aprf) then
      true
    else
      let () =
        if not (valid aprf init) then (
          pp Format.std_formatter aprf ;
          assert false )
      in
      try
        debug (fun _ -> mk_to_string pp prf) ;
        debug (fun () -> "Minimized proof:\n" ^ mk_to_string pp aprf) ;
        Stats.MCCache.call () ;
        let r = CheckCache.find ccache aprf in
        Stats.MCCache.end_call () ;
        Stats.MCCache.hit () ;
        let () =
          debug (fun _ ->
              "Found soundness result in the cache: "
              ^ if r then "OK" else "NOT OK" )
        in
        r
      with Not_found ->
        Stats.MCCache.end_call () ;
        Stats.MCCache.miss () ;
        let r =
          if !use_spot
            then AutomatonCheck.check_proof ~init aprf
            else RelationalCheck.check_proof aprf in
        Stats.MCCache.call () ;
        CheckCache.add ccache aprf r ;
        Stats.MCCache.end_call () ;
        (* if CheckCache.length ccache > !limit then                                          *)
        (*   begin                                                                            *)
        (*     debug (fun () -> "Soundness cache passed limit: " ^ (string_of_int !limit)) ;  *)
        (*     limit := 10 * !limit                                                           *)
        (*   end ;                                                                            *)
        r
