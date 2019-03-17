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

type abstract_node = Int.Set.t * (int * IntPairSet.t * IntPairSet.t) list

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
    (List.map
       (fun (id, tags, premises) ->
         let premises =
           List.map
             (fun (target, allpairs, progpairs) ->
               ( target
               , IntPairSet.of_list allpairs
               , IntPairSet.of_list progpairs ) )
             premises
         in
         (id, (Int.Set.of_list tags, premises)) )
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

let fuse_single_nodes prf' =
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
    (not (Int.equal idx 0))
    && is_single_node idx n
    && not (fathers_grandchild !prf idx n)
  in
  while !cont do
    match Int.Map.find_map p !prf with
    | Some (idx, n) -> fuse_node idx n
    | None -> cont := false
  done ;
  !prf

let minimize_abs_proof prf = fuse_single_nodes (remove_dead_nodes prf)

(* check global soundness condition on proof *)
let check_proof p =
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
  set_initial_vertex 0 ;
  Int.Map.iter create_trace_pairs p ;
  let retval = check_soundness () in
  destroy_aut () ;
  if retval then Stats.MC.accept () else Stats.MC.reject () ;
  debug (fun () ->
      "Checking soundness ends, result=" ^ if retval then "OK" else "NOT OK" ) ;
  retval

let valid prf =
  let projectl = IntPairSet.map_to Int.Set.add Int.Set.empty Pair.left in
  let projectr = IntPairSet.map_to Int.Set.add Int.Set.empty Pair.right in
  Int.Map.mem 0 prf
  && Int.Map.for_all
       (fun _ n ->
         Blist.for_all
           (fun (i, tv, tp) ->
             Int.Map.mem i prf && IntPairSet.subset tp tv
             && Int.Set.subset (projectl tv) (get_tags n)
             && (* Int.Set.subset (projectl tp) (get_tags n) &&                          *)
                (* trivially true, given the previous clause, whenever tp a subset of tv *)
                Int.Set.subset (projectr tv) (get_tags (Int.Map.find i prf))
             (*   &&  *)
             (* Int.Set.subset (projectr tp) (get_tags (Int.Map.find i prf))          *)
             (* trivially true, given the previous clause, whenever tp a subset of tv *)
             )
           (get_subg n) )
       prf

module CheckCache = Hashtbl

let check_proof =
  let ccache = CheckCache.create 1000 in
  (* let limit = ref 1 in  *)
  let f prf =
    let () =
      if not (valid prf) then (
        pp Format.std_formatter prf ;
        assert false )
    in
    let aprf = minimize_abs_proof prf in
    let () =
      if not (valid aprf) then (
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
      let r = check_proof aprf in
      Stats.MCCache.call () ;
      CheckCache.add ccache aprf r ;
      Stats.MCCache.end_call () ;
      (* if CheckCache.length ccache > !limit then                                          *)
      (*   begin                                                                            *)
      (*     debug (fun () -> "Soundness cache passed limit: " ^ (string_of_int !limit)) ;  *)
      (*     limit := 10 * !limit                                                           *)
      (*   end ;                                                                            *)
      r
  in
  f
