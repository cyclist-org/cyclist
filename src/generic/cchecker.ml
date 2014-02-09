open Lib
open Util

external create_aut : int -> unit = "create_aut" ;;
external destroy_aut: unit -> unit = "destroy_aut" ;;
external create_vertex : int -> unit = "create_vertex" ;;
external tag_vertex : int -> int -> unit = "tag_vertex" ;;
external set_successor : int -> int -> unit = "set_successor" ;;
external set_trace_pair : int -> int -> int -> int -> unit = "set_trace_pair" ;;
external set_progress_pair : int -> int -> int -> int -> unit = "set_progress_pair" ;;
external check_soundness : unit -> bool = "check_soundness" ;;
external set_initial_vertex : int -> unit = "set_initial_vertex" ;;

type abstract_proof_node =
  | LeafNode of Tags.t
  | InternalNode of Tags.t * int list * TagPairs.t list * TagPairs.t list
type abstract_proof_t = abstract_proof_node Int.Map.t

let pp_int fmt i = Format.fprintf fmt "@[%i@]" i

let pp_proof_node fmt n =
  let aux fmt = function
    | InternalNode(_, subg, _, _) -> pp_list pp_comma pp_int fmt subg
    | LeafNode _ -> Format.pp_print_string fmt "leaf" in
  Format.fprintf fmt "@[%a@]" aux n

let pp fmt prf =
  Format.open_vbox 0;
  Int.Map.iter
    (fun idx n -> Format.fprintf fmt "%i: %a@," idx pp_proof_node n) prf ;
  Format.close_box ()

let is_leaf = function
  | InternalNode _ -> false
  | LeafNode _ -> true

let in_children child = function
  | InternalNode(_,subg,_,_) -> Blist.exists (fun i -> i=child) subg
  | LeafNode _ -> false

(* has one child and is not a self loop *)
let is_single_node idx = function
  | LeafNode _ -> false
  | InternalNode(_, subg, _, _) -> match subg with
		| [idx'] -> idx'<>idx
		| _ -> false

(* let valid prf =                                                                  *)
(*   let valid_node idx = function                                                  *)
(*     | LeafNode _ -> true                                                         *)
(*     | InternalNode(_, subg, tvs, tps) ->                                         *)
(*       let l = Blist.length subg in l = Blist.length tvs && l = Blist.length tps &&  *)
(*       Blist.for_all (fun i -> Int.Map.mem i prf) subg in                          *)
(*   Int.Map.mem 0 prf && Int.Map.for_all valid_node prf                            *)


let remove_dead_nodes prf' =
  let prf = ref prf' in
  let process_node child par_idx n =
		if not (in_children child n) then () else
    let newparent = match n with
      | LeafNode _ -> assert false
      | InternalNode(tags, subg, tvs, tps) ->
				begin match subg with
  				| [_] -> LeafNode(tags)
  				| _ -> let pos = index (fun i -> i=child) subg in
        		InternalNode
						  (tags, remove_nth pos subg, remove_nth pos tvs, remove_nth pos tps)
				end in
    prf := Int.Map.add par_idx newparent !prf in
  let remove_dead_node idx n =
    let () = prf := Int.Map.remove idx !prf in
    Int.Map.iter (fun p n -> process_node idx p n) !prf in
  let cont = ref true in
  while !cont do
    match Int.Map.find_some (fun idx n -> idx<>0 && is_leaf n) !prf with
      | Some (idx, n) -> remove_dead_node idx n
      | None -> cont := false
  done ;
  !prf

let compose_tag_pairs t1 t2 =
  let compose_tag_pair ((i:Tags.elt),j) (l: (Tags.elt * Tags.elt) list) =
		let l = rev_filter (fun (k,_) -> k=j) l in
		Blist.rev_map (fun (_,l) -> (i,l)) l in
  let xs = TagPairs.to_list t1 in
	let ys = TagPairs.to_list t2 in
	TagPairs.of_list (Blist.flatten (Blist.map (fun p -> compose_tag_pair p ys) xs))

let get_tvs = function
  | InternalNode (_,_,tvs,_) -> tvs
  | LeafNode _ -> invalid_arg "get_tvs"

let get_tps = function
  | InternalNode (_,_,_,tps) -> tps
  | LeafNode _ -> invalid_arg "get_tps"

let get_subg = function
  | InternalNode (_,subg,_,_) -> subg
  | LeafNode _ -> invalid_arg "get_subg"

let fathers_grandchild prf idx node =
  let grandchild = Blist.hd (get_subg node) in
  Int.Map.exists
	  (fun idx' par_node -> in_children idx par_node && in_children grandchild par_node)
		prf

let fuse_single_nodes prf' =
  let prf = ref prf' in
  let process_node child grand_child tv tp par_idx n =
		if not (in_children child n) then () else
		match n with
    | LeafNode _ -> assert false
    | InternalNode(par_tags, par_subg, par_tvs, par_tps) ->
      let pos = index (fun i -> i=child) par_subg in
      let par_tv = Blist.nth par_tvs pos in
      let par_tp = Blist.nth par_tps pos in
      let newsubg = replace_nth grand_child pos par_subg in
      let newtvs = replace_nth (compose_tag_pairs par_tv tv) pos par_tvs in
      let newtps =
        replace_nth (
          TagPairs.union_of_list
          [compose_tag_pairs par_tp tp;
          compose_tag_pairs par_tv tp;
          compose_tag_pairs par_tp tv]
        ) pos par_tps in
      prf :=
        Int.Map.add par_idx (InternalNode(par_tags, newsubg, newtvs, newtps)) !prf in
  let fuse_node idx = function
    | LeafNode _ -> assert false
    | InternalNode(tags, subg, tvs, tps) ->
      let child = Blist.hd subg in
      let tv = Blist.hd tvs in
      let tp = Blist.hd tps in
      Int.Map.iter (fun p n -> process_node idx child tv tp p n) !prf ;
      prf := Int.Map.remove idx !prf in
  let cont = ref true in
  (* if a parent points to the child of the node to be fused then *)
  (* we would run into difficulties when updating that parent to point *)
  (* directly to the grandchild, so we avoid that altogether *)
	let p idx n = idx<>0 && is_single_node idx n && not (fathers_grandchild !prf idx n) in
  while !cont do
    match Int.Map.find_some p !prf with
      | Some (idx, n) -> fuse_node idx n
      | None -> cont := false
  done ;
  !prf

let minimize_abs_proof prf = fuse_single_nodes (remove_dead_nodes prf)


let tags_of_abstract_node = function
  | LeafNode(t) | InternalNode(t, _, _, _) -> t

(* check global soundness condition on proof *)
let check_proof p =
  Stats.MC.call ();
  let create_tags i n =
    Tags.iter (tag_vertex i) (tags_of_abstract_node n) in
  let create_succs i = function
    | InternalNode(_, l, _, _) -> Blist.iter (fun j -> set_successor i j) l
    | LeafNode _ -> () in
  let create_valid_tag_transitions i =
    let do_tag_transitions j tps =
      TagPairs.iter (fun (k,m) -> set_trace_pair i j k m) tps in
    function
      | InternalNode (_, l, vs, _) -> Blist.iter2 do_tag_transitions l vs
      | LeafNode _ -> () in
  let create_prog_tag_transitions i =
    let do_tag_transitions j tps =
      TagPairs.iter (fun (k,m) -> set_progress_pair i j k m) tps in
    function
      | InternalNode (_, l, _, ps) -> Blist.iter2 do_tag_transitions l ps
      | LeafNode _ -> () in
  let size = Int.Map.cardinal p in
  let log2size = 1 + int_of_float (ceil ((log (float_of_int size)) /. (log 2.0))) in
  debug (fun () -> "Checking soundness starts...") ;
  create_aut log2size ;
  Int.Map.iter (fun i _ -> create_vertex i) p ;
  Int.Map.iter create_tags p ;
  Int.Map.iter create_succs p ;
  set_initial_vertex 0 ;
  Int.Map.iter create_valid_tag_transitions p ;
  Int.Map.iter create_prog_tag_transitions p ;
  let retval = check_soundness () in
  destroy_aut () ;
  if retval then Stats.MC.accept () else Stats.MC.reject () ;
  debug
    (fun () -> "Checking soundness ends, result=" ^
    (if retval then "OK" else "NOT OK")) ;
  retval


(*  let abs_proof_node_equal n n' = match (n,n') with                                         *)
(*    | (LeafNode(tags, par_idx), LeafNode(tags', par_idx')) ->                               *)
(*      par_idx=par_idx' && Tags.equal tags tags'                                             *)
(*    | (InternalNode(tags, subg, tvs, tps, par_idx),                                         *)
(*       InternalNode(tags', subg', tvs', tps', par_idx')) ->                                 *)
(*      par_idx=par_idx' && Tags.equal tags tags' &&                                          *)
(*      (try                                                                                  *)
(*        Blist.for_all2 TagPairs.equal tvs tvs' &&                                            *)
(*        Blist.for_all2 TagPairs.equal tps tps'                                               *)
(*      with Invalid_argument _ -> false)                                                     *)
(*    | (_,_) -> false                                                                        *)
(*  let abs_proof_equal p p' = Int.Map.equal abs_proof_node_equal p p'                        *)
(*                                                                                            *)
(*  let abs_proof_node_hash = function                                                        *)
(*    | LeafNode(tags, par_idx) -> genhash par_idx (Hashtbl.hash (Tags.elements tags))        *)
(*    | InternalNode(tags, subg, tvs, tps, par_idx) ->                                        *)
(*      Hashtbl.hash (par_idx,                                                                *)
(*      (Hashtbl.hash (Tags.elements tags)),                                                  *)
(*      (Hashtbl.hash subg),                                                                  *)
(*      Hashtbl.hash (Blist.map Hashtbl.hash tvs),                                             *)
(*      Hashtbl.hash (Blist.map Hashtbl.hash tps))                                             *)
(*                                                                                            *)
(*  let abs_proof_hash p = Hashtbl.hash (Int.Map.bindings (Int.Map.map abs_proof_node_hash p))*)

(*  module CheckCache = Hashtbl.Make*)
(*    (struct                       *)
(*      type t = abstract_proof_t   *)
(*      let equal = abs_proof_equal *)
(*      let hash = abs_proof_hash   *)
(*    end)                          *)


module CheckCache = Hashtbl

let check_proof =
  let ccache = CheckCache.create 1000 in
  (* let limit = ref 1 in  *)
  let f prf =
		let () = Stats.MCCache.call () in
    (* let () = assert (valid prf) in *)
    let aprf = minimize_abs_proof prf in
    (* let () = if not (valid aprf) then  *)
    (*   begin                            *)
    (*     pp Format.std_formatter prf ;  *)
    (*     pp Format.std_formatter aprf ; *)
    (*     assert false                   *)
    (*   end in                           *)
    try
      let r = CheckCache.find ccache aprf in
			Stats.MCCache.accept () ; r
    with Not_found ->
      let r = check_proof aprf in
      CheckCache.add ccache aprf r ;
			(* Stats.MCCache.end_call () ; *)
      (* if CheckCache.length ccache > !limit then                                          *)
      (*   begin                                                                            *)
      (*     debug (fun () -> "Soundness cache passed limit: " ^ (string_of_int !limit)) ;  *)
      (*     limit := 10 * !limit                                                           *)
      (*   end ;                                                                            *)
      r in
  f
