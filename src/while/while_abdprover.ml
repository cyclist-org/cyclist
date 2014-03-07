open Lib
open Util
open Symheap
open While_program

module Apr = Prover.Make(While_program.Seq)(While_program.Defs)
include Apr

(* let latex_defs d =                 *)
(*   let t = !split_heaps in          *)
(*   let () = split_heaps := false in *)
(*   let res = Defs.to_latex d in     *)
(*   split_heaps := t ; res           *)

let dest_sh_seq = While_prover.dest_sh_seq

let last_pred = ref 0
let get_fresh_ident () = Printf.sprintf "I%.3d" (incr last_pred ; !last_pred)

let ex_subst_defs defs =
  let ex_subst_heap h =
    let (ex_eqs, non_ex_eqs) =
      Blist.partition (fun (x,_) -> Term.is_exist_var x) (UF.bindings h.eqs) in
      if ex_eqs=[] then h else
      (* NB order of subst is reversed so that *)
      (* the greater variable replaces the lesser *)
      (* this maintains universal vars *)
      Heap.subst (Term.Map.of_list ex_eqs) { h with eqs=UF.of_list non_ex_eqs } in
  let ex_subst_case c =
    let (p,h) = Case.dest c in
    let p' = Heap.fixpoint ex_subst_heap p in
    Case.mk p' h in
  Blist.map (fun (l,i) -> (Blist.map ex_subst_case l, i)) defs


let empify defs =
  let empify_ c =
    let (p,h) = Case.dest c in
    let inds = Inds.filter (fun pred -> Defs.is_defined pred defs) p.inds in
    Case.mk {p with inds=inds} h in
  Blist.map (fun (l,i) -> (Blist.map empify_ l, i)) defs

let inline defs =
  try
    let (p,h) as q =
      Blist.find
        begin fun (p,h) ->
          (Blist.length p)=1 &&
          let (f,_) = Case.dest (Blist.hd p) in
          let idents =
            Inds.map_to Strng.Set.add Strng.Set.empty (fun (_,(id,_)) -> id) f.inds in
          not (Strng.Set.mem h idents)
        end (Blist.but_last defs) in
    let defs = Blist.filter ((!=)q) defs in
    let unf = While_prover.gen_left_rules_f (p,h) in
    let f orig =
      let (p',h') = Case.dest orig in
      let first_unfold f =
        let apps = unf ([f],0) in
        if apps=[] then f else
          let ((f',_), _, _) = Blist.hd (Blist.hd apps) in
          Blist.hd f' in
          (* print_endline (Heap.to_string f'') ; f'' in             *)
      let p'' = Heap.fixpoint first_unfold p' in
      Case.mk p'' h' in
    Blist.map (fun (l,i) -> (Blist.map f l, i)) defs
  with Not_found -> defs


let used_only_recursively (heap, (ident, params)) pos =
  let var = Blist.nth params pos in
  let heap' = { heap with inds=Inds.empty } in
  if Term.Set.mem var (Heap.vars heap') then false else
  Inds.for_all
    begin fun (_,(ident', params')) ->
      if ident<>ident' then
        Blist.for_all (fun var' -> not (Term.equal var var')) params'
      else
        Blist.for_all2 
          (fun var' pos' -> pos=pos' || not (Term.equal var var')) 
          params' 
          (Blist.indexes params')
    end
    heap.inds

(* for all predicates *)
(* for all integers up to the arity of the predicate *)
(* if the position is unused in all clauses of the definition*)
(* then stop and report predicate and position *)
(* when all are exhausted return None *)
let find_unused_arg defs =
  let check_def (clauses, ident) =
    if clauses = [] then None else
    let check_pos pos =
      if Blist.for_all
        begin fun case ->
          let ((heap, (_, params)) as p) = Case.dest case in
          not (Term.Set.mem (Blist.nth params pos) (Heap.vars heap)) ||
          used_only_recursively p pos
        end
        clauses
      then
        Some (ident, pos)
      else
        None in
    Blist.find_some check_pos (Blist.range 0 (snd (snd (Case.dest (Blist.hd clauses))))) in
  if Blist.length defs=1 then
    None
  else
    Blist.find_some check_def (Blist.but_last defs)

let eliminate (ident, pos) defs =
  let elim_clause case =
    let (heap, (ident', params)) = Case.dest case in
    let elim_pred heap =
      { heap with
          inds = Inds.endomap
            begin fun (t, (ident'', params')) ->
              (t, (ident'', if ident=ident'' then Blist.remove_nth pos params' else params'))
            end
            heap.inds
      } in
    if ident<>ident' then
      Case.mk (elim_pred heap) (ident', params)
    else
      Case.mk (elim_pred heap) (ident', Blist.remove_nth pos params) in
  Blist.map (fun (cl, ident') -> (Blist.map elim_clause cl, ident')) defs

let elim_dead_vars defs =
  match find_unused_arg defs with
    | None -> defs
    | Some (ident, pos) -> eliminate (ident, pos) defs

let simplify_defs defs =
  Defs.fixpoint
    (fun d -> elim_dead_vars (inline (ex_subst_defs d)))
    (empify defs)


let is_possibly_consistent defs = Defs.consistent (empify defs)

let ex_falso_axiom = mk_axiom While_prover.ex_falso_axiom_f "Ex Falso"
let symex_empty_axiom = mk_axiom While_prover.symex_empty_axiom_f "Empty"


let lhs_disj_to_symheaps = mk_inf_rule While_prover.lhs_disj_to_symheaps_f "L.Or"
let eq_subst_ex = mk_inf_rule While_prover.eq_subst_ex_f "= ex subst"

let simpl_deqs =
  mk_inf_rule
    begin fun seq ->
      try
        let (f,cmd) = dest_sh_seq seq in
        let terms =
					Term.Set.add Term.nil (Heap.vars { f with deqs=Deqs.empty }) in
        let newdeqs =
          Deqs.filter
            (fun (x,y) ->
							Term.equal x y || (Term.Set.mem x terms && Term.Set.mem y terms))
          f.deqs in
        let f' = { f with deqs=newdeqs } in
        if Heap.equal f f' then [] else
				let s = ([f'], cmd) in
        [ [ (s, While_prover.tagpairs s, TagPairs.empty) ] ]
      with Not_symheap -> []
    end
  "!= simpl"


let norm =
	let rl (l,cmd) =
    let l' = Form.norm l in
    if Form.equal l l' then [] else
		let s = (l',cmd) in
    [[ (s, While_prover.tagpairs s, TagPairs.empty) ]] in
	mk_inf_rule rl "Norm"

let simplify =
  let or_rules = Proof_tacs.first [ (*norm;*) eq_subst_ex; simpl_deqs ] in
  Proof_tacs.repeat_tac or_rules
let wrap r = Proof_tacs.then_tac r (Proof_tacs.try_tac simplify)



(* symbolic execution rules *)
let symex_stop_axiom = mk_axiom While_prover.symex_stop_axiom_f "Stop"
let symex_load_rule = mk_inf_rule While_prover.symex_load_rule_f "Load"
let symex_store_rule = mk_inf_rule While_prover.symex_store_rule_f "Store"
let symex_free_rule = mk_inf_rule While_prover.symex_free_rule_f "Free"
let symex_new_rule = mk_inf_rule While_prover.symex_new_rule_f "New"
let symex_skip_rule = mk_inf_rule While_prover.symex_skip_rule_f "Skip"
let symex_assign_rule = mk_inf_rule While_prover.symex_assign_rule_f "Assign"

let symex_det_if_rule =
  let rl seq =
    try
      let (f,cmd) = dest_sh_seq seq in
      let (c,cmd') = Cmd.dest_if cmd in
      if Cond.is_non_det c then [] else
      let (x,y) = Cond.dest c in
      let cont = Cmd.get_cont cmd in
      let mk_ret c = While_prover.fix_tps [[ ([f],c) ]] in
      match (Cond.is_deq c, Heap.equates f x y, Heap.disequates f x y) with
        (* cmd wants equality and formula provides it so take the branch *)
        (* cmd wants disequality and formula provides it so take branch *)
        | (false, true, _) | (true, _, true) -> mk_ret (Cmd.mk_seq cmd' cont)
        (* cmd wants equality and formula forbids it so take other branch *)
        (* cmd wants disequality and formula forbids it so take other branch *)
        | (false, _, true) | (true, true, _) -> mk_ret cont
        (* formula allows either fact so fail *)
        (* or otherwise don't know so fail *)
        | _ -> []
    with Not_symheap | WrongCmd -> [] in
  mk_inf_rule rl "If(det)"

let symex_nondet_if_rule =
  let rl seq =
    try
      let (f,cmd) = dest_sh_seq seq in
      let (c,cmd') = Cmd.dest_if cmd in
      if Cond.is_det c then [] else
      let cont = Cmd.get_cont cmd in
      While_prover.fix_tps [[ ([f], Cmd.mk_seq cmd' cont); ([f], cont) ]]
    with Not_symheap | WrongCmd -> [] in
  mk_inf_rule rl "If(nondet)"

let symex_nondet_ifelse_rule =
  let rl seq =
    try
      let (f,cmd) = dest_sh_seq seq in
      let (c,cmd',cmd'') = Cmd.dest_ifelse cmd in
      if Cond.is_det c then [] else
      let cont = Cmd.get_cont cmd in
      While_prover.fix_tps
			  [[ ([f], Cmd.mk_seq cmd' cont); ([f], Cmd.mk_seq cmd'' cont) ]]
    with Not_symheap | WrongCmd -> [] in
  mk_inf_rule rl "IfElse(nondet)"

let symex_nondet_while_rule =
  let rl seq =
    try
      let (f,cmd) = dest_sh_seq seq in
      let (c,cmd') = Cmd.dest_while cmd in
      if Cond.is_det c then [] else
      let cont = Cmd.get_cont cmd in
      While_prover.fix_tps [[ ([f], Cmd.mk_seq cmd' cmd); ([f], cont) ]]
    with Not_symheap | WrongCmd -> [] in
  mk_inf_rule rl "If(nondet)"

let symex_det_ifelse_rule =
  let rl seq =
    try
      let (f,cmd) = dest_sh_seq seq in
      let (c,cmd',cmd'') = Cmd.dest_ifelse cmd in
      if Cond.is_non_det c then [] else
      let (x,y) = Cond.dest c in
      let cont = Cmd.get_cont cmd in
      let mk_ret c = While_prover.fix_tps [[ ([f], c) ]] in
      match (Cond.is_deq c, Heap.equates f x y, Heap.disequates f x y) with
        (* cmd wants equality and formula provides it so take the branch *)
        (* cmd wants disequality and formula provides it so take branch *)
        | (false, true, _) | (true, _, true) -> mk_ret (Cmd.mk_seq cmd' cont)
        (* cmd wants equality and formula forbids it so take other branch *)
        (* cmd wants disequality and formula forbids it so take other branch *)
        | (false, _, true) | (true, true, _) -> mk_ret (Cmd.mk_seq cmd'' cont)
        (* formula allows either fact so fail *)
        (* or don't know so fail *)
        | _ -> []
    with Not_symheap | WrongCmd -> [] in
  mk_inf_rule rl "If(det)"

let symex_det_while_rule =
  let rl seq =
    try
      let (f,cmd) = dest_sh_seq seq in
      let (c,cmd') = Cmd.dest_while cmd in
      if Cond.is_non_det c then [] else
      let (x,y) = Cond.dest c in
      let cont = Cmd.get_cont cmd in
      let mk_ret c = While_prover.fix_tps [[ ([f], c) ]] in
      match (Cond.is_deq c, Heap.equates f x y, Heap.disequates f x y) with
        (* cmd wants equality and formula provides it so take the branch *)
        (* cmd wants disequality, formula provides it so take branch *)
        | (false, true, _) | (true, _, true) -> mk_ret (Cmd.mk_seq cmd' cmd)
        (* cmd wants equality and formula forbids it so take other branch *)
        (* cmd wants disequality and formula forbids it so take other branch *)
        | (false, _, true) | (true, true, _) -> mk_ret cont
        (* formula allows either fact so fail *)
        (* otherwise don't know so fail *)
        | _ -> []
    with Not_symheap | WrongCmd -> [] in
  mk_inf_rule rl "While(det)"


let generalise_while_rule =
  let generalise m h =
		let avoid = ref (Heap.vars h) in
    let gen_term t =
			if Term.Set.mem t m then
				(let r = fresh_evar !avoid in avoid := Term.Set.add r !avoid ; r)
			else t in
    let gen_pto (x,args) =
    	let l = Blist.map gen_term (x::args) in (Blist.hd l, Blist.tl l) in
		{ h with
			eqs = Term.Set.fold UF.remove m h.eqs;
		  deqs =
				Deqs.filter
				  (fun p -> Pair.conj (Pair.map (fun z -> not (Term.Set.mem z m)) p))
					h.deqs;
			ptos = Ptos.endomap gen_pto h.ptos
	  } in
  let rl seq =
    try
      let (f,cmd) = dest_sh_seq seq in
      let (_,cmd') = Cmd.dest_while cmd in
			let m = Term.Set.inter (Cmd.modifies cmd') (Heap.vars f) in
			let subs = Term.Set.subsets m in
			Option.list_get (Blist.map
			  begin fun m' ->
      		let f' = generalise m' f in
      		if Heap.equal f f' then None else
					let s' = ([f'], cmd) in
          Some [ (s', While_prover.tagpairs s', TagPairs.empty) ]
			  end
				subs)
    with Not_symheap | WrongCmd -> [] in
  mk_inf_rule rl "Gen.While"

(* abduction rules*)

let abd_deref =
  let rl seq defs =
    debug (fun () -> "Abd deref") ;
    try
      let (f,cmd) = dest_sh_seq seq in
      let x = Cmd.dest_deref cmd in
      let inds = Inds.elements f.inds in
      (* filter formula predicates by undefinedness *)
      let inds = Blist.filter (fun pred -> not (Defs.is_defined pred defs)) inds in
		  if inds=[] then [] else
      let fresh_ident = get_fresh_ident () in
      let f (_,(ident, params)) =
        let newparams = fresh_uvars (Term.Set.empty) (Blist.length params) in
        let head = (ident, newparams) in
        let pto_params =
          fresh_evars (Term.Set.of_list newparams) (Field.get_no_fields ()) in
        let newxs =
					Blist.map 
            (Blist.nth newparams) 
            (Blist.find_indexes (Heap.equates f x) params) in
        Blist.map
				  begin fun newx ->
						let clause =
							Heap.star
  							(Heap.mk_pto newx pto_params)
  							(Heap.mk_ind 1 fresh_ident (newparams @ pto_params)) in
        	( [Case.mk clause head], ident )::defs
				  end
					newxs in
      Blist.bind f inds
    with Not_symheap | WrongCmd -> [] in
  mk_abd_inf_rule rl "Abd. deref"


let abd_det_guard =
  let rl seq defs =
    try
      let (f,cmd) = dest_sh_seq seq in
      if not (Cmd.is_if cmd || Cmd.is_ifelse cmd || Cmd.is_while cmd) then [] else
      let c =
        begin
          if Cmd.is_if cmd then fst (Cmd.dest_if cmd) else
          if Cmd.is_ifelse cmd then let (c,_,_)  = Cmd.dest_ifelse cmd in c else
          fst (Cmd.dest_while cmd)
        end in
      if Cond.is_non_det c then [] else
			let cv_size = Term.Set.cardinal (Cond.vars c) in
			if cv_size = 0 then [] else
			let with_nil = (cv_size = 1) in
      let (x,y) = Cond.dest c in
      let inds = Inds.elements f.inds in
      (* filter formula predicates by undefinedness *)
      let inds = Blist.filter (fun pred -> not (Defs.is_defined pred defs)) inds in
			if inds=[] then [] else
      let (fresh_ident, fresh_ident') = Pair.map get_fresh_ident ((),()) in
      let f (_,(ident, params)) =
        let newparams = fresh_uvars (Term.Set.empty) (Blist.length params) in
        let head = (ident, newparams) in
				let matches nil_const z =
          if nil_const && Term.is_nil z then [Term.nil] else
          	Blist.map (Blist.nth newparams) (Blist.find_indexes (Heap.equates f z) params) in
				let occurrences =
					Blist.cartesian_product (matches false x) (matches false y) @
					(if with_nil then
					  Blist.cartesian_product (matches true x) (matches true y)
					else
						[]
					)
					in
				let g pair =
          let clause_eq =
            { Heap.empty with
              eqs=UF.of_list [pair] ;
              inds=Inds.singleton (1, (fresh_ident, newparams))
            } in
          let clause_deq =
            { Heap.empty with
              deqs=Deqs.singleton pair ;
              inds=Inds.singleton (1, (fresh_ident', newparams))
            } in
          ( [Case.mk clause_eq head; Case.mk clause_deq head], ident )::defs in
			  Blist.map g occurrences in
      Blist.bind f inds
    with Not_symheap -> [] in
  mk_abd_inf_rule rl "Abd. det guard"

let abd_back_rule =
  let rl s1 s2 defs =
    try
      let ((l1,cmd1),(l2,cmd2)) = Pair.map dest_sh_seq (s1,s2) in
      if
        not (Cmd.equal cmd1 cmd2) ||
        Deqs.cardinal l1.deqs < Deqs.cardinal l2.deqs ||
        Ptos.cardinal l1.ptos < Ptos.cardinal l2.ptos
      then
        []
      else
      (* find set of identifiers of ind preds in s1/s2 *)
      let (inds1,inds2) = Pair.map Heap.get_idents (l1,l2) in
      (* find fresh ones in s1 *)
      let candidates =
				Inds.filter (fun pred -> not (Defs.is_defined pred defs)) l1.inds in
      (* discard those that already exist in s2 *)
      let candidates =
        Inds.filter
          begin fun (_,(ident,_)) ->
            Inds.for_all (fun (_,(ident',_)) -> not (Strng.equal ident ident')) l2.inds
          end
          candidates in
      (* for each candidate there must exist one in s2 which *)
      (* if it replaces the candidate in s1, makes inds2 a subset of inds1 *)
      (* this is to overapproximate subsumption *)
      let cp = Blist.cartesian_product (Inds.to_list candidates) (Inds.to_list l2.inds) in
      let cp = Blist.filter
        (fun ((_,(c,_)),(_,(c',_))) ->
          Strng.MSet.subset inds2 (Strng.MSet.add c' (Strng.MSet.remove c inds1)))
        cp in
			if cp=[] then [] else
      let fresh_ident = get_fresh_ident () in
			let f ((_,(c,params)),(_,(c',params'))) =
        let newparams = fresh_uvars Term.Set.empty (Blist.length params) in
				(* does this need generalising like det_guard? *)
				let matches z =
          if Term.is_var z then
          	Blist.map (Blist.nth newparams) (Blist.find_indexes (Heap.equates l1 z) params)
          else [Term.nil] in
				let combinations = Blist.choose (Blist.map matches params') in
				Blist.map
          (fun perm ->
						let cl =
              { Heap.empty with
                inds=Inds.of_list [(0,(c',perm)); (0, (fresh_ident, newparams))]
              } in
            (( [Case.mk cl (c, newparams)], c )::defs))
				  combinations in
      Blist.bind f cp
    with Not_symheap -> [] in
  mk_abd_back_rule rl "Abd. backlink"


let matches = mk_back_rule While_prover.matches_fun "Backl"

(* NOT UPDATED FOR TERMINATION *)
(* let abd_segment =                                                                           *)
(*   let rl seq defs =                                                                         *)
(*     try                                                                                     *)
(*       let (h,cmd) = dest_sh_seq seq in                                                      *)
(*       let (c,cmd') = Cmd.dest_while cmd in                                                  *)
(* 			(* only consider disequality guards *)                                                *)
(* 			if not (Cond.is_deq c) then [] else                                                   *)
(* 		  let guard_vars = Cond.vars c in                                                       *)
(* 			let guard_terms = Cond.terms c in                                                     *)
(* 			(* only consider disequalities to nil *)                                              *)
(* 			if Term.Set.is_empty guard_terms then [] else                                         *)
(* 			let mod_vars = Cmd.modifies cmd' in                                                   *)
(* 			let gen_vars = Term.Set.inter guard_vars mod_vars in                                  *)
(* 			if Term.Set.cardinal gen_vars <> 1 then [] else                                       *)
(* 		  let y = Term.Set.choose gen_vars in                                                   *)
(*   		let eq_y = Term.Set.filter Term.is_univ_var	(Heap.eq_class h y) in                   *)
(* 			(* xs are prog vars equal to y now, and unmodified by the loop body *)                *)
(* 			let xs = Term.Set.diff eq_y mod_vars in                                               *)
(* 			(* is this always needed ? *)                                                         *)
(* 			if Term.Set.is_empty xs then [] else                                                  *)
(*       (* filter formula predicates by undefinedness *)                                      *)
(*       let inds =                                                                            *)
(* 				Inds.filter (fun pred -> not (Defs.is_defined pred defs)) h.inds in                 *)
(*       (* further filter by having a y-equiv term in parameter list, *)                      *)
(*       let inds =                                                                            *)
(*         Inds.filter (fun (_,(_,params)) ->                                                  *)
(*           Blist.exists (fun x -> Term.Set.mem x xs) params) inds in                          *)
(* 		  if Inds.is_empty inds then [] else                                                    *)
(* 		  (* weaken by removing y from its equivalence class *)                                 *)
(* 			let h' = { h with eqs=UF.remove y h.eqs } in                                          *)
(* 			(* is this always needed ? *)                                                         *)
(* 			if Heap.equal h h' then [] else                                                       *)
(*       let (fresh_ident) = get_fresh_ident () in                                             *)
(*       let f ((_,(ident, params)) as i) =                                                    *)
(*         let newparams = fresh_uvars Term.Set.empty (Blist.length params) in                  *)
(* 				let xi = Blist.find_index (fun x -> Term.Set.mem x xs) params in                               *)
(*         let x = Blist.nth newparams xi in                                                    *)
(* 				let head = (ident, newparams) in                                                    *)
(* 				let newpred = (1, (fresh_ident, newparams @ [x])) in                                *)
(* 				let newpred' = (1, (fresh_ident, newparams @ [Term.nil])) in                        *)
(*         let clause = { Heap.empty with inds=Inds.of_list [newpred; newpred']} in            *)
(*         let new_defs = ( [Case.mk clause head], ident )::defs in                            *)
(* 				let h' = { h' with inds=Inds.filter ((!=)i) h'.inds } in                            *)
(* 				let newpred = (1, (fresh_ident, params @ [y])) in                                   *)
(* 				let newpred' = (1, (fresh_ident, Blist.replace_nth y xi params @ [Term.nil])) in          *)
(* 				let h' = { h' with inds=Inds.union h'.inds (Inds.of_list [newpred;newpred']) } in   *)
(* 				let () = debug (fun () -> "Heap: " ^ (Heap.to_string h')) in                        *)
(* 				let () = debug (fun () -> "Clause: " ^ (Heap.to_string clause)) in                  *)
(*         Some ([ (([h'], cmd), While_prover.symex_tagpairs, TagPairs.empty) ], new_defs) in *)
(*       Option.list_get (Blist.map f (Inds.to_list inds))                                      *)
(*     with Not_symheap | WrongCmd -> [] in                                                    *)
(*   mk_gen_rule rl "Abd. segment"                                                             *)

(* let abd_backlink_cut =                                                           *)
(*   let rl s1 s2 defs =                                                            *)
(*     try                                                                          *)
(*       let ((l1,cmd1),(l2,cmd2)) = Pair.map dest_sh_seq (s1,s2) in                *)
(*   	  if not (Cmd.equal cmd1 cmd2) then [] else                                  *)
(* 			let () = debug (fun () -> "CUTLINK1: trying: " ^ (Seq.to_string s2)) in    *)
(* 			let () = debug (fun () -> "                  " ^ (Seq.to_string s1)) in    *)
(*       let (ids1,ids2) = Pair.map Heap.get_idents (l1,l2) in                      *)
(* 			let (ids1,ids2) =                                                          *)
(* 				(Strng.MSet.diff ids1 ids2, Strng.MSet.diff ids2 ids1) in                *)
(* 			if Strng.MSet.cardinal ids1 <> 1 || Strng.MSet.cardinal ids2 <> 1 then     *)
(* 				(* (print_endline ("1" ^                                            *)   *)
(* 				(* (Strng.MSet.to_string ids1) ^ "/" ^  (Strng.MSet.to_string ids2) *)   *)
(* 				[] else                                                                  *)
(* 		  let (id1,id2) = Pair.map Strng.MSet.choose (ids1,ids2) in                  *)
(* 			if Defs.mem id1 defs then [] else                                          *)
(* 			let f i h = Inds.filter (fun (_,(i',_)) -> i=i') h.inds in                 *)
(* 			let (inds1,inds2) = (f id1 l1, f id2 l2) in                                *)
(*       let cp = Blist.cartesian_product (Inds.to_list inds1) (Inds.to_list inds2) in    *)
(*       let g ((_,(_,params)),(_,(_,params'))) =                                   *)
(*         let newparams = fresh_uvars Term.Set.empty (Blist.length params) in       *)
(*   			(* does this need generalising like det_guard? *)                        *)
(*   			let matches z =                                                          *)
(*           (* if Term.is_var z then *)                                            *)
(*           	List.map (Blist.nth newparams) (Blist.find_indexes (Heap.equates l1 z) params)   *)
(*           (* else [Term.nil]  *)                                                 *)
(*   				in                                                                     *)
(*   			let combinations = choose (Blist.map matches params') in                  *)
(*   			List.map                                                                 *)
(*           (fun perm ->                                                           *)
(*   					let cl = { Heap.empty with inds=Inds.singleton (1,(id2,perm)) } in   *)
(*             (( [Case.mk cl (id1, newparams)], id1 )::defs))                      *)
(*   			  combinations in                                                        *)
(*       Blist.bind g cp                                               *)
(*     with Not_symheap | WrongCmd -> [] in                                         *)
(*   mk_abd_back_rule rl "Abd. Cut backlink"                                        *)

(* let abd_mid_backlink_cut =                                                       *)
(*   let rl _ _ defs =                                                              *)
(* 		let () = debug (fun () -> "CUTLINK2: ") in                                   *)
(* 		let () = debug (fun () -> Defs.to_string (empify defs)) in                   *)
(* 		let l = Blist.length defs in                                                  *)
(* 		if l<2 then [] else                                                          *)
(* 		begin                                                                        *)
(*   		Slprover.minbound := 1;                                                    *)
(*   		Slprover.maxbound := 11;                                                   *)
(*   		Slprover.setup (empify defs) ;                                             *)
(*   		[defs]                                                                     *)
(* 		end in                                                                       *)
(*   mk_abd_back_rule rl "Abd. Mid Cut backlink"                                    *)

(* let step = ref 0                                                                 *)

(* let backlink_cut =                                                               *)
(*   let rl s1 s2 =                                                                 *)
(* 		let () = incr step in                                                        *)
(*     let ((l1,cmd1),(l2,cmd2)) = (s1,s2) in                                       *)
(* 		let () = debug (fun () -> "CUTLINK3: trying: " ^ (Seq.to_string s2)) in      *)
(* 		let () = debug (fun () -> "                  " ^ (Seq.to_string s1)) in      *)
(* 		let () = debug (fun () -> "CUTLINK3: step = " ^ (string_of_int !step)) in    *)
(* 		if !step <> 22 then None else                                                *)
(* 	  if not (Cmd.equal cmd1 cmd2) then                                            *)
(* 			(debug (fun () -> "CUTLINK3: cmds unequal") ; None) else                   *)
(*     let olddebug = !Lib.do_debug in                                              *)
(*     let () = Lib.do_debug := true in                                             *)
(*     let result = Option.is_some (Slprover.idfs (l1, l2)) in                      *)
(*     let () = Lib.do_debug := olddebug in                                         *)
(* 		let () = debug (fun () -> "CUTLINK3: result: " ^ (string_of_bool result)) in *)
(* 		if result then Some While_prover.symex_tagpairs else None in                *)
(*   mk_back_rule rl "Cut backlink"                                                 *)

(* let cut_backlink_tac =                                                           *)
(* 	Proof_tacs.seq [ abd_backlink_cut; abd_mid_backlink_cut; backlink_cut ]        *)

let unfold =
  let gen_left_rule_fun seq defs =
    let rls = Blist.map While_prover.gen_left_rules_f defs in
    let rl = Seq_tacs.or_tac rls in
    let apps = rl seq in
    Blist.map (fun app -> (app,defs)) apps in
  mk_gen_rule gen_left_rule_fun "unfold"

let unfold2 =
  let gen_left_rule_fun seq defs =
    let rls = Blist.map While_prover.gen_left_rules_f defs in
    let rl = Seq_tacs.or_tac rls in
    let apps = rl seq in
    Blist.map (fun app -> (app,defs)) apps in
  mk_gen_rule gen_left_rule_fun "UNFOLD"

let u r = Proof_tacs.then_tac (Proof_tacs.opt unfold) r

let deref_tac =
  (Proof_tacs.first [wrap symex_load_rule; wrap symex_store_rule; wrap symex_free_rule])
let det_guard_tac =
  Proof_tacs.first [wrap symex_det_if_rule; wrap symex_det_ifelse_rule; wrap symex_det_while_rule]


let abd_symex abd symex =
  Proof_tacs.then_tac abd (Proof_tacs.then_tac unfold symex)

let ifwhile_tac =
  Proof_tacs.first [ det_guard_tac; abd_symex abd_det_guard det_guard_tac ]

let gen_ifwhile_tac =
	Proof_tacs.then_tac generalise_while_rule ifwhile_tac

(* these rules can rarely create a non-symex loop in termination checking *)
let mk_rules _ =
  [
		(* lhs_disj_to_symheaps ; *)
    (* simplify ; *)

		u matches;
		u (abd_symex abd_back_rule matches);
    u symex_skip_rule ;
    u symex_new_rule ;
    u symex_nondet_if_rule ;
    u symex_nondet_ifelse_rule ;
    u symex_nondet_while_rule ;
		u symex_assign_rule ;

    u (Proof_tacs.first [ deref_tac; abd_symex abd_deref deref_tac ]);

		u ifwhile_tac;
    u gen_ifwhile_tac;

		(* Proof_tacs.then_tac                                           *)
		(*   abd_segment                                                 *)
		(* 	(Proof_tacs.or_tac [ifwhile_tac; gen_ifwhile_tac]); *)

		(* cut_backlink_tac                                               *)
    unfold2;
  ]

let setup () =
  axiomset := [ symex_empty_axiom ; ex_falso_axiom ; symex_stop_axiom ] ;
