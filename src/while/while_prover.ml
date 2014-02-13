open Lib
open Util
open Symheap
open While_program

let tagpairs s =
	if !termination then
		TagPairs.mk (Seq.tags s)
	else
		Seq.tagpairs_one

(* following is for symex only *)
let progpairs () = 
	if !termination then TagPairs.empty else Seq.tagpairs_one

module SP = Prover.Make(While_program.Seq)(Symheap.Defs)
include SP

let dest_sh_seq (l,cmd) = (Form.dest l, cmd)


(* axioms *)
let ex_falso_axiom_f, ex_falso_axiom = 
  let ax ((f,_):sequent) = Form.inconsistent f in 
  ax, SP.mk_axiom ax "Ex Falso"

let symex_stop_axiom_f, symex_stop_axiom =
  let ax ((_,cmd):sequent) = Cmd.is_stop cmd in 
  ax, SP.mk_axiom ax "Stop"

let symex_empty_axiom_f, symex_empty_axiom =
  let ax ((_,cmd):sequent) = Cmd.is_empty cmd in
  ax, SP.mk_axiom ax "Empty"

(* simplification rules *)
let eq_subst_ex_f ((l,cmd) as s) =
  let l' = Form.subst_existentials l in
  if Form.equal l l' then [] else
  [ [ ((l', cmd), tagpairs s, TagPairs.empty) ] ]

let norm ((l,cmd) as s) = 
  let l' = Form.norm l in
  if Form.equal l l' then [] else
  [ [( (l',cmd), tagpairs s, TagPairs.empty)] ] 

let simplify_rules = [ (norm, "norm") ; (eq_subst_ex_f, "= ex subst") ]

let simplify_seq_rl = 
  SP.Seq_tacs.repeat_tac (SP.Seq_tacs.first (Blist.map fst simplify_rules))

let simplify =
  SP.mk_inf_rule simplify_seq_rl "Simpl"  

let wrap r d =
    SP.mk_inf_rule
      (SP.Seq_tacs.then_tac r (SP.Seq_tacs.try_tac simplify_seq_rl))
      d

(* break LHS disjunctions *)
let lhs_disj_to_symheaps_f, lhs_disj_to_symheaps =
  let rl ((l,cmd): sequent) =
    if Blist.length l < 2 then [] else
    [ Blist.map 
        (fun sh -> let s' = ([sh],cmd) in (s', tagpairs s', TagPairs.empty ) ) 
        l
    ] in
  rl, SP.mk_inf_rule rl "L.Or"

let gen_left_rules_f (def, ident) seq =
  try
    let (l,cmd) = dest_sh_seq seq in
    let preds = 
      Inds.filter (fun (_,(ident',_)) -> Strng.equal ident ident') l.inds in
    if Inds.is_empty preds then [] else
    let left_unfold ((id,(_,pvs)) as p) = 
			let ts = Tags.inter (Heap.tags l) (Heap.tags l) in
      let l' = { l with inds=Inds.remove p l.inds } in
      let do_case case =
        let (f', (_,vs')) = Case.dest (freshen_case_by_seq seq case) in
        let theta = Term.Map.of_list (Blist.combine vs' pvs) in
        let f' = Heap.subst theta f' in
        let f' = Heap.repl_tags id f' in
        let l' = Heap.star l' f' in
        ( 
					([l'],cmd), 
					(if !termination then TagPairs.mk ts else Seq.tagpairs_one), 
					(if !termination then TagPairs.singleton (id,id) else TagPairs.empty)
				) in
      Blist.map do_case def in
    Inds.map_to_list left_unfold preds
  with Not_symheap -> [] 
 
let gen_left_rules (def,ident) = 
  wrap (gen_left_rules_f (def,ident)) (ident ^ " L.Unf.")

(* FOR SYMEX ONLY *)
let fix_tps l = 
  Blist.map 
    (fun g -> Blist.map (fun s -> (s, tagpairs s, progpairs () )) g) l 

let mk_symex f d = 
  let rl ((_,cmd) as seq) =
    let cont = Cmd.get_cont cmd in
    fix_tps 
		  (Blist.map (fun g -> Blist.map (fun h' -> ([h'], cont)) g) (f seq)) in
  rl, wrap rl d
  
(* symbolic execution rules *)
let symex_assign_rule_f, symex_assign_rule =
  let rl seq =
    try
      let (f,cmd) = dest_sh_seq seq in
      let (x,e) = Cmd.dest_assign cmd in
      let fv = fresh_evar (Heap.vars f) in
      let theta = Term.singleton_subst x fv in
      let f' = Heap.subst theta f in
      let e' = Term.subst theta e in
      [[ Heap.norm { f' with eqs=UF.add (e',x) f'.eqs } ]]
    with WrongCmd | Not_symheap -> [] in
  mk_symex rl "Assign"

let find_pto_on f e = 
	Ptos.find (fun (l,_) -> Heap.equates f e l) f.ptos
	
let symex_load_rule_f, symex_load_rule =
  let rl seq =
    try
      let (f,cmd) = dest_sh_seq seq in
      let (x,e,s) = Cmd.dest_load cmd in
      let (_,ys) = find_pto_on f e in
      let t = Blist.nth ys (Field.get_index s) in
      let fv = fresh_evar (Heap.vars f) in
      let theta = Term.singleton_subst x fv in
      let f' = Heap.subst theta f in
      let t' = Term.subst theta t in
      [[ { f' with eqs=UF.add (t',x) f'.eqs } ]]
    with Not_symheap | WrongCmd | Not_found -> [] in
  mk_symex rl "Load"

let symex_store_rule_f, symex_store_rule =
  let rl seq =
    try
      let (f,cmd) = dest_sh_seq seq in
      let (x,s,e) = Cmd.dest_store cmd in
      let ((x',ys) as pto) = find_pto_on f x in
      let pto' = (x', Blist.replace_nth e (Field.get_index s) ys) in
      [[ { f with ptos=Ptos.add pto' (Ptos.remove pto f.ptos) } ]]
    with Not_symheap | WrongCmd | Not_found -> [] in
  mk_symex rl "Store"

let symex_free_rule_f, symex_free_rule =
  let rl seq =
    try
      let (f,cmd) = dest_sh_seq seq in
      let e = Cmd.dest_free cmd in
      let pto = find_pto_on f e in
      [[ { f with ptos=Ptos.remove pto f.ptos } ]]
    with Not_symheap | WrongCmd | Not_found -> [] in
  mk_symex rl "Free"

let symex_new_rule_f, symex_new_rule =
  let rl seq =
    try
      let (f,cmd) = dest_sh_seq seq in
      let x = Cmd.dest_new cmd in
      let l = fresh_evars (Heap.vars f) (1 + (Field.get_no_fields ())) in
      let (fv,fvs) = (Blist.hd l, Blist.tl l) in
      let f' = Heap.subst (Term.singleton_subst x fv) f in
			let f'' = Heap.mk_pto x fvs in
      [[ Heap.star f' f'' ]]
    with Not_symheap | WrongCmd-> [] in
  mk_symex rl "New"

let symex_skip_rule_f, symex_skip_rule =
  let rl seq =
    try
      let (f,cmd) = dest_sh_seq seq in 
      let () = Cmd.dest_skip cmd in [[f]]
    with Not_symheap | WrongCmd -> [] in
  mk_symex rl "Skip"

let symex_if_rule_f, symex_if_rule =
  let rl seq =
    try
      let (f,cmd) = dest_sh_seq seq in
      let (c,cmd') = Cmd.dest_if cmd in
      let cont = Cmd.get_cont cmd in
      let (f',f'') = Cond.fork f c in 
      fix_tps [[ ([f'], Cmd.mk_seq cmd' cont) ; ([f''], cont) ]]
    with Not_symheap | WrongCmd -> [] in
  rl, wrap rl "If"

let symex_ifelse_rule_f, symex_ifelse_rule =
  let rl seq =
    try
      let (f,cmd) = dest_sh_seq seq in
      let (c,cmd1,cmd2) = Cmd.dest_ifelse cmd in
      let cont = Cmd.get_cont cmd in
      let (f',f'') = Cond.fork f c in 
      fix_tps [[ ([f'], Cmd.mk_seq cmd1 cont) ; ([f''], Cmd.mk_seq cmd2 cont) ]]
    with Not_symheap | WrongCmd -> [] in
  rl, wrap rl "IfElse"

let symex_while_rule_f, symex_while_rule =
  let rl seq =
    try
      let (f,cmd) = dest_sh_seq seq in
      let (c,cmd') = Cmd.dest_while cmd in
      let cont = Cmd.get_cont cmd in
      let (f',f'') = Cond.fork f c in 
      fix_tps [[ ([f'], Cmd.mk_seq cmd' cmd) ; ([f''], cont) ]]
    with Not_symheap | WrongCmd -> [] in
  rl, wrap rl "While"

let matches_fun ((l1,cmd1) as s1) ((l2,cmd2) as s2) =
  if not (Cmd.equal cmd1 cmd2) then None else
  match Seq.uni_subsumption s1 s2 with
    | None -> None
    | Some theta ->
			if !termination then
				begin 
  				let tags = Tags.inter (Seq.tags s1) (Seq.tags s2) in
          let s2' = Seq.subst theta s2 in
          let tags' = Tags.fold
            (fun t acc ->
              let new_acc = Tags.add t acc in
              if Seq.subsumed_wrt_tags new_acc s1 s2' then new_acc else acc
            ) tags Tags.empty in
          Some (TagPairs.mk tags')
				end
			else
			  Some Seq.tagpairs_one

let matches = SP.mk_back_rule matches_fun "Backl"

let gen_fold_rules (def, ident) =
  let fold_rule s1 s2 =
    try
      let ((l1,cmd1),(l2,cmd2)) = Pair.map dest_sh_seq (s1,s2) in
      if not (Cmd.equal cmd1 cmd2) then None else
      let preds = Inds.filter (fun (_, (ident', _)) -> ident=ident') l2.inds in
      if Inds.is_empty preds then None else
      let fold_match ((id,(_,pvs)) as p) = 
        let l2' = { l2 with inds=Inds.filter ((!=)p) l2.inds } in
        let do_case case =
          let (f', (_,vs')) = Case.dest (freshen_case_by_seq ([l2],cmd2) case) in
          let theta = Term.Map.of_list (Blist.combine vs' pvs) in
          let f' = Heap.subst theta f' in
          let alltags = Tags.union (Seq.tags s1) (Seq.tags s2) in
          let fresh_tag = 1 + (try Tags.max_elt alltags with Not_found -> 0) in 
          let f' = Heap.repl_tags fresh_tag f' in
          let l2' = Heap.star l2' f' in
          let s2' = ([l2'],cmd2) in
          matches_fun s1 s2' in
        Blist.find_first do_case def in 
      Blist.find_first fold_match (Inds.elements preds)
    with Not_symheap -> None in
  SP.mk_back_rule fold_rule (ident ^ " Fold/Backl")

let setup defs =
  (* Program.set_local_vars seq_to_prove ; *)
  let luf = Blist.map gen_left_rules defs in
  let cutm = Blist.map gen_fold_rules defs in
  SP.axiomset := [ ex_falso_axiom ; symex_stop_axiom; symex_empty_axiom ] ;  
  SP.ruleset := 
    [ 
      lhs_disj_to_symheaps ;
      matches ;
      simplify 
    ]
    @ cutm @
    [
      symex_skip_rule ;
      symex_assign_rule;
      symex_load_rule ;
      symex_store_rule ;
      symex_free_rule ;
      symex_new_rule ;
      symex_if_rule ;
      symex_ifelse_rule ;
      symex_while_rule 
    ] 
    @ luf 
