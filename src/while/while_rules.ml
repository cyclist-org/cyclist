open Lib
open Util
open Symheap
open While_program

module Rule = Proofrule.Make(While_program.Seq)
module Seqtactics = Seqtactics.Make(While_program.Seq)

let tagpairs s =
	if !termination then
		TagPairs.mk (Seq.tags s)
	else
		Seq.tagpairs_one

(* following is for symex only *)
let progpairs () = 
	if !termination then TagPairs.empty else Seq.tagpairs_one

let dest_sh_seq (l,cmd) = (Form.dest l, cmd)


(* axioms *)
let ex_falso_axiom_f, ex_falso_axiom = 
  let ax ((f,_):Seq.t) = Form.inconsistent f in 
  ax, Rule.mk_axiom (fun seq -> Option.mk (ax seq) "Ex Falso")

let symex_stop_axiom_f, symex_stop_axiom =
  let ax ((_,cmd):Seq.t) = Cmd.is_stop cmd in 
  ax, Rule.mk_axiom (fun seq -> Option.mk (ax seq) "Stop")

let symex_empty_axiom_f, symex_empty_axiom =
  let ax ((_,cmd):Seq.t) = Cmd.is_empty cmd in
  ax, Rule.mk_axiom (fun seq -> Option.mk (ax seq) "Empty")

(* simplification rules *)
let eq_subst_ex_f ((l,cmd) as s) =
  let l' = Form.subst_existentials l in
  if Form.equal l l' then [] else
  [ [ ((l', cmd), tagpairs s, TagPairs.empty) ], "" ]

let norm ((l,cmd) as s) = 
  let l' = Form.norm l in
  if Form.equal l l' then [] else
  [ [( (l',cmd), tagpairs s, TagPairs.empty)], "" ] 

let simplify_rules = [ norm; eq_subst_ex_f ]

let simplify_seq_rl = Seqtactics.repeat (Seqtactics.first  simplify_rules)
let simplify = Rule.mk_infrule simplify_seq_rl  

let wrap r =
  Rule.mk_infrule
    (Seqtactics.compose r (Seqtactics.attempt simplify_seq_rl))


(* break LHS disjunctions *)
let lhs_disj_to_symheaps_f, lhs_disj_to_symheaps =
  let rl ((l,cmd): Seq.t) =
    if Blist.length l < 2 then [] else
    [ Blist.map 
        (fun sh -> let s' = ([sh],cmd) in (s', tagpairs s', TagPairs.empty ) ) 
        l,
      ""
    ] in
  rl, Rule.mk_infrule rl

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
      Blist.map do_case def, (ident ^ " L.Unf.") in
    Inds.map_to_list left_unfold preds
  with Not_symheap -> [] 
 
let gen_left_rules (def,ident) = 
  wrap (gen_left_rules_f (def,ident)) 

(* FOR SYMEX ONLY *)
let fix_tps l = 
  Blist.map 
    (fun (g,d) -> Blist.map (fun s -> (s, tagpairs s, progpairs () )) g, d) l 

let mk_symex f = 
  let rl ((_,cmd) as seq) =
    let cont = Cmd.get_cont cmd in
    fix_tps 
		  (Blist.map (fun (g,d) -> Blist.map (fun h' -> ([h'], cont)) g, d) (f seq)) in
  rl, wrap rl
  
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
      [[ Heap.norm { f' with eqs=UF.add (e',x) f'.eqs } ], "Assign"]
    with WrongCmd | Not_symheap -> [] in
  mk_symex rl

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
      [[ { f' with eqs=UF.add (t',x) f'.eqs } ], "Load"]
    with Not_symheap | WrongCmd | Not_found -> [] in
  mk_symex rl

let symex_store_rule_f, symex_store_rule =
  let rl seq =
    try
      let (f,cmd) = dest_sh_seq seq in
      let (x,s,e) = Cmd.dest_store cmd in
      let ((x',ys) as pto) = find_pto_on f x in
      let pto' = (x', Blist.replace_nth e (Field.get_index s) ys) in
      [[ { f with ptos=Ptos.add pto' (Ptos.remove pto f.ptos) } ], "Store"]
    with Not_symheap | WrongCmd | Not_found -> [] in
  mk_symex rl

let symex_free_rule_f, symex_free_rule =
  let rl seq =
    try
      let (f,cmd) = dest_sh_seq seq in
      let e = Cmd.dest_free cmd in
      let pto = find_pto_on f e in
      [[ { f with ptos=Ptos.remove pto f.ptos } ], "Free"]
    with Not_symheap | WrongCmd | Not_found -> [] in
  mk_symex rl

let symex_new_rule_f, symex_new_rule =
  let rl seq =
    try
      let (f,cmd) = dest_sh_seq seq in
      let x = Cmd.dest_new cmd in
      let l = fresh_evars (Heap.vars f) (1 + (Field.get_no_fields ())) in
      let (fv,fvs) = (Blist.hd l, Blist.tl l) in
      let f' = Heap.subst (Term.singleton_subst x fv) f in
			let f'' = Heap.mk_pto x fvs in
      [[ Heap.star f' f'' ], "New"]
    with Not_symheap | WrongCmd-> [] in
  mk_symex rl

let symex_skip_rule_f, symex_skip_rule =
  let rl seq =
    try
      let (f,cmd) = dest_sh_seq seq in 
      let () = Cmd.dest_skip cmd in [[f], "Skip"]
    with Not_symheap | WrongCmd -> [] in
  mk_symex rl

let symex_if_rule_f, symex_if_rule =
  let rl seq =
    try
      let (f,cmd) = dest_sh_seq seq in
      let (c,cmd') = Cmd.dest_if cmd in
      let cont = Cmd.get_cont cmd in
      let (f',f'') = Cond.fork f c in 
      fix_tps [[ ([f'], Cmd.mk_seq cmd' cont) ; ([f''], cont) ], "If"]
    with Not_symheap | WrongCmd -> [] in
  rl, wrap rl

let symex_ifelse_rule_f, symex_ifelse_rule =
  let rl seq =
    try
      let (f,cmd) = dest_sh_seq seq in
      let (c,cmd1,cmd2) = Cmd.dest_ifelse cmd in
      let cont = Cmd.get_cont cmd in
      let (f',f'') = Cond.fork f c in 
      fix_tps 
        [[ ([f'], Cmd.mk_seq cmd1 cont) ; ([f''], Cmd.mk_seq cmd2 cont) ],
         "IfElse"
        ]
    with Not_symheap | WrongCmd -> [] in
  rl, wrap rl

let symex_while_rule_f, symex_while_rule =
  let rl seq =
    try
      let (f,cmd) = dest_sh_seq seq in
      let (c,cmd') = Cmd.dest_while cmd in
      let cont = Cmd.get_cont cmd in
      let (f',f'') = Cond.fork f c in 
      fix_tps [[ ([f'], Cmd.mk_seq cmd' cmd) ; ([f''], cont) ], "While"]
    with Not_symheap | WrongCmd -> [] in
  rl, wrap rl

let matches_fun ((l1,cmd1) as s1) ((l2,cmd2) as s2) =
  if not (Cmd.equal cmd1 cmd2) then [] else
  match Seq.uni_subsumption s1 s2 with
    | None -> []
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
          [ (TagPairs.mk tags', "Backl") ]
				end
			else
			  [ (Seq.tagpairs_one, "Backl") ]

let matches = Rule.mk_backrule true Rule.all_nodes matches_fun

let gen_fold_rules (def, ident) =
  let fold_rule s1 s2 =
    try
      let ((l1,cmd1),(l2,cmd2)) = Pair.map dest_sh_seq (s1,s2) in
      if not (Cmd.equal cmd1 cmd2) then [] else
      let preds = Inds.filter (fun (_, (ident', _)) -> ident=ident') l2.inds in
      if Inds.is_empty preds then [] else
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
        Blist.find_first ((<>)[]) (Blist.map do_case def) in 
      Option.dest 
        []
        Fun.id
        (Blist.find_some fold_match (Inds.elements preds))
    with Not_symheap -> [] in
  Rule.mk_backrule true Rule.all_nodes fold_rule


let rules = ref Rule.fail

let setup defs =
  (* Program.set_local_vars seq_to_prove ; *)
  let luf = Blist.map gen_left_rules defs in
  let cutm = Blist.map gen_fold_rules defs in
  rules := 
    Rule.choice 
      ([ ex_falso_axiom ; symex_stop_axiom; symex_empty_axiom ] @
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
      @ luf) 
