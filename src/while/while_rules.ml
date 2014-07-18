open Lib
open Util
open Symheap
open While_program

module SH = Sl_heap

module Rule = Proofrule.Make(While_program.Seq)
module Seqtactics = Seqtactics.Make(While_program.Seq)
module Proof = Proof.Make(While_program.Seq)
module Slprover = Prover.Make(Sl_seq)

let tagpairs s =
	if !termination then
		TagPairs.mk (Seq.tags s)
	else
		Seq.tagpairs_one

(* following is for symex only *)
let progpairs () = 
	if !termination then TagPairs.empty else Seq.tagpairs_one

let dest_sh_seq (pre, cmd, post) = (Sl_form.dest pre, cmd, post)


(* axioms *)

(* If the precondition of the candidate sequence is inconsistent, then we can *)
(* close it of as instance of the Ex Falso axiom *)
let ex_falso_axiom = 
  Rule.mk_axiom (
		fun (pre, _, _) -> 
			Option.mk (Sl_form.inconsistent pre) "Ex Falso")

(* If the precondition entails the post condition and the command is stop, *)
(* then we can apply the Stop axiom. *)
let mk_symex_stop_axiom entails =
  Rule.mk_axiom (
		fun (pre, cmd, post) ->
	    Option.mk (Cmd.is_stop cmd && Option.is_some (entails pre post)) "Stop")

(* If the precondition entails the post condition and the command list is empty, *)
(* then we can apply the Stop axiom. *)
let mk_symex_empty_axiom entails =
  Rule.mk_axiom (
		fun (pre, cmd, post) -> 
			Option.mk (Cmd.is_empty cmd && Option.is_some (entails pre post)) "Empty")

(* simplification rules *)

(* Tactic which tries to simplify the sequent by replacing existential variables *)
(* in the precondition and fails if no such replacements can be made *)
(* TODO: ?make a similar simplification tactic that replaces existentials in postcondition *)
let eq_subst_ex_f ((pre, cmd, post) as s) =
  let pre' = Sl_form.subst_existentials pre in
  if Sl_form.equal pre pre' then [] else
  [ [ ((pre', cmd, post), tagpairs s, TagPairs.empty) ], "Eq. subst. ex" ]

(* Tactic which tried to simplify the sequent by normalising: that is, using the *)
(* equalities in the formula as a substitution for the disequality, points-to and *)
(* predicate subformulae *)
(* TODO: ?make a similar simplification tactic that normalises the postcondition *)
let norm ((pre ,cmd, post) as s) = 
  let pre' = Sl_form.norm pre in
  if Sl_form.equal pre pre' then [] else
  [ [( (pre', cmd, post), tagpairs s, TagPairs.empty)], "Norm" ] 

let simplify_rules = [ norm; eq_subst_ex_f ]

(* Tactic which performs as many simplifications as possible all in one go *)
let simplify_seq_rl = 
  Seqtactics.relabel "Simplify" 
    (Seqtactics.repeat (Seqtactics.first simplify_rules))
		
let simplify = Rule.mk_infrule simplify_seq_rl  

(* Function which takes a tactic, composes it with a general simplification attempt *)
(* tactic, and creates a compound inference rule out of it *)
let wrap r =
  Rule.mk_infrule
    (Seqtactics.compose r (Seqtactics.attempt simplify_seq_rl))


(* break LHS disjunctions *)
let lhs_disj_to_symheaps =
  let rl ((pre, cmd, post) : Seq.t) =
    if Blist.length pre < 2 then [] else
    [ Blist.map 
        (fun sh -> let s' = ([sh], cmd, post) in (s', tagpairs s', TagPairs.empty ) ) 
        pre,
      "L.Or"
    ] in
  Rule.mk_infrule rl

let gen_left_rules_f (def, ident) seq =
  try
    let (pre, cmd, post) = dest_sh_seq seq in
    let preds = 
      Inds.filter (fun (_,(ident',_)) -> Strng.equal ident ident') pre.SH.inds in
    if Inds.is_empty preds then [] else
    let left_unfold ((id,(_,pvs)) as p) = 
			let ts = Tags.inter (Sl_heap.tags pre) (Sl_heap.tags pre) in
      let pre' = { pre with SH.inds=Inds.remove p pre.SH.inds } in
      let do_case case =
        let (f', (_,vs')) = Sl_indrule.dest (freshen_case_by_seq seq case) in
        let theta = Sl_term.Map.of_list (Blist.combine vs' pvs) in
        let f' = Sl_heap.subst theta f' in
        let f' = Sl_heap.repl_tags id f' in
        let pre' = Sl_heap.star pre' f' in
        ( 
					([pre'], cmd, post), 
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
  let rl ((_, cmd, post) as seq) =
		if (Cmd.is_empty cmd) then
			[]
		else
			let cont = Cmd.get_cont cmd
			in
		    fix_tps 
				  (Blist.map (fun (g,d) -> Blist.map (fun h' -> ([h'], cont, post)) g, d) (f seq))
	in
		wrap rl
  
(* symbolic execution rules *)
let symex_assign_rule =
  let rl seq =
    try
      let (pre , cmd, _) = dest_sh_seq seq in
      let (x,e) = Cmd.dest_assign cmd in
      (* Does fv need to be fresh in the post condition too? *)
			let fv = fresh_evar (Sl_heap.vars pre) in
      let theta = Sl_term.singleton_subst x fv in
      let pre' = Sl_heap.subst theta pre in
      let e' = Sl_term.subst theta e in
      [[ Sl_heap.norm { pre' with SH.eqs=UF.add (e',x) pre'.SH.eqs } ], "Assign"]
    with WrongCmd | Not_symheap -> [] in
  mk_symex rl

let find_pto_on f e = 
	Ptos.find (fun (l,_) -> Sl_heap.equates f e l) f.SH.ptos
	
let symex_load_rule =
  let rl seq =
    try
      let (pre, cmd, _) = dest_sh_seq seq in
      let (x,e,f) = Cmd.dest_load cmd in
      let (_,ys) = find_pto_on pre e in
      let t = Blist.nth ys (Field.get_index f) in
      (* Does fv need to be fresh in the post condition too? *)
      let fv = fresh_evar (Sl_heap.vars pre) in
      let theta = Sl_term.singleton_subst x fv in
      let pre' = Sl_heap.subst theta pre in
      let t' = Sl_term.subst theta t in
      [[ { pre' with SH.eqs=UF.add (t',x) pre'.SH.eqs } ], "Load"]
    with Not_symheap | WrongCmd | Not_found -> [] in
  mk_symex rl

let symex_store_rule =
  let rl seq =
    try
      let (pre, cmd, _) = dest_sh_seq seq in
      let (x,f,e) = Cmd.dest_store cmd in
      let ((x',ys) as pto) = find_pto_on pre x in
      let pto' = (x', Blist.replace_nth e (Field.get_index f) ys) in
      [[ { pre with SH.ptos=Ptos.add pto' (Ptos.remove pto pre.SH.ptos) } ], "Store"]
    with Not_symheap | WrongCmd | Not_found -> [] in
  mk_symex rl

let symex_free_rule =
  let rl seq =
    try
      let (pre, cmd, _) = dest_sh_seq seq in
      let e = Cmd.dest_free cmd in
      let pto = find_pto_on pre e in
      [[ { pre with SH.ptos=Ptos.remove pto pre.SH.ptos } ], "Free"]
    with Not_symheap | WrongCmd | Not_found -> [] in
  mk_symex rl

let symex_new_rule =
  let rl seq =
    try
      let (pre ,cmd, _) = dest_sh_seq seq in
      let x = Cmd.dest_new cmd in
      let l = fresh_evars (Sl_heap.vars pre) (1 + (Field.get_no_fields ())) in
      let (fv,fvs) = (Blist.hd l, Blist.tl l) in
      let pre' = Sl_heap.subst (Sl_term.singleton_subst x fv) pre in
			let new_pto = Sl_heap.mk_pto x fvs in
      [[ Sl_heap.star pre' new_pto ], "New"]
    with Not_symheap | WrongCmd -> [] in
  mk_symex rl

let symex_skip_rule =
  let rl seq =
    try
      let (pre, cmd, _) = dest_sh_seq seq in 
      let () = Cmd.dest_skip cmd in [[pre], "Skip"]
    with Not_symheap | WrongCmd -> [] in
  mk_symex rl

let symex_if_rule =
  let rl seq =
    try
      let (pre ,cmd, post) = dest_sh_seq seq in
      let (c,cmd') = Cmd.dest_if cmd in
      let cont = Cmd.get_cont cmd in
      let (cond_true_pre, cond_false_pre) = Cond.fork pre c in 
      fix_tps 
			  [
					[ ([cond_true_pre], Cmd.mk_seq cmd' cont, post) ; 
					  ([cond_false_pre], cont, post) ], 
				  "If"
			  ]
    with Not_symheap | WrongCmd -> [] in
  wrap rl

let symex_ifelse_rule =
  let rl seq =
    try
      let (pre, cmd, post) = dest_sh_seq seq in
      let (c,cmd1,cmd2) = Cmd.dest_ifelse cmd in
      let cont = Cmd.get_cont cmd in
      let (cond_true_pre, cond_false_pre) = Cond.fork pre c in 
      fix_tps 
        [
					[ ([cond_true_pre], Cmd.mk_seq cmd1 cont, post) ; 
				    ([cond_false_pre], Cmd.mk_seq cmd2 cont, post) ],
         "IfElse"
        ]
    with Not_symheap | WrongCmd -> [] in
  wrap rl

let symex_while_rule =
  let rl seq =
    try
      let (pre, cmd, post) = dest_sh_seq seq in
      let (c,cmd') = Cmd.dest_while cmd in
      let cont = Cmd.get_cont cmd in
      let (cond_true_pre, cond_false_pre) = Cond.fork pre c in 
      fix_tps 
			  [
					[ ([cond_true_pre], Cmd.mk_seq cmd' cmd, post) ; 
					  ([cond_false_pre], cont, post) ], 
				  "While"
				]
    with Not_symheap | WrongCmd -> [] in
  wrap rl

let matches_fun ((pre1, cmd1, post1) as s1) ((pre2, cmd2, post2) as s2) =
  if not (Cmd.equal cmd1 cmd2) || 
     not (Sl_form.is_heap pre1 = Sl_form.is_heap pre2) || 
     not (Sl_form.is_heap post1 = Sl_form.is_heap post2) then [] else
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
          [ ((TagPairs.mk tags', "Backl"),theta) ]
				end
			else
			  [ ((Seq.tagpairs_one, "Backl"),theta) ]

(*    seq'     *)
(* ----------  *)
(* seq'[theta] *)
(* where seq'[theta] = seq *)
let subst_rule theta seq' seq = 
  if Seq.equal (Seq.subst theta seq') seq 
    then 
        [ [(seq', TagPairs.mk (Seq.tags seq'), TagPairs.empty)], "Subst" ]
    else 
        []

(*   F |- G * Pi'  *)
(* --------------- *)
(*   Pi * F |- G   *)
(* where seq' = F |- G * Pi' and seq = Pi * F |- G *)     
let weaken seq' seq = 
  if Seq.subsumed_wrt_tags Tags.empty seq seq' then
    [ [(seq', TagPairs.mk (Tags.inter (Seq.tags seq) (Seq.tags seq')), TagPairs.empty)], "Weaken" ]
  else
    []

(* if there is a backlink achievable through substitution and classical *)
(* weakening then make the proof steps that achieve it explicit so that *)
(* actual backlinking can be done on Seq.equal sequents *) 
let dobackl idx prf =
    let src_seq = Proof.get_seq idx prf in
    let targets = Rule.all_nodes idx prf in
    let apps = 
        Blist.map (fun idx' -> matches_fun src_seq (Proof.get_seq idx' prf)) targets in
    let f targ_idx (p, theta) =
        let targ_seq = Proof.get_seq targ_idx prf in
    let subst_seq = Seq.subst theta targ_seq in
    Rule.sequence [
      if Seq.equal src_seq subst_seq
        then Rule.identity
        else Rule.mk_infrule (weaken subst_seq);
        
      if Sl_term.Map.for_all Sl_term.equal theta
        then Rule.identity
        else Rule.mk_infrule (subst_rule theta targ_seq);
         
      Rule.mk_backrule 
        true 
        (fun _ _ -> [targ_idx]) 
        (fun s s' -> if Seq.equal s s' then [p] else [])
    ] in
    Rule.first 
      (Blist.map2 
          (fun idx' l -> Rule.first (Blist.map (f idx') l)) 
            targets 
            apps) idx prf

let fold (defs, ident) =
  let fold_rl seq = 
    try 
      let (pre, cmd, post) = dest_sh_seq seq in
      if Inds.is_empty pre.SH.inds then [] else
      let tags = Seq.tags seq in
      let freshtag = 1 + (try Tags.max_elt tags with Not_found -> 0) in 
      let do_case case =
        let (f,(ident,vs)) = Sl_indrule.dest case in 
        (* if Inds.is_empty f.SH.inds then [] else *)
        let results : Sl_term.substitution list ref = ref [] in
        let hook sub = results := sub :: !results ; None in 
        let () = ignore (Sl_heap.spw_left_subsumption hook Sl_term.empty_subst f pre) in
        let process_sub theta = 
          let (f, vs) = (Sl_heap.subst theta f, Blist.map (Sl_term.subst theta) vs) in
          let pre' = 
            {
              (* FIXME hacky stuff in SH.eqs : in reality a proper way to diff *)
              (* two union-find structures is required *)
              SH.eqs =
                UF.of_list 
                (Deqs.to_list 
                  (Deqs.diff
                    (Deqs.of_list (UF.bindings pre.SH.eqs))
                    (Deqs.of_list (UF.bindings f.SH.eqs))
                  ));
              SH.deqs = Deqs.diff pre.SH.deqs f.SH.deqs;
              SH.ptos = Ptos.diff pre.SH.ptos f.SH.ptos;
              SH.inds = 
                Inds.fold 
                  (fun (_, (f_ident, f_vs)) a -> 
                    Inds.del_first 
                    (fun (_, (l_ident, l_vs)) -> 
                      f_ident = l_ident && Sl_term.FList.equal f_vs l_vs) a) 
                  f.SH.inds
                  pre.SH.inds;
            } in
          let newpred = (freshtag,(ident,vs)) in
          let pre' = { pre' with SH.inds = Inds.add newpred pre'.SH.inds } in
          let seq' = ([pre'], cmd, post) in
          (* let () = print_endline "Fold match:" in        *)
          (* let () = print_endline (Seq.to_string seq) in  *)
          (* let () = print_endline (Sl_heap.to_string f) in   *)
          (* let () = print_endline (Seq.to_string seq') in *)
            [(
              seq', 
              TagPairs.mk (Tags.inter tags (Seq.tags seq')), 
              TagPairs.empty 
            )], (ident ^ " Fold")  in
        Blist.map process_sub !results in
      Blist.bind do_case defs
    with Not_symheap -> [] in
  Rule.mk_infrule fold_rl 


let generalise_while_rule =
  let generalise m h =
    let avoid = ref (Sl_heap.vars h) in
    let gen_term t =
    if Sl_term.Set.mem t m then
      (let r = fresh_evar !avoid in avoid := Sl_term.Set.add r !avoid ; r)
    else t in
    let gen_pto (x,args) =
    let l = Blist.map gen_term (x::args) in (Blist.hd l, Blist.tl l) in
      { h with
        SH.eqs = Sl_term.Set.fold UF.remove m h.SH.eqs;
        SH.deqs =
          Deqs.filter
          (fun p -> Pair.conj (Pair.map (fun z -> not (Sl_term.Set.mem z m)) p))
          h.SH.deqs;
          SH.ptos = Ptos.endomap gen_pto h.SH.ptos
      } in
    let rl seq =
      try
        let (pre, cmd, post) = dest_sh_seq seq in
        let (_, cmd') = Cmd.dest_while cmd in
        let m = Sl_term.Set.inter (Cmd.modifies cmd') (Sl_heap.vars pre) in
        let subs = Sl_term.Set.subsets m in
        Option.list_get (Blist.map
          begin fun m' ->
            let pre' = generalise m' pre in
            if Sl_heap.equal pre pre' then None else
            let s' = ([pre'], cmd, post) in
            Some ([ (s', tagpairs s', TagPairs.empty) ], "Gen.While")
          end
          subs)
    with Not_symheap | WrongCmd -> [] in
  Rule.mk_infrule rl 

let backlink_cut entails =
  let rl s1 s2 =
    if !termination then [] else
    (* let () = incr step in *)
    let ((pre1, cmd1, _), (pre2, cmd2, _)) = (s1, s2) in
    if not (Cmd.is_while cmd1) then [] else
    (* let () = debug (fun () -> "CUTLINK3: trying: " ^ (Seq.to_string s2)) in   *)
    (* let () = debug (fun () -> "                  " ^ (Seq.to_string s1)) in   *)
    (* let () = debug (fun () -> "CUTLINK3: step = " ^ (string_of_int !step)) in *)
    (* if !step <> 22 then None else *)
    if not (Cmd.equal cmd1 cmd2) then [] else
    (* let olddebug = !Lib.do_debug in *)
    (* let () = Lib.do_debug := true in *)
    let result = 
      Option.is_some (entails pre1 pre2) in
    (* let () = Lib.do_debug := olddebug in *)
    (* let () = debug (fun () -> "CUTLINK3: result: " ^ (string_of_bool result)) in *)
    if result then [ (Seq.tagpairs_one, "Cut/Backl") ] else [] in
  Rule.mk_backrule true Rule.all_nodes rl


let axioms = ref Rule.fail
let rules = ref Rule.fail

let setup defs =
  (* Program.set_local_vars seq_to_prove ; *)
  let () = Sl_rules.setup defs in
	let entails f f' =
		Slprover.idfs 1 11 !Sl_rules.axioms !Sl_rules.rules (f, f')
  in let () =
		axioms := Rule.first [
				ex_falso_axiom ; 
				mk_symex_stop_axiom entails; 
				mk_symex_empty_axiom entails
			]
	in 
    rules := Rule.first [ 
      lhs_disj_to_symheaps ;
      simplify ;
      
      Rule.choice [
        dobackl ;
        Rule.choice (Blist.map (fun c -> Rule.compose (fold c) dobackl) defs);
        
        Rule.first [
          symex_skip_rule ;
          symex_assign_rule;
          symex_load_rule ;
          symex_store_rule ;
          symex_free_rule ;
          symex_new_rule ;
          symex_if_rule ;
          symex_ifelse_rule ;
          symex_while_rule;
        ] ;
        
        generalise_while_rule ;
        (* backlink_cut entails; *)
        
        Rule.choice (Blist.map gen_left_rules defs)
      ]
    ]
