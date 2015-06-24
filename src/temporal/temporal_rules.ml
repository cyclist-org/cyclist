open Lib
open Util
open Temporal_program

module SH = Sl_heap
exception Not_symheap = Sl_form.Not_symheap

module Rule = Proofrule.Make(Temporal_program.Seq)
module Seqtactics = Seqtactics.Make(Temporal_program.Seq)
module Proof = Proof.Make(Temporal_program.Seq)
				 
let tagpairs s =
  Seq.tag_pairs s
		
(* following is for symex only *)
let progpairs s = 
  Seq.tag_pairs s

let dest_sh_seq (sf,cmd,tf) = (Sl_form.dest sf, cmd, tf)
				
(* axioms *)
let ex_falso_axiom = 
  Rule.mk_axiom (fun (sf,_,_) -> Option.mk (Sl_form.inconsistent sf) "Ex Falso")

let symex_check_axiom entails = 
  Rule.mk_axiom (fun (sf,_,tf) -> Option.mk (Tl_form.is_checkable tf && Option.is_some (entails sf (Tl_form.extract_checkable_slformula tf))) "Check")
		
let symex_empty_axiom =
  Rule.mk_axiom (fun (_,cmd,tf) -> Option.mk (Cmd.is_empty cmd && Tl_form.is_box tf) "Empty")

(* simplification rules *)
let eq_subst_ex_f ((sf,cmd,tf) as s) =
  let sf' = Sl_form.subst_existentials sf in
  if Sl_form.equal sf sf' then [] else
    [ [ ((sf', cmd, tf), tagpairs s, TagPairs.empty) ], "Eq. subst. ex" ]

let simplify_rules = [ eq_subst_ex_f ]

let simplify_seq_rl = 
  Seqtactics.relabel "Simplify" 
    (Seqtactics.repeat (Seqtactics.first  simplify_rules))
let simplify = Rule.mk_infrule simplify_seq_rl  

let wrap r =
  Rule.mk_infrule
    (Seqtactics.compose r (Seqtactics.attempt simplify_seq_rl))


(* break LHS disjunctions *)
let lhs_disj_to_symheaps =
  let rl (sf,cmd,tf) =
    if Blist.length sf < 2 then [] else
      [ Blist.map 
          (fun sh -> let s' = ([sh],cmd,tf) in (s', tagpairs s', TagPairs.empty ) ) 
          sf,
      "L.Or"
      ] in
  Rule.mk_infrule rl

let luf_rl seq defs =
  try
    let (sf,cmd,tf) = dest_sh_seq seq in
    let seq_vars = Seq.vars seq in
    let left_unfold ((_, (ident, _)) as p) = 
      let l' = SH.del_ind sf p in
      let clauses = Sl_defs.unfold seq_vars l' p defs in
      let do_case (f', tagpairs) =
        let l' = Sl_heap.star l' f' in
        ( 
					([l'],cmd,tf), 
					(if !termination then tagpairs else Seq.tagpairs_one), 
					(if !termination then tagpairs else TagPairs.empty)
				) in
      Blist.map do_case clauses, ((Sl_predsym.to_string ident) ^ " L.Unf.") in
    Sl_tpreds.map_to_list 
      left_unfold 
      (Sl_tpreds.filter (Sl_defs.is_defined defs) sf.SH.inds)
  with Not_symheap -> []

let luf defs = wrap (fun seq -> luf_rl seq defs)
		    
(* FOR SYMEX ONLY *)
let fix_ts l = 
  Blist.map
    (fun (g,d) -> Blist.map (fun s -> (s, tagpairs s, TagPairs.empty )) g, d) l 
    
let fix_tps l = 
  Blist.map 
    (fun (g,d) -> Blist.map (fun s -> (s, tagpairs s, progpairs s )) g, d) l 
    
let mk_symex f = 
  let rl ((_,cmd,tf) as seq) = 
    try
      if Tl_form.is_diamond tf then
	let cont = Cmd.get_cont cmd in
	let tf' = Tl_form.e_step tf in
	fix_tps 
	  (Blist.map (fun (g,d) -> Blist.map (fun h' -> ([h'], cont, tf')) g, d) (f seq))
      else if Tl_form.is_box tf then
	let cont = Cmd.get_cont cmd in
	let tf' = Tl_form.a_step tf in
	fix_tps 
	  (Blist.map (fun (g,d) -> Blist.map (fun h' -> ([h'], cont, tf')) g, d) (f seq))
      else
	[]
    with WrongCmd -> []
  in wrap rl
       
(* symbolic execution rules *)
let symex_assign_rule =
  let rl seq =
    try
      let (sf,cmd,_) = dest_sh_seq seq in
      let (x,e) = Cmd.dest_assign cmd in
      let fv = fresh_evar (Sl_heap.vars sf) in
      let theta = Sl_term.singleton_subst x fv in
      let sf' = Sl_heap.subst theta sf in
      let e' = Sl_term.subst theta e in
      [[ SH.add_eq sf' (e',x) ], "Assign"]
    with WrongCmd | Not_symheap -> [] in
  mk_symex rl

let find_pto_on f e = 
	Sl_ptos.find (fun (l,_) -> Sl_heap.equates f e l) f.SH.ptos
	
let symex_load_rule =
  let rl seq =
    try
      let (sf,cmd,_) = dest_sh_seq seq in
      let (x,e,s) = Cmd.dest_load cmd in
      let (_,ys) = find_pto_on sf e in
      let t = Blist.nth ys (Field.get_index s) in
      let fv = fresh_evar (Sl_heap.vars sf) in
      let theta = Sl_term.singleton_subst x fv in
      let sf' = Sl_heap.subst theta sf in
      let t' = Sl_term.subst theta t in
      [[ SH.add_eq sf' (t',x) ], "Load"]
    with Not_symheap | WrongCmd | Not_found -> [] in
  mk_symex rl
	   
let symex_store_rule =
  let rl seq =
    try
      let (sf,cmd,_) = dest_sh_seq seq in
      let (x,s,e) = Cmd.dest_store cmd in
      let ((x',ys) as pto) = find_pto_on sf x in
      let pto' = (x', Blist.replace_nth e (Field.get_index s) ys) in
      [[ SH.add_pto (SH.del_pto sf pto) pto' ], "Store"]
    with Not_symheap | WrongCmd | Not_found -> [] in
  mk_symex rl

let symex_free_rule =
  let rl seq =
    try
      let (sf,cmd,_) = dest_sh_seq seq in
      let e = Cmd.dest_free cmd in
      let pto = find_pto_on sf e in
      [[ SH.del_pto sf pto ], "Free"]
    with Not_symheap | WrongCmd | Not_found -> [] in
  mk_symex rl

let symex_new_rule =
  let rl seq =
    try
      let (sf,cmd,_) = dest_sh_seq seq in
      let x = Cmd.dest_new cmd in
      let l = fresh_evars (Sl_heap.vars sf) (1 + (Field.get_no_fields ())) in
      let (fv,fvs) = (Blist.hd l, Blist.tl l) in
      let sf' = Sl_heap.subst (Sl_term.singleton_subst x fv) sf in
      let sf'' = Sl_heap.mk_pto (x, fvs) in
      [[ Sl_heap.star sf' sf'' ], "New"]
    with Not_symheap | WrongCmd-> [] in
  mk_symex rl

let symex_skip_rule =
  let rl seq =
    try
      let (sf,cmd,_) = dest_sh_seq seq in 
      let () = Cmd.dest_skip cmd in [[sf], "Skip"]
    with Not_symheap | WrongCmd -> [] in
  mk_symex rl

let symex_ifelse_rule =
  let rl seq =
    try
      let (sf,cmd,tf) = dest_sh_seq seq in
      let (c,cmd1,cmd2) = Cmd.dest_ifelse cmd in
      let cont = Cmd.get_cont cmd in
      let (sf',sf'') = Cond.fork sf c in 
      if Tl_form.is_box tf then
	let tf' = Tl_form.a_step tf in
	fix_tps 
          [[ ([sf'], Cmd.mk_seq cmd1 cont, tf') ; ([sf''], Cmd.mk_seq cmd2 cont,tf') ], "If-[]"]
      else if Tl_form.is_diamond tf then
	let tf' = Tl_form.e_step tf in
	if Cond.is_non_det c then 
	  fix_tps 
            [[ ([sf'], Cmd.mk_seq cmd1 cont, tf')],
             "If-<>1" ;
	     [ ([sf''], Cmd.mk_seq cmd2 cont, tf')],
             "If-<>2"
            ]
	else if Cond.validated_by sf c then
	  fix_tps [[ ([sf'], Cmd.mk_seq cmd1 cont, tf')], "If-<>1"]
	else 
	  fix_tps [[ ([sf''], Cmd.mk_seq cmd2 cont, tf')], "If-<>2"]	  
      else
	[]
    with Not_symheap | WrongCmd -> [] in
  wrap rl
       
let symex_while_rule =
  let rl seq =
    try
      let (sf,cmd,tf) = dest_sh_seq seq in
      let (c,cmd') = Cmd.dest_while cmd in
      let cont = Cmd.get_cont cmd in
      let (sf',sf'') = Cond.fork sf c in 
      if Tl_form.is_box tf then
	let tf' = Tl_form.a_step tf in
	fix_tps 
	  [[ ([sf'], Cmd.mk_seq cmd' cmd, tf') ; ([sf''], cont, tf') ], "While-Box"]
      else if Tl_form.is_diamond tf then
	let tf' = Tl_form.e_step tf in
	if Cond.is_non_det c then
	  fix_tps 
            [[ ([sf'], Cmd.mk_seq cmd' cmd, tf')], 
             "While-<>1" ;
	     [ ([sf''], cont, tf')],
             "While-<>2"
            ]
	else if Cond.validated_by sf c then
	  fix_tps [[ ([sf'], Cmd.mk_seq cmd' cmd, tf')], "While-<>1"]
	else
	  fix_tps [[ ([sf''], cont, tf')], "While-<>2"]
      else
	[]
    with Not_symheap | WrongCmd -> [] in
  wrap rl

(* There is also a weird, unsound, interaction between looking for substitutions  *)
(* that rewrite a sequent under its equalities, as opposed to bona fide           *)
(* substitutions for back-links, and the union-find structure used to record      *)
(* equalities.  Specifically equalities of the form x=x will not be stored.       *)
(* This means that emp is equivalent to any set of such equalities.  Thus         *)
(*    x=y |- C                                                                    *)
(* -------------- [subst nil over y]                                              *)
(*    emp |- C                                                                    *)
(* -------------- [Frame/Weakening]                                               *)
(*      H |- C                                                                    *)
(* For any H!  For this reason, unless this is fixed in another way we need to    *)
(* restrict to substitutions that do not need rewrites under equalities to yield  *)
(* the required sequent.                                                          *)
let matches ((sf,cmd,tf) as seq) ((sf',cmd',tf') as seq') =
  try
    if not (Cmd.equal cmd cmd' && Tl_form.equal tf tf'
	   && (Tl_form.is_ag tf || Tl_form.is_eg tf)
	   && (Tl_form.is_ag tf' || Tl_form.is_eg tf')) then [] else
      let (sf, sf') = Pair.map Sl_form.dest(sf,sf') in
      let sub_check = Sl_term.combine_subst_checks [
        Sl_term.basic_lhs_down_check ;
        Sl_term.avoids_replacing_check !program_vars ;
      ] in
      let cont =
	Sl_term.mk_verifier
          (Sl_term.mk_assert_check
             (fun (theta, tagpairs) -> 
              let subst_seq = (Seq.subst_tags tagpairs (Seq.subst theta seq')) in
              let () = debug (fun _ -> "term substitution: " ^ ((Format.asprintf " %a" Sl_term.pp_subst theta))) in 
              let () = debug (fun _ -> "tag substitution: " ^ (TagPairs.to_string tagpairs)) in 
              let () = debug (fun _ -> "source seq: " ^ (Seq.to_string seq)) in
              let () = debug (fun _ -> "target seq: " ^ (Seq.to_string seq')) in
              let () = debug (fun _ -> "substituted target seq: " ^ (Seq.to_string subst_seq)) in
              Seq.subsumed seq subst_seq)
          ) in
      let res = 
	Sl_term.backtrack 
	  (Sl_heap.unify_partial ~tagpairs:true)
	  ~sub_check
	  ~cont
	  sf' sf in
      if Tl_form.is_ag tf && Tl_form.is_ag tf' then
	let (t1, _) = Tl_form.dest_ag tf in
	let (t2, _) = Tl_form.dest_ag tf' in
	assert (Tags.equal (Tags.singleton t1) (Tags.singleton t2)); Blist.map (fun (t,tp) -> (t, (TagPairs.add (t1,t2) tp))) res
      else if Tl_form.is_eg tf && Tl_form.is_eg tf' then
	let (t1, _) = Tl_form.dest_eg tf in
	let (t2, _) = Tl_form.dest_eg tf' in
	assert (Tags.equal (Tags.singleton t1) (Tags.singleton t2)); Blist.map (fun (t,tp) -> (t, (TagPairs.add (t1,t2) tp ))) res
      else
	res
  with Not_symheap -> []

(*    seq'     *)
(* ----------  *)
(* seq'[theta] *)
(* where seq'[theta] = seq *)
let subst_rule theta seq' seq = 
  if Seq.equal (Seq.subst theta seq') seq 
    then 
      [ [(seq', Seq.tag_pairs seq', TagPairs.empty)], 
       "Subst "  (* ^ (Format.asprintf "%a" Sl_term.pp_subst theta) *) ]
    else 
      []

let frame seq' seq = 
  if Seq.subsumed seq seq' then
    [ [(seq', Seq.tag_pairs seq', TagPairs.empty)], "Frame" ]
  else
    []
      
let unfold_ag_rule = 
  let rl seq =
    try
      let (sf,cmd,tf) = dest_sh_seq seq in
      if Tl_form.is_ag tf then
	let (tf1,tf2) = Tl_form.unfold_ag tf in
	fix_ts
	  [[([sf],cmd,tf1); ([sf],cmd,tf2)], "UnfoldAG"]
      else
	[]
    with Not_symheap -> [] in
  wrap rl

let unfold_eg_rule = 
  let rl seq =
    try
      let (sf,cmd,tf) = dest_sh_seq seq in
      if Tl_form.is_eg tf then
	let (tf1,tf2) = Tl_form.unfold_eg tf in
	fix_ts
	  [[([sf],cmd,tf1) ; ([sf],cmd,tf2)], "UnfoldEG"]
      else 
	[]
    with Not_symheap -> [] in
  wrap rl

let unfold_af_rule = 
  let rl seq =
    try
      let (sf,cmd,tf) = dest_sh_seq seq in
      if Tl_form.is_af tf then
	let (tf1,tf2) = Tl_form.unfold_af tf in
	fix_tps
	  [[([sf],cmd,tf1)], "UnfoldAF" ;
	   [([sf],cmd,tf2)], "UnfoldAF"]
      else
	[]
    with Not_symheap -> [] in
  wrap rl

let unfold_ef_rule = 
  let rl seq =
    try
      let (sf,cmd,tf) = dest_sh_seq seq in
      if Tl_form.is_ef tf then
	let (tf1,tf2) = Tl_form.unfold_ef tf in
	fix_tps
	  [[([sf],cmd,tf1)], "UnfoldEF" ;
	   [([sf],cmd,tf2)], "UnfoldEF"]
      else
	[]
    with Not_symheap -> [] in
  wrap rl

(* if there is a backlink achievable through substitution and classical *)
(* weakening then make the proof steps that achieve it explicit so that *)
(* actual backlinking can be done on Seq.equal sequents *) 
let dobackl idx prf =
  let src_seq = Proof.get_seq idx prf in
  let targets = Rule.all_nodes idx prf in
  let apps = 
    Blist.bind
      (fun idx' -> 
       Blist.map 
         (fun res -> (idx',res))
         (matches src_seq (Proof.get_seq idx' prf))) 
      targets in
  let f (targ_idx, (theta,tagpairs)) =
      let targ_seq = Proof.get_seq targ_idx prf in
      (* [targ_seq'] is as [targ_seq] but with the tags of [src_seq] *)
      let (sf_targ_seq,cmd_targ_seq,tf_targ_seq) = targ_seq in
      let targ_seq' = 
        (Sl_form.subst_tags tagpairs sf_targ_seq, 
        cmd_targ_seq, tf_targ_seq) in 
      let subst_seq = Seq.subst theta targ_seq' in
      Rule.sequence [
        if Seq.equal src_seq subst_seq
          then Rule.identity
          else Rule.mk_infrule (frame subst_seq);
          
        if Sl_term.Map.for_all Sl_term.equal theta
          then Rule.identity
          else Rule.mk_infrule (subst_rule theta targ_seq');
           
        Rule.mk_backrule 
          false 
          (fun _ _ -> [targ_idx]) 
          (fun s s' -> 
            [(TagPairs.reflect tagpairs), "Backl"])
      ] in
    Rule.first (Blist.map f apps) idx prf


let fold def =
  let fold_rl seq = 
    try 
      let (sf,cmd,tf) = dest_sh_seq seq in
      if Sl_tpreds.is_empty sf.SH.inds then [] else
      let tags = Seq.tags seq in
      let do_case case =
        let (f,(ident,vs)) = Sl_indrule.dest case in
        let results = Sl_indrule.fold case sf in
        let process (theta, sf') = 
          let seq' = ([sf'],cmd,tf) in
          (* let () = print_endline "Fold match:" in         *)
          (* let () = print_endline (Seq.to_string seq) in   *)
          (* let () = print_endline (Sl_heap.to_string f) in *)
          (* let () = print_endline (Seq.to_string seq') in  *)
            [(
              seq', 
              TagPairs.mk (Tags.inter tags (Seq.tags seq')), 
              TagPairs.empty 
            )], ((Sl_predsym.to_string ident) ^ " Fold")  in
        Blist.map process results in
      Blist.bind do_case (Sl_preddef.rules def)
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
      SH.mk 
        (Sl_term.Set.fold Sl_uf.remove m h.SH.eqs)
        (Sl_deqs.filter
          (fun p -> Pair.conj (Pair.map (fun z -> not (Sl_term.Set.mem z m)) p))
          h.SH.deqs)
        (Sl_ptos.endomap gen_pto h.SH.ptos)
        h.SH.inds in
    let rl seq =
      try
        let (sf,cmd,tf) = dest_sh_seq seq in
        let (_,cmd') = Cmd.dest_while cmd in
        let m = Sl_term.Set.inter (Cmd.modifies cmd') (Sl_heap.vars sf) in
        let subs = Sl_term.Set.subsets m in
        Option.list_get (Blist.map
          begin fun m' ->
            let sf' = generalise m' sf in
            if Sl_heap.equal sf sf' then None else
            let s' = ([sf'], cmd,tf) in
            Some ([ (s', tagpairs s', TagPairs.empty) ], "Gen.While")
          end
          subs)
    with Not_symheap | WrongCmd -> [] in
  Rule.mk_infrule rl 

module Slprover = Prover.Make(Sl_seq)

let backlink_cut defs =
  let rl s1 s2 =
    if !termination then [] else
    (* let () = incr step in *)
    let ((sf1,cmd1,tf1),(sf2,cmd2,tf2)) = (s1,s2) in
    if not (Cmd.is_while cmd1) then [] else
    (* let () = debug (fun () -> "CUTLINK3: trying: " ^ (Seq.to_string s2)) in   *)
    (* let () = debug (fun () -> "                  " ^ (Seq.to_string s1)) in   *)
    (* let () = debug (fun () -> "CUTLINK3: step = " ^ (string_of_int !step)) in *)
    (* if !step <> 22 then None else *)
    if not (Cmd.equal cmd1 cmd2) then [] else
    (* let olddebug = !Lib.do_debug in *)
    (* let () = Lib.do_debug := true in *)
    let () = Sl_rules.setup defs in
    let result = 
      Option.is_some (Slprover.idfs 1 11 !Sl_rules.axioms !Sl_rules.rules (sf1, sf2)) in
    (* let () = Lib.do_debug := olddebug in *)
    (* let () = debug (fun () -> "CUTLINK3: result: " ^ (string_of_bool result)) in *)
    if result then [ (Seq.tagpairs_one, "Cut/Backl") ] else [] in
  Rule.mk_backrule true Rule.all_nodes rl


let axioms = 
  let entails f f' =
    Slprover.idfs 1 5 !Sl_rules.axioms !Sl_rules.rules (f, f') in
  ref (Rule.first [ex_falso_axiom; symex_check_axiom entails; symex_empty_axiom])
      
let rules = ref Rule.fail

let symex =       
  Rule.first [
      symex_skip_rule ;
      symex_assign_rule;
      symex_load_rule ;
      symex_store_rule ;
      symex_free_rule ;
      symex_new_rule ;
      symex_ifelse_rule ;
      symex_while_rule;
    ]

let unfold_gs = 
  Rule.first [
      unfold_ag_rule ;
      unfold_eg_rule ;
    ]

let unfold_fs = 
  Rule.first [
      unfold_af_rule ;
      unfold_ef_rule ;
    ]
	     
let setup defs =
  let () = Sl_rules.setup defs in
  (* Program.set_local_vars seq_to_prove ; *)
  rules := Rule.first [
	       lhs_disj_to_symheaps ;
	       simplify ;
	       
	       Rule.choice [
		   dobackl;
		   Rule.choice 
		     (Blist.map 
			(fun c -> Rule.compose (fold c) dobackl) 
			(Sl_defs.to_list defs));
		   Rule.compose_pairwise unfold_gs [Rule.identity; Rule.attempt symex];
		   unfold_fs;
		   symex;
		   luf defs;
		 ]
	     ]
