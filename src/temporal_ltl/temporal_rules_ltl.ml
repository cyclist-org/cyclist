open Lib
open Util
open Temporal_program_ltl

module SH = Sl_heap_rho
module SHNR = Sl_heap
exception Not_symheap = Sl_form_rho.Not_symheap

module Rule = Proofrule.Make(Temporal_program_ltl.Seq)
module Seqtactics = Seqtactics.Make(Temporal_program_ltl.Seq)
module Proof = Proof.Make(Temporal_program_ltl.Seq)
				 
let tagpairs s =
  Seq.tag_pairs s
		
(* following is for symex only *)
let progpairs s = 
  Seq.tag_pairs s

let dest_sh_seq (sf,cmd,tf) = (Sl_form_rho.dest sf, cmd, tf)
				
(* axioms *)
let ex_falso_axiom = 
  Rule.mk_axiom (fun (sf,_,_) -> Option.mk (Sl_form_rho.inconsistent sf) "Ex Falso")

let symex_check_axiom entails = 
  Rule.mk_axiom (fun (sf,_,tf) -> 
		Option.mk (Tl_form_ltl.is_checkable tf && Option.is_some (entails sf (Tl_form_ltl.extract_checkable_slformula tf))) "Check")

(* simplification rules *)
let eq_subst_ex_f ((sf,cmd,tf) as s) =
  let sf' = Sl_form_rho.subst_existentials sf in
  if Sl_form_rho.equal sf sf' then [] else
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

(* SO FAR WE DO NOT USE INDUCTIVE DEFINITIONS, WHEN NEEDED, SL_DEFS WILL HAVE TO ADAPT TO USE THEM *)
(* let luf_rl seq defs =                                                          *)
(*   try                                                                          *)
(*     let (sf,cmd,tf) = dest_sh_seq seq in                                       *)
(*     let seq_vars = Seq.vars seq in                                             *)
(*     let left_unfold ((_, (ident, _)) as p) =                                   *)
(*       let l' = SH.del_ind sf p in                                              *)
(*       let clauses = Sl_defs.unfold seq_vars l' p defs in                       *)
(*       let do_case (f', tagpairs) =                                             *)
(*         let l' = Sl_heap_rho.star l' f' in                                     *)
(*         (                                                                      *)
(* 					([l'],cmd,tf),                                                       *)
(* 					(if !termination then tagpairs else Seq.tagpairs_one),               *)
(* 					(if !termination then tagpairs else TagPairs.empty)                  *)
(* 				) in                                                                   *)
(*       Blist.map do_case clauses, ((Sl_predsym.to_string ident) ^ " L.Unf.") in *)
(*     Sl_tpreds.map_to_list                                                      *)
(*       left_unfold                                                              *)
(*       (Sl_tpreds.filter (Sl_defs.is_defined defs) sf.SH.inds)                  *)
(*   with Not_symheap -> []                                                       *)

(* let luf defs = wrap (fun seq -> luf_rl seq defs)                               *)
		    
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
      if Tl_form_ltl.is_next tf then
				let cont = Cmd.get_cont cmd in
				let tf' = Tl_form_ltl.step tf in
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
      let fv = fresh_evar (Sl_heap_rho.vars sf) in
      let theta = Sl_term.singleton_subst x fv in
      let sf' = Sl_heap_rho.subst theta sf in
      let e' = Sl_term.subst theta e in
      [[ SH.add_eq sf' (e',x) ], "Assign"]
    with WrongCmd | Not_symheap -> [] in
  mk_symex rl

let find_pto_on f e = 
	Sl_ptos.find (fun (l,_) -> Sl_heap_rho.equates f e l) f.SH.ptos
	
let symex_load_rule =
  let rl seq =
    try
      let (sf,cmd,_) = dest_sh_seq seq in
      let (x,e,s) = Cmd.dest_load cmd in
      let (_,ys) = find_pto_on sf e in
      let t = Blist.nth ys (Field.get_index s) in
      let fv = fresh_evar (Sl_heap_rho.vars sf) in
      let theta = Sl_term.singleton_subst x fv in
      let sf' = Sl_heap_rho.subst theta sf in
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
      let l = fresh_evars (Sl_heap_rho.vars sf) (1 + (Field.get_no_fields ())) in
      let (fv,fvs) = (Blist.hd l, Blist.tl l) in
      let sf' = Sl_heap_rho.subst (Sl_term.singleton_subst x fv) sf in
      let sf'' = Sl_heap_rho.mk_pto (x, fvs) in
      [[ Sl_heap_rho.star sf' sf'' ], "New"]
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
      let sf_list = Cond.fork_if sf c in 
      if Tl_form_ltl.is_next tf then
	let () = debug (fun _ -> "IN IF IS NEXT " ) in
	let tf' = Tl_form_ltl.step tf in
	if Cond.is_non_det c then
	  let () = debug (fun _ -> "COND IS NONDET " ) in
	  let rho_stack = sf.SH.rho in
	  let rho_var = Cond.dest_nondet c in
	  let () = debug (fun _ -> "RHO_VAR IS" ^ (Sl_term.to_string rho_var)) in
	  let () = debug (fun _ -> "RHO_STACK IS" ^ (Sl_rho.to_string rho_stack)) in
	  try
    	    match Sl_rho.find rho_var rho_stack with
    	    | (-2) -> let () = debug (fun _ -> "RHO IS -2 " ) in fix_tps 
	  							   [[ ([List.nth sf_list 0], Cmd.mk_seq cmd1 cont, tf')], "If - 2"]
    	    | (-1) -> let () = debug (fun _ -> "RHO IS -1" ) in fix_tps 
	  							  [[ ([List.nth sf_list 0], Cmd.mk_seq cmd2 cont, tf')], "If - 1"]
    	    | _ -> let () = debug (fun _ -> "RHO IS DEFAULT" ) in []
    	  with Not_found -> let () = debug (fun _ -> "RHO WAS NOT FOUND" ) in
			    fix_tps 
	  		      [[ ([List.nth sf_list 0], Cmd.mk_seq cmd1 cont, tf') ; ([List.nth sf_list 1], Cmd.mk_seq cmd2 cont, tf') ], "Determinise If"]
	else
	  let () = debug (fun _ -> "IN IF IS NEXT WITH DETERMINISTIC CONDITION " ) in
	  fix_tps 
            [[ ([List.nth sf_list 0], Cmd.mk_seq cmd1 cont, tf') ; ([List.nth sf_list 1], Cmd.mk_seq cmd2 cont,tf') ], "If"]
      	  
      else
	let () = debug (fun _ -> "IN IF IS NOT NEXT!!!!!!!!!!!!! " ) in
	[]
    with Not_symheap | WrongCmd -> [] in
  wrap rl
       
let symex_while_rule =
  let rl seq =
    try
      let (sf,cmd,tf) = dest_sh_seq seq in
      let (c,cmd') = Cmd.dest_while cmd in
      let cont = Cmd.get_cont cmd in
      let sf_list = Cond.fork sf c in 
      if Tl_form_ltl.is_next tf then
	let () = debug (fun _ -> "IN WHILE IS NEXT " ) in
	let tf' = Tl_form_ltl.step tf in
	if Cond.is_non_det c then
	  let () = debug (fun _ -> "COND IS NONDET " ) in
	  let rho_stack = sf.SH.rho in
	  let rho_var = Cond.dest_nondet c in
	  let () = debug (fun _ -> "RHO_VAR IS" ^ (Sl_term.to_string rho_var)) in
	  let () = debug (fun _ -> "RHO_STACK IS" ^ (Sl_rho.to_string rho_stack)) in
	  try
    	    match Sl_rho.find rho_var rho_stack with
    	    | (-2) -> let () = debug (fun _ -> "RHO IS -2 " ) in fix_tps 
	  							   [[ ([List.nth sf_list 0], cmd, tf') ; ([List.nth sf_list 1], cmd, tf') ], "Determinise"]
    	    | (-1) -> let () = debug (fun _ -> "RHO IS -1" ) in fix_tps 
	  							  [[ ([List.nth sf_list 0], Cmd.mk_seq cmd' cmd, tf')], "While - Bot"]
    	    | 0 -> let () = debug (fun _ -> "RHO IS 0" ) in fix_tps 
	  						      [[ ([List.nth sf_list 0], cont, tf')], "While - Zero"]
    	    | n when n>0 -> let () = debug (fun _ -> "RHO IS >0" ) in fix_tps 
	  								[[ ([List.nth sf_list 0], Cmd.mk_seq cmd' cmd, tf')], "While - N"]
    	    | _ -> let () = debug (fun _ -> "RHO IS DEFAULT" ) in []
    	  with Not_found -> let () = debug (fun _ -> "RHO WAS NOT FOUND" ) in
			    fix_tps 
	  		      [[ ([List.nth sf_list 0], cmd, tf') ; ([List.nth sf_list 1], cmd, tf') ], "Determinise"]
	else
	  fix_tps 
	    [[ ([List.nth sf_list 0], Cmd.mk_seq cmd' cmd, tf') ; ([List.nth sf_list 1], cont, tf') ], "While"]
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
    if not (Cmd.equal cmd cmd' && Tl_form_ltl.equal tf tf'
	    && (Tl_form_ltl.is_g tf || Tl_form_ltl.is_g tf')) then [] else
      let (sf, sf') = Pair.map Sl_form_rho.dest(sf,sf') in
      let sub_check = Sl_term.combine_subst_checks [
			  Sl_term.basic_lhs_down_check ;
			  Sl_term.avoids_replacing_check !program_vars ;
			] in
      let cont =
	Sl_term.mk_verifier
          (fun (theta, tagpairs) -> 
           let subst_seq = (Seq.subst_tags tagpairs (Seq.subst theta seq')) in
           let () = debug (fun _ -> "term substitution: " ^ ((Format.asprintf " %a" Sl_term.pp_subst theta))) in
           let () = debug (fun _ -> "tag substitution: " ^ (TagPairs.to_string tagpairs)) in
           let () = debug (fun _ -> "source seq: " ^ (Seq.to_string seq)) in
           let () = debug (fun _ -> "target seq: " ^ (Seq.to_string seq')) in
           let () = debug (fun _ -> "substituted target seq: " ^ (Seq.to_string subst_seq)) in
           Seq.subsumed seq subst_seq) in
      let res = 
	Sl_term.backtrack 
	  (Sl_heap_rho.unify_partial ~tagpairs:true)
	  ~sub_check
	  ~cont
	  sf' sf in
      if Tl_form_ltl.is_g tf && Tl_form_ltl.is_g tf' then
	let (t1, _) = Tl_form_ltl.dest_g tf in
	let (t2, _) = Tl_form_ltl.dest_g tf' in
	assert (Tags.equal (Tags.singleton t1) (Tags.singleton t2)); Blist.map (fun (t,tp) -> (t, (TagPairs.add (t1,t2) tp))) res
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
  if Seq.equal seq seq' then
    [ [(seq', Seq.tag_pairs seq', TagPairs.empty)], "Frame" ]
  else
    []

let unfold_g_rule = 
  let rl seq =
    try
      let (sf,cmd,tf) = dest_sh_seq seq in
      if Tl_form_ltl.is_g tf then
	let (tf1,tf2) = Tl_form_ltl.unfold_g tf in
	fix_ts
	  [[([sf],cmd,tf1); ([sf],cmd,tf2)], "UnfoldG"]
      else
	[]
    with Not_symheap -> [] in
  wrap rl

let unfold_f_rule = 
  let rl seq =
    try
      let (sf,cmd,tf) = dest_sh_seq seq in
      if Tl_form_ltl.is_f tf then
	let (tf1,tf2) = Tl_form_ltl.unfold_f tf in
	fix_tps
	  [[([sf],cmd,tf1)], "UnfoldF" ;
	   [([sf],cmd,tf2)], "UnfoldF"]
      else
	[]
    with Not_symheap -> [] in
  wrap rl

let disjunction_rule = 
		let rl seq = 
			try
				let (sf,cmd,tf) = dest_sh_seq seq in
				if Tl_form_ltl.is_or tf then
					let (tf1,tf2) = Tl_form_ltl.unfold_or tf in
					fix_tps
					[[([sf],cmd,tf1)], "Disj1" ;
	   				[([sf],cmd,tf2)], "Disj2"]
				else 
					[]
			with Not_symheap -> [] in
			wrap rl	
			
let conjunction_rule = 
		let rl seq = 
			try
				let (sf,cmd,tf) = dest_sh_seq seq in
				if Tl_form_ltl.is_and tf then
					let (tf1,tf2) = Tl_form_ltl.unfold_and tf in
					fix_tps
					[[([sf],cmd,tf1);([sf],cmd,tf2)], "Conj"]
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
        (Sl_form_rho.subst_tags tagpairs sf_targ_seq, 
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


(* THIS IS FOR INDUCTIVE DEFINITIONS *)
(* let fold def =                                                      *)
(*   let fold_rl seq =                                                 *)
(*     try                                                             *)
(*       let (sf,cmd,tf) = dest_sh_seq seq in                          *)
(*       if Sl_tpreds.is_empty sf.SH.inds then [] else                 *)
(*       let tags = Seq.tags seq in                                    *)
(*       let do_case case =                                            *)
(*         let (f,(ident,vs)) = Sl_indrule.dest case in                *)
(*         let results = Sl_indrule.fold case sf in                    *)
(*         let process (theta, sf') =                                  *)
(*           let seq' = ([sf'],cmd,tf) in                              *)
(*           (* let () = print_endline "Fold match:" in         *)     *)
(*           (* let () = print_endline (Seq.to_string seq) in   *)     *)
(*           (* let () = print_endline (Sl_heap_rho.to_string f) in *) *)
(*           (* let () = print_endline (Seq.to_string seq') in  *)     *)
(*             [(                                                      *)
(*               seq',                                                 *)
(*               TagPairs.mk (Tags.inter tags (Seq.tags seq')),        *)
(*               TagPairs.empty                                        *)
(*             )], ((Sl_predsym.to_string ident) ^ " Fold")  in        *)
(*         Blist.map process results in                                *)
(*       Blist.bind do_case (Sl_preddef.rules def)                     *)
(*     with Not_symheap -> [] in                                       *)
(*   Rule.mk_infrule fold_rl                                           *)

(* let generalise_while_rule =                                                      *)
(*   let generalise m h =                                                           *)
(*     let avoid = ref (Sl_heap_rho.vars h) in                                      *)
(*     let gen_term t =                                                             *)
(*     if Sl_term.Set.mem t m then                                                  *)
(*       (let r = fresh_evar !avoid in avoid := Sl_term.Set.add r !avoid ; r)       *)
(*     else t in                                                                    *)
(*     let gen_pto (x,args) =                                                       *)
(*     let l = Blist.map gen_term (x::args) in (Blist.hd l, Blist.tl l) in          *)
(*       SH.mk                                                                      *)
(*         (Sl_term.Set.fold Sl_uf.remove m h.SH.eqs)                               *)
(*         (Sl_deqs.filter                                                          *)
(*           (fun p -> Pair.conj (Pair.map (fun z -> not (Sl_term.Set.mem z m)) p)) *)
(*           h.SH.deqs)                                                             *)
(*         (Sl_ptos.endomap gen_pto h.SH.ptos)                                      *)
(*         h.SH.inds in                                                             *)
(*     let rl seq =                                                                 *)
(*       try                                                                        *)
(*         let (sf,cmd,tf) = dest_sh_seq seq in                                     *)
(*         let (_,cmd') = Cmd.dest_while cmd in                                     *)
(*         let m = Sl_term.Set.inter (Cmd.modifies cmd') (Sl_heap_rho.vars sf) in   *)
(*         let subs = Sl_term.Set.subsets m in                                      *)
(*         Option.list_get (Blist.map                                               *)
(*           begin fun m' ->                                                        *)
(*             let sf' = generalise m' sf in                                        *)
(*             if Sl_heap_rho.equal sf sf' then None else                           *)
(*             let s' = ([sf'], cmd,tf) in                                          *)
(*             Some ([ (s', tagpairs s', TagPairs.empty) ], "Gen.While")            *)
(*           end                                                                    *)
(*           subs)                                                                  *)
(*     with Not_symheap | WrongCmd -> [] in                                         *)
(*   Rule.mk_infrule rl                                                             *)

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
		let temp_list = Blist.map (fun (rho_form:Sl_heap_rho.t) -> (rho_form.rho,SHNR.mk rho_form.eqs rho_form.deqs rho_form.ptos rho_form.inds)) sf1 in
		let rho_list1 = Blist.map (fun (x,_) -> x) temp_list in
		let sl_form1 = Blist.map (fun (_,y) -> y) temp_list in
		let temp_list2 = Blist.map (fun (rho_form:Sl_heap_rho.t) -> (rho_form.rho,SHNR.mk rho_form.eqs rho_form.deqs rho_form.ptos rho_form.inds)) sf2 in
		let rho_list2 = Blist.map (fun (x,_) -> x) temp_list2 in
		let sl_form2 = Blist.map (fun (_,y) -> y) temp_list2 in
		let rec same_rho r1 r2 = begin match r1,r2 with
		| [],[] -> true
		| [],_ -> false
		| _,[] -> false
		| h1::tl1,h2::tl2 -> begin match Sl_rho.equal h1 h2 with
														| true -> same_rho tl1 tl2
														| _ -> false
														end
		end in
		let () = Sl_rules.setup defs in
    let result = 
      Option.is_some (Slprover.idfs 1 11 !Sl_rules.axioms !Sl_rules.rules (sl_form1, sl_form2)) in
    (* let () = Lib.do_debug := olddebug in *)
    (* let () = debug (fun () -> "CUTLINK3: result: " ^ (string_of_bool result)) in *)
    if result && same_rho rho_list1 rho_list2 then [ (Seq.tagpairs_one, "Cut/Backl") ] else [] in
  Rule.mk_backrule true Rule.all_nodes rl


let axioms = 
  let entails f f' =
		let temp_list = Blist.map (fun (rho_form:Sl_heap_rho.t) -> (rho_form.rho,SHNR.mk rho_form.eqs rho_form.deqs rho_form.ptos rho_form.inds)) f in
		let sl_form1 = Blist.map (fun (_,y) -> y) temp_list in
		let temp_list2 = Blist.map (fun (rho_form:Sl_heap_rho.t) -> (rho_form.rho,SHNR.mk rho_form.eqs rho_form.deqs rho_form.ptos rho_form.inds)) f' in
		let sl_form2 = Blist.map (fun (_,y) -> y) temp_list2 in
    Slprover.idfs 1 5 !Sl_rules.axioms !Sl_rules.rules (sl_form1, sl_form2) in
  ref (Rule.first [symex_check_axiom entails])
      
let rules = ref Rule.fail

let symex =       
  Rule.first [
      symex_skip_rule ;
      symex_assign_rule;
      symex_load_rule ;
      symex_store_rule ;
      symex_free_rule ;
      symex_new_rule ;
      (Rule.compose symex_ifelse_rule (Rule.attempt ex_falso_axiom));
      (Rule.compose symex_while_rule (Rule.attempt ex_falso_axiom));
    ]
	     
let setup defs =
  let () = Sl_rules.setup defs in
  (* Program.set_local_vars seq_to_prove ; *)
  rules := Rule.first [
	       lhs_disj_to_symheaps ;
	       simplify ;
	       
	       Rule.choice [
		   dobackl;
		  (*  Rule.choice                              *)
		  (*    (Blist.map                             *)
			(* (fun c -> Rule.compose (fold c) dobackl)  *)
			(* (Sl_defs.to_list defs));                  *)
		   Rule.compose_pairwise unfold_g_rule [Rule.identity; Rule.attempt symex];
		   unfold_f_rule;
		   symex;
		   (* luf defs; *)
			 disjunction_rule;
			 conjunction_rule;
		 ]
	     ]
