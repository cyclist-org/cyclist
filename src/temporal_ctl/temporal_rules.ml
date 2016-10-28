open Lib

open Temporal_program

module SH = Sl_heap
exception Not_symheap = Sl_form.Not_symheap

module Rule = Proofrule.Make(Temporal_program.Seq)
module Seqtactics = Seqtactics.Make(Temporal_program.Seq)
module Proof = Proof.Make(Temporal_program.Seq)
module Slprover = Prover.Make(Sl_seq)

let use_cut = ref true

let check_entail = ref ((fun _ -> None) : Slprover.Seq.t -> Slprover.Proof.t option)

let entails f f' =
  let () = debug (fun () -> "Trying to prove entailment: " ^ (Sl_seq.to_string (f, f'))) in
  let result = 
    let olddebug = !do_debug in
    let res = (do_debug := false; !check_entail (f, f')) in
    do_debug := olddebug; res in
  result
			 
let tagpairs s =
  Seq.tag_pairs s
		
(* following is for symex only *)
(* TODO: review this - I don't think this can be right [RR] *)
let progpairs s = 
  Seq.tag_pairs s

let dest_sh_seq (sf,cmd,tf) = (Sl_form.dest sf, cmd, tf)

let to_sl_form hs = Ord_constraints.empty, hs
				
(* axioms *)
let subformula_axiom =
  Rule.mk_axiom 
    (fun (sf,_,tf) ->
     match Tl_form.is_atom tf with
     | true -> let tf_heap = Tl_form.dest_atom tf in
	       let (_,_,ptos,inds) = Sl_heap.dest tf_heap in
	       (Option.mk (Sl_ptos.is_empty ptos &&
			     Sl_tpreds.is_empty inds &&
			       not (Sl_heap.is_empty tf_heap) &&
				 Sl_form.subsumed ~total:false (to_sl_form [tf_heap]) sf) "Sub-Check")
     | false -> None)

let ex_falso_axiom = 
  Rule.mk_axiom (fun (sf,_,_) -> Option.mk (Sl_form.inconsistent sf) "Ex Falso")

let symex_check_axiom entails = 
  Rule.mk_axiom 
    (fun (pre,_,tf) -> 
      let f = Sl_form.complete_tags Tags.empty (Tl_form.extract_checkable_slformula tf) in
      Option.mk 
      (Tl_form.is_checkable tf && Option.is_some (entails pre f)) 
      "Check")
		
let symex_empty_axiom =
  Rule.mk_axiom (fun (_,cmd,tf) -> Option.mk (Cmd.is_empty cmd && Tl_form.is_box tf) "Empty")

(* simplification rules *)
let eq_subst_ex_f ((sf,cmd,tf) as s) =
  let sf' = Sl_form.subst_existentials sf in
  if Sl_form.equal sf sf' then [] else
    [ [ ((sf', cmd, tf), tagpairs s, Tagpairs.empty) ], "Eq. subst. ex" ]

let final_axiom =
  Rule.mk_axiom (fun (_,cmd,tf) -> Option.mk (Cmd.is_empty cmd && Tl_form.is_final tf) "Final")

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
  let rl ((cs, hs),cmd,tf) =
    if Blist.length hs < 2 then [] else
      [ Blist.map 
          (fun sh -> let s' = ((cs, [sh]),cmd,tf) in (s', tagpairs s', Tagpairs.empty ) ) 
          hs,
      "L.Or"
      ] in
  Rule.mk_infrule rl

let luf_rl seq defs =
  try
    let ((cs,h),cmd,tf) = dest_sh_seq seq in
    (* let (c,_,_) = if Cmd.is_ifelse cmd then Cmd.dest_ifelse cmd else let (c,cont) = Cmd.dest_while cmd in (c,cont,cont) in *)
    (* let cond_vars = Cond.vars c in *)
    let t = if Cmd.is_load cmd then let (t,_,_) = Cmd.dest_load cmd in t else let (t,_,_) = Cmd.dest_store cmd in t in
    let () = debug (fun _ -> "Term in cmd for luf: " ^ (Sl_term.to_string t)) in 
    if (Blist.exists Option.is_some (Blist.map (fun var -> SH.find_lval var h) [t])) then
      let () = debug (fun _ -> "Already exists ") in 
      []
    else
      let () = debug (fun _ -> "Does NOT exists ") in 
      let seq_vars = Seq.vars seq in
      let seq_tags = Seq.tags seq in
      let left_unfold ((t, (ident, _)) as p) = 
        let h' = SH.del_ind h p in
        let clauses = Sl_defs.unfold (seq_vars, seq_tags) p defs in
        let do_case body =
          let tag_subst = Tagpairs.mk_free_subst seq_tags (Sl_heap.tags body) in
          let body = Sl_heap.subst_tags tag_subst body in
          let h' = Sl_heap.star h' body in
          let progpairs = 
            if !termination 
              then Tagpairs.endomap (fun (_, t') -> (t, t')) tag_subst
              else Tagpairs.empty in
          let allpairs = 
            Tagpairs.union
              (Tagpairs.remove (t,t) (Seq.tag_pairs seq))
              (progpairs) in
          ( ((cs,[h']),cmd,tf), 
        	  allpairs,
        	  (if !termination then progpairs else Tagpairs.empty)
        	) in
        Blist.map do_case clauses, ((Sl_predsym.to_string ident) ^ " L.Unf.") in
      Sl_tpreds.map_to_list 
        left_unfold 
        (Sl_tpreds.filter (Sl_defs.is_defined defs) h.SH.inds)
  with WrongCmd | Not_symheap -> []

let luf defs = wrap (fun seq -> luf_rl seq defs)
		    
(* FOR SYMEX ONLY *)
(* this is only used for the unfold_eg rule *)
let fix_ts l =
  Blist.map
    (fun (g,d) ->
     Blist.map
       (fun s -> (s, tagpairs s, Tagpairs.empty))
       g, d)
    l
    
let fix_tps l = 
  Blist.map 
    (fun (g,d) -> Blist.map (fun s -> (s, tagpairs s, progpairs s )) g, d) l 
    
let mk_symex f = 
  let rl ((pre,cmd,tf) as seq) = 
    try
      let cont = Cmd.get_cont cmd in
      Option.dest 
        []
        (fun tf' -> 
          fix_tps 
            (Blist.map 
              (fun (g,d) -> 
                Blist.map (fun h' -> ((Sl_form.with_heaps pre [h']), cont, tf')) g, d) 
              (f seq)))
        (if Tl_form.is_diamond tf then Some (Tl_form.e_step tf)
          else if Tl_form.is_box tf then Some (Tl_form.a_step tf)
          else None)
    with WrongCmd -> []
  in wrap rl
       
(* symbolic execution rules *)
let symex_assign_rule =
  let rl seq =
    try
      let ((_,h),cmd,_) = dest_sh_seq seq in
      let (x,e) = Cmd.dest_assign cmd in
      let fv = fresh_evar (Sl_heap.vars h) in
      let theta = Sl_subst.singleton x fv in
      let h' = Sl_heap.subst theta h in
      let e' = Sl_subst.apply theta e in
      [[ SH.add_eq h' (e',x) ], "Assign"]
    with WrongCmd | Not_symheap -> [] in
  mk_symex rl

let find_pto_on f e = 
	Sl_ptos.find (fun (l,_) -> Sl_heap.equates f e l) f.SH.ptos
	
let symex_load_rule =
  let rl seq =
    try
      let ((_,h),cmd,_) = dest_sh_seq seq in
      let (x,e,s) = Cmd.dest_load cmd in
      let (_,ys) = find_pto_on h e in
      let t = Blist.nth ys (Field.get_index s) in
      let fv = fresh_evar (Sl_heap.vars h) in
      let theta = Sl_subst.singleton x fv in
      let h' = Sl_heap.subst theta h in
      let t' = Sl_subst.apply theta t in
      [[ SH.add_eq h' (t',x) ], "Load"]
    with Not_symheap | WrongCmd | Not_found -> [] in
  mk_symex rl
	   
let symex_store_rule =
  let rl seq =
    try
      let ((_,h),cmd,_) = dest_sh_seq seq in
      let (x,s,e) = Cmd.dest_store cmd in
      let ((x',ys) as pto) = find_pto_on h x in
      let pto' = (x', Blist.replace_nth e (Field.get_index s) ys) in
      [[ SH.add_pto (SH.del_pto h pto) pto' ], "Store"]
    with Not_symheap | WrongCmd | Not_found -> [] in
  mk_symex rl

let symex_free_rule =
  let rl seq =
    try
      let ((_,h),cmd,_) = dest_sh_seq seq in
      let e = Cmd.dest_free cmd in
      let pto = find_pto_on h e in
      [[ SH.del_pto h pto ], "Free"]
    with Not_symheap | WrongCmd | Not_found -> [] in
  mk_symex rl

let symex_new_rule =
  let rl seq =
    try
      let ((_,h),cmd,_) = dest_sh_seq seq in
      let x = Cmd.dest_new cmd in
      let l = fresh_evars (Sl_heap.vars h) (1 + (Field.get_no_fields ())) in
      let (fv,fvs) = (Blist.hd l, Blist.tl l) in
      let h' = Sl_heap.subst (Sl_subst.singleton x fv) h in
      let h'' = Sl_heap.mk_pto (x, fvs) in
      [[ Sl_heap.star h' h'' ], "New"]
    with Not_symheap | WrongCmd-> [] in
  mk_symex rl

let symex_skip_rule =
  let rl seq =
    try
      let ((_,h),cmd,_) = dest_sh_seq seq in 
      let () = Cmd.dest_skip cmd in [[h], "Skip"]
    with Not_symheap | WrongCmd -> [] in
  mk_symex rl

let symex_ifelse_rule =
  let rl seq =
    try
      let ((cs,h),cmd,tf) = dest_sh_seq seq in
      let (c,cmd1,cmd2) = Cmd.dest_ifelse cmd in
      let cont = Cmd.get_cont cmd in
      let (h',h'') = Cond.fork h c in 
      if Tl_form.is_box tf then
        let tf' = Tl_form.a_step tf in
        fix_tps 
          [ [ ((cs,[h']), Cmd.mk_seq cmd1 cont, tf') ; ((cs,[h'']), Cmd.mk_seq cmd2 cont,tf') ], "If-[]" ]
      else if Tl_form.is_diamond tf then
        let tf' = Tl_form.e_step tf in
        if Cond.is_non_det c then 
          fix_tps 
            [ [ ((cs,[h']), Cmd.mk_seq cmd1 cont, tf')], "If-<>1" ;
              [ ((cs,[h'']), Cmd.mk_seq cmd2 cont, tf')], "If-<>2"  ]
        else if Cond.validated_by h c then
          fix_tps [[ ((cs,[h']), Cmd.mk_seq cmd1 cont, tf')], "If-<>1"]
        else 
          fix_tps [[ ((cs,[h'']), Cmd.mk_seq cmd2 cont, tf')], "If-<>2"]	  
      else
        []
    with Not_symheap | WrongCmd -> [] in
  wrap rl
       
let symex_while_rule =
  let rl seq =
    try
      let ((cs,h),cmd,tf) = dest_sh_seq seq in
      let (c,cmd') = Cmd.dest_while cmd in
      let cont = Cmd.get_cont cmd in
      let (h',h'') = Cond.fork h c in 
      if Tl_form.is_box tf then
        let tf' = Tl_form.a_step tf in
        fix_tps 
          [[ ((cs,[h']), Cmd.mk_seq cmd' cmd, tf') ; ((cs,[h'']), cont, tf') ], "While-Box"]
      else if Tl_form.is_diamond tf then
        let tf' = Tl_form.e_step tf in
        if Cond.is_non_det c then
          fix_tps 
            [ [ ((cs,[h']), Cmd.mk_seq cmd' cmd, tf')], "While-<>1" ;
              [ ((cs,[h'']), cont, tf')], "While-<>2" ]
        else if Cond.validated_by h c then
          fix_tps [[ ((cs,[h']), Cmd.mk_seq cmd' cmd, tf')], "While-<>1"]
        else
          fix_tps [[ ((cs,[h'']), cont, tf')], "While-<>2"]
      else
        []
    with Not_symheap | WrongCmd -> [] in
  wrap rl

(* NB. This only returns the first matching it finds - there may be others which  *)
(*     perhaps will make a difference to the cut entailment that is tried in the  *)
(*     matches function below! *)
let match_inds h h' =
  let h_inds = h.SH.inds in
  let h_inds' = h'.SH.inds in
  let rec _match_inds inds inds' acc =
    if Sl_tpreds.is_empty inds then
      let rest = Sl_tpreds.fold (fun (t, _) -> Tagpairs.add (t, Tags.anonymous)) inds' Tagpairs.empty in
      Tagpairs.union acc rest
    else
      let ((t, (_, ts)) as p) = Sl_tpreds.choose inds in
      assert (not (Tags.is_anonymous t));
      let ps = Sl_tpreds.remove p inds in
      Option.dest_lazily
        (fun () -> _match_inds ps inds acc)
        (fun ((t', _) as p') -> 
          let ps' = Sl_tpreds.remove p' inds' in
          let acc = Tagpairs.add (t', t) acc in
          _match_inds ps ps' acc)
        (Option.flatten (Option.mk_lazily
          ((Tags.is_free_var t) && Blist.for_all (Fun.neg Sl_term.is_exist_var) ts)
          (fun () -> 
            Sl_tpreds.find_opt 
              (fun ((t', _) as p') -> Tags.is_free_var t' && Sl_tpred.equal_upto_tags p p') 
              inds'))) in
  _match_inds h_inds h_inds' Tagpairs.empty

let matches ((sf,cmd,tf) as seq) ((sf',cmd',tf') as seq') =
  try
    (* let () = print_endline "At matches with seqs:" in   *)
    (* let () = print_endline (Seq.to_string seq) in   *)
    (* let () = print_endline (Seq.to_string seq') in   *)
    if not (Cmd.equal cmd cmd' && Tl_form.equal tf tf' && Cmd.is_while cmd
	    (* && (Tl_form.is_ag tf || Tl_form.is_eg tf) *)
	    (* && (Tl_form.is_ag tf' || Tl_form.is_eg tf') *)) then 
      (* let () = print_endline "Seqs are not equal" in   *)
      [] 
    else
      let ((cs,h),(cs',h')) = Pair.map Sl_form.dest (sf,sf') in
      let res = Sl_unify.Unidirectional.realize (
        Unification.backtrack
          (Sl_heap.unify_partial
            ~update_check:(Fun.conj
              (Sl_unify.Unidirectional.trm_check)
              (Sl_unify.Unidirectional.avoid_replacing_trms !program_vars)))
            h' h
          (Sl_unify.Unidirectional.unify_tag_constraints 
            cs cs'
          (Sl_unify.Unidirectional.mk_verifier
            (Sl_unify.Unidirectional.mk_assert_check
              (fun (theta, tagpairs) -> 
                let subst_seq = (Seq.subst_tags tagpairs (Seq.subst theta seq')) in
                let () = debug (fun _ -> "term substitution: " ^ ((Format.asprintf " %a" Sl_subst.pp theta))) in 
                let () = debug (fun _ -> "tag substitution: " ^ (Tagpairs.to_string tagpairs)) in 
                let () = debug (fun _ -> "source seq: " ^ (Seq.to_string seq)) in
                let () = debug (fun _ -> "target seq: " ^ (Seq.to_string seq')) in
                let () = debug (fun _ -> "substituted target seq: " ^ (Seq.to_string subst_seq)) in
                Seq.subsumed seq subst_seq))))) in
      (* ATTEMPT CUT *)
      let res = 
        if Blist.is_empty res && !use_cut then
          let (ts', ts) = 
            Tagpairs.partition 
              (fun (_, t) -> Tags.is_anonymous t) 
              (match_inds h h') in
          if Tagpairs.is_empty ts then 
            [] 
          else
            let et = Tags.filter Tags.is_exist_var (Sl_form.tags sf') in
            let ft = Tags.filter Tags.is_free_var (Tagpairs.projectl ts') in
            let theta = Tagpairs.union ts (Tagpairs.mk_ex_subst (Tags.union et (Sl_form.tags sf)) ft) in
            let sf' = Sl_form.subst_tags theta sf' in
            let result = entails sf sf' in
            let () = debug (fun () -> "CUTLINK3: result: " ^ (string_of_bool (Option.is_some result))) in
            if Option.is_some result then
              [(Sl_subst.empty, theta)]
            else
              []
        else
          res in
      let temporal_tag = 
        if Tl_form.is_ag tf then 
          Some (Pair.map (fun tf -> Pair.left (Tl_form.dest_ag tf)) (tf, tf'))
        else if Tl_form.is_eg tf then 
          Some (Pair.map (fun tf -> Pair.left (Tl_form.dest_eg tf)) (tf, tf'))
        else None in
      Option.dest
        (res)
        (fun (t, t') ->
          assert (Tags.Elt.equal t t'); 
          Blist.map (Pair.map_right (Tagpairs.add (t, t'))) res)
        (temporal_tag)
  with Not_symheap -> []
			
(*    seq'     *)
(* ----------  *)
(* seq'[theta] *)
(* where seq'[theta] = seq *)
let subst_rule theta seq' seq = 
  if Seq.equal (Seq.subst theta seq') seq 
    then 
      [ [(seq', Seq.tag_pairs seq', Tagpairs.empty)], "Subst " ]
    else 
      []

let frame seq' seq = 
  if Seq.subsumed seq seq' then
    [ [(seq', Seq.tag_pairs seq', Tagpairs.empty)], "Frame" ]
  else
    []

let cut seq' seq =
  let ((sf1,cmd1,tf1),(sf2,cmd2,tf2)) = (seq,seq') in
  (* if Option.is_some (Slprover.idfs 1 11 !Sl_rules.axioms !Sl_rules.rules (sf1, sf2)) then *)
  if !use_cut then
    [ [(seq',
        Tagpairs.union (Tagpairs.mk (Tl_form.outermost_tag tf1)) (Tagpairs.mk (Tags.inter (Seq.tags seq) (Seq.tags seq')))
	(* Tagpairs.mk (Tags.inter (Seq.tags seq) (Seq.tags seq')) *) (*Seq.tag_pairs seq*),
       Tagpairs.empty (* Tagpairs.mk (Tags.inter (Seq.tags seq) (Seq.tags seq')) *) (*Seq.tag_pairs seq*))], "Cut" ]
  else
    []

let unfold_ag_rule = 
  let rl seq =
    try
      let ((cs,h),cmd,tf) = dest_sh_seq seq in
      let (tf1,tf2) = Tl_form.unfold_ag tf in
        fix_tps
          [[((cs,[h]),cmd,tf1); ((cs,[h]),cmd,tf2)], "UnfoldAG"]
    with Not_symheap | Invalid_argument(_) -> [] in
  wrap rl

let unfold_eg_rule = 
  let rl seq =
    try
      let ((cs,h),cmd,tf) = dest_sh_seq seq in
      let (tf1,tf2) = Tl_form.unfold_eg tf in
      fix_ts
        [[((cs,[h]),cmd,tf1) ; ((cs,[h]),cmd,tf2)], "UnfoldEG"]
    with Not_symheap | Invalid_argument(_) -> [] in
  wrap rl

let unfold_af_rule = 
  let rl seq =
    try
      let ((cs,h),cmd,tf) = dest_sh_seq seq in
      let (tf1,tf2) = Tl_form.unfold_af tf in
      fix_tps
        [[((cs,[h]),cmd,tf1)], "UnfoldAF" ; [((cs,[h]),cmd,tf2)], "UnfoldAF"]
    with Not_symheap | Invalid_argument(_) -> [] in
  wrap rl

let unfold_ef_rule = 
  let rl seq =
    try
      let ((cs,h),cmd,tf) = dest_sh_seq seq in
      let (tf1,tf2) = Tl_form.unfold_ef tf in
      fix_tps
        [[((cs,[h]),cmd,tf1)], "UnfoldEF" ; [((cs,[h]),cmd,tf2)], "UnfoldEF"]
    with Not_symheap | Invalid_argument(_) -> [] in
  wrap rl

let disjunction_rule = 
  let rl seq = 
    try
      let ((cs,h),cmd,tf) = dest_sh_seq seq in
      let (tf1,tf2) = Tl_form.unfold_or tf in
      fix_tps
        [[((cs,[h]),cmd,tf1)], "Disj1" ; [((cs,[h]),cmd,tf2)], "Disj2"]
    with Not_symheap | Invalid_argument(_) -> [] in
  wrap rl	
       
let conjunction_rule = 
  let rl seq = 
    try
      let ((cs,h),cmd,tf) = dest_sh_seq seq in
      let (tf1,tf2) = Tl_form.unfold_and tf in
      fix_tps
        [[((cs,[h]),cmd,tf1); ((cs,[h]),cmd,tf2)], "Conj"]
    with Not_symheap | Invalid_argument(_) -> [] in
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
    let targ_seq' = (Sl_form.subst_tags tagpairs sf_targ_seq, cmd_targ_seq, tf_targ_seq) in 
    let subst_seq = Seq.subst theta targ_seq' in
    Rule.sequence [
        if Seq.equal src_seq subst_seq then 
          Rule.identity
        else if Seq.subsumed src_seq targ_seq then
          Rule.mk_infrule (frame subst_seq)
        else
          Rule.mk_infrule (cut subst_seq);

        if Sl_term.Map.for_all Sl_term.equal theta
        then Rule.identity
        else Rule.mk_infrule (subst_rule theta targ_seq');

        Rule.mk_backrule 
          false 
          (fun _ _ -> [targ_idx]) 
          (fun (_,_,tf) s' -> 
            (* [(if !termination then Tagpairs.empty else Seq.tagpairs_one), "Backl"]) *)
            [(if !termination then Tagpairs.reflect tagpairs else Tagpairs.mk (Tl_form.outermost_tag tf)), "Backl"])
    ] in
  (* let () = print_endline "Attempting backlink with source seq:" in   *)
  (* let () = print_endline (Seq.to_string src_seq) in   *)
  let rule_list = (Blist.map f apps) in
  (* let () = print_endline "IS LIST EMPTY? " in *)
  (* let () = print_endline (string_of_bool (Blist.is_empty rule_list)) in *)
  Rule.first rule_list idx prf
	     
let fold def =
  let fold_rl seq = 
    try 
      let ((cs,h),cmd,tf) = dest_sh_seq seq in
      if Sl_tpreds.is_empty h.SH.inds then [] else
      let tags = Seq.tags seq in
      let do_case case =
        let (f,(ident,vs)) = Sl_indrule.dest case in
        let results = Sl_indrule.fold case h in
        let process (theta, h') = 
          let seq' = ((cs,[h']),cmd,tf) in
          (* let () = print_endline "Fold match:" in *)
          (* let () = print_endline (Seq.to_string seq) in *)
          (* let () = print_endline (Sl_heap.to_string f) in *)
          (* let () = print_endline (Seq.to_string seq') in *)
            [(
              seq', 
              (* Tagpairs.empty *) Tagpairs.mk (Tags.inter tags (Seq.tags seq')), 
              Tagpairs.empty 
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
        let ((cs,h),cmd,tf) = dest_sh_seq seq in
        let (_,cmd') = Cmd.dest_while cmd in
        let m = Sl_term.Set.inter (Cmd.modifies cmd') (Sl_heap.vars h) in
        let subs = Sl_term.Set.subsets m in
        Option.list_get (Blist.map
          begin fun m' ->
            let h' = generalise m' h in
            if Sl_heap.equal h h' then None else
            let s' = ((cs,[h']),cmd,tf) in
            Some ([ (s', tagpairs s', Tagpairs.empty) ], "Gen.While")
          end
          subs)
    with Not_symheap | WrongCmd -> [] in
  Rule.mk_infrule rl 

let axioms = 
  ref (Rule.first [symex_check_axiom entails; symex_empty_axiom; subformula_axiom])
      
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
  let () = check_entail := (Slprover.idfs 1 10 !Sl_rules.axioms !Sl_rules.rules) in
  (* Program.set_local_vars seq_to_prove ; *)
  rules := Rule.first [
	       lhs_disj_to_symheaps ;
	       simplify ;
	       Rule.choice [
		   dobackl;
		   Rule.compose_pairwise unfold_gs [Rule.attempt !axioms; (Rule.first [symex;symex_empty_axiom])];
		   unfold_fs;
		   (* Rule.choice  *)
		   (*   (Blist.map  *)
		   (* 	(fun c -> Rule.compose (fold c) dobackl)  *)
		   (* 	(Sl_defs.to_list defs)); *)
		   (* Rule.choice  *)
		   (*   (Blist.map  *)
		   (* 	(fun c -> Rule.compose (fold c) symex)  *)
		   (* 	(Sl_defs.to_list defs));		    *)
		   symex;
		   (* new_backl_cut; *)
		   (Rule.compose (luf defs) (Rule.attempt ex_falso_axiom));
		   disjunction_rule;
		   conjunction_rule;
		 ];
	     ]
		      
