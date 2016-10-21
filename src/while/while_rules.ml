open Lib

open While_program

module SH = Sl_heap
exception Not_symheap = Sl_form.Not_symheap

module Rule = Proofrule.Make(While_program.Seq)
module Seqtactics = Seqtactics.Make(While_program.Seq)
module Proof = Proof.Make(While_program.Seq)

let tagpairs s =
	if !termination then Seq.tag_pairs s else Seq.tagpairs_one

(* following is for symex only *)
let progpairs () = 
	if !termination then Tagpairs.empty else Seq.tagpairs_one

let dest_sh_seq (l,cmd) = (Sl_form.dest l, cmd)


(* axioms *)
let ex_falso_axiom = 
  Rule.mk_axiom (fun (f,_) -> Option.mk (Sl_form.inconsistent f) "Ex Falso")

let symex_stop_axiom =
  Rule.mk_axiom (fun (_,cmd) -> Option.mk (Cmd.is_stop cmd || Cmd.is_return cmd) "Stop")

let symex_empty_axiom =
  Rule.mk_axiom (fun (_,cmd) -> Option.mk (Cmd.is_empty cmd) "Empty")

(* simplification rules *)
let eq_subst_ex_f ((l,cmd) as s) =
  let l' = Sl_form.subst_existentials l in
  if Sl_form.equal l l' then [] else
  [ [ ((l', cmd), tagpairs s, Tagpairs.empty) ], "Eq. subst. ex" ]

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
  let rl ((cs,hs),cmd) =
    if Blist.length hs < 2 then [] else
    [ Blist.map 
        (fun h -> let s' = ((cs,[h]),cmd) in (s', tagpairs s', Tagpairs.empty ) ) 
        hs,
      "L.Or"
    ] in
  Rule.mk_infrule rl

let luf_rl seq defs =
  try
    let ((cs,h),cmd) = dest_sh_seq seq in
    let seq_vars = Seq.vars seq in
    let seq_tags = Tags.union (Ord_constraints.tags cs) (Sl_heap.tags h) in
    let left_unfold ((t, (ident, _)) as p) = 
      let h' = SH.del_ind h p in
      let clauses = Sl_defs.unfold (seq_vars, seq_tags) p defs in
      let do_case body =
        let tag_subst = Tagpairs.mk_free_subst seq_tags (Sl_heap.tags body) in
        let body = Sl_heap.subst_tags tag_subst body in
        let h' = Sl_heap.star h' body in
        let progpairs = Tagpairs.endomap (fun (_, t') -> (t, t')) tag_subst in
        let allpairs = 
          Tagpairs.union
            (Tagpairs.remove (t,t) (Seq.tag_pairs seq))
            (progpairs) in
        ( 
          ((cs,[h']),cmd), 
          (if !termination then allpairs else Seq.tagpairs_one), 
          (if !termination then progpairs else Tagpairs.empty)
        ) in
      Blist.map do_case clauses, ((Sl_predsym.to_string ident) ^ " L.Unf.") in
    Sl_tpreds.map_to_list 
      left_unfold 
      (Sl_tpreds.filter (Sl_defs.is_defined defs) h.SH.inds)
  with Not_symheap -> []

let luf defs = wrap (fun seq -> luf_rl seq defs)
  
(* FOR SYMEX ONLY *)
let fix_tps l = 
  Blist.map 
    (fun (g,d) -> Blist.map (fun s -> (s, tagpairs s, progpairs () )) g, d) l 

let mk_symex f = 
  let rl ((pre,cmd) as seq) =
    let cont = Cmd.get_cont cmd in
    fix_tps 
      (Blist.map (fun (g,d) -> Blist.map (fun h' -> ((Sl_form.with_heaps pre [h']), cont)) g, d) (f seq)) in
  wrap rl
  
(* symbolic execution rules *)
let symex_assign_rule =
  let rl seq =
    try
      let ((_,h),cmd) = dest_sh_seq seq in
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
      let ((_,h),cmd) = dest_sh_seq seq in
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
      let ((_,h),cmd) = dest_sh_seq seq in
      let (x,s,e) = Cmd.dest_store cmd in
      let ((x',ys) as pto) = find_pto_on h x in
      let pto' = (x', Blist.replace_nth e (Field.get_index s) ys) in
      [[ SH.add_pto (SH.del_pto h pto) pto' ], "Store"]
    with Not_symheap | WrongCmd | Not_found -> [] in
  mk_symex rl

let symex_free_rule =
  let rl seq =
    try
      let ((_,h),cmd) = dest_sh_seq seq in
      let e = Cmd.dest_free cmd in
      let pto = find_pto_on h e in
      [[ SH.del_pto h pto ], "Free"]
    with Not_symheap | WrongCmd | Not_found -> [] in
  mk_symex rl

let symex_new_rule =
  let rl seq =
    try
      let ((_,h),cmd) = dest_sh_seq seq in
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
      let ((_,h),cmd) = dest_sh_seq seq in 
      let () = Cmd.dest_skip cmd in [[h], "Skip"]
    with Not_symheap | WrongCmd -> [] in
  mk_symex rl

let symex_if_rule =
  let rl seq =
    try
      let ((cs,h),cmd) = dest_sh_seq seq in
      let (c,cmd') = Cmd.dest_if cmd in
      let cont = Cmd.get_cont cmd in
      let (h',h'') = Cond.fork h c in 
      fix_tps [[ ((cs,[h']), Cmd.mk_seq cmd' cont) ; ((cs,[h'']), cont) ], "If"]
    with Not_symheap | WrongCmd -> [] in
  wrap rl

let symex_ifelse_rule =
  let rl seq =
    try
      let ((cs,h),cmd) = dest_sh_seq seq in
      let (c,cmd1,cmd2) = Cmd.dest_ifelse cmd in
      let cont = Cmd.get_cont cmd in
      let (h',h'') = Cond.fork h c in 
      fix_tps 
        [[ ((cs,[h']), Cmd.mk_seq cmd1 cont) ; ((cs,[h'']), Cmd.mk_seq cmd2 cont) ],
         "IfElse"
        ]
    with Not_symheap | WrongCmd -> [] in
  wrap rl

let symex_while_rule =
  let rl seq =
    try
      let ((cs,h),cmd) = dest_sh_seq seq in
      let (c,cmd') = Cmd.dest_while cmd in
      let cont = Cmd.get_cont cmd in
      let (h',h'') = Cond.fork h c in 
      fix_tps [[ ((cs,[h']), Cmd.mk_seq cmd' cmd) ; ((cs,[h'']), cont) ], "While"]
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
let matches ((f,cmd) as seq) ((f',cmd') as seq') =
  try
    if not (Cmd.equal cmd cmd') then [] else
    let ((cs,h),(cs',h')) = Pair.map Sl_form.dest (f,f') in
    Sl_unify.Unidirectional.realize (
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
              Seq.subsumed seq subst_seq)))))
  with Not_symheap -> []

(*    seq'     *)
(* ----------  *)
(* seq'[theta] *)
(* where seq'[theta] = seq *)
let subst_rule theta seq' seq = 
  if Seq.equal (Seq.subst theta seq') seq 
    then 
      [ [(seq', Seq.tag_pairs seq', Tagpairs.empty)], 
       "Subst "  (* ^ (Format.asprintf "%a" Sl_subst.pp theta) *) ]
    else 
      []

let frame seq' seq = 
  if Seq.subsumed seq seq' then
    [ [(seq', Seq.tag_pairs seq', Tagpairs.empty)], "Frame" ]
  else
    []

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
    let f (targ_idx, (theta, tagpairs)) =
      let targ_seq = Proof.get_seq targ_idx prf in
      (* [targ_seq'] is as [targ_seq] but with the tags of [src_seq] *)
      let targ_seq' = 
        (Sl_form.subst_tags tagpairs (fst targ_seq), 
        snd targ_seq) in 
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
            [(if !termination then Tagpairs.reflect tagpairs else Seq.tagpairs_one), "Backl"])
      ] in
    Rule.first (Blist.map f apps) idx prf

let fold def =
  let fold_rl seq = 
    try 
      let ((cs,h),cmd) = dest_sh_seq seq in
      if Sl_tpreds.is_empty h.SH.inds then [] else
      let tags = Seq.tags seq in
      let do_case case =
        let (f,(ident,vs)) = Sl_indrule.dest case in
        let results = Sl_indrule.fold case h in
        let process (theta, h') = 
          let seq' = ((cs,[h']),cmd) in
          (* let () = print_endline "Fold match:" in         *)
          (* let () = print_endline (Seq.to_string seq) in   *)
          (* let () = print_endline (Sl_heap.to_string f) in *)
          (* let () = print_endline (Seq.to_string seq') in  *)
          let allpairs =
            if !termination then Tagpairs.mk (Tags.inter tags (Seq.tags seq')) else Seq.tagpairs_one in 
            ( [(seq', allpairs, Tagpairs.empty )], 
                ((Sl_predsym.to_string ident) ^ " Fold") ) in
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
        let ((cs,h),cmd) = dest_sh_seq seq in
        let (_,cmd') = Cmd.dest_while cmd in
        let m = Sl_term.Set.inter (Cmd.modifies cmd') (Sl_heap.vars h) in
        let subs = Sl_term.Set.subsets m in
        Option.list_get (Blist.map
          begin fun m' ->
            let h' = generalise m' h in
            if Sl_heap.equal h h' then None else
            let s' = ((cs,[h']), cmd) in
            Some ([ (s', tagpairs s', Tagpairs.empty) ], "Gen.While")
          end
          subs)
    with Not_symheap | WrongCmd -> [] in
  Rule.mk_infrule rl 

module Slprover = Prover.Make(Sl_seq)

let backlink_cut defs =
  let rl s1 s2 =
    if !termination then [] else
    (* let () = incr step in *)
    let ((l1,cmd1),(l2,cmd2)) = (s1,s2) in
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
      Option.is_some (Slprover.idfs 1 11 !Sl_rules.axioms !Sl_rules.rules (l1, l2)) in
    (* let () = Lib.do_debug := olddebug in *)
    (* let () = debug (fun () -> "CUTLINK3: result: " ^ (string_of_bool result)) in *)
    if result then [ (Seq.tagpairs_one, "Cut/Backl") ] else [] in
  Rule.mk_backrule true Rule.all_nodes rl


let axioms = 
  ref (Rule.first [ex_falso_axiom ; symex_stop_axiom; symex_empty_axiom])

let rules = ref Rule.fail

let symex =       
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
  ] 

let setup defs =
  (* Program.set_local_vars seq_to_prove ; *)
  rules := Rule.first [ 
    lhs_disj_to_symheaps ;
    simplify ;
    
    Rule.choice [
      dobackl ;
      Rule.choice 
        (Blist.map 
          (fun c -> Rule.compose (fold c) dobackl) 
          (Sl_defs.to_list defs));
      
      symex;      
      generalise_while_rule ;
      (* backlink_cut defs; *)
      
      luf defs;
    ]
  ]
