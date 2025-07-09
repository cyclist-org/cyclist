open Lib
open Generic

open
  (Seplog
    : sig include module type of Seplog end
        with module Seq := Seplog.Seq
         and module Form := Seplog.Form)

(* exception Not_symheap = Seplog.Form.Not_symheap *)

module Program = While.Program

module Rule = Proofrule.Make(Seq)
module Seqtactics = Seqtactics.Make(Seq)
module Proof = Proof.Make(Seq)
module Slprover = Prover.Make(Seplog.Seq)

let use_cut = ref true

let check_entail =
  ref ((fun _ -> None) : Slprover.Seq.t -> Slprover.Proof.t option)

let entails f f' =
  let () =
    debug
      (fun () ->
        "Trying to prove entailment: " ^ (Seplog.Seq.to_string (f, f'))) in
  !check_entail (f, f')

let tagpairs s =
  Seq.tag_pairs s

(* following is for symex only *)
(* TODO: review this - I don't think this can be right [RR] *)
let progpairs s =
  Seq.tag_pairs s

let dest_sh_seq (sf,cmd,tf) = (Seplog.Form.dest sf, cmd, tf)

let to_sl_form hs = Ord_constraints.empty, hs

(* axioms *)
let subformula_axiom =
  Rule.mk_axiom
    (fun (sf,_,tf) ->
     match Form.is_atom tf with
     | true -> let tf_heap = Form.dest_atom tf in
	       let (_,_,ptos,inds) = Heap.dest tf_heap in
	       (Option.mk (Ptos.is_empty ptos &&
			     Tpreds.is_empty inds &&
			       not (Heap.is_empty tf_heap) &&
				 Seplog.Form.subsumed ~total:false (to_sl_form [tf_heap]) sf) "Sub-Check")
     | false -> None)

let ex_falso_axiom =
  Rule.mk_axiom (fun (sf,_,_) -> Option.mk (Seplog.Form.inconsistent sf) "Ex Falso")

let symex_check_axiom entails =
  Rule.mk_axiom
    (fun (pre,_,tf) ->
      let f = Seplog.Form.complete_tags Tags.empty (Form.extract_checkable_slformula tf) in
      Option.mk
      (Form.is_checkable tf && Option.is_some (entails pre f))
      "Check")

let symex_empty_axiom =
  Rule.mk_axiom
    (fun (_,cmd,tf) ->
      Option.mk (Program.Cmd.is_empty cmd && Form.is_box tf) "Empty")

(* simplification rules *)
let eq_subst_ex_f ((sf,cmd,tf) as s) =
  let sf' = Seplog.Form.subst_existentials sf in
  if Seplog.Form.equal sf sf' then [] else
    [ [ ((sf', cmd, tf), Seq.tag_pairs s, Tagpairs.empty) ], "Eq. subst. ex" ]

let final_axiom =
  Rule.mk_axiom
    (fun (_,cmd,tf) ->
      Option.mk (Program.Cmd.is_empty cmd && Form.is_final tf) "Final")

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
    let t =
      if Program.Cmd.is_load cmd then
        let (t,_,_) = Program.Cmd.dest_load cmd in
        t
      else
        let (t,_,_) = Program.Cmd.dest_store cmd in
        t in
    let () = debug (fun _ -> "Term in cmd for luf: " ^ (Term.to_string t)) in
    if (Blist.exists Option.is_some (Blist.map (fun var -> Heap.find_lval var h) [t])) then
      let () = debug (fun _ -> "Already exists ") in
      []
    else
      let () = debug (fun _ -> "Does NOT exists ") in
      let seq_vars = Seq.vars seq in
      let seq_tags = Seq.tags seq in
      let left_unfold ((t, (ident, _)) as p) =
        let h' = Heap.del_ind h p in
        let clauses = Defs.unfold (seq_vars, seq_tags) p defs in
        let do_case body =
          let tag_subst = Tagpairs.mk_free_subst seq_tags (Heap.tags body) in
          let body = Heap.subst_tags tag_subst body in
          let h' = Heap.star h' body in
          let progpairs =
            if !Seq.termination
              then Tagpairs.map (fun (_, t') -> (t, t')) tag_subst
              else Tagpairs.empty in
          let allpairs =
            Tagpairs.union
              (Tagpairs.remove (t,t) (Seq.tag_pairs seq))
              (progpairs) in
          ( ((cs,[h']),cmd,tf),
        	  allpairs,
        	  (if !Seq.termination then progpairs else Tagpairs.empty)
        	) in
        Blist.map do_case clauses, ((Predsym.to_string ident) ^ " L.Unf.") in
      Tpreds.map_to_list
        left_unfold
        (Tpreds.filter (Defs.is_defined defs) h.Heap.inds)
  with Program.WrongCmd | Seplog.Form.Not_symheap -> []

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
      let cont = Program.Cmd.get_cont cmd in
      Option.dest
        []
        (fun tf' ->
          fix_tps
            (Blist.map
              (fun (g,d) ->
                Blist.map (fun h' -> ((Seplog.Form.with_heaps pre [h']), cont, tf')) g, d)
              (f seq)))
        (if Form.is_diamond tf then Some (Form.e_step tf)
          else if Form.is_box tf then Some (Form.a_step tf)
          else None)
    with Program.WrongCmd -> []
  in wrap rl

(* symbolic execution rules *)
let symex_assign_rule =
  let rl seq =
    try
      let ((_,h),cmd,_) = dest_sh_seq seq in
      let (x,e) = Program.Cmd.dest_assign cmd in
      let fv = Program.fresh_evar (Heap.vars h) in
      let theta = Subst.singleton x fv in
      let h' = Heap.subst theta h in
      let e' = Subst.apply theta e in
      [[ Heap.add_eq h' (e',x) ], "Assign"]
    with Program.WrongCmd | Seplog.Form.Not_symheap -> [] in
  mk_symex rl

let find_pto_on f e =
	Ptos.find_suchthat (fun (l,_) -> Heap.equates f e l) f.Heap.ptos

let symex_load_rule =
  let rl seq =
    try
      let ((_,h),cmd,_) = dest_sh_seq seq in
      let (x,e,s) = Program.Cmd.dest_load cmd in
      let (_,ys) = find_pto_on h e in
      let t = Blist.nth ys (Program.Field.get_index s) in
      let fv = Program.fresh_evar (Heap.vars h) in
      let theta = Subst.singleton x fv in
      let h' = Heap.subst theta h in
      let t' = Subst.apply theta t in
      [[ Heap.add_eq h' (t',x) ], "Load"]
    with Seplog.Form.Not_symheap | Program.WrongCmd | Not_found -> [] in
  mk_symex rl

let symex_store_rule =
  let rl seq =
    try
      let ((_,h),cmd,_) = dest_sh_seq seq in
      let (x,s,e) = Program.Cmd.dest_store cmd in
      let ((x',ys) as pto) = find_pto_on h x in
      let pto' = (x', Blist.replace_nth e (Program.Field.get_index s) ys) in
      [[ Heap.add_pto (Heap.del_pto h pto) pto' ], "Store"]
    with Seplog.Form.Not_symheap | Program.WrongCmd | Not_found -> [] in
  mk_symex rl

let symex_free_rule =
  let rl seq =
    try
      let ((_,h),cmd,_) = dest_sh_seq seq in
      let e = Program.Cmd.dest_free cmd in
      let pto = find_pto_on h e in
      [[ Heap.del_pto h pto ], "Free"]
    with Seplog.Form.Not_symheap | Program.WrongCmd | Not_found -> [] in
  mk_symex rl

let symex_new_rule =
  let rl seq =
    try
      let ((_,h),cmd,_) = dest_sh_seq seq in
      let x = Program.Cmd.dest_new cmd in
      let l = Program.fresh_evars (Heap.vars h) (1 + (Program.Field.get_no_fields ())) in
      let (fv,fvs) = (Blist.hd l, Blist.tl l) in
      let h' = Heap.subst (Subst.singleton x fv) h in
      let h'' = Heap.mk_pto (x, fvs) in
      [[ Heap.star h' h'' ], "New"]
    with Seplog.Form.Not_symheap | Program.WrongCmd-> [] in
  mk_symex rl

let symex_skip_rule =
  let rl seq =
    try
      let ((_,h),cmd,_) = dest_sh_seq seq in
      let () = Program.Cmd.dest_skip cmd in [[h], "Skip"]
    with Seplog.Form.Not_symheap | Program.WrongCmd -> [] in
  mk_symex rl

let symex_ifelse_rule =
  let rl seq =
    try
      let ((cs,h),cmd,tf) = dest_sh_seq seq in
      let (c,cmd1,cmd2) = Program.Cmd.dest_ifelse cmd in
      let cont = Program.Cmd.get_cont cmd in
      let (h',h'') = Program.Cond.fork h c in
      if Form.is_box tf then
        let tf' = Form.a_step tf in
        fix_tps
          [ [ ((cs,[h']), Program.Cmd.mk_seq cmd1 cont, tf') ; ((cs,[h'']), Program.Cmd.mk_seq cmd2 cont,tf') ], "If-[]" ]
      else if Form.is_diamond tf then
        let tf' = Form.e_step tf in
        if Program.Cond.is_non_det c then
          fix_tps
            [ [ ((cs,[h']), Program.Cmd.mk_seq cmd1 cont, tf')], "If-<>1" ;
              [ ((cs,[h'']), Program.Cmd.mk_seq cmd2 cont, tf')], "If-<>2"  ]
        else if Program.Cond.validated_by h c then
          fix_tps [[ ((cs,[h']), Program.Cmd.mk_seq cmd1 cont, tf')], "If-<>1"]
        else
          fix_tps [[ ((cs,[h'']), Program.Cmd.mk_seq cmd2 cont, tf')], "If-<>2"]
      else
        []
    with Seplog.Form.Not_symheap | Program.WrongCmd -> [] in
  wrap rl

let symex_while_rule =
  let rl seq =
    try
      let ((cs,h),cmd,tf) = dest_sh_seq seq in
      let (c,cmd') = Program.Cmd.dest_while cmd in
      let cont = Program.Cmd.get_cont cmd in
      let (h',h'') = Program.Cond.fork h c in
      if Form.is_box tf then
        let tf' = Form.a_step tf in
        fix_tps
          [[ ((cs,[h']), Program.Cmd.mk_seq cmd' cmd, tf') ; ((cs,[h'']), cont, tf') ], "While-Box"]
      else if Form.is_diamond tf then
        let tf' = Form.e_step tf in
        if Program.Cond.is_non_det c then
          fix_tps
            [ [ ((cs,[h']), Program.Cmd.mk_seq cmd' cmd, tf')], "While-<>1" ;
              [ ((cs,[h'']), cont, tf')], "While-<>2" ]
        else if Program.Cond.validated_by h c then
          fix_tps [[ ((cs,[h']), Program.Cmd.mk_seq cmd' cmd, tf')], "While-<>1"]
        else
          fix_tps [[ ((cs,[h'']), cont, tf')], "While-<>2"]
      else
        []
    with Seplog.Form.Not_symheap | Program.WrongCmd -> [] in
  wrap rl

(* NB. This only returns the first matching it finds - there may be others which  *)
(*     perhaps will make a difference to the cut entailment that is tried in the  *)
(*     matches function below! *)
let match_inds h h' =
  let h_inds = h.Heap.inds in
  let h_inds' = h'.Heap.inds in
  let rec _match_inds inds inds' acc =
    if Tpreds.is_empty inds then
      let rest = Tpreds.fold (fun (t, _) -> Tagpairs.add (t, Tags.anonymous)) inds' Tagpairs.empty in
      Tagpairs.union acc rest
    else
      let ((t, (_, ts)) as p) = Tpreds.choose inds in
      assert (not (Tags.is_anonymous t));
      let ps = Tpreds.remove p inds in
      Option.dest_lazily
        (fun () -> _match_inds ps inds' acc)
        (fun ((t', _) as p') ->
          let ps' = Tpreds.remove p' inds' in
          let acc = Tagpairs.add (t', t) acc in
          _match_inds ps ps' acc)
        (Option.flatten (Option.mk_lazily
          ((Tags.is_free_var t) && Blist.for_all (Fun.neg Term.is_exist_var) ts)
          (fun () ->
            Tpreds.find_suchthat_opt
              (fun ((t', _) as p') -> Tags.is_free_var t' && Tpred.equal_upto_tags p p')
              inds'))) in
  _match_inds h_inds h_inds' Tagpairs.empty

let matches ((sf,cmd,tf) as seq) ((sf',cmd',tf') as seq') =
  try
    (* let () = print_endline "At matches with seqs:" in   *)
    (* let () = print_endline (Seq.to_string seq) in   *)
    (* let () = print_endline (Seq.to_string seq') in   *)
    if not (Program.Cmd.equal cmd cmd' && Form.equal tf tf' && Program.Cmd.is_while cmd
	    (* && (Form.is_ag tf || Form.is_eg tf) *)
	    (* && (Form.is_ag tf' || Form.is_eg tf') *)) then
      (* let () = print_endline "Seqs are not equal" in   *)
      []
    else
      let ((cs,h),(cs',h')) = Pair.map Seplog.Form.dest (sf,sf') in
      let res = Unify.Unidirectional.realize (
        Unification.backtrack
          (Heap.unify_partial
            ~update_check:(Fun.conj
              (Unify.Unidirectional.trm_check)
              (Unify.Unidirectional.avoid_replacing_trms !Program.program_vars)))
            h' h
          (Unify.Unidirectional.unify_tag_constraints
            cs cs'
          (Unify.Unidirectional.mk_verifier
            (Unify.Unidirectional.mk_assert_check
              (fun (theta, tagpairs) ->
                let subst_seq = (Seq.subst_tags tagpairs (Seq.subst theta seq')) in
                let () = debug (fun _ -> "term substitution: " ^ ((Format.asprintf " %a" Subst.pp theta))) in
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
          let et = Tags.filter Tags.is_exist_var (Seplog.Form.tags sf') in
          let ft = Tags.filter Tags.is_free_var (Tagpairs.projectl ts') in
          let theta = Tagpairs.union ts (Tagpairs.mk_ex_subst (Tags.union et (Seplog.Form.tags sf)) ft) in
          let sf' = Seplog.Form.subst_tags theta sf' in
          let result = entails sf sf' in
          let () = debug (fun () -> " CUTLINK3 result: " ^ (string_of_bool (Option.is_some result))) in
          if Option.is_some result then
            [(Subst.empty, theta)]
          else
            []
        else
          res in
      let temporal_tag =
        if Form.is_ag tf then
          Some (Pair.map (fun tf -> Pair.left (Form.dest_ag tf)) (tf, tf'))
        else if Form.is_eg tf then
          Some (Pair.map (fun tf -> Pair.left (Form.dest_eg tf)) (tf, tf'))
        else None in
      Option.dest
        (res)
        (fun (t, t') ->
          assert (Tags.Elt.equal t t');
          Blist.map (Pair.map_right (Tagpairs.add (t, t'))) res)
        (temporal_tag)
  with Seplog.Form.Not_symheap -> []

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
  if !use_cut then
    [ [(seq',
        Tagpairs.union (Tagpairs.mk (Form.outermost_tag tf1)) (Tagpairs.mk (Tags.inter (Seq.tags seq) (Seq.tags seq')))
	(* Tagpairs.mk (Tags.inter (Seq.tags seq) (Seq.tags seq')) *) (*Seq.tag_pairs seq*),
       Tagpairs.empty (* Tagpairs.mk (Tags.inter (Seq.tags seq) (Seq.tags seq')) *) (*Seq.tag_pairs seq*))], "Cut" ]
  else
    []

let unfold_ag_rule =
  let rl seq =
    try
      let ((cs,h),cmd,tf) = dest_sh_seq seq in
      let (tf1,tf2) = Form.unfold_ag tf in
        fix_tps
          [[((cs,[h]),cmd,tf1); ((cs,[h]),cmd,tf2)], "UnfoldAG"]
    with Seplog.Form.Not_symheap | Invalid_argument(_) -> [] in
  wrap rl

let unfold_eg_rule =
  let rl seq =
    try
      let ((cs,h),cmd,tf) = dest_sh_seq seq in
      let (tf1,tf2) = Form.unfold_eg tf in
      fix_ts
        [[((cs,[h]),cmd,tf1) ; ((cs,[h]),cmd,tf2)], "UnfoldEG"]
    with Seplog.Form.Not_symheap | Invalid_argument(_) -> [] in
  wrap rl

let unfold_af_rule =
  let rl seq =
    try
      let ((cs,h),cmd,tf) = dest_sh_seq seq in
      let (tf1,tf2) = Form.unfold_af tf in
      fix_tps
        [[((cs,[h]),cmd,tf1)], "UnfoldAF" ; [((cs,[h]),cmd,tf2)], "UnfoldAF"]
    with Seplog.Form.Not_symheap | Invalid_argument(_) -> [] in
  wrap rl

let unfold_ef_rule =
  let rl seq =
    try
      let ((cs,h),cmd,tf) = dest_sh_seq seq in
      let (tf1,tf2) = Form.unfold_ef tf in
      fix_tps
        [[((cs,[h]),cmd,tf1)], "UnfoldEF" ; [((cs,[h]),cmd,tf2)], "UnfoldEF"]
    with Seplog.Form.Not_symheap | Invalid_argument(_) -> [] in
  wrap rl

let disjunction_rule =
  let rl seq =
    try
      let ((cs,h),cmd,tf) = dest_sh_seq seq in
      let (tf1,tf2) = Form.unfold_or tf in
      fix_tps
        [[((cs,[h]),cmd,tf1)], "Disj1" ; [((cs,[h]),cmd,tf2)], "Disj2"]
    with Seplog.Form.Not_symheap | Invalid_argument(_) -> [] in
  wrap rl

let conjunction_rule =
  let rl seq =
    try
      let ((cs,h),cmd,tf) = dest_sh_seq seq in
      let (tf1,tf2) = Form.unfold_and tf in
      fix_tps
        [[((cs,[h]),cmd,tf1); ((cs,[h]),cmd,tf2)], "Conj"]
    with Seplog.Form.Not_symheap | Invalid_argument(_) -> [] in
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
    let targ_seq' = (Seplog.Form.subst_tags tagpairs sf_targ_seq, cmd_targ_seq, tf_targ_seq) in
    let subst_seq = Seq.subst theta targ_seq' in
    Rule.sequence [
        if Seq.equal src_seq subst_seq then
          Rule.identity
        else if Seq.subsumed src_seq targ_seq then
          Rule.mk_infrule (frame subst_seq)
        else
          Rule.mk_infrule (cut subst_seq);

        if Term.Map.for_all Term.equal theta
        then Rule.identity
        else Rule.mk_infrule (subst_rule theta targ_seq');

        Rule.mk_backrule
          false
          (fun _ _ -> [targ_idx])
          (fun (_,_,tf) s' ->
            (* [(if !Seq.termination then Tagpairs.empty else Seq.tagpairs_one), "Backl"]) *)
            [(if !Seq.termination then Tagpairs.reflect tagpairs else Tagpairs.mk (Form.outermost_tag tf)), "Backl"])
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
      if Tpreds.is_empty h.Heap.inds then [] else
      let tags = Seq.tags seq in
      let do_case case =
        let (f,(ident,vs)) = Indrule.dest case in
        let results = Indrule.fold case h in
        let process (theta, h') =
          let seq' = ((cs,[h']),cmd,tf) in
          (* let () = print_endline "Fold match:" in *)
          (* let () = print_endline (Seq.to_string seq) in *)
          (* let () = print_endline (Heap.to_string f) in *)
          (* let () = print_endline (Seq.to_string seq') in *)
            [(
              seq',
              (* Tagpairs.empty *) Tagpairs.mk (Tags.inter tags (Seq.tags seq')),
              Tagpairs.empty
            )], ((Predsym.to_string ident) ^ " Fold")  in
        Blist.map process results in
      Blist.bind do_case (Preddef.rules def)
    with Seplog.Form.Not_symheap -> [] in
  Rule.mk_infrule fold_rl

let generalise_while_rule =
  let generalise m h =
    let avoid = ref (Heap.vars h) in
    let gen_term t =
    if Term.Set.mem t m then
      (let r = Program.fresh_evar !avoid in avoid := Term.Set.add r !avoid ; r)
    else t in
    let gen_pto (x,args) =
    let l = Blist.map gen_term (x::args) in (Blist.hd l, Blist.tl l) in
      Heap.mk
        (Term.Set.fold Uf.remove m h.Heap.eqs)
        (Deqs.filter
          (fun p -> Pair.conj (Pair.map (fun z -> not (Term.Set.mem z m)) p))
          h.Heap.deqs)
        (Ptos.map gen_pto h.Heap.ptos)
        h.Heap.inds in
    let rl seq =
      try
        let ((cs,h),cmd,tf) = dest_sh_seq seq in
        let (_,cmd') = Program.Cmd.dest_while cmd in
        let m = Term.Set.inter (Program.Cmd.modifies cmd') (Heap.vars h) in
        let subs = Term.Set.subsets m in
        Option.list_get (Blist.map
          begin fun m' ->
            let h' = generalise m' h in
            if Heap.equal h h' then None else
            let s' = ((cs,[h']),cmd,tf) in
            Some ([ (s', tagpairs s', Tagpairs.empty) ], "Gen.While")
          end
          subs)
    with Seplog.Form.Not_symheap | Program.WrongCmd -> [] in
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
  let () = Rules.setup defs in
  let () = check_entail := (Slprover.idfs 1 10 !Rules.axioms !Rules.rules) in
  (* Program.set_local_vars seq_to_prove ; *)
  rules :=
    Rule.first [
      lhs_disj_to_symheaps ;
      simplify ;
      Rule.choice [
        dobackl;
        Rule.compose_pairwise unfold_gs [Rule.attempt !axioms; (Rule.first [symex;symex_empty_axiom])];
        unfold_fs;
        (* Rule.choice  *)
        (*   (Blist.map  *)
        (* 	(fun c -> Rule.compose (fold c) dobackl)  *)
        (* 	(Defs.to_list defs)); *)
        (* Rule.choice  *)
        (*   (Blist.map  *)
        (* 	(fun c -> Rule.compose (fold c) symex)  *)
        (* 	(Defs.to_list defs));		    *)
        symex;
        (* new_backl_cut; *)
        (Rule.compose (luf defs) (Rule.attempt ex_falso_axiom));
        disjunction_rule;
        conjunction_rule;
      ];
    ]
