open Lib
open Util
open Extended_while_program

module SH = Sl_heap

exception Not_symheap = Sl_form.Not_symheap

module Rule = Proofrule.Make(Extended_while_program.Seq)
module Seqtactics = Seqtactics.Make(Extended_while_program.Seq)
module Proof = Proof.Make(Extended_while_program.Seq)
module Slprover = Prover.Make(Sl_seq)

let tagpairs = Seq.tag_pairs

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
(* let norm ((pre ,cmd, post) as s) =                                *)
(*   let pre' = Sl_form.norm pre in                                  *)
(*   if Sl_form.equal pre pre' then [] else                          *)
(*   [ [( (pre', cmd, post), tagpairs s, TagPairs.empty)], "Norm" ]  *)

let simplify_rules = [ eq_subst_ex_f ]

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

(* let gen_left_rules_f (def, ident) seq =                                                *)
(*   try                                                                                  *)
(*     let (pre, cmd, post) = dest_sh_seq seq in                                          *)
(*     let preds =                                                                        *)
(*       Sl_tpreds.filter (fun (_,(ident',_)) -> Strng.equal ident ident') pre.SH.inds in *)
(*     if Sl_tpreds.is_empty preds then [] else                                           *)
(*     let left_unfold ((id,(_,pvs)) as p) =                                              *)
(*       let ts = Tags.inter (Sl_heap.tags pre) (Sl_heap.tags pre) in                     *)
(*       let pre' = SH.del_ind pre p in                                                   *)
(*       let do_case case =                                                               *)
(*         let n = Blist.length (Sl_indrule.formals case) in                              *)
(*         let n' = Blist.length pvs in                                                   *)
(*         let err_msg = fun () ->                                                        *)
(*           (Printf.sprintf                                                              *)
(*             "Skipping unfolding of inductive predicate \"%s\" \                        *)
(*             due to parameter mismatch: definition expects %d parameters, \             *)
(*             but was given %d" ident n n')  in                                          *)
(*         Option.mk_lazily (n == n' || (debug err_msg; false))                           *)
(*         (fun () ->                                                                     *)
(*           let (f', (_,vs')) = Sl_indrule.dest (freshen_case_by_seq seq case) in        *)
(*           let theta = Sl_term.Map.of_list (Blist.combine vs' pvs) in                   *)
(*           let f' = Sl_heap.subst theta f' in                                           *)
(*           let f' = Sl_heap.repl_tags id f' in                                          *)
(*           let pre' = Sl_heap.star pre' f' in                                           *)
(*           (                                                                            *)
(*             ([pre'], cmd, post),                                                       *)
(*             (if !termination then TagPairs.mk ts else Seq.tagpairs_one),               *)
(*             (if !termination then TagPairs.singleton (id,id) else TagPairs.empty)      *)
(*           )) in                                                                        *)
(*       let subgoals = Option.list_get (Blist.map do_case def) in                        *)
(*       Option.mk (not (Blist.is_empty subgoals)) (subgoals, (ident ^ " L.Unf.")) in     *)
(*     Option.list_get (Sl_tpreds.map_to_list left_unfold preds)                          *)
(*   with Not_symheap -> []                                                               *)
 
(* let gen_left_rules (def,ident) =                                                       *)
(*   wrap (gen_left_rules_f (def,ident))                                                  *)

let luf_rl ((pre,cmd,post) as seq) defs =
  try
    let pre = Sl_form.dest pre in
    let seq_vars = Seq.vars seq in
    let left_unfold ((_, (ident, _)) as p) = 
      let pre' = SH.del_ind pre p in
      let clauses = Sl_defs.unfold seq_vars pre' p defs in
      let do_case (f', tagpairs) =
        let pre' = Sl_heap.star pre' f' in
        ( 
          ([pre'],cmd,post), 
          (if !termination then tagpairs else Seq.tagpairs_one), 
          (if !termination then tagpairs else TagPairs.empty)
        ) in
      let () = debug (fun () -> "Unfolding " ^ (Sl_predsym.to_string ident)) in 
      Blist.map do_case clauses, ((Sl_predsym.to_string ident) ^ " L.Unf.") in
    Sl_tpreds.map_to_list 
      left_unfold 
      (Sl_tpreds.filter (Sl_defs.is_defined defs) pre.SH.inds)
  with Not_symheap -> []

let luf defs = wrap (fun seq -> luf_rl seq defs)


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
      let fv = fresh_evar (Seq.vars seq) in
      let theta = Sl_term.singleton_subst x fv in
      let pre' = Sl_heap.subst theta pre in
      let e' = Sl_term.subst theta e in
      [[ SH.add_eq pre' (e',x) ], "Assign"]
    with WrongCmd | Not_symheap -> [] in
  mk_symex rl

let find_pto_on f e = 
  Sl_ptos.find (fun (l,_) -> Sl_heap.equates f e l) f.SH.ptos
  
let symex_load_rule =
  let rl seq =
    try
      let (pre, cmd, _) = dest_sh_seq seq in
      let (x,e,f) = Cmd.dest_load cmd in
      let (_,ys) = find_pto_on pre e in
      let t = Blist.nth ys (Field.get_index f) in
      (* Does fv need to be fresh in the post condition too? *)
      let fv = fresh_evar (Seq.vars seq) in
      let theta = Sl_term.singleton_subst x fv in
      let pre' = Sl_heap.subst theta pre in
      let t' = Sl_term.subst theta t in
      [[ SH.add_eq pre' (t',x) ], "Load"]
    with Not_symheap | WrongCmd | Not_found -> [] in
  mk_symex rl

let symex_store_rule =
  let rl seq =
    try
      let (pre, cmd, _) = dest_sh_seq seq in
      let (x,f,e) = Cmd.dest_store cmd in
      let ((x',ys) as pto) = find_pto_on pre x in
      let pto' = (x', Blist.replace_nth e (Field.get_index f) ys) in
      [[ SH.add_pto (SH.del_pto pre pto) pto' ], "Store"]
    with Not_symheap | WrongCmd | Not_found -> [] in
  mk_symex rl

let symex_free_rule =
  let rl seq =
    try
      let (pre, cmd, _) = dest_sh_seq seq in
      let e = Cmd.dest_free cmd in
      let pto = find_pto_on pre e in
      [[ SH.del_pto pre pto ], "Free"]
    with Not_symheap | WrongCmd | Not_found -> [] in
  mk_symex rl

let symex_new_rule =
  let rl seq =
    try
      let (pre ,cmd, _) = dest_sh_seq seq in
      let x = Cmd.dest_new cmd in
      let l = fresh_evars (Seq.vars seq) (1 + (Field.get_no_fields ())) in
      let (fv,fvs) = (Blist.hd l, Blist.tl l) in
      let pre' = Sl_heap.subst (Sl_term.singleton_subst x fv) pre in
      let new_pto = Sl_heap.mk_pto (x, fvs) in
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
  
let mk_symex_proc_unfold procs = 
  let rl (pre, cmd, post) = 
    try
      let (p, args) = Cmd.dest_proc_call cmd in
      if Cmd.is_empty (Cmd.get_cont cmd) then
        let (_,_,_,_, body) = Blist.find
          (fun (id, params, pre', post', _) -> 
            try
            id=p 
              && Blist.equal Sl_term.equal args params
              (* NB. We only check equality up to tags as we will effectively be
                 'instantiating' the pre/post of the procedure summary using the
                 tags at the call site *)
              && Sl_form.equal_upto_tags pre pre'
              && Sl_form.equal_upto_tags post post'
            with Invalid_argument(_) -> false )
          (procs) in
        fix_tps
          [
            [ (pre, body, post) ],
            "Proc Unf. " ^ p
          ]
      else []
    with WrongCmd | Not_found -> [] in
  Rule.mk_infrule rl

let param_subst_rule theta ((_,cmd',_) as seq') ((_,cmd,_) as seq) =
  if Cmd.is_proc_call cmd && Cmd.is_empty (Cmd.get_cont cmd)
      && Cmd.is_proc_call cmd' && Cmd.is_empty (Cmd.get_cont cmd')
      && Seq.equal (Seq.param_subst theta seq') seq
    then 
      [ [(seq', Seq.tag_pairs seq', TagPairs.empty)], 
       "Param Subst"  (* ^ (Format.asprintf " %a" Sl_term.pp_subst theta) *) ]
    else 
      []

let subst_rule theta seq' seq = 
  if Seq.equal (Seq.subst theta seq') seq 
    then 
      [ [(seq', Seq.tag_pairs seq', TagPairs.empty)], 
       "Subst"  (* ^ (Format.asprintf " %a" Sl_term.pp_subst theta) *) ]
    else 
      []

let weaken seq' seq = 
  if Seq.subsumed seq seq' 
    then
      [ [(seq', Seq.tag_pairs seq', TagPairs.empty)],
       "Weaken" ]
    else
      []
    
let left_or_elim_rule ((pre',cmd',post') as seq') (pre,cmd,post) =
  if Cmd.equal cmd cmd' && Sl_form.equal post post'
      && Sl_form.is_symheap pre
      && Blist.exists (Sl_heap.equal (Sl_form.dest pre)) pre'
    then
      [ [(seq', Seq.tag_pairs seq', TagPairs.empty)],
       "L. Cut (Or Elim.)" ]
    else
      []
      
(* Rule specialising existential variables *)
let ex_sp_rule seq' seq =
  if true (* TO DO *)
    then
      [ [(seq', Seq.tag_pairs seq', TagPairs.empty)],
       "L. Cut (Ex. Sp.)" ]      
    else
      []
      
let proc_call_rule ((pre, cmd, post) as proc_seq) (pre', cmd', post') =
  try
    let pre = Sl_form.dest pre in
    let post = Sl_form.dest post in 
    let pre' = Sl_form.dest pre' in
    let frame = Sl_heap.compute_frame ~freshen_existentials:false pre pre' in
    if Cmd.is_proc_call cmd && Cmd.is_empty (Cmd.get_cont cmd)
        && not (Cmd.is_empty cmd') 
        && Cmd.cmd_equal (Cmd.get_cmd cmd) (Cmd.get_cmd cmd')
        && Option.is_some frame
      then
        let frame = Option.get frame in
        match (Cmd.get_cmd cmd) with 
          | Cmd.ProcCall(procname, _) ->
              [ [ (proc_seq, tagpairs proc_seq, TagPairs.empty) ;
                  (([Sl_heap.star (Sl_heap.freshen_tags frame post) frame], 
                    Cmd.get_cont cmd', 
                    post'), TagPairs.mk (Seq.form_tags [frame]), progpairs()) ;
                ], "Proc. Call " ^  procname]
          | _ -> assert(false)
      else
        []
  with Not_symheap -> []
      
let mk_symex_proc_call procs =
  fun idx prf ->
    let rl = 
      let ((pre, cmd, post) as src_seq) = Proof.get_seq idx prf in
      (*********************** 
         First compute a substitution of the procedure parameters that takes 
         them to the procedure arguments at the call site
         
         Next, apply the substitution to the procedure precondition and unify 
         the result with the source precondition
        
         This should allow us to compute the frame by subtracting the 
         subtstitution instance of the procedure precondition from the source 
         precondition 
      ***********************)
      try
        let pre = Sl_form.dest pre in
        let (p, args) = Cmd.dest_proc_call cmd in
        let prog_cont = Cmd.get_cont cmd in 
        let proc = Blist.find 
          (fun x -> 
            (Proc.get_name x) = p 
            && (Blist.length (Proc.get_params x)) = (Blist.length args))
          (procs) in
        let param_unifier = Sl_term.FList.unify 
          (Option.mk true) 
          (Sl_term.empty_subst, TagPairs.empty)
          (Proc.get_params proc) args in
        match param_unifier with
        | None -> assert false (* This should not happen *)
        | Some (param_sub, _) -> 
          let pre' = Sl_form.subst param_sub (Proc.get_precondition proc) in
          let mk_rl_from_disj f =
            let unifiers = 
              (* The continuation checks that the resulting substitution allows a frame to be computed *)
              let cont ((theta, tagpairs) as state) =
                Option.mk 
                  ((Sl_term.Map.for_all 
                    (fun x y -> 
                      Sl_term.equal x y || not (Sl_term.Set.mem x !program_vars)) 
                    theta)
                    &&
                  (Option.is_some (Sl_heap.compute_frame (Sl_heap.subst_tags tagpairs (Sl_heap.subst theta f)) pre)))
                  state in
              Sl_term.backtrack
                (Sl_heap.unify_partial ~tagpairs:true)
                cont
                (Sl_term.empty_subst, TagPairs.empty)
                f pre in
            let mk_rl (theta, tagpairs) =
              let target_seq = Seq.subst_tags tagpairs (Proc.get_seq proc) in
              let ((_,cmd',post') as seq_newparams) = Seq.param_subst param_sub target_seq in
              let seq_sh_pre = ([Sl_heap.subst_tags tagpairs f], cmd', post') in
              let ((pre',_,post') as subst_seq) = Seq.subst theta seq_sh_pre in
              let pre' = Sl_form.dest pre' in
              let frame = Sl_heap.compute_frame ~avoid:(Sl_form.vars post') pre' pre in
              match frame with
                | None -> assert false (* This should not happen *)
                | Some(frame) ->
                  let call_seq = ([Sl_heap.star pre' frame], cmd, post) in
                  (* Construct all the individual rules that need to be applied *)
                  let sp_ex_rl = 
                    if Seq.equal call_seq src_seq 
                      then Rule.identity
                      else Rule.mk_infrule (ex_sp_rule call_seq) in
                  let (proc_call_rl, cont_rl) = 
                    if (not (Cmd.is_empty prog_cont) || 
                        not (Sl_heap.is_empty frame) || 
                        not (Sl_form.equal post post')) 
                      then (Rule.mk_infrule (proc_call_rule subst_seq), [Rule.identity])
                      else (Rule.identity, []) in
                  let subst_rl = 
                    if Seq.equal seq_sh_pre subst_seq
                      then Rule.identity
                      else Rule.mk_infrule (subst_rule theta seq_sh_pre) in
                  let or_elim_rl = 
                    if Seq.equal seq_newparams seq_sh_pre
                      then Rule.identity
                      else Rule.mk_infrule (left_or_elim_rule seq_newparams) in
                  let param_rl = 
                    if Seq.equal src_seq seq_newparams
                      then Rule.identity
                      else Rule.mk_infrule (param_subst_rule param_sub target_seq) in
                  (* Now combine all the rules *)
                  Rule.compose
                    sp_ex_rl
                    (Rule.compose_pairwise
                      proc_call_rl
                      ((Rule.sequence [
                          subst_rl;
                          or_elim_rl;
                          param_rl;
                        ]) :: cont_rl)) in
            Blist.map mk_rl unifiers in
          Rule.choice (Blist.bind mk_rl_from_disj pre')
      with Not_symheap | WrongCmd | Not_found -> Rule.fail in
    rl idx prf

let matches ((pre,cmd,post) as seq) ((pre',cmd',post') as seq') =
  try
    if not (Cmd.equal cmd cmd') then [] else
    let (pre,pre') = Pair.map Sl_form.dest (pre,pre') in
    let (post,post') = Pair.map Sl_form.dest (post,post') in
    let exvars h = Sl_term.Set.filter Sl_term.is_exist_var (Sl_heap.vars h) in
    (* FIXME *)
    if not 
       (Sl_term.Set.is_empty (Sl_term.Set.inter (exvars pre) (exvars post))) 
        &&    
        Sl_term.Set.is_empty (Sl_term.Set.inter (exvars pre') (exvars post'))
    then [] else    
    let verify ((theta, tagpairs) as state) =
      if not ((if !termination then Seq.subsumed else Seq.subsumed_upto_tags) seq 
          ((if !termination then Seq.subst_tags tagpairs else Fun.id) 
            (Seq.subst theta seq'))) 
      then 
        begin 
          Format.eprintf "%a@." Seq.pp seq;
          Format.eprintf "%a@." Seq.pp seq';
          Format.eprintf "%a@." Sl_term.pp_subst theta;
          Format.eprintf "%a@." TagPairs.pp tagpairs ;
          assert false
        end ;
      Option.mk 
        (Sl_term.Map.for_all 
          (fun x y -> 
            Sl_term.equal x y || not (Sl_term.Set.mem x !program_vars)) 
          theta)
        state in
    let cont state = 
      Sl_heap.classical_unify ~inverse:true verify state post post' in
    Sl_term.backtrack 
      (Sl_heap.classical_unify ~tagpairs:true)
      cont
      (Sl_term.empty_subst, TagPairs.empty)
      pre' pre
  with Not_symheap -> []

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
    let (pre,cmd,post) as targ_seq = Proof.get_seq targ_idx prf in
    (* [targ_seq'] is as [targ_seq] but with the tags of [src_seq] *)
    let targ_seq' = 
      ( Sl_form.subst_tags tagpairs pre, 
        cmd,
        post) in 
    let subst_seq = Seq.subst theta targ_seq' in
    Rule.sequence [
      if Seq.equal src_seq subst_seq
        then Rule.identity
        else Rule.mk_infrule (weaken subst_seq);
        
      if Sl_term.Map.for_all Sl_term.equal theta
        then Rule.identity
        else Rule.mk_infrule (subst_rule theta targ_seq');
         
      Rule.mk_backrule 
        false 
        (fun _ _ -> [targ_idx]) 
        (fun s s' -> 
          [(if !termination then TagPairs.reflect tagpairs else Seq.tagpairs_one), "Backl"])
    ] in
  Rule.first (Blist.map f apps) idx prf

let fold def =
  let fold_rl ((pre,cmd,post) as seq) = 
    try 
      let pre = Sl_form.dest pre in
      if Sl_tpreds.is_empty pre.SH.inds then [] else
      let tags = Seq.tags seq in
      let do_case case =
        let (f,(ident,vs)) = Sl_indrule.dest case in
        let results = Sl_indrule.fold case pre in
        let process (theta, pre') = 
          let seq' = ([pre'],cmd,post) in
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
    let rl seq =
      let generalise m h =
        let avoid = ref (Seq.vars seq) in
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

let setup (defs, procs) =
  (* Program.set_local_vars seq_to_prove ; *)
  let () = Sl_rules.setup defs in
  let entails f f' =
    Slprover.idfs 1 11 !Sl_rules.axioms !Sl_rules.rules (f, f') in
  let () =
    axioms := Rule.first [
        ex_falso_axiom ; 
        mk_symex_stop_axiom entails ; 
        mk_symex_empty_axiom entails
      ] in
  let symex_proc_unfold = mk_symex_proc_unfold procs in
  let symex_proc_call = mk_symex_proc_call procs in
    rules := Rule.first [ 
      lhs_disj_to_symheaps ;
      simplify ;
      
      Rule.choice [
        dobackl ;
        Rule.choice 
          (Blist.map 
            (fun c -> Rule.compose (fold c) dobackl) 
            (Sl_defs.to_list defs)) ;
        
        Rule.first [
          symex_skip_rule ;
          symex_assign_rule ;
          symex_load_rule ;
          symex_store_rule ;
          symex_free_rule ;
          symex_new_rule ;
          symex_if_rule ;
          symex_ifelse_rule ;
          symex_while_rule ;
          symex_proc_unfold ;
          symex_proc_call ;
        ] ;
        
        generalise_while_rule ;
        (* backlink_cut entails; *)
        
        luf defs ;
      ]
    ]
