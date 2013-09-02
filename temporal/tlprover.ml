open Lib
open Util
open Tempform

module SH = Symheap
module TLP = Prover.Make(Tempform.Seq)(Tempform.Defs)
include TLP

let freshen_case_by_seq seq case =
  Case.freshen (Term.Set.union (Program.vars_of_program ()) (Seq.vars seq)) case

(* axioms *)
let ex_falso_axiom = 
  let ax (l,_,_) = SH.Form.inconsistent l in 
  TLP.mk_axiom ax "Ex Falso"

let pure_axiom_f, pure_axiom = 
  let ax ((l,_,r) : Seq.t) = 
    let olddebug = !Lib.do_debug in
    let () = Lib.do_debug := false in
    let rs = Form.dest_or r in
    let result = 
      FormSet.exists
        begin fun f ->
          Form.is_slformula f &&
          (* FIXME the star below is due to the strict entailment of slprover *)
          Option.is_some 
            (Slprover.idfs 
              (l, Symheap.Form.star l (Form.to_slformula f))) 
        end
        rs in
    Lib.do_debug := olddebug ; result in
  ax, TLP.mk_axiom ax "Pure"

(* rules *)
let encap_pr_rule rl (l,i,r) =
  let tvs' = Form.tag_pairs r in
  Blist.map 
    begin fun l -> 
      Blist.map 
        begin fun ((l',i'),tvs,tps) ->
          ((l',i',r), TagPairs.union tvs tvs', tps) 
        end  
        l
    end 
    (rl (l,i))

let eq_subst_ex_f = encap_pr_rule Prprover.eq_subst_ex_f
let norm (l,i,r) = (encap_pr_rule Prprover.norm) (l,i,r) 

let norm_rhs (l,i,r) = 
  let r' = Form.norm r in
  if Form.equal r r' then [] else
  let seq' = (l,i,r') in 
  [ [( seq', Seq.tag_pairs seq', TagPairs.empty )] ] 

let rhs_conj (l,i,r) =
  let rs = Form.dest_or r in
  if not (FormSet.exists Form.is_and rs) then [] else
  let c = FormSet.find Form.is_and rs in
  let rs = FormSet.remove c rs in
  [
    FormSet.map_to_list
      begin fun f -> 
        let s = (l,i, Form.mk_or (FormSet.add f rs)) in 
        (s, Seq.tag_pairs s, TagPairs.empty) 
      end
      (Form.dest_and c)
  ]

let rhs_disj_pure (l,i,r) =
  let rs = Form.dest_or r in
  let (pure, impure) = FormSet.partition Form.is_slformula rs in
  if FormSet.is_empty pure || FormSet.is_empty impure then [] else
  let r' = 
    Form.mk_or (if pure_axiom_f (l,i,Form.mk_or pure) then pure else impure) in
  let seq' = (l, i, r') in 
  [ [(seq', Seq.tag_pairs seq', TagPairs.empty)] ]

let unfold_top_diamonds (l,i,r) =
  let rs = Form.dest_or r in
  let diamonds = FormSet.filter Form.is_diamond rs in
  if FormSet.is_empty diamonds then [] else
  let f = FormSet.choose diamonds in
  let rs' = FormSet.remove f rs in
  let rs' = FormSet.add (Form.dest_diamond f) rs' in 
  let rs' = FormSet.add (Form.mk_circle f) rs' in
  let r' = Form.mk_or rs' in 
  let s = (l, i, r') in 
  [ [(s, Seq.tag_pairs s, TagPairs.empty)] ] 

let unfold_top_boxes (l,i,r) =
  let rs = Form.dest_or r in
  let boxes = FormSet.filter Form.is_box rs in
  if FormSet.is_empty boxes then [] else
  let f = FormSet.choose boxes in
  let rs' = FormSet.remove f rs in
  let (j,g) = Form.dest_box f in
  let s = (l,i, Form.mk_or (FormSet.add g rs')) in
  let s' = (l,i, Form.mk_or (FormSet.add (Form.mk_circle f) rs')) in
  [ [ 
    (s, Seq.tag_pairs s, TagPairs.empty); 
    (s', Seq.tag_pairs s', TagPairs.singleton (j,j))
  ] ] 

let simplify_rules = [ 
  (eq_subst_ex_f, "= ex subst")  ;
  (norm_rhs, "norm rhs") ;
  (rhs_disj_pure, "rhs disj pure");
  (rhs_conj, "conj rhs");
  (unfold_top_diamonds, "U.Diam");
  (unfold_top_boxes, "U.Box");
]

let matches_fun ((l,i,r) as s) ((l',i',r') as s') =
  let () = debug (fun () -> (Seq.to_string s) ^ " matches? " ^ (Seq.to_string s')) in
  if i<>i' or not (Form.equal r r') then None else
  let tags = Tags.inter (Seq.tags s) (Seq.tags s') in
  if Tags.is_empty tags then None else
  let res = Seq.uni_subsumption s s' in
  if Option.is_none res then None else
  let theta = Option.get res in
  let s'' = Seq.subst theta s' in
  let tags' = 
    Tags.fold
      begin fun t acc ->
        let new_acc = Tags.add t acc in
        if Seq.subsumed_wrt_tags new_acc s s'' then new_acc else acc
      end 
      tags 
      Tags.empty in
  Some (TagPairs.mk tags')
      
let matches = TLP.mk_back_rule matches_fun "Backl"

let simplify_seq_rl = 
  TLP.Seq_tacs.repeat_tac (TLP.Seq_tacs.first (Blist.map fst simplify_rules))

let simplify_proof_rl =                                                                          
  TLP.Proof_tacs.repeat_tac                                                                
    (TLP.Proof_tacs.first
      (Blist.map (fun (r,d) -> TLP.mk_inf_rule r d) simplify_rules)) 

let setup defs =
  let simplify =
    if !TLP.expand_proof then
      TLP.rename_rule simplify_proof_rl "Simpl"
    else
      TLP.mk_inf_rule simplify_seq_rl "Simpl" in
  
  let wrap r d =
    if !TLP.expand_proof then                                                                              
      TLP.Proof_tacs.then_tac                                                                  
        (TLP.mk_inf_rule r d)                                                                  
        (TLP.Proof_tacs.try_tac simplify_proof_rl)                                                   
    else
      TLP.mk_inf_rule
        (TLP.Seq_tacs.then_tac r (TLP.Seq_tacs.try_tac simplify_seq_rl))
        d in
  
  let pr_wrap rl d = wrap (encap_pr_rule rl) d in
  
  (* break LHS disjunctions *)
  let lhs_disj_to_symheaps = pr_wrap Prprover.lhs_disj_to_symheaps_f "L.Or" in
  
  let gen_left_rules (def,ident) = 
    pr_wrap (Prprover.gen_left_rules_f (def,ident)) (ident ^ " L.Unf.") in
  
  (* let rhs_disj_pure_prl = TLP.mk_inf_rule rhs_disj_pure "Or Pure" in *)
  
  let encap_symex_rule rl (l,i,r) =
    let rs = Form.dest_or r in
    if not (FormSet.for_all Form.is_circle rs) then [] else
    let r' = Form.mk_or (FormSet.endomap Form.dest_circle rs) in
    (encap_pr_rule rl) (l,i,r') in
  
  let symex_wrap rl d = wrap (encap_symex_rule rl) d in
      
  let symex_assign = symex_wrap Prprover.symex_assign_rule_f "Assign" in
  let symex_load = symex_wrap Prprover.symex_load_rule_f "Load" in
  let symex_store = symex_wrap Prprover.symex_store_rule_f "Store" in
  let symex_free = symex_wrap Prprover.symex_free_rule_f "Free" in
  let symex_new = symex_wrap Prprover.symex_new_rule_f "New" in
  let symex_goto = symex_wrap Prprover.symex_goto_rule_f "Goto" in
  let symex_det_if = symex_wrap Prprover.symex_det_if_rule_f "If(det)" in
  let symex_non_det_if = symex_wrap Prprover.symex_non_det_if_rule_f "If(non-det)" in
  
  
   
  let symex = 
    Proof_tacs.first 
      [ 
        symex_assign;
        symex_load;
        symex_store;
        symex_free;
        symex_new;
        symex_goto;
        symex_det_if;
        symex_non_det_if
      ] in
  Slprover.setup defs ;
  let luf = Proof_tacs.angelic_or_tac (Blist.map gen_left_rules defs) in
  (* let cutm = Blist.map gen_fold_rules defs in *)
  TLP.axiomset := [ ex_falso_axiom ; pure_axiom ] ;
  TLP.ruleset := 
    [
      lhs_disj_to_symheaps ;
      simplify ;
      matches
    ] @
    (* cutm @ *)
    [
      symex;
      luf;
    ]
  