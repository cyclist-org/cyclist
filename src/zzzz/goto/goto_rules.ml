open Lib
open Util
open Goto_program

module SH = Sl_heap
exception Not_symheap = Sl_form.Not_symheap

module Proof = Proof.Make(Goto_program.Seq)
module Rule = Proofrule.Make(Goto_program.Seq)
module Seqtactics = Seqtactics.Make(Goto_program.Seq)

let dest_sh_seq (l,i) = (Sl_form.dest l, i)

(* axioms *)

let ex_falso_axiom_f, ex_falso_axiom = 
  let ax ((l,_):Seq.t) = Sl_form.inconsistent l in 
  ax, Rule.mk_axiom (fun seq -> Option.mk (ax seq) "Ex Falso")

let symex_stop_axiom_f, symex_stop_axiom = 
  let ax ((_,i):Seq.t) = Cmd.is_stop (get_cmd i) in
  ax, Rule.mk_axiom (fun seq -> Option.mk (ax seq) "Stop")

(* rules *)
let eq_subst_ex_f =
  let rl (l,i) =
    let l' = Sl_form.subst_existentials l in
    if Sl_form.equal l l' then [] else
    [ [ ((l', i), Sl_form.tag_pairs l, TagPairs.empty) ], "" ] in
  rl

let simplify_rules = [ eq_subst_ex_f ]

let simplify_seq_rl = Seqtactics.repeat (Seqtactics.first simplify_rules)

let simplify = Rule.mk_infrule simplify_seq_rl

let wrap r =
  Rule.mk_infrule
    (Seqtactics.compose r (Seqtactics.attempt simplify_seq_rl))

(* break LHS disjunctions *)
let lhs_disj_to_symheaps_f, lhs_disj_to_symheaps =
  let rl ((l,i):Seq.t) =
    if (Blist.length l) < 2 then [] else
    [ Blist.map 
        (fun sh -> ( ([sh], i), Sl_heap.tag_pairs sh, TagPairs.empty ) ) 
        l,
      "L.Or"
    ] in
  rl, Rule.mk_infrule rl 


let luf_rl seq defs =
  try
    let (l,i) = dest_sh_seq seq in
    let seq_vars = Seq.vars seq in
    let left_unfold ((tag, (ident, _)) as p) = 
      let l = SH.del_ind l p in
      let clauses = Sl_defs.unfold seq_vars l p defs in
      let do_case (f', tagpairs) =
        let l' = Sl_heap.star l f' in
        (([l'],i), TagPairs.union (Sl_heap.tag_pairs l) tagpairs, tagpairs) in
      Blist.map do_case clauses, (ident ^ " L.Unf.") in
    Sl_tpreds.map_to_list 
      left_unfold 
      (Sl_tpreds.filter (Sl_defs.is_defined defs) l.SH.inds)
  with Not_symheap -> []

let luf defs = wrap (fun seq -> luf_rl seq defs)

(* symbolic execution rules *)
  
let symex_assign_rule_f, symex_assign_rule =
  let rl seq =
    try
      let (f,i) = dest_sh_seq seq in
      let cmd = get_cmd i in
      let (x,e) = Cmd.dest_assign cmd in
      let fv = fresh_evar (Sl_heap.vars f) in
      let theta = Sl_term.singleton_subst x fv in
      let f' = Sl_heap.subst theta f in
      let e' = Sl_term.subst theta e in
      let f' = SH.with_eqs f' (Sl_uf.add (e',x) f'.SH.eqs) in
      [ [ (([f'], i+1), Sl_heap.tag_pairs f, TagPairs.empty) ], "Assign" ]
    with WrongCmd | Not_symheap -> [] in
  rl, wrap rl 

let symex_load_rule_f, symex_load_rule =
  let rl seq =
    try
      let (f,i) = dest_sh_seq seq in
      let cmd = get_cmd i in
      let (x,e,s) = Cmd.dest_load cmd in
      (* now search for pto on e' *)
      let (_,ys) = Sl_ptos.find (fun (l,vs) -> Sl_heap.equates f e l) f.SH.ptos in
      let t = Blist.nth ys (get_sel_index s) in
      let fv = fresh_evar (Sl_heap.vars f) in
      let theta = Sl_term.singleton_subst x fv in
      let f' = Sl_heap.subst theta f in
      let t' = Sl_term.subst theta t in
      let f' = SH.with_eqs f' (Sl_uf.add (t',x) f'.SH.eqs) in
      [ [ (([f'], i+1), Sl_heap.tag_pairs f, TagPairs.empty) ], "Load" ]
    with Not_symheap | WrongCmd | Not_found -> [] in
  rl, wrap rl 

let symex_store_rule_f, symex_store_rule =
  let rl seq =
    try
      let (f,i) = dest_sh_seq seq in
      let cmd = get_cmd i in
      let (x,s,e) = Cmd.dest_store cmd in
      let ((_,ys) as pto) = 
        Sl_ptos.find (fun (v,_) -> Sl_heap.equates f v x) f.SH.ptos in
      let newptos = Sl_ptos.remove pto f.SH.ptos in
      let pto' = 
        try (x, Blist.replace_nth e (get_sel_index s) ys) 
        with Invalid_argument msg ->
          print_endline ("seq= " ^ (Seq.to_string seq) ^ "   x=" ^ (Sl_term.to_string x) ^ " s=" ^ s ^ " e=" ^ (Sl_term.to_string e)) ;
          assert false
        in
      let f' = SH.with_ptos f (Sl_ptos.add pto' newptos) in
      [ [ (([f'], i+1), Sl_heap.tag_pairs f, TagPairs.empty) ], "Store" ]
    with Not_symheap | WrongCmd | Not_found -> [] in
  rl, wrap rl 

let symex_free_rule_f, symex_free_rule =
  let rl seq =
    try
      let (f,i) = dest_sh_seq seq in
      let cmd = get_cmd i in
      let e = Cmd.dest_free cmd in
      let pto = Sl_ptos.find (fun (v,_) -> Sl_heap.equates f v e) f.SH.ptos in
      let newptos = Sl_ptos.remove pto f.SH.ptos in
      let f' = SH.with_ptos f newptos in
      [ [ (([f'], i+1), Sl_heap.tag_pairs f, TagPairs.empty) ], "Free" ]
    with Not_symheap | WrongCmd | Not_found -> [] in
  rl, wrap rl 

let symex_new_rule_f, symex_new_rule =
  let rl seq =
    try
      let (f,i) = dest_sh_seq seq in
      let cmd = get_cmd i in
      let x = Cmd.dest_new cmd in
      let l = fresh_evars (Sl_heap.vars f) (1 + Blist.length (fst !program)) in
      let (fv,fvs) = (Blist.hd l, Blist.tl l) in
      let f' = Sl_heap.subst (Sl_term.singleton_subst x fv) f in
      let f' = SH.with_ptos f' (Sl_ptos.add (x, fvs) f'.SH.ptos) in
      [ [ (([f'], i+1), Sl_heap.tag_pairs f, TagPairs.empty) ], "New" ]
    with Not_symheap | WrongCmd-> [] in
  rl, wrap rl 

let symex_goto_rule_f, symex_goto_rule =
  let rl (f,i) =
    let cmd = get_cmd i in
    try
      let i' = Cmd.dest_goto cmd in
      [ [ ((f, i'), Sl_form.tag_pairs f, TagPairs.empty) ], "Goto" ]
    with WrongCmd -> [] in
  rl, wrap rl 

let symex_skip_rule_f, symex_skip_rule =
  let rl (f,i) =
    let cmd = get_cmd i in
    try
      let () = Cmd.dest_skip cmd in
      [ [ ((f, i+1), Sl_form.tag_pairs f, TagPairs.empty) ], "Skip" ]
    with WrongCmd -> [] in
  rl, wrap rl 

let symex_det_if_rule_f, symex_det_if_rule =
  let rl seq =
    try
      let (f,i) = dest_sh_seq seq in
      let cmd = get_cmd i in
      let (c,i') = Cmd.dest_if cmd in
      if Cmd.is_non_det c then [] else
      let pair = Cmd.dest_cond c in
      let f' =  SH.with_eqs f (Sl_uf.add pair f.SH.eqs) in
      let f'' = SH.with_deqs f (Sl_deqs.add pair f.SH.deqs) in
      let (f',f'') = if (Cmd.is_deq c) then (f'',f') else (f',f'') in 
      let t = Sl_heap.tag_pairs f in
      [ 
        [ (([f'], i'), t, TagPairs.empty) ; (([f''], i+1), t, TagPairs.empty) ],
        "If(det)" 
      ]
    with Not_symheap | WrongCmd -> [] in
  rl, wrap rl 

let symex_non_det_if_rule_f, symex_non_det_if_rule =
  let rl (f,i) =
    let cmd = get_cmd i in
    try
      let (c,i') = Cmd.dest_if cmd in
      if not (Cmd.is_non_det c) then [] else
      let t = Sl_form.tag_pairs f in
      [ 
        [ ((f, i'), t, TagPairs.empty) ; ((f, i+1), t, TagPairs.empty) ], 
        "If(non-det)"
      ]
    with WrongCmd -> [] in
  rl, wrap rl 


let matches (l,i) (l',i') =
  try
    if i<>i' then [] else
    let (l,l') = Pair.map Sl_form.dest (l,l') in
    let results = ref [] in
    let verify ((theta, _) as state) =
      (* assert (Seq.subsumed_upto_tags seq (Seq.subst theta seq')) ; *)
      if 
        Sl_term.Map.for_all 
          (fun x y -> 
            Sl_heap.equates l' x y || not (Sl_term.Set.mem x !program_vars)) 
          theta 
      then
        results := state :: !results;
      None in
    let _ =
      Sl_heap.unify_partial ~tagpairs:true
        verify
        (Sl_term.empty_subst, TagPairs.empty)
        l' l in
    !results
  with Not_symheap -> []

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

(*   seq' = l' |- i'  *)
(* ---------------    *)
(*   seq = l |- i     *)
(* where l = l' * g   *)
let weaken ((l',i') as seq') (l,i) =
  if i<>i' then [] else
  try
    let (l,l') = Pair.map Sl_form.dest (l,l') in
    if Sl_heap.subsumed ~total:false l' l then 
      [ [(seq', TagPairs.mk (Seq.tags seq'), TagPairs.empty)], "Frame" ]
    else
      []
  with Not_symheap -> []

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
    let targ_seq' = (Sl_form.subst_tags tagpairs (fst targ_seq), snd targ_seq) in 
    let subst_seq = Seq.subst theta targ_seq' in
    Rule.sequence [
      if Seq.equal src_seq subst_seq
        then Rule.identity
        else Rule.mk_infrule (weaken subst_seq);
        
      if Sl_term.Map.for_all Sl_term.equal theta
        then Rule.identity
        else Rule.mk_infrule (subst_rule theta targ_seq');
         
      Rule.mk_backrule 
        true 
        (fun _ _ -> [targ_idx]) 
        (fun s s' -> [TagPairs.reflect tagpairs, "Backl"])
    ] in
    Rule.first (Blist.map f apps) idx prf


let fold (defs,ident) =
  let fold_rl seq = 
    try 
      let (l,i) = dest_sh_seq seq in
      if Sl_tpreds.is_empty l.SH.inds then [] else
      let tags = Seq.tags seq in
      let do_case case =
        let (f,(ident,vs)) = Sl_indrule.dest case in
        let results = Sl_indrule.fold case l in
        let process (theta, l') = 
          let seq' = ([l'],i) in
          (* let () = print_endline "Fold match:" in         *)
          (* let () = print_endline (Seq.to_string seq) in   *)
          (* let () = print_endline (Sl_heap.to_string f) in *)
          (* let () = print_endline (Seq.to_string seq') in  *)
            [(
              seq', 
              TagPairs.mk (Tags.inter tags (Seq.tags seq')), 
              TagPairs.empty 
            )], (ident ^ " Fold")  in
        Blist.map process results in
      Blist.bind do_case defs
    with Not_symheap -> [] in
  Rule.mk_infrule fold_rl 



let axioms = ref (Rule.first [ex_falso_axiom ; symex_stop_axiom])

let rules = ref Rule.fail

let setup defs seq_to_prove =
  set_local_vars seq_to_prove ;
  rules := Rule.first [ 
    lhs_disj_to_symheaps ;
    simplify;
    
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
        symex_goto_rule ;
        symex_det_if_rule ;
        symex_non_det_if_rule 
      ];
      
      luf defs
    ]
  ]


(* let coverage prf =                                                            *)
(*   let get_line i = snd (PRP.Proof.get_seq i prf) in                           *)
(*   let lines =                                                                 *)
(*     Blist.fold_left                                                           *)
(*       (fun s (i,_) -> Int.Set.add (get_line i) s)                             *)
(*       Int.Set.empty                                                           *)
(*       (PRP.Proof.to_list prf) in                                              *)
(*   let no_lines = Int.Set.cardinal lines in                                    *)
(*   let prog_lines = Program.get_no_lines () in                                 *)
(*   int_of_float (100. *. (float_of_int no_lines) /. (float_of_int prog_lines)) *)
   