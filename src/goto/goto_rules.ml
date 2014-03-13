open Lib
open Util
open Symheap
open Goto_program

module Rule = Proofrule.Make(Goto_program.Seq)
module Seqtactics = Seqtactics.Make(Goto_program.Seq)

let dest_sh_seq (l,i) = (Form.dest l, i)

(* axioms *)

let ex_falso_axiom_f, ex_falso_axiom = 
  let ax ((l,_):Seq.t) = Form.inconsistent l in 
  ax, Rule.mk_axiom (fun seq -> Option.mk (ax seq) "Ex Falso")

let symex_stop_axiom_f, symex_stop_axiom = 
  let ax ((_,i):Seq.t) = Cmd.is_stop (get_cmd i) in
  ax, Rule.mk_axiom (fun seq -> Option.mk (ax seq) "Stop")

(* rules *)
let eq_subst_ex_f =
  let rl (l,i) =
    let l' = Form.subst_existentials l in
    if Form.equal l l' then [] else
    [ [ ((l', i), Form.tag_pairs l, TagPairs.empty) ], "" ] in
  rl

let norm (l,i) = 
  let l' = Form.norm l in
  if Form.equal l l' then [] else
  [ [( (l',i), Form.tag_pairs l', TagPairs.empty )], "" ] 

let simplify_rules = [ (* norm ; *) eq_subst_ex_f ]

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
        (fun sh -> ( ([sh], i), Heap.tag_pairs sh, TagPairs.empty ) ) 
        l,
      "L.Or"
    ] in
  rl, Rule.mk_infrule rl 


let gen_left_rules_f (def, ident) seq =
  try
    let (l,i) = dest_sh_seq seq in
    let preds = 
      Inds.filter (fun (_,(ident',_)) -> Strng.equal ident ident') l.inds in
    if Inds.is_empty preds then [] else
    let left_unfold ((id,(_,pvs)) as p) = 
      let l' = { l with inds=Inds.remove p l.inds } in
      let do_case case =
        let (f', (_,vs')) = Case.dest (freshen_case_by_seq seq case) in
        let theta = Term.Map.of_list (Blist.combine vs' pvs) in
        let f' = Heap.subst theta f' in
        (* let f' = Heap.sim_subst_ f' pvs vs' in  *)
        (* do not universalize existential vars in pred def *) 
        (* let f' = univ_form f' in*)
        let f' = Heap.repl_tags id f' in
        let l' = Heap.star l' f' in
        let ts = Tags.inter (Heap.tags l') (Heap.tags l) in
        (([l'],i), TagPairs.mk ts, TagPairs.singleton (id, id)) in
      Blist.map do_case def,
      (ident ^ " L.Unf.") in 
    Inds.map_to_list left_unfold preds
  with Not_symheap -> [] 
 
let gen_left_rules (def,ident) = 
  wrap (gen_left_rules_f (def,ident)) 

(* symbolic execution rules *)
  
let symex_assign_rule_f, symex_assign_rule =
  let rl seq =
    try
      let (f,i) = dest_sh_seq seq in
      let cmd = get_cmd i in
      let (x,e) = Cmd.dest_assign cmd in
      let fv = fresh_evar (Heap.vars f) in
      let theta = Term.singleton_subst x fv in
      let f' = Heap.subst theta f in
      let e' = Term.subst theta e in
      let f' = { f' with eqs=UF.add (e',x) f'.eqs } in
      [ [ (([f'], i+1), Heap.tag_pairs f, TagPairs.empty) ], "Assign" ]
    with WrongCmd | Not_symheap -> [] in
  rl, wrap rl 

let symex_load_rule_f, symex_load_rule =
  let rl seq =
    try
      let (f,i) = dest_sh_seq seq in
      let cmd = get_cmd i in
      let (x,e,s) = Cmd.dest_load cmd in
      (* now search for pto on e' *)
      let (_,ys) = Ptos.find (fun (l,vs) -> Heap.equates f e l) f.ptos in
      let t = Blist.nth ys (get_sel_index s) in
      let fv = fresh_evar (Heap.vars f) in
      let theta = Term.singleton_subst x fv in
      let f' = Heap.subst theta f in
      let t' = Term.subst theta t in
      let f' = { f' with eqs=UF.add (t',x) f'.eqs } in
      [ [ (([f'], i+1), Heap.tag_pairs f, TagPairs.empty) ], "Load" ]
    with Not_symheap | WrongCmd | Not_found -> [] in
  rl, wrap rl 

let symex_store_rule_f, symex_store_rule =
  let rl seq =
    try
      let (f,i) = dest_sh_seq seq in
      let cmd = get_cmd i in
      let (x,s,e) = Cmd.dest_store cmd in
      let ((_,ys) as pto) = 
        Ptos.find (fun (v,_) -> Heap.equates f v x) f.ptos in
      let newptos = Ptos.remove pto f.ptos in
      let pto' = 
        try (x, Blist.replace_nth e (get_sel_index s) ys) 
        with Invalid_argument msg ->
          print_endline ("seq= " ^ (Seq.to_string seq) ^ "   x=" ^ (Term.to_string x) ^ " s=" ^ s ^ " e=" ^ (Term.to_string e)) ;
          assert false
        in
      let f' = { f with ptos=Ptos.add pto' newptos } in
      [ [ (([f'], i+1), Heap.tag_pairs f, TagPairs.empty) ], "Store" ]
    with Not_symheap | WrongCmd | Not_found -> [] in
  rl, wrap rl 

let symex_free_rule_f, symex_free_rule =
  let rl seq =
    try
      let (f,i) = dest_sh_seq seq in
      let cmd = get_cmd i in
      let e = Cmd.dest_free cmd in
      let pto = Ptos.find (fun (v,_) -> Heap.equates f v e) f.ptos in
      let newptos = Ptos.remove pto f.ptos in
      let f' = { f with ptos=newptos } in
      [ [ (([f'], i+1), Heap.tag_pairs f, TagPairs.empty) ], "Free" ]
    with Not_symheap | WrongCmd | Not_found -> [] in
  rl, wrap rl 

let symex_new_rule_f, symex_new_rule =
  let rl seq =
    try
      let (f,i) = dest_sh_seq seq in
      let cmd = get_cmd i in
      let x = Cmd.dest_new cmd in
      let l = fresh_evars (Heap.vars f) (1 + Blist.length (fst !program)) in
      let (fv,fvs) = (Blist.hd l, Blist.tl l) in
      let f' = Heap.subst (Term.singleton_subst x fv) f in
      let f' = { f' with ptos=Ptos.add (x, fvs) f'.ptos } in
      [ [ (([f'], i+1), Heap.tag_pairs f, TagPairs.empty) ], "New" ]
    with Not_symheap | WrongCmd-> [] in
  rl, wrap rl 

let symex_goto_rule_f, symex_goto_rule =
  let rl (f,i) =
    let cmd = get_cmd i in
    try
      let i' = Cmd.dest_goto cmd in
      [ [ ((f, i'), Form.tag_pairs f, TagPairs.empty) ], "Goto" ]
    with WrongCmd -> [] in
  rl, wrap rl 

let symex_skip_rule_f, symex_skip_rule =
  let rl (f,i) =
    let cmd = get_cmd i in
    try
      let () = Cmd.dest_skip cmd in
      [ [ ((f, i+1), Form.tag_pairs f, TagPairs.empty) ], "Skip" ]
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
      let f' =  { f with eqs=UF.add pair f.eqs } in
      let f'' = { f with deqs=Deqs.add pair f.deqs } in
      let (f',f'') = if (Cmd.is_deq c) then (f'',f') else (f',f'') in 
      let t = Heap.tag_pairs f in
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
      let t = Form.tag_pairs f in
      [ 
        [ ((f, i'), t, TagPairs.empty) ; ((f, i+1), t, TagPairs.empty) ], 
        "If(non-det)"
      ]
    with WrongCmd -> [] in
  rl, wrap rl 



let is_subsumed s1 s2 = Seq.subsumed_wrt_tags Tags.empty s1 s2 

let matches_fun ((l1,i1) as s1) ((l2,i2) as s2) =
  let () = debug
    (fun () -> "Matches: " ^ (Seq.to_string s1) ^ " -> " ^ (Seq.to_string s2)) in
  if i1<>i2 then [] else
  let tags = Tags.inter (Seq.tags s1) (Seq.tags s2) in
  if Tags.is_empty tags then [] else
  let res = Seq.uni_subsumption s1 s2 in
  if Option.is_none res then [] else
  let theta = Option.get res in
  let s2' = Seq.subst theta s2 in
  let tags' = Tags.fold
    (fun t acc ->
      let new_acc = Tags.add t acc in
      if Seq.subsumed_wrt_tags new_acc s1 s2' then new_acc else acc
    ) tags Tags.empty in
  let () = assert (not (Tags.is_empty tags')) in
  [ (TagPairs.mk tags', "Backl") ]

let matches = Rule.mk_backrule true Rule.all_nodes matches_fun 


let gen_fold_rules (def, ident) =
  let fold_rule s1 s2 =
    debug 
      (fun () -> 
        "Fold/match[" ^ ident ^ "]: " ^ (Seq.to_string s1) ^ " -> " ^ (Seq.to_string s2)) ;
    try
      let ((l1,i1),(l2,i2)) = Pair.map dest_sh_seq (s1,s2) in
      if i1<>i2 then [] else
      let preds = Inds.filter (fun (_, (ident', _)) -> ident=ident') l2.inds in
      if Inds.is_empty preds then [] else
      let fold_match ((id,(_,pvs)) as p) = 
        let l2' = { l2 with inds=Inds.remove p l2.inds } in
        let do_case case =
          let (f', (_,vs')) = Case.dest (freshen_case_by_seq ([l2'],i2) case) in
          let theta = Term.Map.of_list (Blist.combine vs' pvs) in
          let f' = Heap.subst theta f' in
          (* let f' = Heap.sim_subst f' pvs vs' in  *)
          (* do not universalize existential vars in pred def *)
          (* use fresh tag for the ind case so that tracing cannot *)
          (* follow these new tags *)
          let alltags = Tags.union (Seq.tags s1) (Seq.tags s2) in
          let fresh_tag = 1 + (try Tags.max_elt alltags with Not_found -> 0) in 
          let f' = Heap.repl_tags fresh_tag f' in
          let l2' = Heap.star l2' f' in
          let s2' = ([l2'],i2) in
          (* FIXME this enforces ind clause to be symheaps *)
          let res = matches_fun s1 s2' in
          let () = debug
            (fun () -> "CM(" ^ (if res=[] then "T" else "F") ^") " 
            ^ (Seq.to_string s1) ^ " / " ^ (Seq.to_string s2)) in
          match res with
            | [] -> None
            | (ts,_)::_ -> 
              (* the following fixes the bug wrt to the binary tree search example *)
              let ts = TagPairs.filter (fun (a,b) -> a<>id && b<>id) ts in
              if TagPairs.is_empty ts then 
                None
              else 
                Some (ts, ident ^ " Fold/Backl") in
        Blist.find_some do_case def in 
      match Blist.find_some fold_match (Inds.elements preds) with
      | None -> []
      | Some p -> [p]
    with Not_symheap -> [] in
  Rule.mk_backrule true Rule.all_nodes fold_rule 

let axioms = ref (Rule.first [ex_falso_axiom ; symex_stop_axiom])

let rules = ref Rule.fail

let setup defs seq_to_prove =
  set_local_vars seq_to_prove ;
  rules := Rule.first [ 
    lhs_disj_to_symheaps ;
    simplify;
    
    Rule.choice [
      matches ;
      Rule.choice (Blist.map gen_fold_rules defs);
      
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
      
      Rule.choice (Blist.map gen_left_rules defs)
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
   