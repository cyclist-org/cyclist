open Lib
open Util
open Symheap

module SH = Sl_heap

module Proof = Proof.Make(Sl_seq)
module Rule = Proofrule.Make(Sl_seq)
module Seqtactics = Seqtactics.Make(Sl_seq)

let id_axiom =
  Rule.mk_axiom 
    (fun (l,r) -> Option.mk (Sl_form.subsumed_wrt_tags Tags.empty r l) "Id")

let ex_falso_axiom =
  Rule.mk_axiom (fun (l,_) -> Option.mk (Sl_form.inconsistent l) "Ex Falso")

(* break LHS disjunctions *)
let lhs_disj_to_symheaps =
  Rule.mk_infrule 
    (fun (l,r) -> if Blist.length l < 2 then [] else
      [ 
        Blist.map (fun h -> (([h],r), Sl_heap.tag_pairs h, TagPairs.empty)) l,
         "L. Or" 
      ]
    )
    

(* break RHS disjunctions *)
let rhs_disj_to_symheaps =
  Rule.mk_infrule 
    (fun (l,r) -> if Blist.length r < 2 || Blist.length l <> 1 then [] else
      Blist.map 
        (fun s -> [ ((l,[s]), Sl_form.tag_pairs l, TagPairs.empty) ], "R. Or") 
        r)

(* simplification rules *)

(* substitute all equalities from LHS into sequent *)
(* for (x,y) substitute y over x as x<y *)
(* this means representatives of eq classes are the max elems *)
let eq_subst_rule seq =
  try
    let (l,r) = Sl_seq.dest seq in
    if UF.is_empty l.SH.eqs then [] else
		let leqs = UF.bindings l.SH.eqs in
		let (x,y) as p = Blist.find (fun p' -> Pair.disj (Pair.map Term.is_var p')) leqs in
		let leqs = Blist.filter (fun q -> q!=p) leqs in
		let l = { l with SH.eqs = UF.of_list leqs } in
		let (x,y) = if Term.is_var x then p else (y,x) in
		let theta = Term.singleton_subst x y in
    let (l',r') = Pair.map (fun z -> Sl_heap.subst theta z) (l,r) in
    [ [ (([l'], [r']), Sl_heap.tag_pairs l, TagPairs.empty) ], "" ]
  with Not_symheap | Not_found -> []


(* substitute all equalities in RHS involving an existential var *)
let eq_ex_subst_rule seq =
  try
    let (l,r) = Sl_seq.dest seq in
		let reqs = UF.bindings r.SH.eqs in
		let (x,y) as p = Blist.find (fun (x,_) -> Term.is_exist_var x) reqs in
		let reqs = Blist.filter (fun q -> q!=p) reqs in
    let r = { r with SH.eqs=UF.of_list reqs } in
		let (x,y) = if Term.is_var x then p else (y,x) in
    let r' = Sl_heap.subst (Term.singleton_subst x y) r in
    [ [ (([l], [r']), Sl_heap.tag_pairs l, TagPairs.empty) ], "" ]
  with Not_symheap | Not_found -> []

(* remove all RHS eqs that can be discharged *)
let eq_simplify seq =
  try
    let (l,r) = Sl_seq.dest seq in
    let (disch, reqs) =
			Blist.partition (fun (x,y) -> Sl_heap.equates l x y) (UF.bindings r.SH.eqs) in
    if disch=[] then [] else
    [ [ (([l], [ { r with SH.eqs=UF.of_list reqs } ] ), Sl_heap.tag_pairs l, TagPairs.empty) ], "" ]
  with Not_symheap -> []

(* remove all RHS SH.deqs that can be discharged *)
let deq_simplify seq =
  try
    let (l,r) = Sl_seq.dest seq in
    let (disch, rdeqs) =
			Deqs.partition (fun (x,y) -> Sl_heap.disequates l x y) r.SH.deqs in
    if Deqs.is_empty disch then [] else
    [ [ (([l], [ { r with SH.deqs=rdeqs } ] ), Sl_heap.tag_pairs l, TagPairs.empty) ], "" ]
  with Not_symheap -> []

(* do the following transformation for the first x such that *)
(* x->y * A |- x->z * B     if     A |- y=z * B *)
let pto_intro_rule seq =
  try
    let (l,r) = Sl_seq.dest seq in
    let (rx, rys) as p =
      Ptos.find (fun (w,_) -> Option.is_some (Sl_heap.find_lval w l)) r.SH.ptos in
    let (lx, lys) as p' = Option.get (Sl_heap.find_lval rx l) in
    (* take care to remove only the 1st match *)
    let l' = { l with SH.ptos=Ptos.remove p' l.SH.ptos } in
    let r' = { r with SH.ptos=Ptos.remove p r.SH.ptos } in
    let r' = { r' with SH.eqs=UF.union r'.SH.eqs (UF.of_list (Blist.combine rys lys)) } in
    [ [ ( ([l'], [r']), Sl_heap.tag_pairs l, TagPairs.empty ) ], "Pto Intro" ]
  with Not_symheap | Not_found | Invalid_argument _ -> []

(* do the following transformation for the first i,j such that *)
(* P_i(x1,..,xn) * A |- P_j(x1,...,xn) * B     if     A |- B *)
let pred_intro_rule seq =
  try
    let (l,r) = Sl_seq.dest seq in
    let (linds,rinds) = Pair.map Inds.elements (l.SH.inds,r.SH.inds) in
    let cp = Blist.cartesian_product linds rinds in
    let (p,q) =
      Blist.find
        (fun ((_,(id,vs)),(_,(id',vs'))) ->
          Strng.equal id id' &&
					Blist.for_all2 (fun x y -> Sl_heap.equates l x y) vs vs') cp in
    let l' = { l with SH.inds=Inds.remove p (Inds.of_list linds) } in
    let r' = { r with SH.inds=Inds.remove q (Inds.of_list rinds) } in
    [ [ ( ([l'], [r']), Sl_heap.tag_pairs l', TagPairs.empty ) ], "Pred Intro" ]
  with Not_symheap | Not_found -> []

let norm s =
  let s' = Sl_seq.norm s in
  if Sl_seq.equal s s' then [] else
  [ [( s', Sl_seq.tag_pairs s', TagPairs.empty )], "" ]

let simpl_deqs seq =
  try
    let (l,r) = Sl_seq.dest seq in
    let non_deq_vars =
			Term.Set.add Term.nil
				(Term.Set.union
				  (Sl_heap.vars { l with SH.deqs=Deqs.empty }) (Sl_heap.vars r)) in
    let f p = Pair.conj (Pair.map (fun t -> Term.Set.mem t non_deq_vars) p) in
    let l' = { l with SH.deqs=Deqs.filter f l.SH.deqs } in
    if Sl_heap.equal l l' then [] else
    [ [ (([l'], [r]), Sl_heap.tag_pairs l', TagPairs.empty) ], "" ]
  with Not_symheap -> []

let simplify_rules = [
  (* norm ; *)
  eq_subst_rule ;
  eq_ex_subst_rule ;
  eq_simplify ;
  deq_simplify ;
	(* simpl_deqs *)
  (* pto_intro_rule *)
  (* pred_intro_rule  *)
]

let simplify_seq = 
  Seqtactics.relabel "Simplify"
    (Seqtactics.repeat (Seqtactics.first simplify_rules)) 

let simplify = Rule.mk_infrule simplify_seq

let wrap r =
  (Rule.mk_infrule (Seqtactics.compose r (Seqtactics.attempt simplify_seq)))

(* x->ys * A |- e->zs * B if  A |- ys=zs * B[x/e] where e existential *)
(* and at least one var in ys,zs is the same *)
(* multiple applications possible *)
let instantiate_pto =
  let rl seq =
    try
      let (l,r) = Sl_seq.dest seq in
      let (lptos,rptos) = Pair.map Ptos.elements (l.SH.ptos,r.SH.ptos) in
      let eptos = Blist.filter (fun (x,_) -> Term.is_exist_var x) rptos in
      let match_ls xs ys =
        try
					Blist.exists2 (fun x y -> Sl_heap.equates l x y) xs ys
				with Invalid_argument _ -> false in
      let cp = Blist.cartesian_product eptos lptos in
      let cp = Blist.filter (fun ((_,ys),(_,zs)) -> match_ls ys zs) cp in
      let do_instantiation (((x,ys) as p), ((w,zs) as q)) =
        let l' = { l with SH.ptos=Ptos.remove q (Ptos.of_list lptos) } in
        let r' = { r with SH.ptos=Ptos.remove p (Ptos.of_list rptos) } in
        let r' =
          { r' with SH.eqs=UF.union r'.SH.eqs (UF.of_list ((x,w)::(Blist.combine ys zs))) } in
        [ ( ([l'], [r']), Sl_heap.tag_pairs l, TagPairs.empty ) ], "Inst ->"
      in Blist.map do_instantiation cp
    with Not_symheap | Invalid_argument _ -> [] in
  wrap rl 

let mk_ruf defs =
  let gen_right_rules (def,_) =
    let gen_rule case =
      let (_,(ident,_)) = Sl_indrule.dest case in
      let right_rule seq =
        try
          let (l,r) = Sl_seq.dest seq in
          let preds = Inds.filter (fun (_,(ident',_)) -> Strng.equal ident ident') r.SH.inds in
          if Inds.is_empty preds then [] else
          let (f, (ident, vs)) = Sl_indrule.dest (Sl_indrule.freshen (Sl_seq.vars seq) case) in
          let right_unfold ((_,(_,vs')) as p) =
            let r' = { r with SH.inds=Inds.remove p r.SH.inds } in
            (* NB assumes distinct vars in ind pred def *)
            let theta = Term.Map.of_list (Blist.combine vs vs') in
            let f' = Sl_heap.subst theta f in
            [ (([l], [Sl_heap.star r' f']), Sl_heap.tag_pairs l, TagPairs.empty) ],
            (ident ^ " R.Unf.") in
          Inds.map_to_list right_unfold preds
        with Not_symheap -> [] in
      wrap right_rule in
    Blist.map gen_rule def in
  Blist.bind gen_right_rules defs

let gen_left_rules (def, ident) =
  let left_rule seq =
    try
      let (l,r) = Sl_seq.dest seq in
      let preds = Inds.filter (fun (_, (ident', _)) -> Strng.equal ident ident') l.SH.inds in
      if Inds.is_empty preds then [] else
      let left_unfold ((id,(_,pvs)) as p) =
        let l' = { l with SH.inds=Inds.remove p l.SH.inds } in
        let do_case case =
          let (f', (_,vs')) = Sl_indrule.dest (Sl_indrule.freshen (Sl_seq.vars seq) case) in
          (* FIXME assumes distinct vars in ind pred def *)
          let theta = Term.Map.of_list (Blist.combine vs' pvs) in
          let f' = Sl_heap.subst theta f' in
          let f' = Sl_heap.repl_tags id f' in
          let l' = Sl_heap.star l' f' in
					let l' = Sl_heap.univ (Sl_heap.vars r) l' in
          let ts = Tags.inter (Sl_heap.tags l') (Sl_heap.tags l) in
          (([l'], [r]), TagPairs.mk ts, TagPairs.singleton (id, id)) in
        Blist.map do_case def, (ident ^ " L.Unf.") in
      Inds.map_to_list left_unfold preds
    with Not_symheap -> [] in
  wrap left_rule  

(* s2 *)
(* -- *)
(* s1 *)
(* where there exists a substitution theta such that *)
(* s2[theta] entails s1 by classical weakening *)
let matches s1 s2 =
  let tags = Tags.inter (Sl_seq.tags s1) (Sl_seq.tags s2) in
  if Tags.is_empty tags then [] else
  let res = Sl_seq.uni_subsumption s1 s2 in
  if Option.is_none res then [] else
  let theta = Option.get res in
  let s2' = Sl_seq.subst theta s2 in
  let tags' = Tags.fold
    (fun t acc ->
      let new_acc = Tags.add t acc in
      if Sl_seq.subsumed_wrt_tags new_acc s1 s2' then new_acc else acc
    ) tags Tags.empty in
  let () = assert (not (Tags.is_empty tags')) in
  [ ((TagPairs.mk tags',  "Backl"), theta) ]

(*    seq'     *)
(* ----------  *)
(* seq'[theta] *)
(* where seq'[theta] = seq *)
let subst_rule theta seq' seq = 
  if Sl_seq.equal (Sl_seq.subst theta seq') seq 
	then 
		[ [(seq', TagPairs.mk (Sl_seq.tags seq'), TagPairs.empty)], "Subst" ]
	else 
		[]

(*   F |- G * Pi'  *)
(* --------------- *)
(*   Pi * F |- G   *)
(* where seq' = F |- G * Pi' and seq = Pi * F |- G *)     
let weaken seq' seq = 
  if Sl_seq.subsumed_wrt_tags Tags.empty seq seq' then
    [ [(seq', TagPairs.mk (Tags.inter (Sl_seq.tags seq) (Sl_seq.tags seq')), TagPairs.empty)], "Weaken" ]
  else
    []

(* if there is a backlink achievable through substitution and classical *)
(* weakening then make the proof steps that achieve it explicit so that *)
(* actual backlinking can be done on Sl_seq.equal sequents *) 
let dobackl idx prf =
	let src_seq = Proof.get_seq idx prf in
	let targets = Rule.all_nodes idx prf in
	let apps = 
		Blist.map (fun idx' -> matches src_seq (Proof.get_seq idx' prf)) targets in
	let f targ_idx (p, theta) =
		let targ_seq = Proof.get_seq targ_idx prf in
    let subst_seq = Sl_seq.subst theta targ_seq in
    Rule.sequence [
      if Sl_seq.equal src_seq subst_seq
        then Rule.identity
        else Rule.mk_infrule (weaken subst_seq);
        
      if Term.Map.for_all Term.equal theta
        then Rule.identity
        else Rule.mk_infrule (subst_rule theta targ_seq);
         
      Rule.mk_backrule 
        true 
        (fun _ _ -> [targ_idx]) 
        (fun s s' -> if Sl_seq.equal s s' then [p] else [])
    ] in
	Rule.first 
	  (Blist.map2 
		  (fun idx' l -> Rule.first (Blist.map (f idx') l)) 
			targets 
			apps) idx prf

let axioms = ref (Rule.first [id_axiom ; ex_falso_axiom])

let rules = ref Rule.fail

let setup defs = 
  rules := Rule.first [
    lhs_disj_to_symheaps;
    rhs_disj_to_symheaps;
    simplify;
    
    Rule.choice [
      (* brl_matches; *)
			dobackl;
  		wrap pto_intro_rule;
  		wrap pred_intro_rule;
      instantiate_pto ;
      Rule.choice (mk_ruf defs);
      Rule.choice (Blist.map gen_left_rules defs)
    ] 
  ]
