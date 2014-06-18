open Lib
open Util
open Symheap

module Proof = Proof.Make(Seq)
module Rule = Proofrule.Make(Seq)
module Seqtactics = Seqtactics.Make(Seq)

let id_axiom =
  Rule.mk_axiom 
    (fun (l,r) -> Option.mk (Form.subsumed_wrt_tags Tags.empty r l) "Id")

let ex_falso_axiom =
  Rule.mk_axiom (fun (l,_) -> Option.mk (Form.inconsistent l) "Ex Falso")

(* break LHS disjunctions *)
let lhs_disj_to_symheaps =
  Rule.mk_infrule 
    (fun (l,r) -> if Blist.length l < 2 then [] else
      [ 
        Blist.map (fun h -> (([h],r), Heap.tag_pairs h, TagPairs.empty)) l,
         "L. Or" 
      ]
    )
    

(* break RHS disjunctions *)
let rhs_disj_to_symheaps =
  Rule.mk_infrule 
    (fun (l,r) -> if Blist.length r < 2 || Blist.length l <> 1 then [] else
      Blist.map 
        (fun s -> [ ((l,[s]), Form.tag_pairs l, TagPairs.empty) ], "R. Or") 
        r)

(* simplification rules *)

(* substitute all equalities from LHS into sequent *)
(* for (x,y) substitute y over x as x<y *)
(* this means representatives of eq classes are the max elems *)
let eq_subst_rule seq =
  try
    let (l,r) = Seq.dest seq in
    if UF.is_empty l.eqs then [] else
		let leqs = UF.bindings l.eqs in
		let (x,y) as p = Blist.find (fun p' -> Pair.disj (Pair.map Term.is_var p')) leqs in
		let leqs = Blist.filter (fun q -> q!=p) leqs in
		let l = { l with eqs = UF.of_list leqs } in
		let (x,y) = if Term.is_var x then p else (y,x) in
		let theta = Term.singleton_subst x y in
    let (l',r') = Pair.map (fun z -> Heap.subst theta z) (l,r) in
    [ [ (([l'], [r']), Heap.tag_pairs l, TagPairs.empty) ], "" ]
  with Not_symheap | Not_found -> []


(* substitute all equalities in RHS involving an existential var *)
let eq_ex_subst_rule seq =
  try
    let (l,r) = Seq.dest seq in
		let reqs = UF.bindings r.eqs in
		let (x,y) as p = Blist.find (fun (x,_) -> Term.is_exist_var x) reqs in
		let reqs = Blist.filter (fun q -> q!=p) reqs in
    let r = { r with eqs=UF.of_list reqs } in
		let (x,y) = if Term.is_var x then p else (y,x) in
    let r' = Heap.subst (Term.singleton_subst x y) r in
    [ [ (([l], [r']), Heap.tag_pairs l, TagPairs.empty) ], "" ]
  with Not_symheap | Not_found -> []

(* remove all RHS eqs that can be discharged *)
let eq_simplify seq =
  try
    let (l,r) = Seq.dest seq in
    let (disch, reqs) =
			Blist.partition (fun (x,y) -> Heap.equates l x y) (UF.bindings r.eqs) in
    if disch=[] then [] else
    [ [ (([l], [ { r with eqs=UF.of_list reqs } ] ), Heap.tag_pairs l, TagPairs.empty) ], "" ]
  with Not_symheap -> []

(* remove all RHS deqs that can be discharged *)
let deq_simplify seq =
  try
    let (l,r) = Seq.dest seq in
    let (disch, rdeqs) =
			Deqs.partition (fun (x,y) -> Heap.disequates l x y) r.deqs in
    if Deqs.is_empty disch then [] else
    [ [ (([l], [ { r with deqs=rdeqs } ] ), Heap.tag_pairs l, TagPairs.empty) ], "" ]
  with Not_symheap -> []

(* do the following transformation for the first x such that *)
(* x->y * A |- x->z * B     if     A |- y=z * B *)
let pto_intro_rule seq =
  try
    let (l,r) = Seq.dest seq in
    let (rx, rys) as p =
      Ptos.find (fun (w,_) -> Option.is_some (Heap.find_lval w l)) r.ptos in
    let (lx, lys) as p' = Option.get (Heap.find_lval rx l) in
    (* take care to remove only the 1st match *)
    let l' = { l with ptos=Ptos.remove p' l.ptos } in
    let r' = { r with ptos=Ptos.remove p r.ptos } in
    let r' = { r' with eqs=UF.union r'.eqs (UF.of_list (Blist.combine rys lys)) } in
    [ [ ( ([l'], [r']), Heap.tag_pairs l, TagPairs.empty ) ], "Pto Intro" ]
  with Not_symheap | Not_found | Invalid_argument _ -> []

(* do the following transformation for the first i,j such that *)
(* P_i(x1,..,xn) * A |- P_j(x1,...,xn) * B     if     A |- B *)
let pred_intro_rule seq =
  try
    let (l,r) = Seq.dest seq in
    let (linds,rinds) = Pair.map Inds.elements (l.inds,r.inds) in
    let cp = Blist.cartesian_product linds rinds in
    let (p,q) =
      Blist.find
        (fun ((_,(id,vs)),(_,(id',vs'))) ->
          Strng.equal id id' &&
					Blist.for_all2 (fun x y -> Heap.equates l x y) vs vs') cp in
    let l' = { l with inds=Inds.remove p (Inds.of_list linds) } in
    let r' = { r with inds=Inds.remove q (Inds.of_list rinds) } in
    [ [ ( ([l'], [r']), Heap.tag_pairs l', TagPairs.empty ) ], "Pred Intro" ]
  with Not_symheap | Not_found -> []

let norm s =
  let s' = Seq.norm s in
  if Seq.equal s s' then [] else
  [ [( s', Seq.tag_pairs s', TagPairs.empty )], "" ]

let simpl_deqs seq =
  try
    let (l,r) = Seq.dest seq in
    let non_deq_vars =
			Term.Set.add Term.nil
				(Term.Set.union
				  (Heap.vars { l with deqs=Deqs.empty }) (Heap.vars r)) in
    let f p = Pair.conj (Pair.map (fun t -> Term.Set.mem t non_deq_vars) p) in
    let l' = { l with deqs=Deqs.filter f l.deqs } in
    if Heap.equal l l' then [] else
    [ [ (([l'], [r]), Heap.tag_pairs l', TagPairs.empty) ], "" ]
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
      let (l,r) = Seq.dest seq in
      let (lptos,rptos) = Pair.map Ptos.elements (l.ptos,r.ptos) in
      let eptos = Blist.filter (fun (x,_) -> Term.is_exist_var x) rptos in
      let match_ls xs ys =
        try
					Blist.exists2 (fun x y -> Heap.equates l x y) xs ys
				with Invalid_argument _ -> false in
      let cp = Blist.cartesian_product eptos lptos in
      let cp = Blist.filter (fun ((_,ys),(_,zs)) -> match_ls ys zs) cp in
      let do_instantiation (((x,ys) as p), ((w,zs) as q)) =
        let l' = { l with ptos=Ptos.remove q (Ptos.of_list lptos) } in
        let r' = { r with ptos=Ptos.remove p (Ptos.of_list rptos) } in
        let r' =
          { r' with eqs=UF.union r'.eqs (UF.of_list ((x,w)::(Blist.combine ys zs))) } in
        [ ( ([l'], [r']), Heap.tag_pairs l, TagPairs.empty ) ], "Inst ->"
      in Blist.map do_instantiation cp
    with Not_symheap | Invalid_argument _ -> [] in
  wrap rl 

let mk_ruf defs =
  let gen_right_rules (def,_) =
    let gen_rule case =
      let (_,(ident,_)) = Case.dest case in
      let right_rule seq =
        try
          let (l,r) = Seq.dest seq in
          let preds = Inds.filter (fun (_,(ident',_)) -> Strng.equal ident ident') r.inds in
          if Inds.is_empty preds then [] else
          let (f, (ident, vs)) = Case.dest (Case.freshen (Seq.vars seq) case) in
          let right_unfold ((_,(_,vs')) as p) =
            let r' = { r with inds=Inds.remove p r.inds } in
            (* NB assumes distinct vars in ind pred def *)
            let theta = Term.Map.of_list (Blist.combine vs vs') in
            let f' = Heap.subst theta f in
            [ (([l], [Heap.star r' f']), Heap.tag_pairs l, TagPairs.empty) ],
            (ident ^ " R.Unf.") in
          Inds.map_to_list right_unfold preds
        with Not_symheap -> [] in
      wrap right_rule in
    Blist.map gen_rule def in
  Blist.bind gen_right_rules defs

let gen_left_rules (def, ident) =
  let left_rule seq =
    try
      let (l,r) = Seq.dest seq in
      let preds = Inds.filter (fun (_, (ident', _)) -> Strng.equal ident ident') l.inds in
      if Inds.is_empty preds then [] else
      let left_unfold ((id,(_,pvs)) as p) =
        let l' = { l with inds=Inds.remove p l.inds } in
        let do_case case =
          let (f', (_,vs')) = Case.dest (Case.freshen (Seq.vars seq) case) in
          (* FIXME assumes distinct vars in ind pred def *)
          let theta = Term.Map.of_list (Blist.combine vs' pvs) in
          let f' = Heap.subst theta f' in
          let f' = Heap.repl_tags id f' in
          let l' = Heap.star l' f' in
					let l' = Heap.univ (Heap.vars r) l' in
          let ts = Tags.inter (Heap.tags l') (Heap.tags l) in
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
  [ ((TagPairs.mk tags',  "Backl"), theta) ]

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

(*   F |- G * Pi'  *)
(* --------------- *)
(*   Pi * F |- G   *)
(* where seq' = F |- G * Pi' and seq = Pi * F |- G *)     
let weaken seq' seq = 
  if Seq.subsumed_wrt_tags Tags.empty seq seq' then
    [ [(seq', TagPairs.mk (Tags.inter (Seq.tags seq) (Seq.tags seq')), TagPairs.empty)], "Weaken" ]
  else
    []

(* if there is a backlink achievable through substitution and classical *)
(* weakening then make the proof steps that achieve it explicit so that *)
(* actual backlinking can be done on Seq.equal sequents *) 
let dobackl idx prf =
	let src_seq = Proof.get_seq idx prf in
	let targets = Rule.all_nodes idx prf in
	let apps = 
		Blist.map (fun idx' -> matches src_seq (Proof.get_seq idx' prf)) targets in
	let f targ_idx (p, theta) =
		let targ_seq = Proof.get_seq targ_idx prf in
    let subst_seq = Seq.subst theta targ_seq in
    Rule.sequence [
      if Seq.equal src_seq subst_seq
        then Rule.identity
        else Rule.mk_infrule (weaken subst_seq);
        
      if Term.Map.for_all Term.equal theta
        then Rule.identity
        else Rule.mk_infrule (subst_rule theta targ_seq);
         
      Rule.mk_backrule 
        true 
        (fun _ _ -> [targ_idx]) 
        (fun s s' -> if Seq.equal s s' then [p] else [])
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
