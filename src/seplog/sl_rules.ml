open Lib
open Util

module SH = Sl_heap

exception Not_symheap = Sl_form.Not_symheap

module Proof = Proof.Make(Sl_seq)
module Rule = Proofrule.Make(Sl_seq)
module Seqtactics = Seqtactics.Make(Sl_seq)

let id_axiom =
  Rule.mk_axiom 
    (fun (l,r) -> Option.mk (Sl_form.subsumed r l) "Id")

let preddefs = ref Sl_defs.empty 

let ex_falso_axiom =
  Rule.mk_axiom 
    (fun (l,_) -> 
      Option.mk 
        (Sl_form.inconsistent l (*|| not (Sl_basepair.form_sat !preddefs l)*)) 
        "Ex Falso")

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

(* substitute one equality from LHS into sequent *)
(* for (x,y) substitute y over x as x<y *)
(* this means representatives of eq classes are the max elems *)
let eq_subst_rule seq =
  try
    let (l,r) = Sl_seq.dest seq in
		let leqs = Sl_uf.bindings l.SH.eqs in
		let (x,y) as p = 
      Blist.find (fun p' -> Pair.disj (Pair.map Sl_term.is_var p')) leqs in
		let leqs = Blist.filter (fun q -> q!=p) leqs in
		let l = SH.with_eqs l (Sl_uf.of_list leqs) in
		let (x,y) = if Sl_term.is_var x then p else (y,x) in
		let theta = Sl_term.singleton_subst x y in
    let (l',r') = Pair.map (Sl_heap.subst theta) (l,r) in
    [ [ (([l'], [r']), Sl_heap.tag_pairs l, TagPairs.empty) ], "" ]
  with Not_symheap | Not_found -> []


(* substitute one equality in RHS involving an existential var *)
let eq_ex_subst_rule seq =
  try
    let (l,r) = Sl_seq.dest seq in
		let reqs = Sl_uf.bindings r.SH.eqs in
		let (x,y) as p = Blist.find (fun (x,_) -> Sl_term.is_exist_var x) reqs in
		let reqs = Blist.filter (fun q -> q!=p) reqs in
    let r = SH.with_eqs r (Sl_uf.of_list reqs) in
    let r' = Sl_heap.subst (Sl_term.singleton_subst x y) r in
    [ [ (([l], [r']), Sl_heap.tag_pairs l, TagPairs.empty) ], "" ]
  with Not_symheap | Not_found -> []

(* remove all RHS eqs that can be discharged *)
let eq_simplify seq =
  try
    let (l,r) = Sl_seq.dest seq in
    let (disch, reqs) =
			Blist.partition 
        (fun (x,y) -> Sl_heap.equates l x y) (Sl_uf.bindings r.SH.eqs) in
    if disch=[] then [] else
    [ 
      [ 
        (([l], [ SH.with_eqs r (Sl_uf.of_list reqs) ] ), 
        Sl_heap.tag_pairs l, 
        TagPairs.empty) 
      ], "" 
    ]
  with Not_symheap -> []

(* remove all RHS deqs that can be discharged *)
let deq_simplify seq =
  try
    let (l,r) = Sl_seq.dest seq in
    let (disch, rdeqs) =
			Sl_deqs.partition (fun (x,y) -> Sl_heap.disequates l x y) r.SH.deqs in
    if Sl_deqs.is_empty disch then [] else
    [ 
      [ 
        (([l], [ SH.with_deqs r rdeqs ] ), 
        Sl_heap.tag_pairs l, 
        TagPairs.empty) 
      ], "" 
    ]
  with Not_symheap -> []

(* do the following transformation for the first x such that *)
(* x->y * A |- x->z * B     if     A |- y=z * B *)
let pto_intro_rule seq =
  try
    let (l,r) = Sl_seq.dest seq in
    let (rx, rys) as p =
      Sl_ptos.find (fun (w,_) -> Option.is_some (Sl_heap.find_lval w l)) r.SH.ptos in
    let (lx, lys) as p' = Option.get (Sl_heap.find_lval rx l) in
    (* take care to remove only the 1st match *)
    let l' = SH.del_pto l p' in
    let r' = SH.del_pto r p in
    let r' = SH.with_eqs r' (Sl_uf.union r'.SH.eqs (Sl_uf.of_list (Blist.combine rys lys))) in
    [ [ ( ([l'], [r']), Sl_heap.tag_pairs l, TagPairs.empty ) ], "Pto Intro" ]
  with Not_symheap | Not_found | Invalid_argument _ -> []

(* do the following transformation for the first i,j such that *)
(* P_i(x1,..,xn) * A |- P_j(x1,...,xn) * B     if     A |- B *)
let pred_intro_rule seq =
  try
    let (l,r) = Sl_seq.dest seq in
    let (linds,rinds) = Pair.map Sl_tpreds.elements (l.SH.inds,r.SH.inds) in
    let cp = Blist.cartesian_product linds rinds in
    let (p,q) =
      Blist.find
        (fun ((_,(id,vs)),(_,(id',vs'))) ->
          Sl_predsym.equal id id' &&
					Blist.for_all2 (fun x y -> Sl_heap.equates l x y) vs vs') cp in
    let l' = SH.del_ind l p in
    let r' = SH.del_ind r q in
    [ [ ( ([l'], [r']), Sl_heap.tag_pairs l', TagPairs.empty ) ], "Pred Intro" ]
  with Not_symheap | Not_found -> []

let simplify_rules = [
  eq_subst_rule ;
  eq_ex_subst_rule ;
  eq_simplify ;
  deq_simplify ;
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
      let (lptos,rptos) = Pair.map Sl_ptos.elements (l.SH.ptos,r.SH.ptos) in
      let eptos = Blist.filter (fun (x,_) -> Sl_term.is_exist_var x) rptos in
      let match_ls xs ys =
        try
					Blist.exists2 (fun x y -> Sl_heap.equates l x y) xs ys
				with Invalid_argument _ -> false in
      let cp = Blist.cartesian_product eptos lptos in
      let cp = Blist.filter (fun ((_,ys),(_,zs)) -> match_ls ys zs) cp in
      let do_instantiation (((x,ys) as p), ((w,zs) as q)) =
        let l' = SH.del_pto l q in
        let r' = SH.del_pto r p in
        let r' =
          SH.with_eqs r' (Sl_uf.union r'.SH.eqs (Sl_uf.of_list ((x,w)::(Blist.combine ys zs)))) in
        [ ( ([l'], [r']), Sl_heap.tag_pairs l, TagPairs.empty ) ], "Inst ->"
      in Blist.map do_instantiation cp
    with Not_symheap | Invalid_argument _ -> [] in
  wrap rl 

let ruf defs = 
  let rl seq = 
    try
      let (l,r) = Sl_seq.dest seq in
      let seq_vars = Sl_seq.vars seq in
      let right_unfold ((_, (ident,_)) as p) =
        if not (Sl_defs.mem ident defs) then [] else
        let cases = Sl_defs.get_def ident defs in 
        let r' = SH.del_ind r p in
        let do_case c = 
          let (f', _) = Sl_indrule.unfold seq_vars r' p c in
          let f' = Sl_heap.freshen_tags r' f' in
          [ (([l], [Sl_heap.star r' f']), Sl_heap.tag_pairs l, TagPairs.empty) ],
          ((Sl_predsym.to_string ident) ^ " R.Unf.") in
        Blist.map do_case cases in
      Blist.flatten (Sl_tpreds.map_to_list right_unfold r.SH.inds)  
    with Not_symheap -> [] in
  wrap rl 

      
let luf defs =
  let rl seq =
    try
      let (l,r) = Sl_seq.dest seq in
      let seq_vars = Sl_seq.vars seq in
      let left_unfold ((_, (ident, _)) as p) =
        let l = SH.with_inds l (Sl_tpreds.remove p l.SH.inds) in
        let cases = Sl_defs.unfold seq_vars l p defs in
        let do_case (f, tagpairs) =
          let l' = Sl_heap.star l f in
					let l' = Sl_heap.univ (Sl_heap.vars r) l' in
          (([l'], [r]), TagPairs.union (Sl_heap.tag_pairs l) tagpairs, tagpairs) in
        Blist.map do_case cases, ((Sl_predsym.to_string ident) ^ " L.Unf.") in
      Sl_tpreds.map_to_list left_unfold l.SH.inds
    with Not_symheap -> [] in
  wrap rl

(* seq' = (l',r') *)
(* ------------   *)
(* seq = (l ,r )  *)
(* where there exists a substitution theta such that *)
(* seq'[theta] entails seq by subsumption    *)
(* that is whenever *)
(* l subsumes l'[theta] *)
(* and *)
(* r'[theta] subsumes r *)
let matches seq seq' =
  try
    let (l,r), (l',r') = Pair.map Sl_seq.dest (seq, seq') in
    let verify ((theta, _) as state) =
      assert (Sl_seq.subsumed_upto_tags seq (Sl_seq.subst theta seq')) ;
      Some state in 
    let cont state = Sl_heap.classical_unify ~inverse:true verify state r r' in
    let results =
      Sl_term.backtrack  
        (Sl_heap.classical_unify ~tagpairs:true) 
        cont 
        (Sl_term.empty_subst, TagPairs.empty) 
        l' l in
    results
  with Not_symheap -> []
  

(*    seq'     *)
(* ----------  *)
(* seq'[theta] *)
(* where seq'[theta] = seq *)
let subst_rule theta seq' seq = 
  if Sl_seq.equal (Sl_seq.subst theta seq') seq 
	then 
		[ [(seq', Sl_seq.tag_pairs seq', TagPairs.empty)], "Subst" ]
	else 
		[]

(*   F |- G * Pi'  *)
(* --------------- *)
(*   Pi * F |- G   *)
(* where seq' = F |- G * Pi' and seq = Pi * F |- G *)     
let weaken seq' seq = 
  if Sl_seq.subsumed seq seq' then
    [ [(seq', Sl_seq.tag_pairs seq', TagPairs.empty)], "Weaken" ]
  else
    []

(* if there is a backlink achievable through substitution and classical *)
(* weakening then make the proof steps that achieve it explicit so that *)
(* actual backlinking can be done on Sl_seq.equal sequents *) 
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
    let targ_seq' = Sl_seq.subst_tags tagpairs targ_seq in 
    let subst_seq = Sl_seq.subst theta targ_seq' in
    Rule.sequence [
      if Sl_seq.equal src_seq subst_seq
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

let axioms = ref (Rule.first [id_axiom ; ex_falso_axiom])

let rules = ref Rule.fail

let setup defs =
  preddefs := defs ; 
  rules := 
    Rule.first [
      lhs_disj_to_symheaps;
      rhs_disj_to_symheaps;
      simplify;
      
      Rule.choice [
        (* brl_matches; *)
  			dobackl;
    		wrap pto_intro_rule;
    		wrap pred_intro_rule;
        instantiate_pto ;
        ruf defs;
        luf defs
      ] 
    ]