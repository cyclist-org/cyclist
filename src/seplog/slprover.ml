open Lib
open Util
open Symheap

module SLP = Prover.Make(Symheap.Seq)(Symheap.Defs)
include SLP

let id_axiom =
  let ax (l,r) = Form.subsumed_wrt_tags Tags.empty r l in
  mk_axiom ax "Id"

let ex_falso_axiom =
  let ax (l,_) = Form.inconsistent l in
  mk_axiom ax "Ex Falso"


(* break LHS disjunctions *)
let lhs_disj_to_symheaps =
  let rl (l,r) =
    if Blist.length l < 2 then [] else
    [ Blist.map (fun h -> (([h],r), Heap.tag_pairs h, TagPairs.empty)) l ] in
  mk_inf_rule rl "L. Or"

(* break RHS disjunctions *)
let rhs_disj_to_symheaps =
  let rl (l,r) =
    if Blist.length r < 2 || Blist.length l <> 1 then [] else
    Blist.map (fun s -> [ ((l,[s]), Form.tag_pairs l, TagPairs.empty) ]) r in
  mk_inf_rule rl "R. Or"

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
    [ [ (([l'], [r']), Heap.tag_pairs l, TagPairs.empty) ] ]
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
    [ [ (([l], [r']), Heap.tag_pairs l, TagPairs.empty) ] ]
  with Not_symheap | Not_found -> []

(* remove all RHS eqs that can be discharged *)
let eq_simplify seq =
  try
    let (l,r) = Seq.dest seq in
    let (disch, reqs) =
			Blist.partition (fun (x,y) -> Heap.equates l x y) (UF.bindings r.eqs) in
    if disch=[] then [] else
    [ [ (([l], [ { r with eqs=UF.of_list reqs } ] ), Heap.tag_pairs l, TagPairs.empty) ] ]
  with Not_symheap -> []

(* remove all RHS deqs that can be discharged *)
let deq_simplify seq =
  try
    let (l,r) = Seq.dest seq in
    let (disch, rdeqs) =
			Deqs.partition (fun (x,y) -> Heap.disequates l x y) r.deqs in
    if Deqs.is_empty disch then [] else
    [ [ (([l], [ { r with deqs=rdeqs } ] ), Heap.tag_pairs l, TagPairs.empty) ] ]
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
    let l' = { l with ptos=Ptos.filter (fun q -> q!=p') l.ptos } in
    let r' = { r with ptos=Ptos.filter (fun q -> q!=p) r.ptos } in
    let r' = { r' with eqs=UF.union r'.eqs (UF.of_list (Blist.combine rys lys)) } in
    [ [ ( ([l'], [r']), Heap.tag_pairs l, TagPairs.empty ) ] ]
  with Not_symheap | Not_found -> []

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
    [ [ ( ([l'], [r']), Heap.tag_pairs l', TagPairs.empty ) ] ]
  with Not_symheap | Not_found -> []

let norm s =
  let s' = Seq.norm s in
  if Seq.equal s s' then [] else
  [ [( s', Seq.tag_pairs s', TagPairs.empty )] ]

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
    [ [ (([l'], [r]), Heap.tag_pairs l', TagPairs.empty) ] ]
  with Not_symheap -> []

let simplify_rules = [
  (* (norm, "norm") ; *)
  (eq_subst_rule, "= subst") ;
  (eq_ex_subst_rule, "= ex subst") ;
  (eq_simplify, "= simpl R") ;
  (deq_simplify, "!= simpl R") ;
	(* (simpl_deqs, "!= simpl L") ; *)
  (* (pto_intro_rule, "-> intro");    *)
  (* (pred_intro_rule, "pred intro")  *)
]

let simplify_seq_rl =
  Seq_tacs.repeat_tac (Seq_tacs.first (Blist.map fst simplify_rules))

let simplify_proof_rl =
  Proof_tacs.repeat_tac
    (Proof_tacs.first
      (Blist.map (fun (r,d) -> mk_inf_rule r d) simplify_rules))

let simplify =
  if !expand_proof then
    simplify_proof_rl
  else
    mk_inf_rule simplify_seq_rl "Simpl"

let simplify =
  let or_rules =
    Proof_tacs.first
      (Blist.map (fun (r,d) -> mk_inf_rule r d) simplify_rules) in
  Proof_tacs.repeat_tac or_rules

let wrap r d =
  if !expand_proof then
    Proof_tacs.then_tac
      (mk_inf_rule r d)
      (Proof_tacs.try_tac simplify_proof_rl)
  else
    mk_inf_rule
      (Seq_tacs.then_tac r (Seq_tacs.try_tac simplify_seq_rl))
      d

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
        [ ( ([l'], [r']), Heap.tag_pairs l, TagPairs.empty ) ]
      in Blist.map do_instantiation cp
    with Not_symheap -> [] in
  wrap rl "Inst ->"

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
            [ (([l], [Heap.star r' f']), Heap.tag_pairs l, TagPairs.empty) ] in
          Inds.map_to_list right_unfold preds
        with Not_symheap -> [] in
      wrap right_rule (ident ^ " R.Unf.")  in
    Blist.map gen_rule def in
  Blist.flatten (Blist.map gen_right_rules defs)

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
        Blist.map do_case def in
      Inds.map_to_list left_unfold preds
    with Not_symheap -> [] in
  wrap left_rule  (ident ^ " L.Unf.")

(* s2 *)
(* -- *)
(* s1 *)
let is_subsumed s1 s2 = Seq.subsumed_wrt_tags Tags.empty s1 s2

let matches s1 s2 =
  let tags = Tags.inter (Seq.tags s1) (Seq.tags s2) in
  if Tags.is_empty tags then None else
  let res = Seq.uni_subsumption s1 s2 in
  if Option.is_none res then None else
  let theta = Option.get res in
  let s2' = Seq.subst theta s2 in
  let tags' = Tags.fold
    (fun t acc ->
      let new_acc = Tags.add t acc in
      if Seq.subsumed_wrt_tags new_acc s1 s2' then new_acc else acc
    ) tags Tags.empty in
  let () = assert (not (Tags.is_empty tags')) in
  Some (TagPairs.mk tags')

let brl_matches = mk_back_rule matches "Backl"

let setup defs =
  let ruf = Proof_tacs.angelic_or_tac (mk_ruf defs) in
  let luf = Proof_tacs.angelic_or_tac (Blist.map gen_left_rules defs) in
  axiomset := [ ex_falso_axiom; id_axiom ] ;
  ruleset := [
    lhs_disj_to_symheaps;
    rhs_disj_to_symheaps;
    simplify;
    brl_matches;
		wrap pto_intro_rule "Pto Intro";
		wrap pred_intro_rule "Pred Intro";
    instantiate_pto;
		ruf ;
		luf ;
  ]

(* let setup defs =                                                        *)
(*   let ruf = Proof_tacs.angelic_or_tac (mk_ruf defs) in                  *)
(*   let luf = Proof_tacs.angelic_or_tac (Blist.map gen_left_rules defs) in *)
(*   axiomset := [ ex_falso_axiom; id_axiom ] ;                            *)
(*   ruleset := [                                                          *)
(*     lhs_disj_to_symheaps;                                               *)
(*     rhs_disj_to_symheaps;                                               *)
(*     (* simplify; *)                                                     *)
(*     brl_matches;                                                        *)
(* 		luf ;                                                               *)
(* 		ruf ;                                                               *)
(* 		wrap pto_intro_rule "Pto Intro";                                    *)
(* 		wrap pred_intro_rule "Pred Intro";                                  *)
(*     instantiate_pto;                                                    *)
(*   ]                                                                     *)
