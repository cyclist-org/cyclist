open Lib
open Util

module SH = Sl_heap

exception Not_symheap = Sl_form.Not_symheap

module Proof = Proof.Make(Sl_seq)
module Rule = Proofrule.Make(Sl_seq)
module Seqtactics = Seqtactics.Make(Sl_seq)


let id_axiom =
  let update_check = Ord_constraints.mk_update_check 
    (Fun.list_disj 
      [ (fun (_, (t, t')) -> Tags.is_univ_var t && Tags.Elt.equal t t') ;
        (fun (_, (t, t')) -> Tags.is_exist_var t && Tags.is_univ_var t') ;
        (fun (state, (t, t')) -> Tags.is_exist_var t && Tags.is_exist_var t'
          && not (Tags.mem t' (TagPairs.projectr state))) ]) in
  Rule.mk_axiom
    (fun (((cs, _) as l), ((cs', _) as r)) ->
      let cs = Ord_constraints.close cs in
      let do_unify () = 
        Ord_constraints.unify cs' cs ~update_check
          Unification.trivial_continuation TagPairs.empty in
      Option.mk 
        (Sl_form.subsumed_upto_constraints ~total:true r l &&
          (Ord_constraints.subsumed cs cs' || Option.is_some (do_unify ())))
        "Id") 

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
    (fun ((cs, hs),r) -> if Blist.length hs < 2 then [] else
      [
        Blist.map (fun h -> (((cs, [h]),r), Sl_heap.tag_pairs h, TagPairs.empty)) hs,
         "L. Or"
      ]
    )

(* break RHS disjunctions *)
let rhs_disj_to_symheaps =
  Rule.mk_infrule
    (fun ((_, hs) as l, ((_, hs') as r)) -> 
      if Blist.length hs' < 2 || Blist.length hs <> 1 then [] else
      Blist.map
        (fun h -> 
          [ ((l, (Sl_form.with_heaps r [h])), 
              Sl_form.tag_pairs l, TagPairs.empty) ], "R. Or" )
        hs')
        
        
(* Left Instantiation Rules *)

let lhs_instantiate_ex_tags (l, r) =
  let lhs_tags = Sl_form.tags l in
  let (exs, univs) = Tags.partition Tags.is_exist_var lhs_tags in
  if Tags.is_empty exs then []
  else
    let rhs_tags = Sl_form.tags r in
    let new_tags = Tags.fresh_uvars (Tags.union lhs_tags rhs_tags) (Tags.cardinal exs) in
    let subst = TagPairs.of_list (Blist.combine (Tags.to_list exs) new_tags) in
    [ [ (((Sl_form.subst_tags subst l), r), 
          TagPairs.union subst (TagPairs.mk univs), TagPairs.empty) ],
          "Inst. LHS Tags" ] 
  
let lhs_instantiate_ex_vars ((l, r) as seq) =
  try
    let ((_, h), (_, h')) = Sl_seq.dest seq in 
    let ex_vars = Sl_term.Set.filter Sl_term.is_exist_var (Sl_heap.vars h) in
    if Sl_term.Set.is_empty ex_vars then [] else 
    [ [ ( ((Sl_form.with_heaps l [Sl_heap.univ (Sl_heap.vars h') h]), r), 
          Sl_form.tag_pairs l, 
          TagPairs.empty) ], 
        "Inst. LHS Vars" ]
  with Not_symheap -> []
  
let lhs_instantiation_rules = [
    lhs_instantiate_ex_tags ;
    lhs_instantiate_ex_vars ;
  ]

let lhs_instantiate_seq =
  Seqtactics.relabel "LHS Inst."
    (Seqtactics.repeat (Seqtactics.first lhs_instantiation_rules))

let lhs_instantiate = Rule.mk_infrule lhs_instantiate_seq

(* simplification rules *)

(* substitute one equality from LHS into sequent *)
(* for (x,y) substitute y over x as x<y *)
(* this means representatives of eq classes are the max elems *)
let eq_subst_rule ((lhs, rhs) as seq) =
  try
    let ((_, l),(_, r)) = Sl_seq.dest seq in
    let leqs = Sl_uf.bindings l.SH.eqs in
    let (x,y) as p =
      Blist.find (fun p' -> Pair.disj (Pair.map Sl_term.is_var p')) leqs in
    let leqs = Blist.filter (fun q -> q!=p) leqs in
    let l = SH.with_eqs l (Sl_uf.of_list leqs) in
    let (x,y) = if Sl_term.is_var x then p else (y,x) in
    let theta = Sl_term.singleton_subst x y in
    let (l',r') = Pair.map (Sl_heap.subst theta) (l,r) in
    [ [ (((Sl_form.with_heaps lhs [l']), (Sl_form.with_heaps rhs [r'])), 
        Sl_form.tag_pairs lhs, (* OK since we didn't modify any tags *)
        TagPairs.empty) ], "" ]
  with Not_symheap | Not_found -> []


(* substitute one equality in RHS involving an existential var *)
let eq_ex_subst_rule ((lhs, rhs) as seq) =
  try
    let ((_, l),(_, r)) = Sl_seq.dest seq in
    let reqs = Sl_uf.bindings r.SH.eqs in
    let (x,y) as p = Blist.find (fun (x,_) -> Sl_term.is_exist_var x) reqs in
    let reqs = Blist.filter (fun q -> q!=p) reqs in
    let r = SH.with_eqs r (Sl_uf.of_list reqs) in
    let r' = Sl_heap.subst (Sl_term.singleton_subst x y) r in
    [ [ ((lhs, (Sl_form.with_heaps rhs [r'])), 
        Sl_form.tag_pairs lhs, 
        TagPairs.empty) ], "" ]
  with Not_symheap | Not_found -> []

(* remove all RHS eqs that can be discharged *)
let eq_simplify ((lhs, rhs) as seq) =
  try
    let ((_, l), (_, r)) = Sl_seq.dest seq in
    let (disch, reqs) =
      Blist.partition
        (fun (x,y) -> Sl_heap.equates l x y) (Sl_uf.bindings r.SH.eqs) in
    if disch=[] then [] else
    [
      [
        ( (lhs, (Sl_form.with_heaps rhs [ SH.with_eqs r (Sl_uf.of_list reqs) ]) ),
          Sl_form.tag_pairs lhs,
          TagPairs.empty )
      ], ""
    ]
  with Not_symheap -> []

(* remove all RHS deqs that can be discharged *)
let deq_simplify ((lhs, rhs) as seq) =
  try
    let ((_, l), (_, r)) = Sl_seq.dest seq in
    let (disch, rdeqs) =
      Sl_deqs.partition (fun (x,y) -> Sl_heap.disequates l x y) r.SH.deqs in
    if Sl_deqs.is_empty disch then [] else
    [
      [
        ( (lhs, (Sl_form.with_heaps rhs [ SH.with_deqs r rdeqs ]) ),
          Sl_form.tag_pairs lhs,
          TagPairs.empty )
      ], ""
    ]
  with Not_symheap -> []
  
(* Remove all RHS constraints that can be discharged *)
let constraint_simplify ((lhs, rhs) as seq) =
  try
    let ((cs, _), (cs', _)) = Sl_seq.dest seq in
    let cs = Ord_constraints.close cs in
    let (discharged, remaining) = 
      Ord_constraints.partition
        (Fun.disj
          (fun c -> Ord_constraints.Elt.valid c) 
          (fun c -> Tags.for_all Tags.is_univ_var (Ord_constraints.Elt.tags c)
            && Ord_constraints.mem c cs))
        cs' in
    if Ord_constraints.is_empty discharged then [] else
    [
      [
        ( (lhs, (Sl_form.with_constraints rhs remaining)),
          Sl_form.tag_pairs lhs,
          TagPairs.empty )
      ], ""
    ]
  with Not_symheap -> []

let norm seq =
  let seq' = Sl_seq.norm seq in
  if Sl_seq.equal seq seq' then [] else
  [ [ (seq', Sl_seq.tag_pairs seq', TagPairs.empty) ], "" ]

let simplify_rules = [
  eq_subst_rule ;
  eq_ex_subst_rule ;
  eq_simplify ;
  deq_simplify ;
  constraint_simplify ;
  norm ;
]

let simplify_seq =
  Seqtactics.relabel "Simplify"
    (Seqtactics.repeat (Seqtactics.first simplify_rules))

let simplify = Rule.mk_infrule simplify_seq

let wrap r =
  (Rule.mk_infrule 
    (Seqtactics.compose r (Seqtactics.attempt simplify_seq)))


(* do the following transformation for the first x such that *)
(* x->y * A |- x->z * B     if     A |- y=z * B *)
let pto_intro_rule =
  let rl seq =
    try
      let ((cs, l), (cs', r)) = Sl_seq.dest seq in
      let (rx, rys) as p =
        Sl_ptos.find (fun (w,_) -> Option.is_some (Sl_heap.find_lval w l)) r.SH.ptos in
      let (lx, lys) as p' = Option.get (Sl_heap.find_lval rx l) in
      (* avoid scope jumping *)
      if Blist.exists Sl_term.is_exist_var lys then [] else
      (* take care to remove only the 1st match *)
      let l' = SH.del_pto l p' in
      let r' = SH.del_pto r p in
      let r' = SH.with_eqs r' (Sl_uf.union r'.SH.eqs (Sl_uf.of_list (Blist.combine rys lys))) in
      [ [ ( ((cs, [l']), (cs', [r'])), Sl_heap.tag_pairs l, TagPairs.empty ) ], "Pto Intro" ]
    with Not_symheap | Not_found | Invalid_argument _ -> [] in
  wrap rl

(* do the following transformation for the first P, (x_1,...,x_n) such that *)
(*   P[a](x_1, ..., x_n) * A |- P[b](x_1, ..., x_n) * B    if  A |- B[a/b]  *)
(* with [a] a universal tag and either [b] = [a] or [b] existential         *)
let pred_intro_rule =
  let rl ((l, r) as seq) =
    try
      let ((_, h), (_, h')) = Sl_seq.dest seq in
      let (linds,rinds) = Pair.map Sl_tpreds.elements (h.SH.inds,h'.SH.inds) in
      let cp = Blist.cartesian_product linds rinds in
      let (p,q) =
        Blist.find
          (fun ((t, (id, vs)), (t', (id', vs'))) ->
            Sl_predsym.equal id id' &&
            Tags.is_univ_var t && 
            (Tags.is_exist_var t' || Tags.Elt.equal t t') &&
            Blist.for_all2 (fun x y -> Sl_heap.equates h x y) vs vs') cp in
      let h = SH.del_ind h p in
      let h' = SH.del_ind h' q in
      let (t, t') = Pair.map Sl_tpred.tag (p, q) in
      let subst = TagPairs.singleton (t' ,t) in
      let rl_name =
        if Tags.Elt.equal t t' then "Pred Intro"
        else "Tag.Inst+Pred.Intro" in
      [ [ ( (Sl_form.with_heaps l [h], 
              Sl_form.subst_tags subst (Sl_form.with_heaps r [h'])), 
            Sl_heap.tag_pairs h, 
            TagPairs.empty ) ], rl_name ]
    with Not_symheap | Not_found -> [] in
  wrap rl

(* x->ys * A |- e->zs * B if  A |- ys=zs * B[x/e] where e existential *)
(* and at least one var in ys,zs is the same *)
(* multiple applications possible *)
let instantiate_pto =
  let rl seq =
    try
      let ((cs, l), (cs', r)) = Sl_seq.dest seq in
      let (lptos,rptos) = Pair.map Sl_ptos.elements (l.SH.ptos,r.SH.ptos) in
      let eptos = Blist.filter (fun (x,_) -> Sl_term.is_exist_var x) rptos in
      let match_ls xs ys =
        try
          (* avoid scope jumping *)
          not (Blist.exists Sl_term.is_exist_var xs) &&
            Blist.exists2 (fun x y -> Sl_heap.equates l x y) xs ys
        with Invalid_argument _ -> false in
      let cp = Blist.cartesian_product eptos lptos in
      let cp = Blist.filter (fun ((_,zs),(_,ys)) -> match_ls ys zs) cp in
      let do_instantiation (((x,ys) as p), ((w,zs) as q)) =
        let l' = SH.del_pto l q in
        let r' = SH.del_pto r p in
        let r' =
          SH.with_eqs r' (Sl_uf.union r'.SH.eqs (Sl_uf.of_list ((x,w)::(Blist.combine ys zs)))) in
        [ ( ((cs, [l']), (cs', [r'])), Sl_heap.tag_pairs l, TagPairs.empty ) ], "Inst Pto"
      in Blist.map do_instantiation cp
    with Not_symheap | Invalid_argument _ -> [] in
  wrap rl
  
(* ([a] <(=) [b], ...) : F |- ([c] <(=) [d], ...) : G            *)
(*   if ([a] <(=) [b], ...) : F |- theta((...) : G)              *)
(* where [a] and [b] universal and either :                      *)
(*   - [a] = [c] and [d] existential with theta = ([d], [b]); or *)
(*   - [b] = [d] and [c] existential with theta = ([c], [a])     *)
let constraint_match_tag_instantiate =
  let rl ((cs, _) as l, ((cs', _) as r)) =
    let do_instantiation c =
      let singleton = Ord_constraints.singleton c in
      let tags = Ord_constraints.tags singleton in
      if (Tags.for_all Tags.is_exist_var tags) then None else
      let unifier = 
        Ord_constraints.unify
          ~update_check:
            (Ord_constraints.mk_update_check (Fun.disj
              (fun (_, (t, t')) -> Tags.is_univ_var t && Tags.Elt.equal t t')
              (fun (_, (t, t')) -> Tags.is_exist_var t && Tags.is_univ_var t'))) in 
      let subs =
        Unification.backtrack unifier singleton cs 
          Unification.trivial_continuation
          TagPairs.empty in
      if Blist.is_empty subs then None else
      let ruleapps = Blist.map
        (fun theta -> 
          [ (l, Sl_form.subst_tags theta r), Sl_form.tag_pairs l, TagPairs.empty ], 
          "Inst.Tag (Match)" )
        subs in
      Option.some ruleapps in
    Option.dest [] Fun.id (Ord_constraints.find_map do_instantiation cs') in
  wrap rl

(* F |- ([b'] <= [a] ...) : G  if  F |- theta((...) : G)           *)
(*   where [a] universal, [b'] existential and theta = ([b'], [a]) *)
let upper_bound_tag_instantiate =
  let rl (l, ((cs, _) as r)) =
    let do_instantiation t =
      let ts = Ord_constraints.upper_bounds t cs in
      let ts = Tags.filter Tags.is_univ_var ts in
      if Tags.is_empty ts then None else
      let ruleapps = Tags.map_to_list
        (fun t' ->
          let theta = TagPairs.singleton (t, t') in 
          [ (l, Sl_form.subst_tags theta r), Sl_form.tag_pairs l, TagPairs.empty ],
          "Inst.Tag (Sel.UBound)" )
        ts in
      Some ruleapps in
    let ts = Tags.filter Tags.is_exist_var (Ord_constraints.tags cs) in
    let ruleapps = Tags.find_map do_instantiation ts in
    Option.dest [] Fun.id ruleapps in
  wrap rl
  
(* Lower and Upper Bound Constraint Introduction - do one of:               *)
(*   A |- b' <= a_1, ..., b' <= a_n : B  if  A |- B                         *)
(*   A |- a_1 <= b', ..., a_n <= b' : B  if  A |- B                         *)
(*   A |- a_1 < b', ..., a_n < b' : B    if  A |- B                         *)
(* where b' is a fresh existential tag not occurring in B and a_1, ..., a_n *)
(* can be any tags *)
let bounds_intro = 
  let rl ((l, r) as seq) =
    try
      let (_, (cs, h)) = Sl_seq.dest seq in
      let f (cs, descr) = 
        [ [ (l, Sl_form.with_constraints r cs), 
            Sl_form.tag_pairs l, 
            TagPairs.empty ], (descr ^ " Intro") ] in
      let result = Ord_constraints.remove_schema cs (Sl_heap.tags h) in
      Option.dest [] f result    
    with Not_symheap -> []
  in Rule.mk_infrule rl

  
let ruf defs =
  let rl seq =
    try
      let ((cs, l), (cs', r)) = Sl_seq.dest seq in
      let seq_vars = Sl_seq.vars seq in
      let seq_tags = Sl_seq.tags seq in
      let right_unfold ((tag, (ident,_)) as p) =
        if not (Sl_defs.mem ident defs) then [] else
        let r' = SH.del_ind r p in
        let cases = Sl_defs.unfold (seq_vars, seq_tags) p defs in
        let do_case f =
          let new_tags = Util.Tags.diff (Sl_heap.tags f) seq_tags in
          let cs' = Ord_constraints.union cs' (Ord_constraints.generate tag new_tags) in
          let r' = Sl_heap.star r' f in
          [ (((cs, [l]), (cs', [r'])), Sl_heap.tag_pairs l, TagPairs.empty) ],
          ((Sl_predsym.to_string ident) ^ " R.Unf.") in
        Blist.map do_case cases in
      Blist.flatten (Sl_tpreds.map_to_list right_unfold r.SH.inds)
    with Not_symheap -> [] in
  wrap rl


let luf defs =
  let rl seq =
    try
      let ((cs, l), (cs', r)) = Sl_seq.dest seq in
      let seq_vars = Sl_seq.vars seq in
      let seq_tags = Sl_seq.tags seq in
      let left_unfold ((tag, (ident, _)) as p) =
        if not (Sl_defs.mem ident defs) then None else
        let l = SH.with_inds l (Sl_tpreds.remove p l.SH.inds) in
        let cases = Sl_defs.unfold (seq_vars, seq_tags) p defs in
        let do_case f =
          let new_tags = Util.Tags.diff (Sl_heap.tags f) seq_tags in
          let new_cs = Ord_constraints.union cs (Ord_constraints.generate tag new_tags) in
          let cclosure = Ord_constraints.close new_cs in
          let (vt, pt) = 
            let collect tps = TagPairs.endomap Pair.swap
              (TagPairs.filter (fun (_, t) -> Tags.mem t seq_tags) tps) in
            Pair.map collect 
              (Ord_constraints.all_pairs cclosure, 
                Ord_constraints.prog_pairs cclosure) in
          ( ((new_cs, [Sl_heap.star l f]), (cs', [r])), vt, pt ) in
        Some (Blist.map do_case cases, ((Sl_predsym.to_string ident) ^ " L.Unf.")) in
      Option.list_get (Sl_tpreds.map_to_list left_unfold l.SH.inds)
    with Not_symheap -> [] in
  wrap 
    (Seqtactics.compose rl
      (Seqtactics.attempt lhs_instantiate_seq))

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
    let ((lcs, l),(rcs, r)), ((lcs', l'),(rcs', r')) =
      Pair.map Sl_seq.dest (seq, seq') in
    Sl_unify.realize (
      Unification.backtrack
      (Sl_heap.classical_unify ~inverse:false ~tagpairs:true
        ~update_check:(Fun.conj Sl_unify.trm_check Sl_unify.tag_check))
        l' l
      (Sl_heap.classical_unify ~inverse:true ~tagpairs:true 
        ~update_check:(Fun.conj Sl_unify.trm_check Sl_unify.tag_check) 
        r r'
      (Sl_unify.unify_tag_constraints ~inverse:false
        ~update_check:Sl_unify.tag_check
        lcs' lcs
      (Sl_unify.unify_tag_constraints ~inverse:true
        ~update_check:Sl_unify.tag_check
        rcs rcs'
      (Sl_unify.mk_verifier
        (Sl_unify.mk_assert_check
          (fun (theta, tps) ->
            let () = debug (fun _ -> "Checking results of unification for subsumption:\n\t" ^ 
              "Term subst: " ^ (Sl_term.Map.to_string Sl_term.to_string theta) ^ ", " ^
              "Tag subst: " ^ (Strng.Pairing.Set.to_string (TagPairs.to_names tps)) ^ "\n\t" ^
              (Sl_seq.to_string seq) ^ "\n\t" ^ (Sl_seq.to_string seq')) in
            let seq' = Sl_seq.subst_tags tps (Sl_seq.subst theta seq') in
            Sl_seq.subsumed seq seq')))))))
  with Not_symheap -> []


(*    seq'     *)
(* ----------  *)
(* seq'[theta] *)
(* where seq'[theta] = seq *)
let subst_rule (theta, tps) ((l', _) as seq') ((l, _) as seq) =
  if Sl_seq.equal seq (Sl_seq.subst_tags tps (Sl_seq.subst theta seq'))
  then
    let tagpairs = TagPairs.filter
      (fun (t, t') -> (Tags.mem t' (Sl_form.tags l')) && (Tags.mem t (Sl_form.tags l)))
      (TagPairs.reflect tps) in
    let unmapped = Tags.diff (Sl_form.tags l) (TagPairs.projectl tagpairs) in
    let remaining = Tags.inter unmapped (Sl_form.tags l') in
    let tagpairs = TagPairs.union tagpairs (TagPairs.mk remaining) in
    [ [(seq', tagpairs, TagPairs.empty)], "Subst" ]
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

      if (Sl_term.Map.for_all Sl_term.equal theta
          && TagPairs.for_all (Fun.uncurry Tags.Elt.equal) tagpairs)
        then Rule.identity
        else Rule.mk_infrule (subst_rule (theta, tagpairs) targ_seq);

      Rule.mk_backrule
        true
        (fun _ _ -> [targ_idx])
        (fun s s' -> [Sl_seq.tag_pairs s', "Backl"])
    ] in
  Rule.first (Blist.map f apps) idx prf

(* let axioms = ref (Rule.first [id_axiom ; ex_falso_axiom]) *)
let axioms = ref Rule.fail

let rules = ref Rule.fail

let use_invalidity_heuristic = ref false

let setup defs =
  preddefs := defs ;
  rules :=
    Rule.first [
      lhs_disj_to_symheaps;
      rhs_disj_to_symheaps;
      lhs_instantiate;
      simplify;

      bounds_intro ;
      constraint_match_tag_instantiate ;
      upper_bound_tag_instantiate ;

      Rule.choice [
        dobackl;
        pto_intro_rule;
        pred_intro_rule;
        instantiate_pto ;
        Rule.conditional 
          (fun (_, (cs, _)) -> 
            Ord_constraints.for_all
              (fun c -> 
                Tags.exists Tags.is_univ_var (Ord_constraints.Elt.tags c))
              cs)
          (ruf defs) ;
        luf defs ;
      ]
    ] ;
  let axioms = Rule.first [id_axiom ; ex_falso_axiom] in
  rules := Rule.combine_axioms axioms !rules ;
  if !use_invalidity_heuristic then
    rules := Rule.conditional (fun s -> not (Sl_invalid.check defs s)) !rules
