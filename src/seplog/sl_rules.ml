open Lib


module SH = Sl_heap

exception Not_symheap = Sl_form.Not_symheap

module Proof = Proof.Make(Sl_seq)
module Rule = Proofrule.Make(Sl_seq)
module Seqtactics = Seqtactics.Make(Sl_seq)

(* This is a flag which switches the behaviour of the lemma application  *)
(* functionality. Permissive lemma application allows matching to back-  *)
(* links which would require an left existential introduction step. This *)
(* is sound only if we also ensure there is a corresponding right exist- *)
(* ential introduction step when the variables appear in the succedent.  *)
(* Experiments show that allowing permissive lemma application does not  *)
(* actually prove more entailments in practice, so we make turn this     *)
(* flag off by default.                                                  *) 
let permissive_lemma_application = ref false

type t_lemma_level = 
    NO_LEMMAS
  | ONLY_WITH_PREDICATES
  | NON_EMPTY
  | ANY

let lemma_level = ref ONLY_WITH_PREDICATES

let set_lemma_level level = 
  lemma_level := 
    match level with
      | 0 -> NO_LEMMAS
      | 1 -> ONLY_WITH_PREDICATES
      | 2 -> NON_EMPTY
      | 3 -> ANY
      | _ -> raise (Arg.Bad "Unrecognised value for lemma application level")

let lemma_option_descr_str =
  "0 -- do not attempt to apply any lemmas" ^ "\n\t" ^
  "1 -- only apply lemmas containing predicate instances (default)" ^ "\n\t" ^
  "2 -- only apply lemmas with non-empty spatial components" ^ "\n\t" ^
  "3 -- attempt all applicable lemmas"

let id_axiom =
  Rule.mk_axiom 
    (fun (l,r) -> 
      Option.mk
        (Blist.for_all
          (fun h -> 
            Blist.exists
              (fun h' ->
                Option.is_some
                  (Sl_heap.classical_unify
                    ~sub_check:(fun _ t _ -> Sl_term.is_exist_var t)
                    h' h)) 
              r)
          l)
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
    (fun (l,r) -> if Blist.length l < 2 then [] else
      [ 
        Blist.map (fun h -> (([h],r), Sl_heap.tag_pairs h, Tagpairs.empty)) l,
         "L. Or" 
      ]
    )

(* break RHS disjunctions *)
let rhs_disj_to_symheaps =
  Rule.mk_infrule 
    (fun (l,r) -> if Blist.length r < 2 || Blist.length l <> 1 then [] else
      Blist.map 
        (fun s -> [ ((l,[s]), Sl_form.tag_pairs l, Tagpairs.empty) ], "R. Or") 
        r)

(* Instantiate LHS existential variables *)
let lhs_instantiate_ex_vars_rl seq =
  try
    let (l, r) = Sl_seq.dest seq in 
    let ex_vars = Sl_term.Set.filter Sl_term.is_exist_var (Sl_heap.vars l) in
    if Sl_term.Set.is_empty ex_vars then [] else 
    [ [ ( [Sl_heap.univ (Sl_heap.vars r) l], [r] ), 
          Sl_heap.tag_pairs l, 
          Tagpairs.empty ], 
        "Inst. LHS Vars" ]
  with Not_symheap -> []
  
let lhs_instantiate_ex_vars = Rule.mk_infrule lhs_instantiate_ex_vars_rl

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
		let theta = Sl_subst.singleton x y in
    let (l',r') = Pair.map (Sl_heap.subst theta) (l,r) in
    [ [ (([l'], [r']), Sl_heap.tag_pairs l, Tagpairs.empty) ], "" ]
  with Not_symheap | Not_found -> []


(* substitute one equality in RHS involving an existential var *)
let eq_ex_subst_rule seq =
  try
    let (l,r) = Sl_seq.dest seq in
		let reqs = Sl_uf.bindings r.SH.eqs in
    let (x,y) as p = 
      Blist.find (fun p' -> Pair.disj (Pair.map Sl_term.is_exist_var p')) reqs in
		let reqs = Blist.filter (fun q -> q!=p) reqs in
    let (x,y) = if Sl_term.is_exist_var x then p else (y,x) in
    let r = SH.with_eqs r (Sl_uf.of_list reqs) in
    let r' = Sl_heap.subst (Sl_subst.singleton x y) r in
    [ [ (([l], [r']), Sl_heap.tag_pairs l, Tagpairs.empty) ], "" ]
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
        Tagpairs.empty) 
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
        Tagpairs.empty) 
      ], "" 
    ]
  with Not_symheap -> []

let norm seq =
  let seq' = Sl_seq.norm seq in
  if Sl_seq.equal seq seq' then [] else 
  [ [ (seq', Sl_seq.tag_pairs seq', Tagpairs.empty) ], "" ]

let simplify_rules = [
  eq_subst_rule ;
  eq_ex_subst_rule ;
  eq_simplify ;
  deq_simplify ;
  norm ;
]

let simplify_seq = 
  Seqtactics.relabel "Simplify"
    (Seqtactics.repeat (Seqtactics.first simplify_rules)) 

let simplify = Rule.mk_infrule simplify_seq

let wrap r =
  (Rule.mk_infrule (Seqtactics.compose r (Seqtactics.attempt simplify_seq)))


(* do the following transformation for the first x such that *)
(* x->y * A |- x->z * B     if     A |- y=z * B *)
let pto_intro_rule seq =
  try
    let (l,r) = Sl_seq.dest seq in
    let (rx, rys) as p =
      Sl_ptos.find (fun (w,_) -> Option.is_some (Sl_heap.find_lval w l)) r.SH.ptos in
    let (lx, lys) as p' = Option.get (Sl_heap.find_lval rx l) in
    (* avoid scope jumping *)
    if Blist.exists Sl_term.is_exist_var lys then [] else
    (* take care to remove only the 1st match *)
    let l' = SH.del_pto l p' in
    let r' = SH.del_pto r p in
    let r' = SH.with_eqs r' (Sl_uf.union r'.SH.eqs (Sl_uf.of_list (Blist.combine rys lys))) in
    [ [ ( ([l'], [r']), Sl_heap.tag_pairs l, Tagpairs.empty ) ], "Pto Intro" ]
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
    [ [ ( ([l'], [r']), Sl_heap.tag_pairs l', Tagpairs.empty ) ], "Pred Intro" ]
  with Not_symheap | Not_found -> []

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
        [ ( ([l'], [r']), Sl_heap.tag_pairs l, Tagpairs.empty ) ], "Inst ->"
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
          [ (([l], [Sl_heap.star r' f']), Sl_heap.tag_pairs l, Tagpairs.empty) ],
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
          (([l'], [r]), Tagpairs.union (Sl_heap.tag_pairs l) tagpairs, tagpairs) in
        Blist.map do_case cases, ((Sl_predsym.to_string ident) ^ " L.Unf.") in
      Sl_tpreds.map_to_list left_unfold l.SH.inds
    with Not_symheap -> [] in
  wrap
    (Seqtactics.compose rl
      (Seqtactics.attempt lhs_instantiate_ex_vars_rl))

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
    let verify = 
      Sl_unifier.mk_verifier
        (Sl_unifier.mk_assert_check
          (fun (theta, _) -> 
            Sl_seq.subsumed_upto_tags seq (Sl_seq.subst theta seq'))) in 
    let cont ((theta, _) as state) = 
			let l' = Sl_heap.subst theta  l' in
      assert (Sl_heap.subsumed_upto_tags ~total:false l' l) ;
      if not (Sl_heap.subsumed_upto_tags l' l) then 
        if !lemma_level = NO_LEMMAS then
          None
        else if !lemma_level = ONLY_WITH_PREDICATES && Tags.is_empty (Sl_heap.tags l') then
          None
        else if !lemma_level = NON_EMPTY && Sl_heap.subsumed Sl_heap.empty l' then
          None
        else if !permissive_lemma_application then
          Some state
        else
          (* If we are not allowing permissive lemma application then *)
          (* we must check any free variable that is mapped to an     *)
          (* existential is not also present in the succedent         *)
          let succ_fvs = Sl_term.Set.filter Sl_term.is_free_var (Sl_heap.terms r) in
          Option.mk
            (Sl_term.Map.for_all
              (fun t t' ->
                Sl_term.is_exist_var t ||
                Sl_term.is_free_var t' ||
                not (Sl_term.Set.mem t succ_fvs))
              theta)
            state
      else
        Sl_heap.classical_unify 
          ~inverse:true ~sub_check:Sl_subst.basic_lhs_down_check ~cont:verify 
          ~init_state:state r r' in
    let results =
      Sl_unifier.backtrack  
        (Sl_heap.unify_partial ~tagpairs:true)
        ~sub_check:Sl_subst.basic_lhs_down_check
        ~cont:cont 
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
		[ [(seq', Sl_seq.tag_pairs seq', Tagpairs.empty)], "Subst" ]
	else 
		[]

(*   F |- G * Pi'  *)
(* --------------- *)
(*   Pi * F |- G   *)
(* where seq' = F |- G * Pi' and seq = Pi * F |- G *)     
let weaken seq' seq = 
  if Sl_seq.subsumed seq seq' then
    [ [(seq', Sl_seq.tag_pairs seq', Tagpairs.empty)], "Weaken" ]
  else
    []
		
let apply_lemma (lemma_seq, ((lhs, rhs) as cont_seq)) ((lhs', rhs') as seq) =
  let () = debug (fun _ -> "Trying to apply lemma to subgoal: " ^ (Sl_seq.to_string seq)) in
  let () = debug (fun _ -> "Lemma: " ^ (Sl_seq.to_string lemma_seq)) in
  let () = debug (fun _ -> "Continuation: " ^ (Sl_seq.to_string cont_seq)) in
  let (l, r) = Sl_seq.dest lemma_seq in
  let h = Sl_form.dest lhs in
  try
    let h' = Sl_form.dest lhs' in
    assert ( Sl_ptos.subset l.SH.ptos h'.SH.ptos ) ;
    assert ( Sl_tpreds.subset l.SH.inds h'.SH.inds ) ;
    assert ( Sl_ptos.subset r.SH.ptos h.SH.ptos ) ;
    assert ( Sl_tpreds.subset r.SH.inds h.SH.inds ) ;
    (* The separating conjunction of the lemma antecedent and the frame may *)
    (* introduce more disequalities that simply the union *)
    assert ( Sl_deqs.subset (Sl_deqs.union l.SH.deqs r.SH.deqs) h.SH.deqs ) ;
    assert ( Sl_uf.subsumed l.SH.eqs h.SH.eqs ) ;
    assert ( Sl_uf.subsumed r.SH.eqs h.SH.eqs ) ;
    assert ( 
      Sl_uf.subsumed 
        h.SH.eqs 
        (Sl_uf.fold (Fun.curry Sl_uf.add) l.SH.eqs r.SH.eqs) ) ;
    let _ = debug (fun _ -> "Before") in
    let _ = debug (fun _ -> "Lem. Ant. Preds: " ^ (Sl_tpreds.to_string (l.SH.inds))) in
    let _ = debug (fun _ -> "Lem. Suc. Preds: " ^ (Sl_tpreds.to_string (r.SH.inds))) in
    let _ = debug (fun _ -> "Cont. Ant. Preds: " ^ (Sl_tpreds.to_string (h.SH.inds))) in
    let _ = debug (fun _ -> "Frame Preds: " ^ (Sl_tpreds.to_string (Sl_tpreds.diff h.SH.inds r.SH.inds))) in
    let _ = debug (fun _ -> "Expected Preds: " ^ (Sl_tpreds.to_string (Sl_tpreds.union l.SH.inds (Sl_tpreds.diff h.SH.inds r.SH.inds)))) in
    let expected = 
      Sl_heap.with_ptos 
        (Sl_heap.with_inds l (Sl_tpreds.union l.SH.inds (Sl_tpreds.diff h.SH.inds r.SH.inds))) 
        (Sl_ptos.union l.SH.ptos (Sl_ptos.diff h.SH.ptos r.SH.ptos)) in
    let _ = debug (fun _ -> "After") in
    (* let () = debug (fun _ -> "Heap as expected: " ^ (string_of_bool (Sl_heap.equal h' expected))) in      *)
    (* let () = debug (fun _ -> "RHS match: " ^ (string_of_bool (Sl_form.equal rhs rhs'))) in                *)
    if (Sl_heap.equal h' expected) && (Sl_form.equal rhs rhs') then
      let cont_tags = Tagpairs.inter (Sl_form.tag_pairs lhs) (Sl_form.tag_pairs lhs') in 
      [ [ (lemma_seq, (Sl_seq.tag_pairs lemma_seq), Tagpairs.empty) ;
          (cont_seq, cont_tags, Tagpairs.empty) 
        ], "Lemma.App" ]        
    else
      let () = debug (fun _ -> "Unsuccessfully tried to apply lemma - open node does not match expected!") in
      []
  with Not_symheap -> 
    let () = debug (fun _ -> "Unsuccessfully tried to apply lemma - LHS of open node not a symbolic heap!") in
    []

let mk_backlink_rule_seq (theta, tagpairs) src_seq (targ_idx, targ_seq) =
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
      (fun s s' -> [Tagpairs.reflect tagpairs, "Backl"])
  ]

let mk_lemma_rule_seq (trm_subst, tag_subst) (src_lhs, src_rhs) 
    (targ_idx, ((lhs, rhs) as targ_seq)) =
  let _ = debug (fun _ -> "Constructing Lemma Application Rule Sequence") in
  let _ = debug (fun _ -> "Backlink target is: " ^ "[" ^ (string_of_int targ_idx) ^ "]" ^ (Sl_seq.to_string targ_seq)) in
  let _ = debug (fun _ -> "Source Sequent is: " ^ (Sl_seq.to_string (src_lhs, src_rhs))) in
  let _ = debug (fun _ -> "Substitution is: " ^ (Sl_term.Subst.to_string trm_subst)) in
  let h = Sl_form.dest src_lhs in
  let (trm_theta, var_theta) = Sl_term.partition_subst trm_subst in
  let _ = debug (fun _ -> "Partitioned substitution is:") in
  let _ = debug (fun _ -> "\t" ^ (Sl_term.Subst.to_string trm_theta)) in
  let _ = debug (fun _ -> "\t" ^ (Sl_term.Subst.to_string var_theta)) in
  let subst_lhs = Sl_form.subst trm_subst (Sl_form.subst_tags tag_subst lhs) in
  let subst_rhs = 
    if not !permissive_lemma_application then 
      Sl_form.subst trm_theta rhs
    else
      (* If we allow permissive lemma application, then we must make sure *)
      (* there is right existential introduction where it is required     *)
      let rhs_ex_tx = Sl_term.Map.filter (fun k v -> Sl_term.is_free_var k && Sl_term.is_exist_var v) var_theta in
      let rhs_free_to_nil = Sl_term.Map.filter (fun k v -> Sl_term.is_free_var k && Sl_term.is_nil v) var_theta in
      let (_, rhs_free_to_nil) =
        Sl_term.Map.fold
          (fun t _ (fresh, theta) -> ((Blist.tl fresh), Sl_term.Map.add t (Blist.hd fresh) theta))
          (rhs_free_to_nil)
          ((Sl_term.fresh_evars
              (Sl_term.Set.union (Sl_form.terms rhs) (Sl_form.terms src_lhs))
              (Sl_term.Map.cardinal rhs_free_to_nil)),
            rhs_free_to_nil) in
      let rhs_var_theta =
        Sl_term.Map.of_list
          ((Sl_term.Map.bindings trm_theta)
            @ (Sl_term.Map.bindings rhs_ex_tx)
            @ (Sl_term.Map.bindings rhs_free_to_nil)) in
      let _ = debug (fun _ -> "Generated RHS existential substitution is:") in
      let _ = debug (fun _ -> "\t" ^ (Sl_term.Subst.to_string rhs_var_theta)) in
      Sl_form.subst rhs_var_theta rhs in
  let subst_seq = (subst_lhs, subst_rhs) in
  (* let () = debug (fun _ -> "substituted seq is " ^ (Sl_seq.to_string subst_seq)) in *)
  let subst_h = Sl_form.dest (subst_lhs) in
  (* Calculate the frame *)
  let frame =
    Sl_ptos.fold (Fun.swap Sl_heap.del_pto) subst_h.SH.ptos
      (Sl_tpreds.fold (Fun.swap Sl_heap.del_ind) subst_h.SH.inds h) in
  (* let () = debug (fun _ -> "Calculated frame is " ^ (Sl_heap.to_string frame)) in *)
  (* Alpha-rename any clashing existential variables in the succedent of the lemma *)
  let ctxt_vars = 
    Sl_term.Set.union (Sl_heap.terms frame) (Sl_form.terms src_rhs) in
  let clashed_vars =
    Sl_term.Set.inter ctxt_vars
      (Sl_term.Set.filter Sl_term.is_exist_var (Sl_form.terms subst_rhs)) in
  let all_vars = Sl_term.Set.union ctxt_vars (Sl_seq.vars subst_seq) in
  let trm_subst' = Sl_term.mk_ex_subst all_vars clashed_vars in 
  let subst_rhs = Sl_form.subst trm_subst' subst_rhs in
  (* freshen the tags in the right-hand side so they don't clash with any in the frame *)
  let subst_rhs = List.map (Sl_heap.freshen_tags h) subst_rhs in	
  (* Construct the new subgoals *)
  let lemma_seq =
    let subst_h = 
      Sl_heap.with_eqs (Sl_heap.with_deqs subst_h h.SH.deqs) h.SH.eqs in  
    ([subst_h], subst_rhs) in
  (* let () = debug (fun _ -> (Sl_heap.to_string subst_h') ^ " * " ^ (Sl_heap.to_string frame) ^ " = " ^ (Sl_heap.to_string (Sl_heap.star subst_h' frame))) in *)
  let cont_seq = ((Sl_form.star [frame] subst_rhs), src_rhs) in
  let targ_tags_subst = Sl_seq.subst_tags tag_subst targ_seq in
  (* Construct the rule sequence *)
  Rule.compose_pairwise
    (Rule.mk_infrule (apply_lemma (lemma_seq, cont_seq)))
    [ (Rule.sequence [
        (if Sl_seq.equal lemma_seq subst_seq then Rule.identity else
         Rule.mk_infrule 
           (fun seq -> [[(subst_seq, Sl_seq.tag_pairs subst_seq, Tagpairs.empty)], "Weaken"])) ;
        (if Sl_seq.equal subst_seq targ_tags_subst then Rule.identity else
         Rule.mk_infrule
          (fun seq -> [[(targ_tags_subst, Sl_seq.tag_pairs targ_tags_subst, Tagpairs.empty)], "Subst"])) ;
        (Rule.mk_backrule true 
          (fun _ _ -> [targ_idx]) 
          (fun s s' -> [Tagpairs.reflect tag_subst, "Backl"])) ]) ; 
      Rule.identity ]

type backlink_t = FULL of Rule.t | PARTIAL of Rule.t

let dest_taggedrule = function
  | FULL(r) -> r
  | PARTIAL(r) -> r
  
let cmp_taggedrule r r' = 
  match (r, r') with
  | (FULL(_), PARTIAL(_)) -> -1
  | (PARTIAL(_), FULL(_)) -> 1
  | _ -> 0


(** if there is a backlink achievable through substitution and
    classical weakening or lemma application then make the proof
    steps that achieve it explicit so that actual backlinking can
    be done on Sl_seq.equal sequents
 **) 
let dobackl idx prf =
  let ((src_lhs, src_rhs) as src_seq) = Proof.get_seq idx prf in
	let targets = Rule.all_nodes idx prf in
	let apps = 
		Blist.bind
      (fun idx' -> 
        Blist.map 
          (fun res -> (idx',res))
          (matches src_seq (Proof.get_seq idx' prf))) 
      targets in
	let f (targ_idx, ((theta, tagpairs) as subst)) =
		let targ_seq = Proof.get_seq targ_idx prf in
    let (subst_lhs, _) = 
      Sl_seq.subst theta (Sl_seq.subst_tags tagpairs targ_seq) in 
    let () = debug (fun _ -> "\t" ^ "Checking for subsumption:" ^ "\n\t\t" ^
      "subst_lhs: " ^ (Sl_form.to_string subst_lhs) ^ "\n\t\t" ^
      "src_lhs: " ^ (Sl_form.to_string src_lhs)) in
    if Sl_form.subsumed subst_lhs src_lhs then
      let () = debug (fun _ -> "\t\t" ^ "FULL") in
      FULL (mk_backlink_rule_seq subst src_seq (targ_idx, targ_seq))
    else
      let () = debug (fun _ -> "\t\t" ^ "PARTIAL") in
      PARTIAL (mk_lemma_rule_seq subst src_seq (targ_idx, targ_seq)) in
  (* Although application of all the constructed rule sequences will *)
  (* succeed by construction the backlinking may fail to satisfy the *)
  (* soundness condition, and so we pick the first one that is sound *)
  (* and we also prefer full backlinking to lemma application        *)
  let rules = 
    Blist.map dest_taggedrule 
      (Blist.stable_sort cmp_taggedrule (Blist.map f apps)) in
	Rule.first rules idx prf

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
      lhs_instantiate_ex_vars;
      simplify;
      
      Rule.choice [
  			dobackl;
    		wrap pto_intro_rule;
    		wrap pred_intro_rule;
        instantiate_pto ;
        ruf defs;
        luf defs 
      ] 
    ] ;
  let axioms = Rule.first [id_axiom ; ex_falso_axiom] in
  rules := Rule.combine_axioms axioms !rules ;
  if !use_invalidity_heuristic then 
    rules := Rule.conditional (fun s -> not (Sl_invalid.check defs s)) !rules
