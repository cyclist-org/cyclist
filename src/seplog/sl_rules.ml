open Lib


module SH = Sl_heap

exception Not_symheap = Sl_form.Not_symheap

module Proof = Proof.Make(Sl_seq)
module Rule = Proofrule.Make(Sl_seq)
module Seqtactics = Seqtactics.Make(Sl_seq)

type t_lemma_level =
    NO_LEMMAS
  | ONLY_WITH_PREDICATES
  | NON_EMPTY
  | ANY

let lemma_equal lemma lemma' = match lemma, lemma' with
  | NO_LEMMAS, NO_LEMMAS
  | ONLY_WITH_PREDICATES, ONLY_WITH_PREDICATES
  | NON_EMPTY, NON_EMPTY
  | ANY, ANY -> true
  | _, _ -> false


let lemma_level = ref ONLY_WITH_PREDICATES

let set_lemma_level level =
  lemma_level :=
    match level with
      | 0 -> NO_LEMMAS
      | 1 -> ONLY_WITH_PREDICATES
      | 2 -> NON_EMPTY
      | 3 -> ANY
      | _ -> raise (Arg.Bad "Unrecognised value for lemma application level")

let lemma_option_descr_str ?(line_prefix="\t") () =
  let default_str level = if lemma_equal !lemma_level level then " (default)" else "" in
    line_prefix ^
  "0 -- do not attempt to apply any lemmas" ^ (default_str NO_LEMMAS) ^
    "\n" ^ line_prefix ^
  "1 -- only apply lemmas containing predicate instances" ^ (default_str ONLY_WITH_PREDICATES) ^
    "\n" ^ line_prefix ^
  "2 -- only apply lemmas with non-empty spatial components" ^ (default_str NON_EMPTY) ^
    "\n" ^ line_prefix ^
  "3 -- attempt all applicable lemmas" ^ (default_str ANY)

(* TODO: Construct extra rule applications for the case that we need to do    *)
(*       some alpha-renaming, existential introduction or splitting of        *)
(*       existentials (i.e. when the right-hand side is not simply subsumed   *)
(*       the left-hand side). *)
let id_axiom =
  Rule.mk_axiom
    (fun ((cs, f), (cs', f')) ->
      let cs = Ord_constraints.close cs in
      Option.map
        (fun _ -> "Id")
        (Sl_unify.Unidirectional.realize
          (Sl_unify.Unidirectional.unify_tag_constraints
            ~update_check:Sl_unify.Unidirectional.modulo_entl cs' cs
          (Sl_unify.Unidirectional.mk_verifier
            (fun theta ->
              (not (Blist.is_empty f')) &&
              Blist.for_all
                (fun h ->
                  Option.is_some
                    (Blist.find_map
                      (fun h' ->
                        Sl_heap.classical_unify
                          ~update_check:Sl_unify.Unidirectional.modulo_entl
                          h' h Unification.trivial_continuation theta)
                      f'))
                f)))))

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
    (fun ((cs, hs),r) -> match hs with
      | [] | [_] -> []
      | _ ->
        [
          Blist.map (fun h -> (((cs, [h]),r), Sl_heap.tag_pairs h, Tagpairs.empty)) hs,
           "L. Or"
        ]
    )

(* break RHS disjunctions *)
let rhs_disj_to_symheaps_rl ((_, hs) as l, ((_, hs') as r)) =
  match hs', hs with
  | [], _ | [_], _ | _, [] | _, _::_::_ -> []
  | _ ->
  Blist.map
    (fun h ->
      [ ((l, (Sl_form.with_heaps r [h])),
          Sl_form.tag_pairs l, Tagpairs.empty) ], "R. Or" )
    hs'

let rhs_disj_to_symheaps = Rule.mk_infrule rhs_disj_to_symheaps_rl


(* Left Instantiation Rules *)

let lhs_instantiate_ex_tags (l, r) =
  let lhs_tags = Sl_form.tags l in
  let (exs, univs) = Tags.partition Tags.is_exist_var lhs_tags in
  if Tags.is_empty exs then []
  else
    let rhs_tags = Sl_form.tags r in
    let subst = Tagpairs.mk_free_subst (Tags.union lhs_tags rhs_tags) exs in
    [ [ (((Sl_form.subst_tags subst l), r),
          Tagpairs.union subst (Tagpairs.mk univs), Tagpairs.empty) ],
          "Inst. LHS Tags" ]

let lhs_instantiate_ex_vars ((l, r) as seq) =
  try
    let ((_, h), (_, h')) = Sl_seq.dest seq in
    let ex_vars = Sl_term.Set.filter Sl_term.is_exist_var (Sl_heap.vars h) in
    if Sl_term.Set.is_empty ex_vars then [] else
    [ [ ( ((Sl_form.with_heaps l [Sl_heap.univ (Sl_heap.vars h') h]), r),
          Sl_form.tag_pairs l,
          Tagpairs.empty) ],
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
      Blist.find
        (fun p' -> not (Pair.either (Pair.map Sl_term.is_exist_var p')))
        leqs in
    let leqs = Blist.filter (fun q -> q!=p) leqs in
    let l = SH.with_eqs l (Sl_uf.of_list leqs) in
    let (x,y) = if Sl_term.is_var x then p else (y,x) in
		let theta = Sl_subst.singleton x y in
    let (l',r') = Pair.map (Sl_heap.subst theta) (l,r) in
    [ [ (((Sl_form.with_heaps lhs [l']), (Sl_form.with_heaps rhs [r'])),
        Sl_form.tag_pairs lhs, (* OK since we didn't modify any tags *)
        Tagpairs.empty) ], "" ]
  with Not_symheap | Not_found -> []


(* substitute one equality in RHS involving an existential var *)
let eq_ex_subst_rule ((lhs, rhs) as seq) =
  try
    let (_, (_, r)) = Sl_seq.dest seq in
    let reqs = Sl_uf.bindings r.SH.eqs in
    let (x,y) as p =
      Blist.find
        (fun vs -> Pair.either (Pair.map Sl_term.is_exist_var vs))
        reqs in
    let reqs = Blist.filter (fun q -> q!=p) reqs in
    let r = SH.with_eqs r (Sl_uf.of_list reqs) in
    let theta = if Sl_term.is_exist_var x
      then Sl_subst.singleton x y
      else Sl_subst.singleton y x in
    let r' = Sl_heap.subst theta r in
    [ [ ((lhs, (Sl_form.with_heaps rhs [r'])),
        Sl_form.tag_pairs lhs,
        Tagpairs.empty) ], "" ]
  with Not_symheap | Not_found -> []

(* remove all RHS eqs that can be discharged *)
let eq_simplify ((lhs, rhs) as seq) =
  try
    let ((_, l), (_, r)) = Sl_seq.dest seq in
    let (disch, reqs) =
      Blist.partition
        (fun (x,y) ->
          (not (Pair.either (Pair.map Sl_term.is_exist_var (x,y))))
            && Sl_heap.equates l x y)
        (Sl_uf.bindings r.SH.eqs) in
    if Blist.is_empty disch then [] else
    [
      [
        ( (lhs, (Sl_form.with_heaps rhs [ SH.with_eqs r (Sl_uf.of_list reqs) ]) ),
          Sl_form.tag_pairs lhs,
          Tagpairs.empty )
      ], ""
    ]
  with Not_symheap -> []

(* remove all RHS deqs that can be discharged *)
let deq_simplify ((lhs, rhs) as seq) =
  try
    let ((_, l), (_, r)) = Sl_seq.dest seq in
    let (disch, rdeqs) =
      Sl_deqs.partition
        (fun (x,y) ->
          (not (Pair.either (Pair.map Sl_term.is_exist_var (x,y))))
            && Sl_heap.disequates l x y)
        r.SH.deqs in
    if Sl_deqs.is_empty disch then [] else
    [
      [
        ( (lhs, (Sl_form.with_heaps rhs [ SH.with_deqs r rdeqs ]) ),
          Sl_form.tag_pairs lhs,
          Tagpairs.empty )
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
          (fun c -> Tags.for_all Tags.is_free_var (Ord_constraints.Elt.tags c)
            && Ord_constraints.mem c cs))
        cs' in
    if Ord_constraints.is_empty discharged then [] else
    [
      [
        ( (lhs, (Sl_form.with_constraints rhs remaining)),
          Sl_form.tag_pairs lhs,
          Tagpairs.empty )
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
      [ [ ( ((cs, [l']), (cs', [r'])), Sl_heap.tag_pairs l, Tagpairs.empty ) ], "Pto Intro" ]
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
      let matches eq ((t, (id, vs)), (t', (id', vs'))) =
        Sl_predsym.equal id id' &&
        Blist.for_all (Fun.neg Sl_term.is_exist_var) vs &&
        Blist.for_all (Fun.neg Sl_term.is_exist_var) vs' &&
        Tags.is_free_var t &&
        (Tags.is_exist_var t' || Tags.Elt.equal t t') &&
        Blist.for_all2 eq vs vs' in
      let combine_eqs h h'=
        let (ts, ts') = Pair.map Sl_heap.vars (h, h') in
        let exs = Sl_term.Set.filter Sl_term.is_exist_var ts in
        let h_eqs =
          if Sl_term.Set.is_empty exs then
            h.SH.eqs
          else
            let theta =
              Sl_subst.mk_free_subst
                (Sl_term.Set.union ts ts')
                exs in
            Sl_uf.subst theta h.SH.eqs in
        let combined_eqs = Sl_uf.union h_eqs h'.SH.eqs in
        Sl_uf.equates combined_eqs in
      let (p,q) =
        Option.dest
          (Blist.find (matches (combine_eqs h h')) cp)
          (Fun.id)
          (Blist.find_opt (matches (Sl_heap.equates h)) cp) in
      let h = SH.del_ind h p in
      let h' = SH.del_ind h' q in
      let (t, t') = Pair.map Sl_tpred.tag (p, q) in
      let subst = Tagpairs.singleton (t' ,t) in
      let rl_name =
        if Tags.Elt.equal t t' then "Pred Intro"
        else "Tag.Inst+Pred.Intro" in
      [ [ ( (Sl_form.with_heaps l [h],
              Sl_form.subst_tags subst (Sl_form.with_heaps r [h'])),
            Sl_heap.tag_pairs h,
            Tagpairs.empty ) ], rl_name ]
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
        [ ( ((cs, [l']), (cs', [r'])), Sl_heap.tag_pairs l, Tagpairs.empty ) ], "Inst Pto"
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
              (fun (_, (t, t')) -> Tags.is_free_var t && Tags.Elt.equal t t')
              (fun (_, (t, t')) -> Tags.is_exist_var t && Tags.is_free_var t'))) in
      let subs =
        Unification.backtrack unifier singleton cs
          Unification.trivial_continuation
          Tagpairs.empty in
      if Blist.is_empty subs then None else
      let ruleapps = Blist.map
        (fun theta ->
          [ (l, Sl_form.subst_tags theta r), Sl_form.tag_pairs l, Tagpairs.empty ],
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
      let ts = Tags.filter Tags.is_free_var ts in
      if Tags.is_empty ts then None else
      let ruleapps = Tags.map_to_list
        (fun t' ->
          let theta = Tagpairs.singleton (t, t') in
          [ (l, Sl_form.subst_tags theta r), Sl_form.tag_pairs l, Tagpairs.empty ],
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
let bounds_intro_rl ((l, r) as seq) =
  try
    let (_, (cs, h)) = Sl_seq.dest seq in
    let f (cs, descr) =
      [ [ (l, Sl_form.with_constraints r cs),
          Sl_form.tag_pairs l,
          Tagpairs.empty ], (descr ^ " Intro") ] in
    let result = Ord_constraints.remove_schema cs (Sl_heap.tags h) in
    Option.dest [] f result
  with Not_symheap -> []
let bounds_intro = Rule.mk_infrule bounds_intro_rl


let ruf_rl defs seq =
  try
    let ((cs, l), (cs', r)) = Sl_seq.dest seq in
    let seq_vars = Sl_seq.vars seq in
    let seq_tags = Sl_seq.tags seq in
    let right_unfold ((tag, (ident,_)) as p) =
      if not (Sl_defs.mem ident defs) then [] else
      let r' = SH.del_ind r p in
      let cases = Sl_defs.unfold (seq_vars, seq_tags) p defs in
      let do_case f =
        let cs' =
          Ord_constraints.union cs'
            (Ord_constraints.generate ~augment:false tag (Sl_heap.tags f)) in
        let r' = Sl_heap.star r' f in
        let tps = Tagpairs.union (Sl_heap.tag_pairs l) (Ord_constraints.tag_pairs cs) in
        [ (((cs, [l]), (cs', [r'])), tps, Tagpairs.empty) ],
        ((Sl_predsym.to_string ident) ^ " R.Unf.") in
      Blist.map do_case cases in
    Blist.flatten (Sl_tpreds.map_to_list right_unfold r.SH.inds)
  with Not_symheap -> []

let ruf defs = wrap (ruf_rl defs)

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
          let new_cs =
            Ord_constraints.union cs
              (Ord_constraints.generate ~avoid:seq_tags tag (Sl_heap.tags f)) in
          let cclosure = Ord_constraints.close new_cs in
          let (vts, pts) =
            let collect tps = Tagpairs.endomap Pair.swap
              (Tagpairs.filter (fun (_, t) -> Tags.mem t seq_tags) tps) in
            Pair.map collect
              (Ord_constraints.all_pairs cclosure,
                Ord_constraints.prog_pairs cclosure) in
          let vts = Tagpairs.union vts (Tagpairs.mk (Sl_heap.tags l)) in
          ( ((new_cs, [Sl_heap.star l f]), (cs', [r])), vts, pts ) in
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
let matches ((lhs, rhs) as seq) =
  try
    let ((lcs, l),(rcs, r)) = Sl_seq.dest seq in
    fun ((lhs', rhs') as seq') ->
      try
        let ((lcs', l'),(rcs', r')) = Sl_seq.dest seq' in
        if Sl_tpreds.is_empty l'.SH.inds then [] else
        let lcs = Ord_constraints.close lcs in
        let lhs_check =
          Fun.disj
            Sl_unify.Unidirectional.is_substitution
            Sl_unify.Unidirectional.modulo_entl in
        Sl_unify.Unidirectional.realize (
          Unification.backtrack
          (Sl_heap.unify_partial
            ~update_check:lhs_check)
            l' l
          (Sl_unify.Unidirectional.unify_tag_constraints ~inverse:false
            ~update_check:lhs_check
            lcs' lcs
          (fun ((trm_subst, tag_subst) as state) ->
            let () = debug (fun _ -> "Checking results of unification for LHS:\n\t" ^
              "Term subst: " ^ (Sl_term.Map.to_string Sl_term.to_string trm_subst) ^ ", " ^
              "Tag subst: " ^ (Tagpairs.to_string tag_subst) ^ "\n\t" ^
              (Sl_form.to_string lhs) ^ "\n\t" ^ (Sl_form.to_string lhs')) in
            let lhs = Sl_form.with_constraints lhs lcs in
            let lhs' =
              Sl_form.subst_tags tag_subst (Sl_form.subst trm_subst lhs') in
            let (_, l') = Sl_form.dest lhs' in
            assert (Sl_form.subsumed ~total:false lhs' lhs) ;
            if not (Sl_heap.subsumed l' l) then
              if lemma_equal !lemma_level NO_LEMMAS then
                None
              else if lemma_equal !lemma_level ONLY_WITH_PREDICATES && Sl_tpreds.is_empty l'.SH.inds then
                None
              else if lemma_equal !lemma_level NON_EMPTY
                  && Sl_tpreds.is_empty l'.SH.inds && Sl_ptos.is_empty l'.SH.ptos then
                None
              else
                Option.some state
            else
            let () = debug (fun _ -> "Continuing with unification of RHS") in
            let (trm_theta, _) = Sl_subst.partition trm_subst in
            let (tag_theta, _) = Tagpairs.partition_subst tag_subst in
            let rcs' = Ord_constraints.close rcs' in
            let rhs_check =
              Fun.conj
                (Sl_unify.Bidirectional.updchk_inj_left
                  Sl_unify.Unidirectional.modulo_entl)
                (Sl_unify.Bidirectional.updchk_inj_right
                  Sl_unify.Unidirectional.is_substitution) in
            let bisubst =
              (Sl_heap.classical_biunify
                ~update_check:rhs_check
                r r'
              (Sl_unify.Bidirectional.unify_tag_constraints
                ~update_check:rhs_check
                rcs rcs'
              (fun (((trm_subst, tag_subst), (trm_subst', tag_subst')) as state) ->
                let () = debug (fun _ -> "Checking results of biunification for RHS:\n\t" ^
                  "Term subst': " ^ (Sl_term.Map.to_string Sl_term.to_string trm_subst') ^ ", " ^
                  "Tag subst': " ^ (Tagpairs.to_string tag_subst') ^ "\n\t" ^
                  "Term subst: " ^ (Sl_term.Map.to_string Sl_term.to_string trm_subst) ^ ", " ^
                  "Tag subst: " ^ (Tagpairs.to_string tag_subst) ^ "\n\t" ^
                  (Sl_form.to_string rhs') ^ "\n\t" ^ (Sl_form.to_string rhs)) in
                let rhs' = Sl_form.with_constraints rhs' rcs' in
                let rhs' =
                  Sl_form.subst_tags tag_subst' (Sl_form.subst trm_subst' rhs') in
                let rhs =
                  Sl_form.subst_tags tag_subst (Sl_form.subst trm_subst rhs) in
                assert (Sl_form.subsumed rhs rhs') ;
                Option.some state)))
              (Sl_unify.Unidirectional.empty_state, (trm_theta, tag_theta)) in
            Option.map
              (fun (_, (trm_subst', tag_subst')) ->
                let (trm_subst', _) = Sl_subst.partition trm_subst' in
                let (tag_subst', _) = Tagpairs.partition_subst tag_subst' in
                ((Sl_term.Map.union trm_subst trm_subst'),
                  (Tagpairs.union tag_subst tag_subst')))
              (bisubst))))
      with Not_symheap -> []
  with Not_symheap -> (fun _ -> [])


(*    seq'     *)
(* ----------  *)
(* seq'[theta] *)
(* where seq'[theta] = seq *)
let subst_rule (theta, tps) ((l', _) as seq') ((l, _) as seq) =
  if Sl_seq.equal seq (Sl_seq.subst_tags tps (Sl_seq.subst theta seq'))
  then
    let tagpairs = Tagpairs.filter
      (fun (t, t') -> (Tags.mem t' (Sl_form.tags l')) && (Tags.mem t (Sl_form.tags l)))
      (Tagpairs.reflect tps) in
    let unmapped = Tags.diff (Sl_form.tags l) (Tagpairs.projectl tagpairs) in
    let remaining = Tags.inter unmapped (Sl_form.tags l') in
    let tagpairs = Tagpairs.union tagpairs (Tagpairs.mk remaining) in
    [ [(seq', tagpairs, Tagpairs.empty)], "Subst" ]
  else
    let () = debug (fun _ -> "Unsuccessfully tried to apply substitution rule!") in
    []

(*   F |- G * Pi'  *)
(* --------------- *)
(*   Pi * F |- G   *)
(* where seq' = F |- G * Pi' and seq = Pi * F |- G *)
let weaken seq' seq =
  (* let () = debug (fun _ -> "Trying to apply weakening:") in *)
  (* let () = debug (fun _ -> Sl_seq.to_string seq') in        *)
  (* let () = debug (fun _ -> Sl_seq.to_string seq) in         *)
  if Sl_seq.subsumed seq seq' then
    [ [(seq', Sl_seq.tag_pairs seq', Tagpairs.empty)], "Weaken" ]
  else
    let () = debug (fun _ -> "Unsuccessfully tried to apply weakening rule!") in
    let () = debug (fun _ -> Sl_seq.to_string seq') in
    let () = debug (fun _ -> Sl_seq.to_string seq) in
    []

let left_transform_rule ((lhs', rhs') as seq') (lhs, rhs) =
  try
    let (lhs_cs', lhs_h') = Sl_form.dest lhs' in
    let (lhs_cs, lhs_h) = Sl_form.dest lhs in
    if Sl_form.equal rhs' rhs then
      let transform =
        Sl_unify.Unidirectional.realize
          ((Sl_heap.classical_unify
              ~update_check:Sl_unify.Unidirectional.modulo_entl
              lhs_h' lhs_h)
           (Sl_unify.Unidirectional.unify_tag_constraints
              ~update_check:Sl_unify.Unidirectional.modulo_entl
              lhs_cs' lhs_cs
           (Unification.trivial_continuation))) in
      if Option.is_some transform then
        let (_, tps) = Option.get transform in
        let tps = Tagpairs.reflect tps in
        [ [(seq', tps, Tagpairs.empty)], "L.Trans.Ex" ]
      else
        let () = debug (fun _ -> "Unsuccessfully tried to apply left transformation rule!") in
        []
    else
      let () = debug (fun _ -> "Unsuccessfully tried to apply left transformation rule - right-hand sides not equal!") in
      []
  with Not_symheap ->
    let () = debug (fun _ -> "Unsuccessfully tried to apply left transformation rule - one left-hand side not a symbolic heap!") in
    []

let right_transform_rule ((lhs', rhs') as seq') (lhs, rhs) =
  try
    let (rhs_cs', rhs_h') = Sl_form.dest rhs' in
    let (rhs_cs, rhs_h) = Sl_form.dest rhs in
    if Sl_form.equal lhs' lhs then
      let transform =
        Sl_unify.Unidirectional.realize
          ((Sl_heap.classical_unify
              ~update_check:Sl_unify.Unidirectional.modulo_entl
              rhs_h rhs_h')
           (Sl_unify.Unidirectional.unify_tag_constraints
              ~update_check:Sl_unify.Unidirectional.modulo_entl
              rhs_cs rhs_cs'
           (Unification.trivial_continuation))) in
      if Option.is_some transform then
        [ [(seq', Sl_seq.tag_pairs seq', Tagpairs.empty)], "R.Trans.Ex" ]
      else
        let () = debug (fun _ -> "Unsuccessfully tried to apply right transformation rule!") in
        []
    else
      let () = debug (fun _ -> "Unsuccessfully tried to apply right transformation rule - left-hand sides not equal!") in
      []
  with Not_symheap ->
    let () = debug (fun _ -> "Unsuccessfully tried to apply right transformation rule - one right-hand side not a symbolic heap!") in
    []

let apply_lemma (lemma_seq, ((lhs, rhs) as cont_seq)) ((lhs', rhs') as seq) =
  let () = debug (fun _ -> "Trying to apply lemma to subgoal: " ^ (Sl_seq.to_string seq)) in
  let () = debug (fun _ -> "Lemma: " ^ (Sl_seq.to_string lemma_seq)) in
  let () = debug (fun _ -> "Continuation: " ^ (Sl_seq.to_string cont_seq)) in
  let ((lcs, l),(rcs, r)) = Sl_seq.dest lemma_seq in
  let (cs, h) = Sl_form.dest lhs in
  assert ( Sl_ptos.subset r.SH.ptos h.SH.ptos ) ;
  assert ( Sl_tpreds.subset r.SH.inds h.SH.inds ) ;
  assert ( Ord_constraints.equal cs (Ord_constraints.union lcs rcs) ) ;
  (* The separating conjunction of the lemma antecedent and the frame may *)
  (* introduce more disequalities that simply the union *)
  assert ( Sl_deqs.subset (Sl_deqs.union l.SH.deqs r.SH.deqs) h.SH.deqs ) ;
  assert ( Sl_uf.subsumed l.SH.eqs h.SH.eqs ) ;
  assert ( Sl_uf.subsumed r.SH.eqs h.SH.eqs ) ;
  assert (
    Sl_uf.subsumed
      h.SH.eqs
      (Sl_uf.fold (Fun.curry Sl_uf.add) l.SH.eqs r.SH.eqs) ) ;
  try
    let (cs', h') = Sl_form.dest lhs' in
    let expected =
      Sl_heap.with_ptos
        (Sl_heap.with_inds l (Sl_tpreds.union l.SH.inds (Sl_tpreds.diff h.SH.inds r.SH.inds)))
        (Sl_ptos.union l.SH.ptos (Sl_ptos.diff h.SH.ptos r.SH.ptos)) in
    (* let () = debug (fun _ -> "Constraints match: " ^ (string_of_bool (Ord_constraints.equal lcs cs'))) in *)
    (* let () = debug (fun _ -> "Heap as expected: " ^ (string_of_bool (Sl_heap.equal h' expected))) in      *)
    (* let () = debug (fun _ -> "RHS match: " ^ (string_of_bool (Sl_form.equal rhs rhs'))) in                *)
    if (Ord_constraints.equal lcs cs')
        && (Sl_heap.equal h' expected) && (Sl_form.equal rhs rhs') then
      let (vts, pts) = Sl_seq.get_tracepairs seq cont_seq in
      [ [ (lemma_seq, (Sl_seq.tag_pairs lemma_seq), Tagpairs.empty) ;
          (cont_seq, vts, pts)
        ], "Lemma.App" ]
    else
      let () = debug (fun _ -> "Unsuccessfully tried to apply lemma - open node does not match expected!") in
      []
  with Not_symheap ->
    let () = debug (fun _ -> "Unsuccessfully tried to apply lemma - LHS of open node not a symbolic heap!") in
    []


let mk_backlink_rule_seq (trm_subst, tag_subst)
    ((src_lhs, src_rhs) as src_seq) (targ_idx, targ_seq) =
  let ((subst_lhs, subst_rhs) as subst_seq) =
    Sl_seq.subst trm_subst (Sl_seq.subst_tags tag_subst targ_seq) in
  let ((subst_lhs_cs, subst_lhs_h), (subst_rhs_cs, subst_rhs_h)) =
    Sl_seq.dest subst_seq in
  let ((src_lhs_cs, src_lhs_h), (src_rhs_cs, src_rhs_h)) =
    Sl_seq.dest src_seq in
  let src_lhs_cs = Ord_constraints.close src_lhs_cs in
  let subst_rhs_cs = Ord_constraints.close subst_rhs_cs in
  let lhs_transform =
    Sl_unify.Unidirectional.realize
      ((Sl_heap.classical_unify
          ~update_check:Sl_unify.Unidirectional.modulo_entl
          subst_lhs_h src_lhs_h)
       (Sl_unify.Unidirectional.unify_tag_constraints
          ~update_check:Sl_unify.Unidirectional.modulo_entl
          subst_lhs_cs src_lhs_cs
       (Unification.trivial_continuation))) in
  let rhs_transform =
    Sl_unify.Unidirectional.realize
      ((Sl_heap.classical_unify
          ~update_check:Sl_unify.Unidirectional.modulo_entl
          src_rhs_h subst_rhs_h)
       (Sl_unify.Unidirectional.unify_tag_constraints
          ~update_check:Sl_unify.Unidirectional.modulo_entl
          src_rhs_cs subst_rhs_cs
       (Unification.trivial_continuation))) in
  let () = debug (fun _ -> "Checking transform for LHS:\n\t" ^
    (Sl_form.to_string subst_lhs) ^ "\n\t" ^ (Sl_form.to_string src_lhs)) in
  assert (Option.is_some lhs_transform) ;
  let () = debug (fun _ -> "Checking transform for RHS:\n\t" ^
    (Sl_form.to_string subst_rhs) ^ "\n\t" ^ (Sl_form.to_string src_rhs)) in
  assert (Option.is_some rhs_transform) ;
  let (lhs_trm_transform, lhs_tag_transform) = Option.get lhs_transform in
  let (rhs_trm_transform, rhs_tag_transform) = Option.get rhs_transform in
  let transformed_lhs =
    Sl_form.subst_tags lhs_tag_transform
        (Sl_form.subst lhs_trm_transform subst_lhs) in
  let transformed_rhs =
    Sl_form.subst_tags rhs_tag_transform
      (Sl_form.subst rhs_trm_transform src_rhs) in
  let left_transformed_seq = (transformed_lhs, subst_rhs) in
  let right_transformed_seq = (src_lhs, transformed_rhs) in
  Rule.sequence [
    if Sl_seq.equal src_seq right_transformed_seq
      then Rule.identity
      else Rule.mk_infrule (right_transform_rule right_transformed_seq) ;

    if Sl_seq.equal right_transformed_seq left_transformed_seq
      then Rule.identity
      else Rule.mk_infrule (weaken left_transformed_seq) ;

    if Sl_seq.equal left_transformed_seq subst_seq
      then Rule.identity
      else Rule.mk_infrule (left_transform_rule subst_seq) ;

    if Sl_seq.equal subst_seq targ_seq
      then Rule.identity
      else Rule.mk_infrule (subst_rule (trm_subst, tag_subst) targ_seq) ;

    Rule.mk_backrule
      true
      (fun _ _ -> [targ_idx])
      (fun s s' -> [Sl_seq.tag_pairs s', "Backl"])
  ]

let mk_lemma_rule_seq (trm_subst, tag_subst) (src_lhs, src_rhs)
    (targ_idx, ((lhs, rhs) as targ_seq)) =
  let (cs, h) = Sl_form.dest src_lhs in
  let (trm_theta, _) = Sl_subst.partition trm_subst in
  let (tag_theta, _) = Tagpairs.partition_subst tag_subst in
  let subst_lhs = Sl_form.subst trm_subst (Sl_form.subst_tags tag_subst lhs) in
  let subst_rhs = Sl_form.subst trm_theta (Sl_form.subst_tags tag_theta rhs) in
  let subst_seq = (subst_lhs, subst_rhs) in
  (* let () = debug (fun _ -> "substituted seq is " ^ (Sl_seq.to_string subst_seq)) in *)
  let (subst_cs, subst_h) = Sl_form.dest (subst_lhs) in
  (* Calculate the frame *)
  let frame =
    Sl_ptos.fold (Fun.swap Sl_heap.del_pto) subst_h.SH.ptos
      (Sl_tpreds.fold (Fun.swap Sl_heap.del_ind) subst_h.SH.inds h) in
  (* let () = debug (fun _ -> "Calculated frame is " ^ (Sl_heap.to_string frame)) in *)
  (* Alpha-rename any clashing existential variables in the succedent of the lemma *)
  let ctxt_vars =
    Sl_term.Set.union (Sl_heap.terms frame) (Sl_form.terms src_rhs) in
  let ctxt_tags =
    Tags.union_of_list [
      (Sl_heap.tags frame) ;
      (Ord_constraints.tags cs) ;
      (Sl_form.tags src_rhs) ] in
  let clashed_tags =
    Tags.inter ctxt_tags
      (Tags.filter Tags.is_exist_var (Sl_form.tags subst_rhs)) in
  let clashed_vars =
    Sl_term.Set.inter ctxt_vars
      (Sl_term.Set.filter Sl_term.is_exist_var (Sl_form.terms subst_rhs)) in
  let all_tags = Tags.union ctxt_tags (Sl_seq.tags subst_seq) in
  let all_vars = Sl_term.Set.union ctxt_vars (Sl_seq.vars subst_seq) in
  let tag_subst' = Tagpairs.mk_ex_subst all_tags clashed_tags in
  let trm_subst' = Sl_subst.mk_ex_subst all_vars clashed_vars in
  let subst_rhs = Sl_form.subst trm_subst' (Sl_form.subst_tags tag_subst' subst_rhs) in
  (* Construct the new subgoals *)
  let lemma_seq =
    let subst_h =
      Sl_heap.with_eqs (Sl_heap.with_deqs subst_h h.SH.deqs) h.SH.eqs in
    ((cs, [subst_h]), subst_rhs) in
  (* let () = debug (fun _ -> (Sl_heap.to_string subst_h') ^ " * " ^ (Sl_heap.to_string frame) ^ " = " ^ (Sl_heap.to_string (Sl_heap.star subst_h' frame))) in *)
  let cont_seq = ((Sl_form.star (cs, [frame]) subst_rhs), src_rhs) in
  (* Construct the rule sequence *)
  Rule.compose_pairwise
    (Rule.mk_infrule (apply_lemma (lemma_seq, cont_seq)))
    [ mk_backlink_rule_seq
        (trm_theta, tag_theta) lemma_seq (targ_idx, targ_seq) ;
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

(* If there is a backlink achievable through substitution and classical   *)
(* weakening (possibly after applying a lemma), then make the proof steps *)
(* that achieve it explicit so that actual backlinking can be done on     *)
(* Sl_seq.equal sequents *)
let dobackl idx prf =
  let ((src_lhs, src_rhs) as src_seq) = Proof.get_seq idx prf in
  let matches = matches src_seq in
  let targets = Rule.all_nodes idx prf in
  let apps =
    Blist.bind
      (fun idx' -> Blist.map (Pair.mk idx') (matches (Proof.get_seq idx' prf)))
      targets in
  let f (targ_idx, ((theta, tagpairs) as subst)) =
    let () = debug (fun _ -> "Constructing backlink") in
    let targ_seq = Proof.get_seq targ_idx prf in
    let () = debug (fun _ -> "Target seq is " ^ (Int.to_string targ_idx) ^ ": " ^ (Sl_seq.to_string targ_seq)) in
    let () = debug (fun _ -> "Term Subst: " ^ (Sl_term.Map.to_string Sl_term.to_string theta)) in
    let () = debug (fun _ -> "Tag Subst: " ^ (Tagpairs.to_string tagpairs)) in
    let (subst_lhs, _) =
      Sl_seq.subst theta (Sl_seq.subst_tags tagpairs targ_seq) in
    let () = debug (fun _ -> "\t" ^ "Checking for subsumption:" ^ "\n\t\t" ^
      "subst_lhs: " ^ (Sl_form.to_string subst_lhs) ^ "\n\t\t" ^
      "src_lhs: " ^ (Sl_form.to_string src_lhs)) in
    if Sl_form.subsumed subst_lhs src_lhs then
      let () = debug (fun _ -> "\t\t" ^ "FULL") in
      let (theta, _) = Sl_subst.partition theta in
      let (tagpairs, _) = Tagpairs.partition_subst tagpairs in
      FULL (mk_backlink_rule_seq (theta, tagpairs) src_seq (targ_idx, targ_seq))
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
                Tags.exists Tags.is_free_var (Ord_constraints.Elt.tags c))
              cs)
          (ruf defs) ;
        luf defs ;
      ]
    ] ;
  let axioms = Rule.first [id_axiom ; ex_falso_axiom] in
  rules := Rule.combine_axioms axioms !rules ;
  if !use_invalidity_heuristic then
    rules := Rule.conditional (fun s -> not (Sl_invalid.check defs s)) !rules
