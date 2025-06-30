open Lib
open Generic

module SH = Heap

exception Not_symheap = Form.Not_symheap

module Proof = Proof.Make (Seq)
module Rule = Proofrule.Make (Seq)
module Seqtactics = Seqtactics.Make (Seq)

include (Rule : sig
  val set_default_select_f : int -> unit
  val default_select_f_descr : ?line_prefix:string -> unit -> string
end)

type t_lemma_level = NO_LEMMAS | ONLY_WITH_PREDICATES | NON_EMPTY | ANY

let lemma_equal lemma lemma' =
  match (lemma, lemma') with
  | NO_LEMMAS, NO_LEMMAS
  | ONLY_WITH_PREDICATES, ONLY_WITH_PREDICATES
  | NON_EMPTY, NON_EMPTY
  | ANY, ANY ->
    true
  | _, _ ->
    false

let lemma_level = ref ONLY_WITH_PREDICATES

let set_lemma_level level =
  lemma_level :=
    match level with
    | 0 -> NO_LEMMAS
    | 1 -> ONLY_WITH_PREDICATES
    | 2 -> NON_EMPTY
    | 3 -> ANY
    | _ -> raise (Arg.Bad "Unrecognised value for lemma application level")

let lemma_option_descr_str ?(line_prefix = "\t") () =
  let default_str level =
    if lemma_equal !lemma_level level then " (default)" else "" in
  line_prefix ^ "0 -- do not attempt to apply any lemmas"
  ^ default_str NO_LEMMAS ^ "\n" ^ line_prefix
  ^ "1 -- only apply lemmas containing predicate instances"
  ^ default_str ONLY_WITH_PREDICATES
  ^ "\n" ^ line_prefix
  ^ "2 -- only apply lemmas with non-empty spatial components"
  ^ default_str NON_EMPTY ^ "\n" ^ line_prefix
  ^ "3 -- attempt all applicable lemmas" ^ default_str ANY

(* TODO: Construct extra rule applications for the case that we need to do    *)
(*       some alpha-renaming, existential introduction or splitting of        *)
(*       existentials (i.e. when the right-hand side is not simply subsumed   *)
(*       the left-hand side). *)
let id_axiom =
  Rule.mk_axiom (fun ((cs, f), (cs', f')) ->
      let cs = Ord_constraints.close cs in
      Option.map
        (fun _ -> "Id")
        (Unify.Unidirectional.realize
           (Unify.Unidirectional.unify_tag_constraints
              ~update_check:Unify.Unidirectional.modulo_entl cs' cs
              (Unify.Unidirectional.mk_verifier (fun theta ->
                   (not (Blist.is_empty f'))
                   && Blist.for_all
                        (fun h ->
                          Option.is_some
                            (Blist.find_map
                               (fun h' ->
                                 Heap.classical_unify
                                   ~update_check:
                                     Unify.Unidirectional.modulo_entl h' h
                                   Unification.trivial_continuation theta )
                               f') )
                        f )))) )

let preddefs = ref Defs.empty

let ex_falso_axiom =
  Rule.mk_axiom (fun (l, _) ->
      Option.mk
        (Form.inconsistent l (*|| not (Basepair.form_sat !preddefs l)*))
        "Ex Falso" )

(* break LHS disjunctions *)
let lhs_disj_to_symheaps =
  Rule.mk_infrule (fun ((cs, hs), r) ->
      match hs with
      | [] | [_] -> []
      | _ ->
          [ ( Blist.map
                (fun h -> (((cs, [h]), r), Heap.tag_pairs h, Tagpairs.empty))
                hs
            , "L. Or" ) ] )

(* break RHS disjunctions *)
let rhs_disj_to_symheaps_rl (((_, hs) as l), ((_, hs') as r)) =
  match (hs', hs) with
  | [], _ | [_], _ | _, [] | _, _ :: _ :: _ -> []
  | _ ->
      Blist.map
        (fun h ->
          ( [ ( (l, Form.with_heaps r [h])
              , Form.tag_pairs l
              , Tagpairs.empty ) ]
          , "R. Or" ) )
        hs'

let rhs_disj_to_symheaps = Rule.mk_infrule rhs_disj_to_symheaps_rl

(* Left Instantiation Rules *)

let lhs_instantiate_ex_tags (l, r) =
  let lhs_tags = Form.tags l in
  let exs, univs = Tags.partition Tags.is_exist_var lhs_tags in
  if Tags.is_empty exs then []
  else
    let rhs_tags = Form.tags r in
    let subst = Tagpairs.mk_free_subst (Tags.union lhs_tags rhs_tags) exs in
    [ ( [ ( (Form.subst_tags subst l, r)
          , Tagpairs.union subst (Tagpairs.mk univs)
          , Tagpairs.empty ) ]
      , "Inst. LHS Tags" ) ]

let lhs_instantiate_ex_vars ((l, r) as seq) =
  try
    let (_, h), (_, h') = Seq.dest seq in
    let ex_vars = Term.Set.filter Term.is_exist_var (Heap.vars h) in
    if Term.Set.is_empty ex_vars then []
    else
      [ ( [ ( (Form.with_heaps l [Heap.univ (Heap.vars h') h], r)
            , Form.tag_pairs l
            , Tagpairs.empty ) ]
        , "Inst. LHS Vars" ) ]
  with Not_symheap -> []

let lhs_instantiation_rules = [lhs_instantiate_ex_tags; lhs_instantiate_ex_vars]

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
    let (_, l), (_, r) = Seq.dest seq in
    let leqs = Uf.bindings l.SH.eqs in
    let ((x, y) as p) =
      Blist.find
        (fun p' -> not (Pair.either (Pair.map Term.is_exist_var p')))
        leqs
    in
    let leqs = Blist.filter (fun q -> q != p) leqs in
    let l = SH.with_eqs l (Uf.of_list leqs) in
    let x, y = if Term.is_var x then p else (y, x) in
    let theta = Subst.singleton x y in
    let l', r' = Pair.map (Heap.subst theta) (l, r) in
    [ ( [ ( (Form.with_heaps lhs [l'], Form.with_heaps rhs [r'])
          , Form.tag_pairs lhs
          , (* OK since we didn't modify any tags *)
            Tagpairs.empty ) ]
      , "" ) ]
  with
  | Not_symheap | Not_found -> []

(* substitute one equality in RHS involving an existential var *)
let eq_ex_subst_rule ((lhs, rhs) as seq) =
  try
    let _, (_, r) = Seq.dest seq in
    let reqs = Uf.bindings r.SH.eqs in
    let ((x, y) as p) =
      Blist.find
        (fun vs -> Pair.either (Pair.map Term.is_exist_var vs))
        reqs
    in
    let reqs = Blist.filter (fun q -> q != p) reqs in
    let r = SH.with_eqs r (Uf.of_list reqs) in
    let theta =
      if Term.is_exist_var x then Subst.singleton x y
      else Subst.singleton y x
    in
    let r' = Heap.subst theta r in
    [ ( [ ( (lhs, Form.with_heaps rhs [r'])
          , Form.tag_pairs lhs
          , Tagpairs.empty ) ]
      , "" ) ]
  with
  | Not_symheap | Not_found -> []

(* remove all RHS eqs that can be discharged *)
let eq_simplify ((lhs, rhs) as seq) =
  try
    let (_, l), (_, r) = Seq.dest seq in
    let disch, reqs =
      Blist.partition
        (fun (x, y) ->
          (not (Pair.either (Pair.map Term.is_exist_var (x, y))))
          && Heap.equates l x y )
        (Uf.bindings r.SH.eqs)
    in
    if Blist.is_empty disch then []
    else
      [ ( [ ( (lhs, Form.with_heaps rhs [SH.with_eqs r (Uf.of_list reqs)])
            , Form.tag_pairs lhs
            , Tagpairs.empty ) ]
        , "" ) ]
  with Not_symheap -> []

(* remove all RHS deqs that can be discharged *)
let deq_simplify ((lhs, rhs) as seq) =
  try
    let (_, l), (_, r) = Seq.dest seq in
    let disch, rdeqs =
      Deqs.partition
        (fun (x, y) ->
          (not (Pair.either (Pair.map Term.is_exist_var (x, y))))
          && Heap.disequates l x y )
        r.SH.deqs
    in
    if Deqs.is_empty disch then []
    else
      [ ( [ ( (lhs, Form.with_heaps rhs [SH.with_deqs r rdeqs])
            , Form.tag_pairs lhs
            , Tagpairs.empty ) ]
        , "" ) ]
  with Not_symheap -> []

(* Remove all RHS constraints that can be discharged *)
let constraint_simplify ((lhs, rhs) as seq) =
  try
    let (cs, _), (cs', _) = Seq.dest seq in
    let cs = Ord_constraints.close cs in
    let discharged, remaining =
      Ord_constraints.partition
        (Fun.disj
           (fun c -> Ord_constraints.Elt.valid c)
           (fun c ->
             Tags.for_all Tags.is_free_var (Ord_constraints.Elt.tags c)
             && Ord_constraints.mem c cs ))
        cs'
    in
    if Ord_constraints.is_empty discharged then []
    else
      [ ( [ ( (lhs, Form.with_constraints rhs remaining)
            , Form.tag_pairs lhs
            , Tagpairs.empty ) ]
        , "" ) ]
  with Not_symheap -> []

let norm seq =
  let seq' = Seq.norm seq in
  if Seq.equal seq seq' then []
  else [([(seq', Seq.tag_pairs seq', Tagpairs.empty)], "")]

let simplify_rules =
  [ eq_subst_rule
  ; eq_ex_subst_rule
  ; eq_simplify
  ; deq_simplify
  ; constraint_simplify
  ; norm ]

let simplify_seq =
  Seqtactics.relabel "Simplify"
    (Seqtactics.repeat (Seqtactics.first simplify_rules))

let simplify = Rule.mk_infrule simplify_seq

let wrap r =
  Rule.mk_infrule (Seqtactics.compose r (Seqtactics.attempt simplify_seq))

(* do the following transformation for the first x such that *)
(* x->y * A |- x->z * B     if     A |- y=z * B *)
let pto_intro_rule =
  let rl seq =
    try
      let (cs, l), (cs', r) = Seq.dest seq in
      let ((rx, rys) as p) =
        Ptos.find_suchthat
          (fun (w, _) -> Option.is_some (Heap.find_lval w l))
          r.SH.ptos
      in
      let ((lx, lys) as p') = Option.get (Heap.find_lval rx l) in
      (* avoid scope jumping *)
      if Blist.exists Term.is_exist_var lys then []
      else
        (* take care to remove only the 1st match *)
        let l' = SH.del_pto l p' in
        let r' = SH.del_pto r p in
        let r' =
          SH.with_eqs r'
            (Uf.union r'.SH.eqs (Uf.of_list (Blist.combine rys lys)))
        in
        [ ( [(((cs, [l']), (cs', [r'])), Heap.tag_pairs l, Tagpairs.empty)]
          , "Pto Intro" ) ]
    with
    | Not_symheap | Not_found | Invalid_argument _ -> []
  in
  wrap rl

(* do the following transformation for the first P, (x_1,...,x_n) such that *)
(*   P[a](x_1, ..., x_n) * A |- P[b](x_1, ..., x_n) * B    if  A |- B[a/b]  *)
(* with [a] a universal tag and either [b] = [a] or [b] existential         *)
let pred_intro_rule =
  let rl ((l, r) as seq) =
    try
      let (_, h), (_, h') = Seq.dest seq in
      let linds, rinds = Pair.map Tpreds.elements (h.SH.inds, h'.SH.inds) in
      let cp = Blist.cartesian_product linds rinds in
      let matches eq ((t, (id, vs)), (t', (id', vs'))) =
        Predsym.equal id id'
        && Blist.for_all (Fun.neg Term.is_exist_var) vs
        && Blist.for_all (Fun.neg Term.is_exist_var) vs'
        && Tags.is_free_var t
        && (Tags.is_exist_var t' || Tags.Elt.equal t t')
        && Blist.for_all2 eq vs vs'
      in
      let combine_eqs h h' =
        let ts, ts' = Pair.map Heap.vars (h, h') in
        let exs = Term.Set.filter Term.is_exist_var ts in
        let h_eqs =
          if Term.Set.is_empty exs then h.SH.eqs
          else
            let theta =
              Subst.mk_free_subst (Term.Set.union ts ts') exs
            in
            Uf.subst theta h.SH.eqs
        in
        let combined_eqs = Uf.union h_eqs h'.SH.eqs in
        Uf.equates combined_eqs
      in
      let p, q =
        Option.dest
          (Blist.find (matches (combine_eqs h h')) cp)
          Fun.id
          (Blist.find_opt (matches (Heap.equates h)) cp)
      in
      let h = SH.del_ind h p in
      let h' = SH.del_ind h' q in
      let t, t' = Pair.map Tpred.tag (p, q) in
      let subst = Tagpairs.singleton (t', t) in
      let rl_name =
        if Tags.Elt.equal t t' then "Pred Intro" else "Tag.Inst+Pred.Intro"
      in
      [ ( [ ( ( Form.with_heaps l [h]
              , Form.subst_tags subst (Form.with_heaps r [h']) )
            , Heap.tag_pairs h
            , Tagpairs.empty ) ]
        , rl_name ) ]
    with
    | Not_symheap | Not_found -> []
  in
  wrap rl

(* x->ys * A |- e->zs * B if  A |- ys=zs * B[x/e] where e existential *)
(* and at least one var in ys,zs is the same *)
(* multiple applications possible *)
let instantiate_pto =
  let rl seq =
    try
      let (cs, l), (cs', r) = Seq.dest seq in
      let lptos, rptos = Pair.map Ptos.elements (l.SH.ptos, r.SH.ptos) in
      let eptos = Blist.filter (fun (x, _) -> Term.is_exist_var x) rptos in
      let match_ls xs ys =
        try
          (* avoid scope jumping *)
          (not (Blist.exists Term.is_exist_var xs))
          && Blist.exists2 (fun x y -> Heap.equates l x y) xs ys
        with Invalid_argument _ -> false
      in
      let cp = Blist.cartesian_product eptos lptos in
      let cp = Blist.filter (fun ((_, zs), (_, ys)) -> match_ls ys zs) cp in
      let do_instantiation (((x, ys) as p), ((w, zs) as q)) =
        let l' = SH.del_pto l q in
        let r' = SH.del_pto r p in
        let r' =
          SH.with_eqs r'
            (Uf.union r'.SH.eqs
               (Uf.of_list ((x, w) :: Blist.combine ys zs)))
        in
        ( [(((cs, [l']), (cs', [r'])), Heap.tag_pairs l, Tagpairs.empty)]
        , "Inst Pto" )
      in
      Blist.map do_instantiation cp
    with
    | Not_symheap | Invalid_argument _ -> []
  in
  wrap rl

(* ([a] <(=) [b], ...) : F |- ([c] <(=) [d], ...) : G            *)
(*   if ([a] <(=) [b], ...) : F |- theta((...) : G)              *)
(* where [a] and [b] universal and either :                      *)
(*   - [a] = [c] and [d] existential with theta = ([d], [b]); or *)
(*   - [b] = [d] and [c] existential with theta = ([c], [a])     *)
let constraint_match_tag_instantiate =
  let rl (((cs, _) as l), ((cs', _) as r)) =
    let do_instantiation c =
      let singleton = Ord_constraints.singleton c in
      let tags = Ord_constraints.tags singleton in
      if Tags.for_all Tags.is_exist_var tags then None
      else
        let unifier =
          Ord_constraints.unify
            ~update_check:
              (Ord_constraints.mk_update_check
                 (Fun.disj
                    (fun (_, (t, t')) ->
                      Tags.is_free_var t && Tags.Elt.equal t t' )
                    (fun (_, (t, t')) ->
                      Tags.is_exist_var t && Tags.is_free_var t' )))
        in
        let subs =
          Unification.backtrack unifier singleton cs
            Unification.trivial_continuation Tagpairs.empty
        in
        if Blist.is_empty subs then None
        else
          let ruleapps =
            Blist.map
              (fun theta ->
                ( [ ( (l, Form.subst_tags theta r)
                    , Form.tag_pairs l
                    , Tagpairs.empty ) ]
                , "Inst.Tag (Match)" ) )
              subs
          in
          Option.some ruleapps
    in
    Option.dest [] Fun.id (Ord_constraints.find_map do_instantiation cs')
  in
  wrap rl

(* F |- ([b'] <= [a] ...) : G  if  F |- theta((...) : G)           *)
(*   where [a] universal, [b'] existential and theta = ([b'], [a]) *)
let upper_bound_tag_instantiate =
  let rl (l, ((cs, _) as r)) =
    let do_instantiation t =
      let ts = Ord_constraints.upper_bounds t cs in
      let ts = Tags.filter Tags.is_free_var ts in
      if Tags.is_empty ts then None
      else
        let ruleapps =
          Tags.map_to_list
            (fun t' ->
              let theta = Tagpairs.singleton (t, t') in
              ( [ ( (l, Form.subst_tags theta r)
                  , Form.tag_pairs l
                  , Tagpairs.empty ) ]
              , "Inst.Tag (Sel.UBound)" ) )
            ts
        in
        Some ruleapps
    in
    let ts = Tags.filter Tags.is_exist_var (Ord_constraints.tags cs) in
    let ruleapps = Tags.find_map do_instantiation ts in
    Option.dest [] Fun.id ruleapps
  in
  wrap rl

(* Lower and Upper Bound Constraint Introduction - do one of:               *)
(*   A |- b' <= a_1, ..., b' <= a_n : B  if  A |- B                         *)
(*   A |- a_1 <= b', ..., a_n <= b' : B  if  A |- B                         *)
(*   A |- a_1 < b', ..., a_n < b' : B    if  A |- B                         *)
(* where b' is a fresh existential tag not occurring in B and a_1, ..., a_n *)
(* can be any tags *)
let bounds_intro_rl ((l, r) as seq) =
  try
    let _, (cs, h) = Seq.dest seq in
    let f (cs, descr) =
      [ ( [ ( (l, Form.with_constraints r cs)
            , Form.tag_pairs l
            , Tagpairs.empty ) ]
        , descr ^ " Intro" ) ]
    in
    let result = Ord_constraints.remove_schema cs (Heap.tags h) in
    Option.dest [] f result
  with Not_symheap -> []

let bounds_intro = Rule.mk_infrule bounds_intro_rl

let ruf_rl defs seq =
  try
    let (cs, l), (cs', r) = Seq.dest seq in
    let seq_vars = Seq.vars seq in
    let seq_tags = Seq.tags seq in
    let right_unfold ((tag, (ident, _)) as p) =
      if not (Defs.mem ident defs) then []
      else
        let r' = SH.del_ind r p in
        let cases = Defs.unfold (seq_vars, seq_tags) p defs in
        let do_case f =
          let cs' =
            Ord_constraints.union cs'
              (Ord_constraints.generate ~augment:false tag (Heap.tags f))
          in
          let r' = Heap.star r' f in
          let tps =
            Tagpairs.union (Heap.tag_pairs l) (Ord_constraints.tag_pairs cs)
          in
          ( [(((cs, [l]), (cs', [r'])), tps, Tagpairs.empty)]
          , Predsym.to_string ident ^ " R.Unf." )
        in
        Blist.map do_case cases
    in
    Blist.flatten (Tpreds.map_to_list right_unfold r.SH.inds)
  with Not_symheap -> []

let ruf defs = wrap (ruf_rl defs)

let luf defs =
  let rl seq =
    try
      let (cs, l), (cs', r) = Seq.dest seq in
      let seq_vars = Seq.vars seq in
      let seq_tags = Seq.tags seq in
      let left_unfold ((tag, (ident, _)) as p) =
        if not (Defs.mem ident defs) then None
        else
          let l = SH.with_inds l (Tpreds.remove p l.SH.inds) in
          let cases = Defs.unfold (seq_vars, seq_tags) p defs in
          let do_case f =
            let new_cs =
              Ord_constraints.union cs
                (Ord_constraints.generate ~avoid:seq_tags tag (Heap.tags f))
            in
            let cclosure = Ord_constraints.close new_cs in
            let vts, pts =
              let collect tps =
                Tagpairs.map Pair.swap
                  (Tagpairs.filter (fun (_, t) -> Tags.mem t seq_tags) tps)
              in
              Pair.map collect
                ( Ord_constraints.all_pairs cclosure
                , Ord_constraints.prog_pairs cclosure )
            in
            let vts = Tagpairs.union vts (Tagpairs.mk (Heap.tags l)) in
            (((new_cs, [Heap.star l f]), (cs', [r])), vts, pts)
          in
          Some (Blist.map do_case cases, Predsym.to_string ident ^ " L.Unf.")
      in
      Option.list_get (Tpreds.map_to_list left_unfold l.SH.inds)
    with Not_symheap -> []
  in
  wrap (Seqtactics.compose rl (Seqtactics.attempt lhs_instantiate_seq))

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
    let (lcs, l), (rcs, r) = Seq.dest seq in
    fun ((lhs', rhs') as seq') ->
      try
        let (lcs', l'), (rcs', r') = Seq.dest seq' in
        if Tpreds.is_empty l'.SH.inds then []
        else
          let lcs = Ord_constraints.close lcs in
          let lhs_check =
            Fun.disj Unify.Unidirectional.is_substitution
              Unify.Unidirectional.modulo_entl
          in
          Unify.Unidirectional.realize
            (Unification.backtrack
               (Heap.unify_partial ~update_check:lhs_check)
               l' l
               (Unify.Unidirectional.unify_tag_constraints ~inverse:false
                  ~update_check:lhs_check lcs' lcs
                  (fun ((trm_subst, tag_subst) as state) ->
                    let () =
                      debug (fun _ ->
                          "Checking results of unification for LHS:\n\t"
                          ^ "Term subst: "
                          ^ Term.Map.to_string Term.to_string trm_subst
                          ^ ", " ^ "Tag subst: "
                          ^ Tagpairs.to_string tag_subst
                          ^ "\n\t" ^ Form.to_string lhs ^ "\n\t"
                          ^ Form.to_string lhs' )
                    in
                    let lhs = Form.with_constraints lhs lcs in
                    let lhs' =
                      Form.subst_tags tag_subst
                        (Form.subst trm_subst lhs')
                    in
                    let _, l' = Form.dest lhs' in
                    assert (Form.subsumed ~total:false lhs' lhs) ;
                    if not (Heap.subsumed l' l) then
                      if lemma_equal !lemma_level NO_LEMMAS then None
                      else if
                        lemma_equal !lemma_level ONLY_WITH_PREDICATES
                        && Tpreds.is_empty l'.SH.inds
                      then None
                      else if
                        lemma_equal !lemma_level NON_EMPTY
                        && Tpreds.is_empty l'.SH.inds
                        && Ptos.is_empty l'.SH.ptos
                      then None
                      else Option.some state
                    else
                      let () =
                        debug (fun _ -> "Continuing with unification of RHS")
                      in
                      let trm_theta, _ = Subst.partition trm_subst in
                      let tag_theta, _ = Tagpairs.partition_subst tag_subst in
                      let rcs' = Ord_constraints.close rcs' in
                      let rhs_check =
                        Fun.conj
                          (Unify.Bidirectional.updchk_inj_left
                             Unify.Unidirectional.modulo_entl)
                          (Unify.Bidirectional.updchk_inj_right
                             Unify.Unidirectional.is_substitution)
                      in
                      let bisubst =
                        (Heap.classical_biunify ~update_check:rhs_check r r'
                           (Unify.Bidirectional.unify_tag_constraints
                              ~update_check:rhs_check rcs rcs'
                              (fun ( ( (trm_subst, tag_subst)
                                     , (trm_subst', tag_subst') ) as state )
                              ->
                                let () =
                                  debug (fun _ ->
                                      "Checking results of biunification for \
                                       RHS:\n\
                                       \t" ^ "Term subst': "
                                      ^ Term.Map.to_string Term.to_string
                                          trm_subst'
                                      ^ ", " ^ "Tag subst': "
                                      ^ Tagpairs.to_string tag_subst'
                                      ^ "\n\t" ^ "Term subst: "
                                      ^ Term.Map.to_string Term.to_string
                                          trm_subst
                                      ^ ", " ^ "Tag subst: "
                                      ^ Tagpairs.to_string tag_subst
                                      ^ "\n\t" ^ Form.to_string rhs'
                                      ^ "\n\t" ^ Form.to_string rhs )
                                in
                                let rhs' =
                                  Form.with_constraints rhs' rcs'
                                in
                                let rhs' =
                                  Form.subst_tags tag_subst'
                                    (Form.subst trm_subst' rhs')
                                in
                                let rhs =
                                  Form.subst_tags tag_subst
                                    (Form.subst trm_subst rhs)
                                in
                                assert (Form.subsumed rhs rhs') ;
                                Option.some state )))
                          ( Unify.Unidirectional.empty_state
                          , (trm_theta, tag_theta) )
                      in
                      Option.map
                        (fun (_, (trm_subst', tag_subst')) ->
                          let trm_subst', _ = Subst.partition trm_subst' in
                          let tag_subst', _ =
                            Tagpairs.partition_subst tag_subst'
                          in
                          ( Term.Map.union trm_subst trm_subst'
                          , Tagpairs.union tag_subst tag_subst' ) )
                        bisubst )))
      with Not_symheap -> []
  with Not_symheap -> fun _ -> []

(*    seq'     *)
(* ----------  *)
(* seq'[theta] *)
(* where seq'[theta] = seq *)
let subst_rule (theta, tps) ((l', _) as seq') ((l, _) as seq) =
  if Seq.equal seq (Seq.subst_tags tps (Seq.subst theta seq')) then
    let tagpairs =
      Tagpairs.filter
        (fun (t, t') ->
          Tags.mem t' (Form.tags l') && Tags.mem t (Form.tags l) )
        (Tagpairs.reflect tps)
    in
    let unmapped = Tags.diff (Form.tags l) (Tagpairs.projectl tagpairs) in
    let remaining = Tags.inter unmapped (Form.tags l') in
    let tagpairs = Tagpairs.union tagpairs (Tagpairs.mk remaining) in
    [([(seq', tagpairs, Tagpairs.empty)], "Subst")]
  else
    let () =
      debug (fun _ -> "Unsuccessfully tried to apply substitution rule!")
    in
    []

(*   F |- G * Pi'  *)
(* --------------- *)
(*   Pi * F |- G   *)
(* where seq' = F |- G * Pi' and seq = Pi * F |- G *)
let weaken seq' seq =
  (* let () = debug (fun _ -> "Trying to apply weakening:") in *)
  (* let () = debug (fun _ -> Seq.to_string seq') in        *)
  (* let () = debug (fun _ -> Seq.to_string seq) in         *)
  if Seq.subsumed seq seq' then
    [([(seq', Seq.tag_pairs seq', Tagpairs.empty)], "Weaken")]
  else
    let () =
      debug (fun _ -> "Unsuccessfully tried to apply weakening rule!")
    in
    let () = debug (fun _ -> Seq.to_string seq') in
    let () = debug (fun _ -> Seq.to_string seq) in
    []

let left_transform_rule ((lhs', rhs') as seq') (lhs, rhs) =
  try
    let lhs_cs', lhs_h' = Form.dest lhs' in
    let lhs_cs, lhs_h = Form.dest lhs in
    if Form.equal rhs' rhs then
      let transform =
        Unify.Unidirectional.realize
          ((Heap.classical_unify
              ~update_check:Unify.Unidirectional.modulo_entl lhs_h' lhs_h)
             (Unify.Unidirectional.unify_tag_constraints
                ~update_check:Unify.Unidirectional.modulo_entl lhs_cs'
                lhs_cs Unification.trivial_continuation))
      in
      if Option.is_some transform then
        let _, tps = Option.get transform in
        let tps = Tagpairs.reflect tps in
        [([(seq', tps, Tagpairs.empty)], "L.Trans.Ex")]
      else
        let () =
          debug (fun _ ->
              "Unsuccessfully tried to apply left transformation rule!" )
        in
        []
    else
      let () =
        debug (fun _ ->
            "Unsuccessfully tried to apply left transformation rule - \
             right-hand sides not equal!" )
      in
      []
  with Not_symheap ->
    let () =
      debug (fun _ ->
          "Unsuccessfully tried to apply left transformation rule - one \
           left-hand side not a symbolic heap!" )
    in
    []

let right_transform_rule ((lhs', rhs') as seq') (lhs, rhs) =
  try
    let rhs_cs', rhs_h' = Form.dest rhs' in
    let rhs_cs, rhs_h = Form.dest rhs in
    if Form.equal lhs' lhs then
      let transform =
        Unify.Unidirectional.realize
          ((Heap.classical_unify
              ~update_check:Unify.Unidirectional.modulo_entl rhs_h rhs_h')
             (Unify.Unidirectional.unify_tag_constraints
                ~update_check:Unify.Unidirectional.modulo_entl rhs_cs
                rhs_cs' Unification.trivial_continuation))
      in
      if Option.is_some transform then
        [([(seq', Seq.tag_pairs seq', Tagpairs.empty)], "R.Trans.Ex")]
      else
        let () =
          debug (fun _ ->
              "Unsuccessfully tried to apply right transformation rule!" )
        in
        []
    else
      let () =
        debug (fun _ ->
            "Unsuccessfully tried to apply right transformation rule - \
             left-hand sides not equal!" )
      in
      []
  with Not_symheap ->
    let () =
      debug (fun _ ->
          "Unsuccessfully tried to apply right transformation rule - one \
           right-hand side not a symbolic heap!" )
    in
    []

let apply_lemma (lemma_seq, ((lhs, rhs) as cont_seq)) ((lhs', rhs') as seq) =
  let () =
    debug (fun _ -> "Trying to apply lemma to subgoal: " ^ Seq.to_string seq)
  in
  let () = debug (fun _ -> "Lemma: " ^ Seq.to_string lemma_seq) in
  let () = debug (fun _ -> "Continuation: " ^ Seq.to_string cont_seq) in
  let (lcs, l), (rcs, r) = Seq.dest lemma_seq in
  let cs, h = Form.dest lhs in
  assert (Ptos.subset r.SH.ptos h.SH.ptos) ;
  assert (Tpreds.subset r.SH.inds h.SH.inds) ;
  assert (Ord_constraints.equal cs (Ord_constraints.union lcs rcs)) ;
  (* The separating conjunction of the lemma antecedent and the frame may *)
  (* introduce more disequalities that simply the union *)
  assert (Deqs.subset (Deqs.union l.SH.deqs r.SH.deqs) h.SH.deqs) ;
  assert (Uf.subsumed l.SH.eqs h.SH.eqs) ;
  assert (Uf.subsumed r.SH.eqs h.SH.eqs) ;
  assert (
    Uf.subsumed h.SH.eqs
      (Uf.fold (Fun.curry Uf.add) l.SH.eqs r.SH.eqs) ) ;
  try
    let cs', h' = Form.dest lhs' in
    let expected =
      Heap.with_ptos
        (Heap.with_inds l
           (Tpreds.union l.SH.inds (Tpreds.diff h.SH.inds r.SH.inds)))
        (Ptos.union l.SH.ptos (Ptos.diff h.SH.ptos r.SH.ptos))
    in
    (* let () = debug (fun _ -> "Constraints match: " ^ (string_of_bool (Ord_constraints.equal lcs cs'))) in *)
    (* let () = debug (fun _ -> "Heap as expected: " ^ (string_of_bool (Heap.equal h' expected))) in      *)
    (* let () = debug (fun _ -> "RHS match: " ^ (string_of_bool (Form.equal rhs rhs'))) in                *)
    if
      Ord_constraints.equal lcs cs'
      && Heap.equal h' expected && Form.equal rhs rhs'
    then
      let vts, pts = Seq.get_tracepairs seq cont_seq in
      [ ( [ (lemma_seq, Seq.tag_pairs lemma_seq, Tagpairs.empty)
          ; (cont_seq, vts, pts) ]
        , "Lemma.App" ) ]
    else
      let () =
        debug (fun _ ->
            "Unsuccessfully tried to apply lemma - open node does not match \
             expected!" )
      in
      []
  with Not_symheap ->
    let () =
      debug (fun _ ->
          "Unsuccessfully tried to apply lemma - LHS of open node not a \
           symbolic heap!" )
    in
    []

let mk_backlink_rule_seq (trm_subst, tag_subst) ((src_lhs, src_rhs) as src_seq)
    (targ_idx, targ_seq) =
  let ((subst_lhs, subst_rhs) as subst_seq) =
    Seq.subst trm_subst (Seq.subst_tags tag_subst targ_seq)
  in
  let (subst_lhs_cs, subst_lhs_h), (subst_rhs_cs, subst_rhs_h) =
    Seq.dest subst_seq
  in
  let (src_lhs_cs, src_lhs_h), (src_rhs_cs, src_rhs_h) = Seq.dest src_seq in
  let src_lhs_cs = Ord_constraints.close src_lhs_cs in
  let subst_rhs_cs = Ord_constraints.close subst_rhs_cs in
  let lhs_transform =
    Unify.Unidirectional.realize
      ((Heap.classical_unify
          ~update_check:Unify.Unidirectional.modulo_entl subst_lhs_h
          src_lhs_h)
         (Unify.Unidirectional.unify_tag_constraints
            ~update_check:Unify.Unidirectional.modulo_entl subst_lhs_cs
            src_lhs_cs Unification.trivial_continuation))
  in
  let rhs_transform =
    Unify.Unidirectional.realize
      ((Heap.classical_unify
          ~update_check:Unify.Unidirectional.modulo_entl src_rhs_h
          subst_rhs_h)
         (Unify.Unidirectional.unify_tag_constraints
            ~update_check:Unify.Unidirectional.modulo_entl src_rhs_cs
            subst_rhs_cs Unification.trivial_continuation))
  in
  let () =
    debug (fun _ ->
        "Checking transform for LHS:\n\t"
        ^ Form.to_string subst_lhs
        ^ "\n\t" ^ Form.to_string src_lhs )
  in
  assert (Option.is_some lhs_transform) ;
  let () =
    debug (fun _ ->
        "Checking transform for RHS:\n\t"
        ^ Form.to_string subst_rhs
        ^ "\n\t" ^ Form.to_string src_rhs )
  in
  assert (Option.is_some rhs_transform) ;
  let lhs_trm_transform, lhs_tag_transform = Option.get lhs_transform in
  let rhs_trm_transform, rhs_tag_transform = Option.get rhs_transform in
  let transformed_lhs =
    Form.subst_tags lhs_tag_transform
      (Form.subst lhs_trm_transform subst_lhs)
  in
  let transformed_rhs =
    Form.subst_tags rhs_tag_transform
      (Form.subst rhs_trm_transform src_rhs)
  in
  let left_transformed_seq = (transformed_lhs, subst_rhs) in
  let right_transformed_seq = (src_lhs, transformed_rhs) in
  Rule.sequence
    [ ( if Seq.equal src_seq right_transformed_seq then Rule.identity
      else Rule.mk_infrule (right_transform_rule right_transformed_seq) )
    ; ( if Seq.equal right_transformed_seq left_transformed_seq then
        Rule.identity
      else Rule.mk_infrule (weaken left_transformed_seq) )
    ; ( if Seq.equal left_transformed_seq subst_seq then Rule.identity
      else Rule.mk_infrule (left_transform_rule subst_seq) )
    ; ( if Seq.equal subst_seq targ_seq then Rule.identity
      else Rule.mk_infrule (subst_rule (trm_subst, tag_subst) targ_seq) )
    ; Rule.mk_backrule true
        (fun _ _ -> [targ_idx])
        (fun s s' -> [(Seq.tag_pairs s', "Backl")]) ]

let mk_lemma_rule_seq (trm_subst, tag_subst) (src_lhs, src_rhs)
    (targ_idx, ((lhs, rhs) as targ_seq)) =
  let cs, h = Form.dest src_lhs in
  let trm_theta, _ = Subst.partition trm_subst in
  let tag_theta, _ = Tagpairs.partition_subst tag_subst in
  let subst_lhs = Form.subst trm_subst (Form.subst_tags tag_subst lhs) in
  let subst_rhs = Form.subst trm_theta (Form.subst_tags tag_theta rhs) in
  let subst_seq = (subst_lhs, subst_rhs) in
  (* let () = debug (fun _ -> "substituted seq is " ^ (Seq.to_string subst_seq)) in *)
  let subst_cs, subst_h = Form.dest subst_lhs in
  (* Calculate the frame *)
  let frame =
    Ptos.fold (Fun.swap Heap.del_pto) subst_h.SH.ptos
      (Tpreds.fold (Fun.swap Heap.del_ind) subst_h.SH.inds h)
  in
  (* let () = debug (fun _ -> "Calculated frame is " ^ (Heap.to_string frame)) in *)
  (* Alpha-rename any clashing existential variables in the succedent of the lemma *)
  let ctxt_vars =
    Term.Set.union (Heap.terms frame) (Form.terms src_rhs)
  in
  let ctxt_tags =
    Tags.union_of_list
      [Heap.tags frame; Ord_constraints.tags cs; Form.tags src_rhs]
  in
  let clashed_tags =
    Tags.inter ctxt_tags
      (Tags.filter Tags.is_exist_var (Form.tags subst_rhs))
  in
  let clashed_vars =
    Term.Set.inter ctxt_vars
      (Term.Set.filter Term.is_exist_var (Form.terms subst_rhs))
  in
  let all_tags = Tags.union ctxt_tags (Seq.tags subst_seq) in
  let all_vars = Term.Set.union ctxt_vars (Seq.vars subst_seq) in
  let tag_subst' = Tagpairs.mk_ex_subst all_tags clashed_tags in
  let trm_subst' = Subst.mk_ex_subst all_vars clashed_vars in
  let subst_rhs =
    Form.subst trm_subst' (Form.subst_tags tag_subst' subst_rhs)
  in
  (* Construct the new subgoals *)
  let lemma_seq =
    let subst_h =
      Heap.with_eqs (Heap.with_deqs subst_h h.SH.deqs) h.SH.eqs
    in
    ((cs, [subst_h]), subst_rhs)
  in
  (* let () = debug (fun _ -> (Heap.to_string subst_h') ^ " * " ^ (Heap.to_string frame) ^ " = " ^ (Heap.to_string (Heap.star subst_h' frame))) in *)
  let cont_seq = (Form.star (cs, [frame]) subst_rhs, src_rhs) in
  (* Construct the rule sequence *)
  Rule.compose_pairwise
    (Rule.mk_infrule (apply_lemma (lemma_seq, cont_seq)))
    [ mk_backlink_rule_seq (trm_theta, tag_theta) lemma_seq (targ_idx, targ_seq)
    ; Rule.identity ]

type backlink_t = FULL of Rule.t | PARTIAL of Rule.t

let dest_taggedrule = function FULL r -> r | PARTIAL r -> r

let cmp_taggedrule r r' =
  match (r, r') with
  | FULL _, PARTIAL _ -> -1
  | PARTIAL _, FULL _ -> 1
  | _ -> 0

(* If there is a backlink achievable through substitution and classical   *)
(* weakening (possibly after applying a lemma), then make the proof steps *)
(* that achieve it explicit so that actual backlinking can be done on     *)
(* Seq.equal sequents *)
let dobackl idx prf =
  let ((src_lhs, src_rhs) as src_seq) = Proof.get_seq idx prf in
  let matches = matches src_seq in
  let targets = !Rule.default_select_f idx prf in
  let apps =
    Blist.bind
      (fun idx' -> Blist.map (Pair.mk idx') (matches (Proof.get_seq idx' prf)))
      targets
  in
  let f (targ_idx, ((theta, tagpairs) as subst)) =
    let () = debug (fun _ -> "Constructing backlink") in
    let targ_seq = Proof.get_seq targ_idx prf in
    let () =
      debug (fun _ ->
          "Target seq is " ^ Int.to_string targ_idx ^ ": "
          ^ Seq.to_string targ_seq )
    in
    let () =
      debug (fun _ ->
          "Term Subst: " ^ Term.Map.to_string Term.to_string theta )
    in
    let () = debug (fun _ -> "Tag Subst: " ^ Tagpairs.to_string tagpairs) in
    let subst_lhs, _ =
      Seq.subst theta (Seq.subst_tags tagpairs targ_seq)
    in
    let () =
      debug (fun _ ->
          "\t" ^ "Checking for subsumption:" ^ "\n\t\t" ^ "subst_lhs: "
          ^ Form.to_string subst_lhs
          ^ "\n\t\t" ^ "src_lhs: " ^ Form.to_string src_lhs )
    in
    if Form.subsumed subst_lhs src_lhs then
      let () = debug (fun _ -> "\t\t" ^ "FULL") in
      let theta, _ = Subst.partition theta in
      let tagpairs, _ = Tagpairs.partition_subst tagpairs in
      FULL
        (mk_backlink_rule_seq (theta, tagpairs) src_seq (targ_idx, targ_seq))
    else
      let () = debug (fun _ -> "\t\t" ^ "PARTIAL") in
      PARTIAL (mk_lemma_rule_seq subst src_seq (targ_idx, targ_seq))
  in
  (* Although application of all the constructed rule sequences will *)
  (* succeed by construction the backlinking may fail to satisfy the *)
  (* soundness condition, and so we pick the first one that is sound *)
  (* and we also prefer full backlinking to lemma application        *)
  let rules =
    Blist.map dest_taggedrule
      (Blist.stable_sort cmp_taggedrule (Blist.map f apps))
  in
  Rule.first rules idx prf

(* let axioms = ref (Rule.first [id_axiom ; ex_falso_axiom]) *)
let axioms = ref Rule.fail

let rules = ref Rule.fail

let use_invalidity_heuristic = ref false

let setup defs =
  preddefs := defs ;
  rules :=
    Rule.first
      [ lhs_disj_to_symheaps
      ; rhs_disj_to_symheaps
      ; lhs_instantiate
      ; simplify
      ; bounds_intro
      ; constraint_match_tag_instantiate
      ; upper_bound_tag_instantiate
      ; Rule.choice
          [ dobackl
          ; pto_intro_rule
          ; pred_intro_rule
          ; instantiate_pto
          ; Rule.conditional
              (fun (_, (cs, _)) ->
                Ord_constraints.for_all
                  (fun c ->
                    Tags.exists Tags.is_free_var (Ord_constraints.Elt.tags c)
                    )
                  cs )
              (ruf defs)
          ; luf defs ] ] ;
  let axioms = Rule.first [id_axiom; ex_falso_axiom] in
  rules := Rule.combine_axioms axioms !rules ;
  if !use_invalidity_heuristic then
    rules := Rule.conditional (fun s -> not (Invalid.check defs s)) !rules
