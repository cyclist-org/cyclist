open Lib
open Generic
open Seplog

open Program

exception Not_symheap = Form.Not_symheap

module Rule = Proofrule.Make (Seq)
module Seqtactics = Seqtactics.Make (Seq)
module Proof = Proof.Make (Seq)
module Slprover = Prover.Make (Seplog.Seq)
module EntlSeqHash = Hashtbl.Make (Seplog.Seq)
module ProofNode = Proofnode.Make (Seq)

let check_invalid = ref (Invalid.check Defs.empty)

let show_invalidity_debug = ref false

let show_entailment_debug = ref false

let show_frame_debug = ref false

let entl_depth = ref 4

(* Wrapper for the entailment prover *)
let entailment_table : (int * Slprover.Proof.t option) EntlSeqHash.t =
  EntlSeqHash.create 11

let entails f f' =
  let () =
    debug (fun _ ->
        "Trying to prove entailment:\n\t"
        ^ Seplog.Seq.to_string (f, f')
        ^ "\n\t" ^ "with depth " ^ string_of_int !entl_depth )
  in
  let prove seq =
    let depth = if Int.( < ) !entl_depth 1 then max_int else !entl_depth in
    let invalid =
      let dbg = !do_debug in
      let res =
        do_debug := !do_debug && !show_invalidity_debug ;
        !check_invalid seq
      in
      do_debug := dbg ;
      res
    in
    if invalid then
      let () = debug (fun _ -> "Entailment is invalid!") in
      None
    else
      let dbg = !do_debug in
      let prf =
        do_debug := !do_debug && !show_entailment_debug ;
        Slprover.idfs 1 depth !Rules.axioms !Rules.rules seq
      in
      do_debug := dbg ;
      let () =
        debug (fun _ ->
            "Entailment was " ^ Option.dest "not " (fun _ -> "") prf ^ "proved"
        )
      in
      prf
  in
  let seq = (f, f') in
  if EntlSeqHash.mem entailment_table seq then (
    let depth, prf = EntlSeqHash.find entailment_table seq in
    let () =
      debug (fun _ ->
          "Found result in cache: entailment "
          ^ (if Option.is_none prf then "not " else "")
          ^ "proved up to depth " ^ string_of_int depth )
    in
    if Option.is_some prf || Int.( <= ) !entl_depth depth then prf
    else
      let prf = prove seq in
      EntlSeqHash.replace entailment_table seq (!entl_depth, prf) ;
      prf )
  else
    let () =
      debug (fun _ -> "Result not found in cache: attempting to prove")
    in
    let prf = prove seq in
    EntlSeqHash.replace entailment_table seq (!entl_depth, prf) ;
    prf

(* Wrappers for backlink abducers *)
module AbdTblElt = struct
  include Pair.Make (Tags) (Term.Set)
  include Containers.Make (Pair.Make (Tags) (Term.Set))
end

module AbdTblMap = AbdTblElt.Hashmap

let abd_pre_table :
    ( int
    * ((Form.t * Form.t) * Unify.Unidirectional.state list) option )
    AbdTblMap.t
    EntlSeqHash.t =
  EntlSeqHash.create 11

let abd_pre_transforms ((used_tags, prog_vars) as key) f f' =
  let abd f f' =
    let res =
      Abduce.abd_substs ~used_tags
        ~update_check:
          (Fun.disj Unify.Unidirectional.modulo_entl
             (Fun.conj Unify.Unidirectional.is_substitution
                (Unify.Unidirectional.avoid_replacing_trms prog_vars)))
        f f'
    in
    Option.map
      (fun (interpolant, substs) ->
        (interpolant, Unify.Unidirectional.remove_dup_substs substs) )
      res
  in
  let seq = (f, f') in
  if EntlSeqHash.mem abd_pre_table seq then (
    let map = EntlSeqHash.find abd_pre_table seq in
    if AbdTblMap.mem map key then (
      let depth, res = AbdTblMap.find map key in
      if Option.is_some res || Int.( <= ) Abduce.max_depth depth then res
      else
        let res = abd f f' in
        AbdTblMap.replace map key (Abduce.max_depth, res) ;
        res )
    else
      let res = abd f f' in
      AbdTblMap.replace map key (Abduce.max_depth, res) ;
      res )
  else
    let res = abd f f' in
    let map = AbdTblMap.create 11 in
    AbdTblMap.replace map key (Abduce.max_depth, res) ;
    EntlSeqHash.replace abd_pre_table seq map ;
    res

let tagpairs = Seq.tag_pairs

(* following is for symex only *)
let progpairs tps = if !termination then tps else Seq.tagpairs_one

let dest_sh_seq (pre, cmd, post) = (Form.dest pre, cmd, post)

(* axioms *)

(* If the precondition of the candidate sequence is inconsistent, then we can *)
(* close it of as instance of the Ex Falso axiom *)
let ex_falso_axiom =
  Rule.mk_axiom (fun (pre, _, _) ->
      Option.mk (Form.inconsistent pre) "Ex Falso" )

(* If the precondition entails the post condition and the command is a final  *)
(* one possibly preceded by assertions then we can apply the Empty axiom.     *)
let mk_symex_empty_axiom =
  let ax (pre, cmd, post) =
    let default_depth = !entl_depth in
    let rec aux f cmd =
      if Cmd.is_assert cmd then
        let f' = Cmd.dest_assert cmd in
        let f' = Form.complete_tags Tags.empty f' in
        if Option.is_none (entails f f') then None
        else aux f' (Cmd.get_cont cmd)
      else
        let () = entl_depth := default_depth in
        entails f post
    in
    if Cmd.is_final (Cmd.strip_asserts cmd) then
      let () = entl_depth := 0 in
      Option.map (fun _ -> "Axiom") (aux pre cmd)
    else None
  in
  Rule.mk_axiom ax

(* simplification rules *)

(* Tactic which tries to simplify the sequent by replacing existential variables *)
(* in the precondition and fails if no such replacements can be made *)
(* TODO: ?make a similar simplification tactic that replaces existentials in postcondition *)
let eq_subst_ex_f ((pre, cmd, post) as s) =
  let pre' = Form.subst_existentials pre in
  if Form.equal pre pre' then []
  else [([((pre', cmd, post), tagpairs s, Tagpairs.empty)], "Eq. subst. ex")]

(* Tactic which tries to simplify the sequent by normalising: that is, using the  *)
(* equalities in the formula as a substitution for the disequality, points-to and *)
(* predicate subformulae.                                                         *)
(* TODO: ?make a similar simplification tactic that normalises the postcondition  *)
(* let norm ((pre ,cmd, post) as s) =                               *)
(*   let pre' = Form.norm pre in                                 *)
(*   if Form.equal pre pre' then [] else                         *)
(*   [ [( (pre', cmd, post), tagpairs s, Tagpairs.empty)], "Norm" ] *)

let simplify_rules = [eq_subst_ex_f]

(* Tactic which performs as many simplifications as possible all in one go *)
let simplify_seq_rl =
  Seqtactics.relabel "Simplify"
    (Seqtactics.repeat (Seqtactics.first simplify_rules))

let simplify = Rule.mk_infrule simplify_seq_rl

(* Function which takes a tactic, composes it with a general simplification attempt *)
(* tactic, and creates a compound inference rule out of it *)
let wrap r =
  Rule.mk_infrule (Seqtactics.compose r (Seqtactics.attempt simplify_seq_rl))

let lab_ex_intro =
  let rl ((pre, _, _) as seq) =
    try
      let _ = Form.dest pre in
      let exs = Tags.filter Tags.is_exist_var (Form.tags pre) in
      if Tags.is_empty exs then []
      else
        let subst = Tagpairs.mk_free_subst (Seq.all_tags seq) exs in
        let pre' = Form.subst_tags subst pre in
        let tps =
          if !termination then
            Tagpairs.union
              (Tagpairs.mk (Tags.filter Tags.is_free_var (Form.tags pre)))
              subst
          else Seq.tagpairs_one
        in
        [([(Seq.with_pre seq pre', tps, Tagpairs.empty)], "Lab.Ex.Intro")]
    with Not_symheap -> []
  in
  Rule.mk_infrule rl

(* break LHS disjunctions *)
let lhs_disj_to_symheaps =
  let rl (((_, hs) as pre), cmd, post) =
    match hs with
    | [] | [_] -> []
    | _ ->
        [ ( Blist.map
              (fun h ->
                let s' = (Form.with_heaps pre [h], cmd, post) in
                (s', tagpairs s', Tagpairs.empty) )
              hs
          , "L.Or" ) ]
  in
  Rule.mk_infrule rl

(* let gen_left_rules_f (def, ident) seq =                                                *)
(*   try                                                                                  *)
(*     let (pre, cmd, post) = dest_sh_seq seq in                                          *)
(*     let preds =                                                                        *)
(*       Tpreds.filter (fun (_,(ident',_)) -> Strng.equal ident ident') pre.Heap.inds in *)
(*     if Tpreds.is_empty preds then [] else                                           *)
(*     let left_unfold ((id,(_,pvs)) as p) =                                              *)
(*       let ts = Tags.inter (Heap.tags pre) (Heap.tags pre) in                     *)
(*       let pre' = Heap.del_ind pre p in                                                   *)
(*       let do_case case =                                                               *)
(*         let n = Blist.length (Indrule.formals case) in                              *)
(*         let n' = Blist.length pvs in                                                   *)
(*         let err_msg = fun () ->                                                        *)
(*           (Printf.sprintf                                                              *)
(*             "Skipping unfolding of inductive predicate \"%s\" \                        *)
(*             due to parameter mismatch: definition expects %d parameters, \             *)
(*             but was given %d" ident n n')  in                                          *)
(*         Option.mk_lazily (n == n' || (debug err_msg; false))                           *)
(*         (fun () ->                                                                     *)
(*           let (f', (_,vs')) = Indrule.dest (freshen_case_by_seq seq case) in        *)
(*           let theta = Term.Map.of_list (Blist.combine vs' pvs) in                   *)
(*           let f' = Heap.subst theta f' in                                           *)
(*           let f' = Heap.repl_tags id f' in                                          *)
(*           let pre' = Heap.star pre' f' in                                           *)
(*           (                                                                            *)
(*             ([pre'], cmd, post),                                                       *)
(*             (if !termination then Tagpairs.mk ts else Seq.tagpairs_one),               *)
(*             (if !termination then Tagpairs.singleton (id,id) else Tagpairs.empty)      *)
(*           )) in                                                                        *)
(*       let subgoals = Option.list_get (Blist.map do_case def) in                        *)
(*       Option.mk (not (Blist.is_empty subgoals)) (subgoals, (ident ^ " L.Unf.")) in     *)
(*     Option.list_get (Tpreds.map_to_list left_unfold preds)                          *)
(*   with Not_symheap -> []                                                               *)

(* let gen_left_rules (def,ident) =                                                       *)
(*   wrap (gen_left_rules_f (def,ident))                                                  *)

let luf_rl defs ((pre, cmd, post) as seq) =
  try
    let cs, pre = Form.dest pre in
    let seq_vars = Seq.vars seq in
    let seq_tags = Seq.all_tags seq in
    let left_unfold ((tag, (ident, _)) as p) =
      let pre' = Heap.del_ind pre p in
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
        let vts = Tagpairs.union vts (Tagpairs.mk (Heap.tags pre')) in
        ( ((new_cs, [Heap.star pre' f]), cmd, post)
        , (if !termination then vts else Seq.tagpairs_one)
        , if !termination then pts else Tagpairs.empty )
      in
      let () =
        debug (fun () -> "L. Unfolding " ^ Predsym.to_string ident)
      in
      (Blist.map do_case cases, Predsym.to_string ident ^ " L.Unf.")
    in
    Tpreds.map_to_list left_unfold
      (Tpreds.filter (Defs.is_defined defs) pre.Heap.inds)
  with Not_symheap -> []

let luf defs = wrap (luf_rl defs)

let ruf_rl defs ((pre, cmd, post) as seq) =
  let cs, h = Form.dest post in
  let seq_vars = Seq.vars seq in
  let seq_tags = Seq.all_tags seq in
  let preds = Tpreds.filter (Defs.is_defined defs) h.Heap.inds in
  let right_unfold ((tag, (ident, _)) as p) =
    let h' = Heap.del_ind h p in
    let clauses = Defs.unfold (seq_vars, seq_tags) p defs in
    let do_case f =
      let cs' =
        Ord_constraints.union cs
          (Ord_constraints.generate tag (Heap.tags f))
      in
      let h' = Heap.star h' f in
      let seq' = (pre, cmd, (cs', [h'])) in
      ( [(seq', tagpairs seq', Tagpairs.empty)]
      , Predsym.to_string ident ^ " R.Unf." )
    in
    let () = debug (fun () -> "R. Unfolding " ^ Predsym.to_string ident) in
    Blist.map do_case clauses
  in
  Blist.flatten (Tpreds.map_to_list right_unfold preds)

let ruf defs = wrap (ruf_rl defs)

(* FOR SYMEX ONLY *)
let fix_tps l =
  Blist.map
    (fun (g, d) ->
      (Blist.map (fun s -> (s, tagpairs s, progpairs Tagpairs.empty)) g, d) )
    l

let mk_symex f =
  let rl ((pre, cmd, post) as seq) =
    if Cmd.is_empty cmd then []
    else
      let cont = Cmd.get_cont cmd in
      fix_tps
        (Blist.map
           (fun (g, d) ->
             ( Blist.map (fun h' -> (Form.with_heaps pre [h'], cont, post)) g
             , d ) )
           (f seq))
  in
  wrap rl

  (* symbolic execution rules *)
  (*
  {x = e /\ P } C {Q}
  --------------------
  {P} x = e ; C {Q}
  *)
let symex_assign_rule =
  let rl ((pre, cmd, _) as seq) =
    try
      let _, h = Form.dest pre in
      let x, e = Cmd.dest_assign cmd in
      let fv = fresh_evar (Seq.vars seq) in
      let theta = Subst.singleton x fv in
      let h' = Heap.subst theta h in
      let e' = Subst.apply theta e in
      [([Heap.add_eq h' (e', x)], "Assign")]
    with
    | WrongCmd | Not_symheap -> []
  in
  mk_symex rl

let find_pto_on f e =
  Ptos.find (fun (l, _) -> Heap.equates f e l) f.Heap.ptos

let symex_load_rule =
  let rl ((pre, cmd, _) as seq) =
    try
      let _, h = Form.dest pre in
      let x, e, f = Cmd.dest_load cmd in
      let _, ys = find_pto_on h e in
      let t = Blist.nth ys (Field.get_index f) in
      let fv = fresh_evar (Seq.vars seq) in
      let theta = Subst.singleton x fv in
      let h' = Heap.subst theta h in
      let t' = Subst.apply theta t in
      [([Heap.add_eq h' (t', x)], "Load")]
    with
    | Not_symheap | WrongCmd | Not_found -> []
  in
  mk_symex rl

let symex_store_rule =
  let rl (pre, cmd, _) =
    try
      let _, h = Form.dest pre in
      let x, f, e = Cmd.dest_store cmd in
      let ((x', ys) as pto) = find_pto_on h x in
      let pto' = (x', Blist.replace_nth e (Field.get_index f) ys) in
      [([Heap.add_pto (Heap.del_pto h pto) pto'], "Store")]
    with
    | Not_symheap | WrongCmd | Not_found -> []
  in
  mk_symex rl

let symex_free_rule =
  let rl (pre, cmd, _) =
    try
      let _, h = Form.dest pre in
      let e = Cmd.dest_free cmd in
      let pto = find_pto_on h e in
      [([Heap.del_pto h pto], "Free")]
    with
    | Not_symheap | WrongCmd | Not_found -> []
  in
  mk_symex rl

let symex_new_rule =
  let rl ((pre, cmd, _) as seq) =
    try
      let _, h = Form.dest pre in
      let x = Cmd.dest_new cmd in
      let l = fresh_evars (Seq.vars seq) (1 + Field.get_no_fields ()) in
      let fv, fvs = (Blist.hd l, Blist.tl l) in
      let h' = Heap.subst (Subst.singleton x fv) h in
      let new_pto = Heap.mk_pto (x, fvs) in
      [([Heap.star h' new_pto], "New")]
    with
    | Not_symheap | WrongCmd -> []
  in
  mk_symex rl

  (*            *)
  (* ---------- *)
  (* {A}skip{A} *)
  let symex_skip_rule =
  let rl (pre, cmd, _) =
    try
      let _, h = Form.dest pre in
      let () = Cmd.dest_skip cmd in
      [([h], "Skip")]
    with
    | Not_symheap | WrongCmd -> []
  in
  mk_symex rl


  let symex_if_rule =
  let rl (pre, cmd, post) =
    try
      let _, h = Form.dest pre in
      let c, cmd' = Cmd.dest_if cmd in
      let cont = Cmd.get_cont cmd in
      let cond_true, cond_false = Cond.fork h c in
      fix_tps
        [ ( [ (Form.with_heaps pre [cond_true], Cmd.mk_seq cmd' cont, post)
            ; (Form.with_heaps pre [cond_false], cont, post) ]
          , "If" ) ]
    with
    | Not_symheap | WrongCmd -> []
  in
  wrap rl

let symex_ifelse_rule =
  let rl (pre, cmd, post) =
    try
      let _, h = Form.dest pre in
      let c, cmd1, cmd2 = Cmd.dest_ifelse cmd in
      let cont = Cmd.get_cont cmd in
      let cond_true, cond_false = Cond.fork h c in
      fix_tps
        [ ( [ (Form.with_heaps pre [cond_true], Cmd.mk_seq cmd1 cont, post)
            ; (Form.with_heaps pre [cond_false], Cmd.mk_seq cmd2 cont, post)
            ]
          , "IfElse" ) ]
    with
    | Not_symheap | WrongCmd -> []
  in
  wrap rl

let symex_while_rule =
  let rl (pre, cmd, post) =
    try
      let _, h = Form.dest pre in
      let c, cmd' = Cmd.dest_while cmd in
      let cont = Cmd.get_cont cmd in
      let cond_true, cond_false = Cond.fork h c in
      fix_tps
        [ ( [ (Form.with_heaps pre [cond_true], Cmd.mk_seq cmd' cmd, post)
            ; (Form.with_heaps pre [cond_false], cont, post) ]
          , "While" ) ]
    with
    | Not_symheap | WrongCmd -> []
  in
  wrap rl

  let symex_parallel_rule =
    let rl (pre, cmd, post) =
      try
        let _, _ = Form.dest pre in
        let cmd1, cmd2 = Cmd.dest_parallel cmd in
        let cont = Cmd.get_cont cmd in 
        fix_tps
          [ ( [ (pre, Cmd.mk_seq cmd1 cont, post)
              ; (pre, Cmd.mk_seq cmd2 cont, post)
              ]
            , "Parallel" ) ]
      with
      | Not_symheap | WrongCmd -> []
    in
    wrap rl


let proc_unfold_str = "Proc Unf. "

let mk_symex_proc_unfold procs prf_cache =
  let rl (pre, cmd, post) =
    try
      let p, args = Cmd.dest_proc_call cmd in
      if Cmd.is_empty (Cmd.get_cont cmd) then
        let _, _, _, body =
          Blist.find
            (fun (id, params, specs, _) ->
              try
                String.equal id p
                && Blist.equal Term.equal args params
                && Blist.exists
                     (fun ((pre', post') as spec) ->
                       (not (Proc.SigMap.mem ((id, params), spec) !prf_cache))
                       && Form.subsumed pre' pre && Form.equal post post'
                       )
                     specs
                (* We only unfold procedures for which proofs have not been already *)
                (* found and we use subsumption here to handle the case that the    *)
                (* procedure has a disjunctive precondition which has already been  *)
                (* split and that we now want to unfold                             *)
              with Invalid_argument _ -> false )
            procs
        in
        fix_tps [([(pre, body, post)], proc_unfold_str ^ p)]
      else []
    with
    | WrongCmd | Not_found -> []
  in
  Rule.mk_infrule rl

let is_proc_unfold_node n =
  let _, descr = ProofNode.dest n in
  Int.( >= ) (String.length descr) (String.length proc_unfold_str)
  && Strng.equal
       (Str.first_chars descr (String.length proc_unfold_str))
       proc_unfold_str


  let assert_rule =
  let rl ((pre, cmd, post) as seq) =
    try
      let f = Cmd.dest_assert cmd in
      let cs, h = Form.dest pre in
      let cont = Cmd.get_cont cmd in
      let h = Heap.explode_deqs h in
      let f = Form.complete_tags (Form.tags f) f in
      let default_depth = !entl_depth in
      entl_depth := 0 ;
      let entl_result = entails (cs, [h]) f in
      entl_depth := default_depth ;
      if Option.is_some entl_result then
        let seq' = (f, cont, post) in
        let allpairs, progressing =
          if !termination then Seq.get_tracepairs seq seq'
          else (Seq.tagpairs_one, Tagpairs.empty)
        in
        [([(seq', allpairs, progressing)], "LHS.Cons")]
      else
        let () =
          debug (fun _ ->
              "Unsuccesfully tried to apply the assert rule:" ^ "\n\t"
              ^ Seplog.Seq.to_string (pre, f) )
        in
        []
    with
    | WrongCmd | Not_symheap -> []
  in
  Rule.mk_infrule (Seqtactics.relabel "LHS.Cons" (Seqtactics.repeat rl))

let param_subst_rule theta ((_, cmd', _) as seq') ((_, cmd, _) as seq) =
  if
    Cmd.is_proc_call cmd
    && Cmd.is_empty (Cmd.get_cont cmd)
    && Cmd.is_proc_call cmd'
    && Cmd.is_empty (Cmd.get_cont cmd')
    && Seq.equal (Seq.param_subst theta seq') seq
  then
    [ ([(seq', Seq.tag_pairs seq', Tagpairs.empty)], "Param Subst")
    (* ^ (Format.asprintf " %a" Term.pp_subst theta) *) ]
  else
    let () =
      debug (fun _ ->
          "Unsuccessfully tried to apply parameter substitution rule" )
    in
    []

let subst_rule (theta, tps) ((pre', _, _) as seq') ((pre, _, _) as seq) =
  if Seq.equal seq (Seq.subst_tags tps (Seq.subst theta seq')) then
    let tagpairs () =
      if !termination then
        let tagpairs =
          Tagpairs.filter
            (fun (t, t') ->
              Tags.mem t' (Form.tags pre') && Tags.mem t (Form.tags pre)
              )
            (Tagpairs.reflect tps)
        in
        let unmapped =
          Tags.diff (Form.tags pre) (Tagpairs.projectl tagpairs)
        in
        let remaining = Tags.inter unmapped (Form.tags pre') in
        Tagpairs.union tagpairs (Tagpairs.mk remaining)
      else Seq.tagpairs_one
    in
    [ ([(seq', tagpairs (), Tagpairs.empty)], "Subst")
    (* ^ (Format.asprintf " %a" Term.pp_subst theta) *) ]
  else
    let () =
      debug (fun _ -> "Unsuccessfully tried to apply substitution rule!")
    in
    []

let left_or_elim_rule (((_, hs'), cmd', post') as seq')
    ((pre, cmd, post) as seq) =
  try
    if
      Cmd.equal cmd cmd' && Form.equal post post'
      &&
      let _, h = Form.dest pre in
      Blist.exists (Heap.equal h) hs'
    then
      let vt =
        Tagpairs.filter
          (fun tp -> Pair.both (Pair.map Tags.is_free_var tp))
          (Seq.tag_pairs seq)
      in
      [([(seq', vt, Tagpairs.empty)], "L. Cut (Or Elim.)")]
    else
      let () =
        debug (fun _ -> "Unsuccessfully tried to apply left_or_elim rule!")
      in
      []
  with Not_symheap ->
    let () =
      debug (fun _ ->
          "Unsuccessfully tried to apply left_or_elim rule - precondition not \
           a symbolic heap!" )
    in
    []

let left_cut_rule ((pre, cmd, post) as seq) ((pre', cmd', post') as seq') =
  if
    Cmd.equal cmd cmd' && Form.equal post post'
    && Option.is_some (entails pre' pre)
  then
    let valid, progressing =
      if !termination then Seq.get_tracepairs seq' seq
      else (Seq.tagpairs_one, Tagpairs.empty)
    in
    [([(seq, valid, progressing)], "LHS.Conseq")]
  else
    let () = debug (fun _ -> "Unsuccessfully tried to apply left cut rule!") in
    []

let right_cut_rule ((pre, cmd, post) as seq) (pre', cmd', post') =
  if
    Cmd.equal cmd cmd' && Form.equal pre pre'
    && Option.is_some (entails post post')
  then [([(seq, Seq.tag_pairs seq, Tagpairs.empty)], "RHS.Conseq")]
  else
    let () =
      debug (fun _ -> "Unsuccessfully tried to apply right cut rule!")
    in
    []

let ex_intro_rule ((pre, cmd, post) as seq) (pre', cmd', post') =
  if Cmd.equal cmd cmd' && Form.equal post post' then
    try
      let cs, h = Form.dest pre in
      let cs', h' = Form.dest pre' in
      let post_utrms =
        Term.Set.filter Term.is_free_var (Form.vars post)
      in
      let post_utags = Tags.filter Tags.is_free_var (Form.tags post) in
      let update_check ((_, (trm_subst, tag_subst)) as state_update) =
        let result =
          (Fun.list_conj
             [ Unify.Unidirectional.existential_intro
             ; Unify.Unidirectional.avoid_replacing_trms ~inverse:true
                 post_utrms
             ; Unify.Unidirectional.avoid_replacing_tags ~inverse:true
                 post_utags ])
            state_update
        in
        result
      in
      let subst =
        Unify.Unidirectional.realize
          (Unify.Unidirectional.unify_tag_constraints ~total:true
             ~inverse:true ~update_check cs cs'
             (Heap.classical_unify ~inverse:true ~update_check h h'
                Unification.trivial_continuation))
      in
      if Option.is_some subst then
        let _, tag_subst = Option.get subst in
        let tps =
          if !termination then
            Tagpairs.union
              (Tagpairs.mk (Tags.inter (Form.tags pre) (Form.tags pre')))
              tag_subst
          else Seq.tagpairs_one
        in
        [([(seq, tps, Tagpairs.empty)], "Ex.Intro.")]
      else
        let () =
          debug (fun _ ->
              "Unsuccessfully tried to apply the existential introduction rule!"
          )
        in
        []
    with Not_symheap ->
      let () =
        debug (fun _ ->
            "Unsuccessfully tried to apply the existential introduction rule \
             - something was not a symbolic heap!" )
      in
      []
  else
    let () =
      debug (fun _ ->
          "Unsuccessfully tried to apply the existential introduction rule - \
           commands not equal!" )
    in
    []

let seq_rule mid ((pre, cmd, post) as src_seq) =
  try
    let cmd, cont = Cmd.split cmd in
    let left_seq = (pre, cmd, mid) in
    let right_seq = (mid, cont, post) in
    let allpairs, progressing =
      if !termination then Seq.get_tracepairs src_seq right_seq
      else (Seq.tagpairs_one, Tagpairs.empty)
    in
    [ ( [ (left_seq, tagpairs left_seq, Tagpairs.empty)
        ; (right_seq, allpairs, progressing) ]
      , "Seq." ) ]
  with WrongCmd ->
    let () =
      debug (fun _ ->
          "Unsuccessfully tried to apply sequence rule - either command or \
           continuation was empty!" )
    in
    []

let frame_rule frame ((pre, cmd, post) as seq) ((pre', cmd', post') as seq') =
  if
    Cmd.equal cmd cmd'
    && Seq.equal (Seq.frame frame seq) seq'
    && Term.Set.is_empty
         (Term.Set.inter
            (Cmd.modifies ~strict:false cmd)
            (Form.vars frame))
  then [([(seq, Seq.tag_pairs seq, Tagpairs.empty)], "Frame")]
  else
    let () = debug (fun _ -> "Unsuccessfully tried to apply frame rule!") in
    []

let schema_intro_rule (((cs, hs), cmd, post) as seq)
    ((((cs', hs') as pre'), cmd', post') as seq') =
  if
    Cmd.equal cmd cmd'
    && Ord_constraints.subset cs' cs
    && Blist.equal Heap.equal hs hs'
    && Form.equal post post'
  then
    let schema = Ord_constraints.diff cs cs' in
    if Ord_constraints.verify_schemas (Form.tags pre') schema then
      let allpairs, progressing =
        if !termination then Seq.get_tracepairs seq' seq
        else (Seq.tagpairs_one, Tagpairs.empty)
      in
      [([(seq, allpairs, progressing)], "Constraint Schema Intro.")]
    else
      let () =
        debug (fun _ ->
            "Unsuccessfully tried to apply schema introduction rule - was not \
             a valid schema!" )
      in
      []
  else
    let () =
      debug (fun _ -> "Unsuccessfully tried to apply schema introduction rule!")
    in
    []

let transform_seq ((pre, cmd, post) as seq) ?(match_post = true)
    ((pre', cmd', post') as seq') =
  if Cmd.is_assert cmd' || not (Cmd.equal cmd cmd') then Blist.empty
  else if Form.equal pre pre' && Form.equal post post' then
    [(seq, Rule.identity)]
  else
    let dbg = !do_debug in
    do_debug := !do_debug && !show_frame_debug ;
    let () =
      debug (fun _ ->
          "Trying to unify left-hand sides of:" ^ "\n\t" ^ "bud: "
          ^ Seq.to_string seq ^ "\n\t" ^ "candidate companion: "
          ^ Seq.to_string seq' )
    in
    let u_tag_theta =
      Tagpairs.mk_free_subst
        (Tags.union (Seq.all_tags seq) (Seq.all_tags seq'))
        (Tags.filter Tags.is_exist_var (Form.tags pre))
    in
    let u_trm_theta =
      Subst.mk_free_subst
        (Term.Set.union (Seq.all_vars seq) (Seq.all_vars seq'))
        (Term.Set.filter Term.is_exist_var (Form.vars pre))
    in
    let ((ucs, _) as upre) =
      Form.subst_tags u_tag_theta (Form.subst u_trm_theta pre)
    in
    let () =
      debug (fun _ ->
          "Instantiated existentials in bud precondition:" ^ "\n\t"
          ^ Form.to_string upre ^ "\n\t" ^ "tag subst: "
          ^ Tagpairs.to_string u_tag_theta
          ^ "\n\t" ^ "var subst: "
          ^ Term.Map.to_string Term.to_string u_trm_theta )
    in
    let pre_transforms =
      let used_tags =
        Tags.union_of_list
          [ Form.tags upre
          ; Form.tags post
          ; Form.tags pre'
          ; Form.tags post' ]
      in
      abd_pre_transforms (used_tags, Cmd.vars cmd) upre pre'
    in
    let mk_transform (g, g') (trm_subst, tag_subst) =
      let () =
        debug (fun _ ->
            "Found interpolant: (" ^ Form.to_string g ^ ", "
            ^ Form.to_string g' ^ ")" )
      in
      let () =
        debug (fun _ ->
            "Term sub: " ^ Term.Map.to_string Term.to_string trm_subst )
      in
      let () = debug (fun _ -> "Tag sub: " ^ Tagpairs.to_string tag_subst) in
      let ((cs, _) as f) =
        Form.subst trm_subst (Form.subst_tags tag_subst g')
      in
      let trm_theta, trm_subst = Subst.partition trm_subst in
      let tag_theta, tag_subst = Tagpairs.partition_subst tag_subst in
      let f' = Form.subst trm_theta (Form.subst_tags tag_theta post') in
      let used_tags = Tags.union (Form.tags f) (Form.tags f') in
      let used_trms = Term.Set.union (Form.vars f) (Form.vars f') in
      let () =
        debug (fun _ ->
            "Computing frame left over from: " ^ Form.to_string f )
      in
      let abd_schema = Ord_constraints.diff cs ucs in
      let schema_tags =
        Tags.filter Tags.is_free_var
          (Tags.diff (Ord_constraints.tags abd_schema) (Form.tags upre))
      in
      let ((cs_for_frame, _) as g) = Form.add_constraints g abd_schema in
      let frame = Form.compute_frame ~avoid:(used_tags, used_trms) f g in
      assert (Option.is_some frame) ;
      let frame = Form.add_constraints (Option.get frame) cs_for_frame in
      let clashing_prog_vars =
        Term.Set.inter (Cmd.modifies ~strict:false cmd) (Form.vars frame)
      in
      let ex_subst =
        Subst.mk_ex_subst
          (Term.Set.union used_trms (Form.vars frame))
          clashing_prog_vars
      in
      let frame = Form.subst ex_subst frame in
      let () = debug (fun _ -> "Computed frame: " ^ Form.to_string frame) in
      let framed_post = Form.star ~augment_deqs:false f' frame in
      let () =
        debug (fun _ ->
            "Companion postcondition after substitution and framing: "
            ^ Form.to_string framed_post )
      in
      let clashing_utags =
        Tags.inter
          (Tags.union schema_tags
             (Tagpairs.map_to Tags.add Tags.empty snd u_tag_theta))
          (Form.tags framed_post)
      in
      let clashing_uvars =
        Term.Set.inter
          (Term.Set.of_list
             (Blist.map snd (Term.Map.bindings u_trm_theta)))
          (Form.vars framed_post)
      in
      let post_tag_subst =
        Tagpairs.mk_ex_subst
          (Tags.union (Form.tags framed_post) (Form.tags post))
          clashing_utags
      in
      let post_trm_subst =
        Subst.mk_ex_subst
          (Term.Set.union (Form.vars framed_post) (Form.vars post))
          clashing_uvars
      in
      let ex_post =
        Form.subst_tags post_tag_subst
          (Form.subst post_trm_subst framed_post)
      in
      let schema_subst =
        Tagpairs.mk_ex_subst
          (Tags.union (Form.tags pre) (Form.tags ex_post))
          schema_tags
      in
      let schema_subst =
        Tagpairs.union schema_subst (Tagpairs.reflect u_tag_theta)
      in
      let ex_schema = Ord_constraints.subst_tags schema_subst abd_schema in
      if not (Ord_constraints.verify_schemas (Form.tags pre) ex_schema) then
        let () =
          debug (fun () ->
              "Could not verify constraint schema "
              ^ Ord_constraints.to_string ex_schema )
        in
        None
      else
        let pre_with_schema = Form.add_constraints pre ex_schema in
        let subst_avoid_tags =
          Tags.union (Form.tags frame) (Tagpairs.flatten tag_theta)
        in
        let subst_avoid_trms =
          Term.Set.union (Form.vars frame)
            (Term.Map.fold
               (fun x y vs -> Term.Set.add x (Term.Set.add y vs))
               trm_theta Term.Set.empty)
        in
        let post_transforms =
          if match_post then
            Abduce.abd_bi_substs ~allow_frame:false
              ~update_check:
                (Fun.conj
                   (Unify.Bidirectional.updchk_inj_left
                      (Fun.list_conj
                         [ Unify.Unidirectional.is_substitution
                         ; Unify.Unidirectional.avoid_replacing_trms
                             subst_avoid_trms
                         ; Unify.Unidirectional.avoid_replacing_tags
                             subst_avoid_tags ]))
                   (Unify.Bidirectional.updchk_inj_right
                      Unify.Unidirectional.modulo_entl))
              ex_post post
          else
            Some
              ( (ex_post, post)
              , [((trm_theta, tag_theta), Unify.Unidirectional.empty_state)]
              )
        in
        Option.map
          (fun (_, substs) ->
            (* We just need one, since it does not affect subsequent proof search *)
            let subst = Blist.hd substs in
            let theta = fst subst in
            let () = debug (fun _ -> "Left-hand sub:") in
            let () =
              debug (fun _ ->
                  "\tterms: "
                  ^ Term.Map.to_string Term.to_string (fst (fst subst))
              )
            in
            let () =
              debug (fun _ -> "\ttags: " ^ Tagpairs.to_string (snd (fst subst)))
            in
            let () = debug (fun _ -> "Right-hand sub:") in
            let () =
              debug (fun _ ->
                  "\tterms: "
                  ^ Term.Map.to_string Term.to_string (fst (snd subst))
              )
            in
            let () =
              debug (fun _ -> "\ttags: " ^ Tagpairs.to_string (snd (snd subst)))
            in
            let trm_theta =
              Term.Map.union trm_theta (Subst.strip (fst theta))
            in
            let tag_theta =
              Tagpairs.union tag_theta (Tagpairs.strip (snd theta))
            in
            let () =
              debug (fun _ -> "Verified entailment with bud postcondition")
            in
            let () =
              debug (fun _ ->
                  "Final term substitution: "
                  ^ Term.Map.to_string Term.to_string trm_theta )
            in
            let () =
              debug (fun _ ->
                  "Final tag substitution: " ^ Tagpairs.to_string tag_theta )
            in
            let subst_seq =
              Seq.subst_tags tag_theta (Seq.subst trm_theta seq')
            in
            let interpolated_seq = (f, cmd, f') in
            let framed_seq =
              let framed_pre = Form.star ~augment_deqs:false frame f in
              (framed_pre, cmd, framed_post)
            in
            let univ_seq = Seq.with_pre framed_seq g in
            let partial_univ_seq = Seq.with_post univ_seq ex_post in
            let schema_seq = Seq.with_pre partial_univ_seq pre_with_schema in
            let ex_seq = Seq.with_pre partial_univ_seq pre in
            let rule =
              Rule.sequence
                [ ( if (not match_post) || Seq.equal seq ex_seq then
                    Rule.identity
                  else Rule.mk_infrule (right_cut_rule ex_seq) )
                ; ( if Seq.equal ex_seq schema_seq then Rule.identity
                  else Rule.mk_infrule (schema_intro_rule schema_seq) )
                ; ( if Seq.equal schema_seq partial_univ_seq then Rule.identity
                  else Rule.mk_infrule (ex_intro_rule partial_univ_seq) )
                ; ( if Seq.equal partial_univ_seq univ_seq then Rule.identity
                  else Rule.mk_infrule (right_cut_rule univ_seq) )
                ; ( if Seq.equal univ_seq framed_seq then Rule.identity
                  else Rule.mk_infrule (left_cut_rule framed_seq) )
                ; ( if Seq.equal framed_seq interpolated_seq then Rule.identity
                  else Rule.mk_infrule (frame_rule frame interpolated_seq) )
                ; ( if Seq.equal interpolated_seq subst_seq then Rule.identity
                  else Rule.mk_infrule (left_cut_rule subst_seq) )
                ; ( if Seq.equal subst_seq seq' then Rule.identity
                  else Rule.mk_infrule (subst_rule (trm_theta, tag_theta) seq')
                  ) ]
            in
            ((if match_post then seq else ex_seq), rule) )
          post_transforms
    in
    let result =
      Option.dest Blist.empty
        (fun (interpolant, substs) ->
          Option.list_get (Blist.map (mk_transform interpolant) substs) )
        pre_transforms
    in
    let () = debug (fun _ -> "Done") in
    do_debug := dbg ;
    result

let mk_proc_call_rule_seq
    ( (((_, target_cmd, _) as target_seq), param_subst)
    , ((src_pre, src_cmd, src_post) as src_seq)
    , tags_instantiated ) ((link_pre, link_cmd, link_post), bridge_rule) =
  assert (Cmd.is_proc_call target_cmd) ;
  assert (Cmd.is_empty (Cmd.get_cont target_cmd)) ;
  let proc_id, params = Cmd.dest_proc_call target_cmd in
  let prog_cont = Cmd.get_cont src_cmd in
  assert (Cmd.is_proc_call src_cmd) ;
  assert (
    let p, args = Cmd.dest_proc_call src_cmd in
    String.equal p proc_id
    && Int.( = ) (Blist.length args) (Blist.length params) ) ;
  assert (Form.equal src_pre link_pre) ;
  assert ((not (Cmd.is_empty prog_cont)) || Form.equal src_post link_post) ;
  assert (
    Cmd.is_empty prog_cont || Cmd.equal (fst (Cmd.split src_cmd)) link_cmd ) ;
  let ((inst_pre, _, _) as seq_newparams) =
    Seq.param_subst param_subst target_seq
  in
  let tag_inst_rl =
    if tags_instantiated then Rule.mk_infrule (ex_intro_rule src_seq)
    else Rule.identity
  in
  let link_rl, cont_rl =
    if Cmd.is_empty prog_cont then (Rule.identity, Blist.empty)
    else (Rule.mk_infrule (seq_rule link_post), [Rule.identity])
  in
  let or_elim_rl =
    if Form.is_symheap inst_pre then Rule.identity
    else Rule.mk_infrule (left_or_elim_rule seq_newparams)
  in
  let param_rl =
    if Seq.equal seq_newparams target_seq then Rule.identity
    else Rule.mk_infrule (param_subst_rule param_subst target_seq)
  in
  Rule.compose tag_inst_rl
    (Rule.compose_pairwise link_rl
       (Rule.sequence [bridge_rule; or_elim_rl; param_rl] :: cont_rl))

let mk_symex_proc_call procs idx prf =
  let rl =
    let ((pre, cmd, post) as src_seq) = Proof.get_seq idx prf in
    try
      let _ = Form.dest pre in
      let p, args = Cmd.dest_proc_call cmd in
      let proc =
        Blist.find
          (fun x ->
            String.equal (Proc.get_name x) p
            && Int.equal (Blist.length (Proc.get_params x)) (Blist.length args)
            )
          procs
      in
      let param_unifier =
        Term.FList.unify (Proc.get_params proc) args
          Unification.trivial_continuation Subst.empty
      in
      let param_sub = Option.get param_unifier in
      let mk_rules_from_seq proc_seq =
        let (((pre_cs', pre_hs'), _, _) as inst_proc_seq) =
          Seq.param_subst param_sub proc_seq
        in
        let tag_inst_subst =
          Tagpairs.mk_free_subst (Seq.all_tags src_seq)
            (Tags.filter Tags.is_exist_var (Form.tags pre))
        in
        let pre_inst_src_seq =
          Seq.with_pre src_seq (Form.subst_tags tag_inst_subst pre)
        in
        let proc_call_seq =
          Seq.with_cmd pre_inst_src_seq (Cmd.mk_proc_call p args)
        in
        let build_rule_seq =
          mk_proc_call_rule_seq
            ( (proc_seq, param_sub)
            , pre_inst_src_seq
            , not (Tagpairs.is_empty tag_inst_subst) )
        in
        let mk_rules_from_disj h =
          let proc_sh_pre_seq = Seq.with_pre inst_proc_seq (pre_cs', [h]) in
          let transforms =
            transform_seq proc_call_seq
              ~match_post:(Cmd.is_empty (Cmd.get_cont cmd))
              proc_sh_pre_seq
          in
          Blist.map build_rule_seq transforms
        in
        Blist.bind mk_rules_from_disj pre_hs'
      in
      Rule.choice (Blist.bind mk_rules_from_seq (Proc.get_seqs proc))
    with
    | Not_symheap | WrongCmd | Not_found -> Rule.fail
  in
  rl idx prf

let dobackl ?(get_targets = Rule.all_nodes) ?(choose_all = false) idx prf =
  let src_seq = Proof.get_seq idx prf in
  let targets = get_targets idx prf in
  let ident, rest =
    Blist.partition
      (fun idx -> Seq.equal src_seq (Proof.get_seq idx prf))
      targets
  in
  let targets = ident @ rest in
  let () =
    debug (fun _ -> "Beginning calculation of potential backlink targets")
  in
  let transformations =
    Blist.bind
      (fun idx' ->
        Blist.map
          (fun (_, rule) -> (idx', rule))
          (transform_seq src_seq (Proof.get_seq idx' prf)) )
      targets
  in
  let () =
    debug (fun _ -> "Finished calculation of potential backlink targets")
  in
  let mk_backlink (targ_idx, rule_sequence) =
    let targ_seq = Proof.get_seq targ_idx prf in
    Rule.compose rule_sequence
      (Rule.mk_backrule false
         (fun _ _ -> [targ_idx])
         (fun s s' ->
           let tps =
             if !termination then Seq.tag_pairs targ_seq else Seq.tagpairs_one
           in
           [(tps, "Backl")] ))
  in
  if choose_all then Rule.first (Blist.map mk_backlink transformations) idx prf
  else Rule.choice (Blist.map mk_backlink transformations) idx prf

let use_proc_prf prf_cache idx prf =
  let pre, cmd, post = Proof.get_seq idx prf in
  try
    let proc = Cmd.dest_proc_call cmd in
    if not (Cmd.is_empty (Cmd.get_cont cmd)) then Rule.fail idx prf
    else
      let signature = (proc, (pre, post)) in
      let proc_prf = Option.get (Proc.SigMap.find signature !prf_cache) in
      [([], Proof.add_subprf proc_prf idx prf)]
  with
  | WrongCmd | Not_found | Invalid_argument _ -> Rule.fail idx prf

(* let generalise_while_rule =                                                                                                                                                                                                                 *)
(*     let rl seq =                                                                                                                                                                                                                            *)
(*       let generalise m h =                                                                                                                                                                                                                  *)
(*         let avoid = ref (Seq.vars seq) in                                                                                                                                                                                                   *)
(*         let gen_term t =                                                                                                                                                                                                                    *)
(*           if Term.Set.mem t m then                                                                                                                                                                                                       *)
(*             (let r = fresh_evar !avoid in avoid := Term.Set.add r !avoid ; r)                                                                                                                                                            *)
(*           else t in                                                                                                                                                                                                                         *)
(*         let gen_pto (x,args) =                                                                                                                                                                                                              *)
(*           let l = Blist.map gen_term (x::args) in (Blist.hd l, Blist.tl l) in                                                                                                                                                               *)
(*             Heap.mk                                                                                                                                                                                                                           *)
(*               (Term.Set.fold Uf.remove m h.Heap.eqs)                                                                                                                                                                                    *)
(*               (Deqs.filter                                                                                                                                                                                                               *)
(*                 (fun p -> Pair.conj (Pair.map (fun z -> not (Term.Set.mem z m)) p))                                                                                                                                                      *)
(*                 h.Heap.deqs)                                                                                                                                                                                                                  *)
(*               (Ptos.map gen_pto h.Heap.ptos)                                                                                                                                                                                           *)
(*               h.Heap.inds in                                                                                                                                                                                                                  *)
(*       try                                                                                                                                                                                                                                   *)
(*         let (pre, cmd, post) = dest_sh_seq seq in                                                                                                                                                                                           *)
(*         let (_, cmd') = Cmd.dest_while cmd in                                                                                                                                                                                               *)
(*         let m = Term.Set.inter (Cmd.modifies cmd') (Heap.vars pre) in                                                                                                                                                                 *)
(*         let subs = Term.Set.subsets m in                                                                                                                                                                                                 *)
(*         Option.list_get (Blist.map                                                                                                                                                                                                          *)
(*           begin fun m' ->                                                                                                                                                                                                                   *)
(*             let pre' = generalise m' pre in                                                                                                                                                                                                 *)
(*             if Heap.equal pre pre' then None else                                                                                                                                                                                        *)
(*             let s' = ([pre'], cmd, post) in                                                                                                                                                                                                 *)
(*             Some ([ (s', tagpairs s', Tagpairs.empty) ], "Gen.While")                                                                                                                                                                       *)
(*           end                                                                                                                                                                                                                               *)
(*           subs)                                                                                                                                                                                                                             *)
(*     with Not_symheap | WrongCmd -> [] in                                                                                                                                                                                                    *)
(*   Rule.mk_infrule rl                                                                                                                                                                                                                        *)

let axioms = ref Rule.fail

let rules = ref Rule.fail

let setup (defs, procs, prf_cache) =
  let () = Rules.setup defs in
  let () = Abduce.set_defs defs in
  let () = check_invalid := Invalid.check defs in
  let symex_proc_unfold = mk_symex_proc_unfold procs prf_cache in
  let symex_proc_call = mk_symex_proc_call procs in
  rules :=
    Rule.first
      [ (* Simplification *)
        lhs_disj_to_symheaps
      ; simplify
      ; (* Assertions *)
        assert_rule
      ; (* While loops *)
        Rule.choice
          [ Rule.conditional
              (fun (_, cmd, _) -> Cmd.is_while cmd)
              (fun idx prf -> dobackl idx prf)
          ; Rule.compose (Rule.attempt lab_ex_intro) symex_while_rule ]
      ; (* Procedure calls *)
        Rule.conditional
          (fun (_, cmd, _) ->
            Cmd.is_proc_call cmd && Cmd.is_empty (Cmd.get_cont cmd) )
          (dobackl ~get_targets:Rule.syntactically_equal_nodes)
      ; use_proc_prf prf_cache
      ; symex_proc_unfold
      ; symex_proc_call
      ; (* Atomic symbolic execution *)
        symex_skip_rule
      ; symex_assign_rule
      ; symex_load_rule
      ; symex_store_rule
      ; symex_free_rule
      ; symex_new_rule
      ; symex_if_rule
      ; (* Branching constructs *)
        symex_ifelse_rule
      ; (* Parallel composition*)
        symex_parallel_rule
      ; (* Predicate unfolding *)
        luf defs ] ;
  let axioms = Rule.first [ex_falso_axiom; mk_symex_empty_axiom] in
  rules := Rule.combine_axioms axioms !rules
