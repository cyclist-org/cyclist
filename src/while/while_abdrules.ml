open Lib
open Generic
open Seplog

open While
open   While_program

module SH = Heap

exception Not_symheap = Form.Not_symheap

module Defs = Defs
module Rule = Proofrule.Make (While_program.Seq)
module Seqtactics = Seqtactics.Make (While_program.Seq)
module Abdrule = Abdrule.Make (While_program.Seq) (Defs)

(* let latex_defs d =                 *)
(*   let t = !split_heaps in          *)
(*   let () = split_heaps := false in *)
(*   let res = Defs.to_latex d in     *)
(*   split_heaps := t ; res           *)

let dest_sh_seq = While_rules.dest_sh_seq

let last_pred = ref 0

let get_fresh_ident () =
  Predsym.of_string (Printf.sprintf "I%.3d" (incr last_pred ; !last_pred))

let get_undefined defs h = Tpreds.filter (Defs.is_undefined defs) h.SH.inds

let ex_subst_defs defs =
  let ex_subst_heap h =
    let ex_eqs, non_ex_eqs =
      Blist.partition
        (fun (x, _) -> Term.is_exist_var x)
        (Uf.bindings h.SH.eqs)
    in
    if Blist.is_empty ex_eqs then h
    else
      (* NB order of subst is reversed so that *)
      (* the greater variable replaces the lesser *)
      (* this maintains universal vars *)
      Heap.subst
        (Term.Map.of_list ex_eqs)
        (SH.with_eqs h (Uf.of_list non_ex_eqs))
  in
  let ex_subst_case c =
    let p, h = Indrule.dest c in
    let p' = Heap.fixpoint ex_subst_heap p in
    Indrule.mk p' h
  in
  Defs.of_list
    (Blist.map
       (fun def ->
         Preddef.mk
           ( Blist.map ex_subst_case (Preddef.rules def)
           , Preddef.predsym def ) )
       (Defs.to_list defs))

let empify defs =
  let empify_ c =
    let p, h = Indrule.dest c in
    let inds = Tpreds.filter (Defs.is_defined defs) p.SH.inds in
    Indrule.mk (SH.with_inds p inds) h
  in
  Defs.of_list
    (Blist.map
       (fun def ->
         Preddef.mk
           (Blist.map empify_ (Preddef.rules def), Preddef.predsym def)
         )
       (Defs.to_list defs))

let inline defs =
  let defs = Defs.to_list defs in
  try
    let q =
      Blist.find
        (fun def ->
          let p, h = Preddef.dest def in
          (match p with [_] -> true | _ -> false)
          &&
          let f, _ = Indrule.dest (Blist.hd p) in
          let idents =
            Tpreds.map_to Predsym.Set.add Predsym.Set.empty
              (fun (_, (id, _)) -> id)
              f.SH.inds
          in
          not (Predsym.Set.mem h idents) )
        (Blist.but_last defs)
    in
    let p, h = Preddef.dest q in
    let defs = Blist.filter (( != ) q) defs in
    let case = Blist.hd p in
    let f orig =
      let p', h' = Indrule.dest orig in
      let first_unfold f =
        try
          let pred =
            Tpreds.find
              (fun (_, (h'', _)) -> Predsym.equal h h'')
              f.SH.inds
          in
          let f = SH.with_inds f (Tpreds.remove pred f.SH.inds) in
          let g =
            Indrule.unfold ~gen_tags:false
              (Indrule.vars orig, Tags.empty)
              pred case
          in
          Heap.star f g
        with Not_found -> f
      in
      let p'' = Heap.fixpoint first_unfold p' in
      Indrule.mk p'' h'
    in
    Defs.of_list
      (Blist.map
         (fun def ->
           Preddef.mk
             (Blist.map f (Preddef.rules def), Preddef.predsym def) )
         defs)
  with Not_found -> Defs.of_list defs

let used_only_recursively (heap, (ident, params)) pos =
  let var = Blist.nth params pos in
  let heap' = SH.with_inds heap Tpreds.empty in
  (not (Term.Set.mem var (Heap.vars heap')))
  && Tpreds.for_all
       (fun (_, (ident', params')) ->
         if Predsym.equal ident ident' then
           not (Blist.exists (Term.equal var) params')
         else
           Blist.for_all2
             (fun var' pos' ->
               Int.( = ) pos pos' || not (Term.equal var var') )
             params' (Blist.indexes params') )
       heap.SH.inds

(* for all predicates *)
(* for all integers up to the arity of the predicate *)
(* if the position is unused in all clauses of the definition*)
(* then stop and report predicate and position *)
(* when all are exhausted return None *)
let find_unused_arg defs =
  let defs = Defs.to_list defs in
  let check_def def =
    let clauses, ident = Preddef.dest def in
    if Blist.is_empty clauses then None
    else
      let check_pos pos =
        if
          Blist.for_all
            (fun case ->
              let ((heap, (_, params)) as p) = Indrule.dest case in
              (not (Term.Set.mem (Blist.nth params pos) (Heap.vars heap)))
              || used_only_recursively p pos )
            clauses
        then Some (ident, pos)
        else None
      in
      Blist.find_map check_pos
        (Blist.range 0 (snd (snd (Indrule.dest (Blist.hd clauses)))))
  in
  if Int.( = ) (Blist.length defs) 1 then None
  else Blist.find_map check_def (Blist.but_last defs)

let eliminate (ident, pos) defs =
  let elim_clause case =
    let heap, (ident', params) = Indrule.dest case in
    let elim_pred heap =
      SH.with_inds heap
        (Tpreds.map
           (fun (t, (ident'', params')) ->
             ( t
             , ( ident''
               , if Predsym.equal ident ident'' then
                   Blist.remove_nth pos params'
                 else params' ) ) )
           heap.SH.inds)
    in
    if not (Predsym.equal ident ident') then
      Indrule.mk (elim_pred heap) (ident', params)
    else Indrule.mk (elim_pred heap) (ident', Blist.remove_nth pos params)
  in
  Defs.of_list
    (Blist.map
       (fun def ->
         let cl, ident' = Preddef.dest def in
         Preddef.mk (Blist.map elim_clause cl, ident') )
       (Defs.to_list defs))

let elim_dead_vars defs =
  match find_unused_arg defs with
  | None -> defs
  | Some (ident, pos) -> eliminate (ident, pos) defs

let simplify_defs defs =
  Defs.fixpoint
    (fun d -> elim_dead_vars (inline (ex_subst_defs d)))
    (empify defs)

let is_sat defs = Basepair.satisfiable (empify defs)

let ex_falso_axiom = Abdrule.lift While_rules.ex_falso_axiom

let symex_empty_axiom = Abdrule.lift While_rules.symex_empty_axiom

let lhs_disj_to_symheaps = Abdrule.lift While_rules.lhs_disj_to_symheaps

let eq_subst_ex = While_rules.eq_subst_ex_f

let simpl_deqs seq =
  try
    let (cs, f), cmd = dest_sh_seq seq in
    let terms =
      Term.Set.add Term.nil (Heap.vars (SH.with_deqs f Deqs.empty))
    in
    let newdeqs =
      Deqs.filter
        (fun (x, y) ->
          Term.equal x y
          || (Term.Set.mem x terms && Term.Set.mem y terms) )
        f.SH.deqs
    in
    let f' = SH.with_deqs f newdeqs in
    if Heap.equal f f' then []
    else
      let s = ((cs, [f']), cmd) in
      [([(s, While_rules.tagpairs s, Tagpairs.empty)], "Simpl Deqs")]
  with Not_symheap -> []

let simplify =
  Abdrule.lift
    (Rule.mk_infrule
       (Seqtactics.relabel "Simplify"
          (Seqtactics.repeat (Seqtactics.first [eq_subst_ex; simpl_deqs]))))

let wrap r = Abdrule.compose r (Abdrule.attempt simplify)

(* symbolic execution rules *)
let symex_stop_axiom = Abdrule.lift While_rules.symex_stop_axiom

let symex_load_rule = Abdrule.lift While_rules.symex_load_rule

let symex_store_rule = Abdrule.lift While_rules.symex_store_rule

let symex_free_rule = Abdrule.lift While_rules.symex_free_rule

let symex_new_rule = Abdrule.lift While_rules.symex_new_rule

let symex_skip_rule = Abdrule.lift While_rules.symex_skip_rule

let symex_assign_rule = Abdrule.lift While_rules.symex_assign_rule

let symex_nondet_if_rule =
  let rl seq =
    try
      let (cs, f), cmd = dest_sh_seq seq in
      let c, cmd' = Cmd.dest_if cmd in
      if Cond.is_det c then []
      else
        let cont = Cmd.get_cont cmd in
        While_rules.fix_tps
          [ ( [((cs, [f]), Cmd.mk_seq cmd' cont); ((cs, [f]), cont)]
            , "If(nondet)" ) ]
    with
    | Not_symheap | WrongCmd -> []
  in
  Abdrule.lift (Rule.mk_infrule rl)

let symex_nondet_ifelse_rule =
  let rl seq =
    try
      let (cs, f), cmd = dest_sh_seq seq in
      let c, cmd', cmd'' = Cmd.dest_ifelse cmd in
      if Cond.is_det c then []
      else
        let cont = Cmd.get_cont cmd in
        While_rules.fix_tps
          [ ( [ ((cs, [f]), Cmd.mk_seq cmd' cont)
              ; ((cs, [f]), Cmd.mk_seq cmd'' cont) ]
            , "IfElse(nondet)" ) ]
    with
    | Not_symheap | WrongCmd -> []
  in
  Abdrule.lift (Rule.mk_infrule rl)

let symex_nondet_while_rule =
  let rl seq =
    try
      let (cs, f), cmd = dest_sh_seq seq in
      let c, cmd' = Cmd.dest_while cmd in
      if Cond.is_det c then []
      else
        let cont = Cmd.get_cont cmd in
        While_rules.fix_tps
          [ ( [((cs, [f]), Cmd.mk_seq cmd' cmd); ((cs, [f]), cont)]
            , "If(nondet)" ) ]
    with
    | Not_symheap | WrongCmd -> []
  in
  Abdrule.lift (Rule.mk_infrule rl)

let symex_det_if_rule =
  let rl seq =
    try
      let (cs, f), cmd = dest_sh_seq seq in
      let c, cmd' = Cmd.dest_if cmd in
      if Cond.is_non_det c then []
      else
        let x, y = Cond.dest c in
        let cont = Cmd.get_cont cmd in
        let mk_ret c = While_rules.fix_tps [([((cs, [f]), c)], "If(det)")] in
        match
          (Cond.is_deq c, Heap.equates f x y, Heap.disequates f x y)
        with
        (* cmd wants equality and formula provides it so take the branch *)
        (* cmd wants disequality and formula provides it so take branch *)
        | false, true, _ | true, _, true -> mk_ret (Cmd.mk_seq cmd' cont)
        (* cmd wants equality and formula forbids it so take other branch *)
        (* cmd wants disequality and formula forbids it so take other branch *)
        | false, _, true | true, true, _ -> mk_ret cont
        (* formula allows either fact so fail *)
        (* or otherwise don't know so fail *)
        | _ -> []
    with
    | Not_symheap | WrongCmd -> []
  in
  Abdrule.lift (Rule.mk_infrule rl)

let symex_det_ifelse_rule =
  let rl seq =
    try
      let (cs, f), cmd = dest_sh_seq seq in
      let c, cmd', cmd'' = Cmd.dest_ifelse cmd in
      if Cond.is_non_det c then []
      else
        let x, y = Cond.dest c in
        let cont = Cmd.get_cont cmd in
        let mk_ret c = While_rules.fix_tps [([((cs, [f]), c)], "If(det)")] in
        match
          (Cond.is_deq c, Heap.equates f x y, Heap.disequates f x y)
        with
        (* cmd wants equality and formula provides it so take the branch *)
        (* cmd wants disequality and formula provides it so take branch *)
        | false, true, _ | true, _, true -> mk_ret (Cmd.mk_seq cmd' cont)
        (* cmd wants equality and formula forbids it so take other branch *)
        (* cmd wants disequality and formula forbids it so take other branch *)
        | false, _, true | true, true, _ -> mk_ret (Cmd.mk_seq cmd'' cont)
        (* formula allows either fact so fail *)
        (* or don't know so fail *)
        | _ -> []
    with
    | Not_symheap | WrongCmd -> []
  in
  Abdrule.lift (Rule.mk_infrule rl)

let symex_det_while_rule =
  let rl seq =
    try
      let (cs, f), cmd = dest_sh_seq seq in
      let c, cmd' = Cmd.dest_while cmd in
      if Cond.is_non_det c then []
      else
        let x, y = Cond.dest c in
        let cont = Cmd.get_cont cmd in
        let mk_ret c =
          While_rules.fix_tps [([((cs, [f]), c)], "While(det)")]
        in
        match
          (Cond.is_deq c, Heap.equates f x y, Heap.disequates f x y)
        with
        (* cmd wants equality and formula provides it so take the branch *)
        (* cmd wants disequality, formula provides it so take branch *)
        | false, true, _ | true, _, true -> mk_ret (Cmd.mk_seq cmd' cmd)
        (* cmd wants equality and formula forbids it so take other branch *)
        (* cmd wants disequality and formula forbids it so take other branch *)
        | false, _, true | true, true, _ -> mk_ret cont
        (* formula allows either fact so fail *)
        (* otherwise don't know so fail *)
        | _ -> []
    with
    | Not_symheap | WrongCmd -> []
  in
  Abdrule.lift (Rule.mk_infrule rl)

let generalise_while_rule =
  let generalise m h =
    let avoid = ref (Heap.vars h) in
    let gen_term t =
      if Term.Set.mem t m then (
        let r = fresh_evar !avoid in
        avoid := Term.Set.add r !avoid ;
        r )
      else t
    in
    let gen_pto (x, args) =
      let l = Blist.map gen_term (x :: args) in
      (Blist.hd l, Blist.tl l)
    in
    SH.mk
      (Term.Set.fold Uf.remove m h.SH.eqs)
      (Deqs.filter
         (fun p -> Pair.conj (Pair.map (fun z -> not (Term.Set.mem z m)) p))
         h.SH.deqs)
      (Ptos.map gen_pto h.SH.ptos)
      h.SH.inds
  in
  let rl seq =
    try
      let (cs, f), cmd = dest_sh_seq seq in
      let _, cmd' = Cmd.dest_while cmd in
      let m = Term.Set.inter (Cmd.modifies cmd') (Heap.vars f) in
      let subs = Term.Set.subsets m in
      Option.list_get
        (Blist.map
           (fun m' ->
             let f' = generalise m' f in
             if Heap.equal f f' then None
             else
               let s' = ((cs, [f']), cmd) in
               Some
                 ([(s', While_rules.tagpairs s', Tagpairs.empty)], "Gen.While")
             )
           subs)
    with
    | Not_symheap | WrongCmd -> []
  in
  Abdrule.lift (Rule.mk_infrule rl)

(* abduction rules*)

let abd_deref =
  let rl seq defs =
    debug (fun () -> "Abd deref") ;
    try
      let (cs, f), cmd = dest_sh_seq seq in
      let x = Cmd.dest_deref cmd in
      let inds = Tpreds.to_list (get_undefined defs f) in
      if Blist.is_empty inds then []
      else
        let fresh_ident = get_fresh_ident () in
        let f (_, (ident, params)) =
          let newparams =
            fresh_fvars Term.Set.empty (Blist.length params)
          in
          let head = (ident, newparams) in
          let pto_params =
            fresh_evars
              (Term.Set.of_list newparams)
              (Field.get_no_fields ())
          in
          let newxs =
            Blist.map (Blist.nth newparams)
              (Blist.find_indexes (Heap.equates f x) params)
          in
          Blist.map
            (fun newx ->
              let clause =
                Heap.star
                  (Heap.mk_pto (newx, pto_params))
                  (Heap.mk_ind
                     (Tags.anonymous, (fresh_ident, newparams @ pto_params)))
              in
              Defs.add
                (Preddef.mk ([Indrule.mk clause head], ident))
                defs )
            newxs
        in
        Blist.bind f inds
    with
    | Not_symheap | WrongCmd -> []
  in
  Abdrule.mk_abdinfrule rl

let abd_det_guard =
  let rl seq defs =
    try
      let (cs, f), cmd = dest_sh_seq seq in
      if not (Cmd.is_if cmd || Cmd.is_ifelse cmd || Cmd.is_while cmd) then []
      else
        let c =
          if Cmd.is_if cmd then fst (Cmd.dest_if cmd)
          else if Cmd.is_ifelse cmd then
            let c, _, _ = Cmd.dest_ifelse cmd in
            c
          else fst (Cmd.dest_while cmd)
        in
        if Cond.is_non_det c then []
        else
          let cv_size = Term.Set.cardinal (Cond.vars c) in
          if Int.( = ) cv_size 0 then []
          else
            let with_nil = Int.( = ) cv_size 1 in
            let x, y = Cond.dest c in
            let inds = Tpreds.to_list (get_undefined defs f) in
            if Blist.is_empty inds then []
            else
              let fresh_ident, fresh_ident' =
                Pair.map get_fresh_ident ((), ())
              in
              let f (_, (ident, params)) =
                let newparams =
                  fresh_fvars Term.Set.empty (Blist.length params)
                in
                let head = (ident, newparams) in
                let matches nil_const z =
                  if nil_const && Term.is_nil z then [Term.nil]
                  else
                    Blist.map (Blist.nth newparams)
                      (Blist.find_indexes (Heap.equates f z) params)
                in
                let occurrences =
                  Blist.cartesian_product (matches false x) (matches false y)
                  @
                  if with_nil then
                    Blist.cartesian_product (matches true x) (matches true y)
                  else []
                in
                let g pair =
                  let clause_eq =
                    SH.mk
                      (Uf.of_list [pair])
                      Deqs.empty Ptos.empty
                      (Tpreds.singleton
                         (Tags.anonymous, (fresh_ident, newparams)))
                  in
                  let clause_deq =
                    SH.mk Uf.empty (Deqs.singleton pair) Ptos.empty
                      (Tpreds.singleton
                         (Tags.anonymous, (fresh_ident', newparams)))
                  in
                  Defs.add
                    (Preddef.mk
                       ( [ Indrule.mk clause_eq head
                         ; Indrule.mk clause_deq head ]
                       , ident ))
                    defs
                in
                Blist.map g occurrences
              in
              Blist.bind f inds
    with Not_symheap -> []
  in
  Abdrule.mk_abdinfrule rl

let abd_back_rule =
  let rl s1 s2 defs =
    try
      let ((cs1, l1), cmd1), ((cs2, l2), cmd2) =
        Pair.map dest_sh_seq (s1, s2)
      in
      if
        (not (Cmd.equal cmd1 cmd2))
        || Int.( < )
             (Deqs.cardinal l1.SH.deqs)
             (Deqs.cardinal l2.SH.deqs)
        || Int.( < )
             (Ptos.cardinal l1.SH.ptos)
             (Ptos.cardinal l2.SH.ptos)
      then []
      else
        (* find multiset of identifiers of ind preds in s1/s2 *)
        let inds1, inds2 = Pair.map Heap.idents (l1, l2) in
        (* find fresh ones in s1 *)
        let candidates = get_undefined defs l1 in
        (* discard those that already exist in s2 *)
        let candidates =
          Tpreds.filter
            (fun (_, (ident, _)) ->
              Tpreds.for_all
                (fun (_, (ident', _)) -> not (Predsym.equal ident ident'))
                l2.SH.inds )
            candidates
        in
        (* for each candidate there must exist one in s2 which *)
        (* if it replaces the candidate in s1, makes inds2 a subset of inds1 *)
        (* this is to overapproximate subsumption *)
        let cp =
          Blist.cartesian_product
            (Tpreds.to_list candidates)
            (Tpreds.to_list l2.SH.inds)
        in
        let cp =
          Blist.filter
            (fun ((_, (c, _)), (_, (c', _))) ->
              Predsym.MSet.subset inds2
                (Predsym.MSet.add c' (Predsym.MSet.remove c inds1)) )
            cp
        in
        if Blist.is_empty cp then []
        else
          let fresh_ident = get_fresh_ident () in
          let f ((_, (c, params)), (_, (c', params'))) =
            let newparams =
              fresh_fvars Term.Set.empty (Blist.length params)
            in
            (* does this need generalising like det_guard? *)
            let matches z =
              if Term.is_var z then
                Blist.map (Blist.nth newparams)
                  (Blist.find_indexes (Heap.equates l1 z) params)
              else [Term.nil]
            in
            let combinations = Blist.choose (Blist.map matches params') in
            Blist.map
              (fun perm ->
                let cl =
                  SH.with_inds Heap.empty
                    (Tpreds.of_list
                       [ (Tags.anonymous, (c', perm))
                       ; (Tags.anonymous, (fresh_ident, newparams)) ])
                in
                Defs.add
                  (Preddef.mk ([Indrule.mk cl (c, newparams)], c))
                  defs )
              combinations
          in
          Blist.bind f cp
    with Not_symheap -> []
  in
  Abdrule.mk_abdbackrule Rule.all_nodes rl

let matches = Abdrule.lift While_rules.dobackl

(* NOT UPDATED FOR TERMINATION *)
(* let abd_segment =                                                                         *)
(*   let rl seq defs =                                                                       *)
(*     try                                                                                   *)
(*       let (h,cmd) = dest_sh_seq seq in                                                    *)
(*       let (c,cmd') = Cmd.dest_while cmd in                                                *)
(* 			(* only consider disequality guards *)                                              *)
(* 			if not (Cond.is_deq c) then [] else                                                 *)
(* 		  let guard_vars = Cond.vars c in                                                     *)
(* 			let guard_terms = Cond.terms c in                                                   *)
(* 			(* only consider disequalities to nil *)                                            *)
(* 			if Term.Set.is_empty guard_terms then [] else                                       *)
(* 			let mod_vars = Cmd.modifies cmd' in                                                 *)
(* 			let gen_vars = Term.Set.inter guard_vars mod_vars in                                *)
(* 			if Term.Set.cardinal gen_vars <> 1 then [] else                                     *)
(* 		  let y = Term.Set.choose gen_vars in                                                 *)
(*   		let eq_y = Term.Set.filter Term.is_free_var	(Heap.eq_class h y) in                 *)
(* 			(* xs are prog vars equal to y now, and unmodified by the loop body *)              *)
(* 			let xs = Term.Set.diff eq_y mod_vars in                                             *)
(* 			(* is this always needed ? *)                                                       *)
(* 			if Term.Set.is_empty xs then [] else                                                *)
(*       (* filter formula predicates by undefinedness *)                                    *)
(*       let inds =                                                                          *)
(* 				Tpreds.filter (fun pred -> not (Defs.is_defined pred defs)) h.inds in               *)
(*       (* further filter by having a y-equiv term in parameter list, *)                    *)
(*       let inds =                                                                          *)
(*         Tpreds.filter (fun (_,(_,params)) ->                                                *)
(*           Blist.exists (fun x -> Term.Set.mem x xs) params) inds in                       *)
(* 		  if Tpreds.is_empty inds then [] else                                                  *)
(* 		  (* weaken by removing y from its equivalence class *)                               *)
(* 			let h' = { h with eqs=Uf.remove y h.eqs } in                                        *)
(* 			(* is this always needed ? *)                                                       *)
(* 			if Heap.equal h h' then [] else                                                     *)
(*       let fresh_ident = get_fresh_ident () in                                             *)
(*       let f ((_,(ident, params)) as i) =                                                  *)
(*         let newparams = fresh_fvars Term.Set.empty (Blist.length params) in               *)
(* 				let xi = Blist.find_index (fun x -> Term.Set.mem x xs) params in                  *)
(*         let x = Blist.nth newparams xi in                                                 *)
(* 				let head = (ident, newparams) in                                                  *)
(* 				let newpred = (1, (fresh_ident, newparams @ [x])) in                              *)
(* 				let newpred' = (1, (fresh_ident, newparams @ [Term.nil])) in                      *)
(*         let clause = { Heap.empty with inds=Tpreds.of_list [newpred; newpred']} in          *)
(*         let new_defs = ( [Indrule.mk clause head], ident )::defs in                          *)
(* 				let h' = { h' with inds=Tpreds.filter ((!=)i) h'.inds } in                          *)
(* 				let newpred = (1, (fresh_ident, params @ [y])) in                                 *)
(* 				let newpred' = (1, (fresh_ident, Blist.replace_nth y xi params @ [Term.nil])) in  *)
(* 				let h' = { h' with inds=Tpreds.union h'.inds (Tpreds.of_list [newpred;newpred']) } in *)
(* 				let () = debug (fun () -> "Heap: " ^ (Heap.to_string h')) in                      *)
(* 				let () = debug (fun () -> "Clause: " ^ (Heap.to_string clause)) in                *)
(*         Some ([ (([h'], cmd), While_rules.symex_tagpairs, Tagpairs.empty) ], new_defs) in *)
(*       Option.list_get (Blist.map f (Tpreds.to_list inds))                                   *)
(*     with Not_symheap | WrongCmd -> [] in                                                  *)
(*   mk_gen_rule rl "Abd. segment"                                                           *)

(* let abd_backlink_cut =                                                           *)
(*   let rl s1 s2 defs =                                                            *)
(*     try                                                                          *)
(*       let ((l1,cmd1),(l2,cmd2)) = Pair.map dest_sh_seq (s1,s2) in                *)
(*   	  if not (Cmd.equal cmd1 cmd2) then [] else                                  *)
(* 			let () = debug (fun () -> "CUTLINK1: trying: " ^ (Seq.to_string s2)) in    *)
(* 			let () = debug (fun () -> "                  " ^ (Seq.to_string s1)) in    *)
(*       let (ids1,ids2) = Pair.map Heap.get_idents (l1,l2) in                      *)
(* 			let (ids1,ids2) =                                                          *)
(* 				(Predsym.MSet.diff ids1 ids2, Predsym.MSet.diff ids2 ids1) in                *)
(* 			if Predsym.MSet.cardinal ids1 <> 1 || Predsym.MSet.cardinal ids2 <> 1 then     *)
(* 				(* (print_endline ("1" ^                                            *)   *)
(* 				(* (Predsym.MSet.to_string ids1) ^ "/" ^  (Predsym.MSet.to_string ids2) *)   *)
(* 				[] else                                                                  *)
(* 		  let (id1,id2) = Pair.map Predsym.MSet.choose (ids1,ids2) in                  *)
(* 			if Defs.mem id1 defs then [] else                                          *)
(* 			let f i h = Tpreds.filter (fun (_,(i',_)) -> i=i') h.inds in                 *)
(* 			let (inds1,inds2) = (f id1 l1, f id2 l2) in                                *)
(*       let cp = Blist.cartesian_product (Tpreds.to_list inds1) (Tpreds.to_list inds2) in    *)
(*       let g ((_,(_,params)),(_,(_,params'))) =                                   *)
(*         let newparams = fresh_fvars Term.Set.empty (Blist.length params) in       *)
(*   			(* does this need generalising like det_guard? *)                        *)
(*   			let matches z =                                                          *)
(*           (* if Term.is_var z then *)                                            *)
(*           	List.map (Blist.nth newparams) (Blist.find_indexes (Heap.equates l1 z) params)   *)
(*           (* else [Term.nil]  *)                                                 *)
(*   				in                                                                     *)
(*   			let combinations = choose (Blist.map matches params') in                  *)
(*   			List.map                                                                 *)
(*           (fun perm ->                                                           *)
(*   					let cl = { Heap.empty with inds=Tpreds.singleton (1,(id2,perm)) } in   *)
(*             (( [Indrule.mk cl (id1, newparams)], id1 )::defs))                      *)
(*   			  combinations in                                                        *)
(*       Blist.bind g cp                                               *)
(*     with Not_symheap | WrongCmd -> [] in                                         *)
(*   mk_abd_back_rule rl "Abd. Cut backlink"                                        *)

(* let abd_mid_backlink_cut =                                                       *)
(*   let rl _ _ defs =                                                              *)
(* 		let () = debug (fun () -> "CUTLINK2: ") in                                   *)
(* 		let () = debug (fun () -> Defs.to_string (empify defs)) in                   *)
(* 		let l = Blist.length defs in                                                  *)
(* 		if l<2 then [] else                                                          *)
(* 		begin                                                                        *)
(*   		Slprover.minbound := 1;                                                    *)
(*   		Slprover.maxbound := 11;                                                   *)
(*   		Slprover.setup (empify defs) ;                                             *)
(*   		[defs]                                                                     *)
(* 		end in                                                                       *)
(*   mk_abd_back_rule rl "Abd. Mid Cut backlink"                                    *)

(* let step = ref 0                                                                 *)

(* let backlink_cut =                                                               *)
(*   let rl s1 s2 =                                                                 *)
(* 		let () = incr step in                                                        *)
(*     let ((l1,cmd1),(l2,cmd2)) = (s1,s2) in                                       *)
(* 		let () = debug (fun () -> "CUTLINK3: trying: " ^ (Seq.to_string s2)) in      *)
(* 		let () = debug (fun () -> "                  " ^ (Seq.to_string s1)) in      *)
(* 		let () = debug (fun () -> "CUTLINK3: step = " ^ (string_of_int !step)) in    *)
(* 		if !step <> 22 then None else                                                *)
(* 	  if not (Cmd.equal cmd1 cmd2) then                                            *)
(* 			(debug (fun () -> "CUTLINK3: cmds unequal") ; None) else                   *)
(*     let olddebug = !Lib.do_debug in                                              *)
(*     let () = Lib.do_debug := true in                                             *)
(*     let result = Option.is_some (Slprover.idfs (l1, l2)) in                      *)
(*     let () = Lib.do_debug := olddebug in                                         *)
(* 		let () = debug (fun () -> "CUTLINK3: result: " ^ (string_of_bool result)) in *)
(* 		if result then Some While_rules.symex_tagpairs else None in                *)
(*   mk_back_rule rl "Cut backlink"                                                 *)

(* let cut_backlink_tac =                                                           *)
(* 	Proof_tacs.seq [ abd_backlink_cut; abd_mid_backlink_cut; backlink_cut ]        *)

let unfold =
  Abdrule.mk_abdgenrule (fun seq defs ->
      Blist.map (fun app -> (app, defs)) (While_rules.luf_rl seq defs) )

let unfold_last =
  Abdrule.mk_abdgenrule (fun seq defs ->
      Blist.map
        (fun app -> (app, defs))
        (While_rules.luf_rl seq
           (Defs.of_list [Blist.hd (Defs.to_list defs)])) )

let deref_tac =
  Abdrule.first
    [wrap symex_load_rule; wrap symex_store_rule; wrap symex_free_rule]

let det_guard_tac =
  Abdrule.first
    [ wrap symex_det_if_rule
    ; wrap symex_det_ifelse_rule
    ; wrap symex_det_while_rule ]

let abd_symex abd symex =
  Abdrule.compose abd (Abdrule.compose unfold_last symex)

let ifwhile_tac =
  Abdrule.first [det_guard_tac; abd_symex abd_det_guard det_guard_tac]

let gen_ifwhile_tac = Abdrule.compose generalise_while_rule ifwhile_tac

let rec straightline =
  Abdrule.first
    [ symex_empty_axiom
    ; ex_falso_axiom
    ; symex_stop_axiom
    ; Abdrule.choice
        [ matches
        ; abd_symex abd_back_rule matches
        ; Abdrule.first
            [ symex_skip_rule
            ; symex_new_rule
            ; symex_nondet_if_rule
            ; symex_nondet_ifelse_rule
            ; symex_nondet_while_rule
            ; symex_assign_rule
            ; deref_tac
            ; abd_symex abd_deref deref_tac ] ] ]

(* these rules can rarely create a non-symex loop in termination checking *)
let rules =
  Abdrule.first
    [ symex_empty_axiom
    ; ex_falso_axiom
    ; symex_stop_axiom
    ; (* lhs_disj_to_symheaps ; *)
      (* simplify ; *)
      Abdrule.choice
        [ matches
        ; abd_symex abd_back_rule matches
        ; Abdrule.first
            [ symex_skip_rule
            ; symex_new_rule
            ; symex_nondet_if_rule
            ; symex_nondet_ifelse_rule
            ; symex_nondet_while_rule
            ; symex_assign_rule
            ; deref_tac
            ; abd_symex abd_deref deref_tac
            ; Abdrule.choice [ifwhile_tac; gen_ifwhile_tac; unfold] ]
        (* Proof_tacs.then_tac                                           *)
        (*   abd_segment                                                 *)
        (* 	(Proof_tacs.or_tac [ifwhile_tac; gen_ifwhile_tac]); *)
        
        (* cut_backlink_tac                                               *)
         ] ]
