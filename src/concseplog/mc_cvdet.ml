open Lib
open   Symbols

open Generic

open MParser

module Make (Sig : Mc_core.ValueSig) = struct
  module GenModelChecker = Mc_core.Make (Sig)
  module Value = GenModelChecker.Value
  module Location = GenModelChecker.Location
  module Var = Mc_core.Var
  module Stack = GenModelChecker.Stack
  module ConcreteHeap = GenModelChecker.ConcreteHeap

  module Reduction = struct
    type t =
      { stack: Stack.t
      ; heap: ConcreteHeap.t option ref
      ; symheap: Heap.t
      ; remainder: ConcreteHeap.t option ref }

    let dest r = (r.stack, Option.get !(r.heap), r.symheap, r.remainder)

    let mk (s, h, sh, rem) =
      {stack= s; heap= ref (Some h); symheap= sh; remainder= ref rem}

    let equal r r' =
      Stack.equal r.stack r'.stack
      && Heap.equal r.symheap r'.symheap
      &&
      match (!(r.heap), !(r'.heap)) with
      | None, None -> true
      | Some h, Some h' -> ConcreteHeap.equal h h'
      | _ -> (
          false
          &&
          match (!(r.remainder), !(r'.remainder)) with
          | None, None -> true
          | Some h, Some h' -> ConcreteHeap.equal h h'
          | _ -> false )

    let equal_upto_tags = equal

    let tags _ = Tags.empty

    let pp fmt r =
      Format.fprintf fmt "@[s, h %s %a -> h':%a@]"
        (* Stack.pp r.stack  *)
        (* (Option.pp ConcreteHeap.pp) !(r.heap)  *)
        symb_turnstile.str Heap.pp r.symheap (Option.pp ConcreteHeap.pp)
        !(r.remainder)

    let to_string r = mk_to_string pp r
  end

  module Rule = Proofrule.Make (Reduction)

  let set_metavar rem h =
    assert (Option.is_none !rem) ;
    rem := Some h

  let emp_axiom =
    let f r =
      let _, h, symheap, remainder = Reduction.dest r in
      Heap.is_empty symheap && (set_metavar remainder h ; true)
    in
    Rule.mk_axiom (fun r -> Option.mk (f r) "emp")

  let interpret stack trm =
    if Term.is_nil trm then Value.nil
    else
      let var = Var.of_term trm in
      Var.Map.find var stack

  let is_location = function Value.Location _ -> true | _ -> false

  let get_location = function Value.Location l -> l | _ -> assert false

  let all_vars_free h =
    not (Term.Set.exists Term.is_exist_var (Heap.vars h))

  let points_to_axiom =
    let f r =
      let s, h, symheap, remainder = Reduction.dest r in
      all_vars_free symheap
      && Uf.is_empty symheap.Heap.eqs
      && Deqs.is_empty symheap.Heap.deqs
      && Tpreds.is_empty symheap.Heap.inds
      && Int.( = ) (Ptos.cardinal symheap.Heap.ptos) 1
      &&
      let l, rs = Ptos.choose symheap.Heap.ptos in
      let l, rs = (interpret s l, Blist.map (interpret s) rs) in
      is_location l
      &&
      let lloc = get_location l in
      Location.Map.mem lloc h
      &&
      let heap_rs = Location.Map.find lloc h in
      Value.FList.equal rs heap_rs
      &&
      ( set_metavar remainder (Location.Map.remove lloc h) ;
        true )
    in
    Rule.mk_axiom (fun r -> Option.mk (f r) "|->")

  let mk_infrule r =
    let r' red =
      Blist.map
        (fun (subgoals, descr) ->
          let subgoals' =
            Blist.map (fun g -> (g, Tagpairs.empty, Tagpairs.empty)) subgoals
          in
          (subgoals', descr) )
        (r red)
    in
    Rule.mk_infrule r'

  let discharge_eq =
    let rl red =
      let s, h, symheap, remainder = Reduction.dest red in
      if not (all_vars_free symheap) then []
      else if Uf.is_empty symheap.Heap.eqs then []
      else
        let eqs = Uf.bindings symheap.Heap.eqs in
        let eq, tl = Blist.decons eqs in
        let x, y = Pair.map (interpret s) eq in
        if not (Value.equal x y) then []
        else
          let symheap' = Heap.with_eqs symheap (Uf.of_list tl) in
          [([{red with Reduction.symheap= symheap'}], "=")]
    in
    mk_infrule rl

  let discharge_deq =
    let rl red =
      let s, h, symheap, remainder = Reduction.dest red in
      if not (all_vars_free symheap) then []
      else if not (Uf.is_empty symheap.Heap.eqs) then []
      else if Deqs.is_empty symheap.Heap.deqs then []
      else
        let deq = Deqs.choose symheap.Heap.deqs in
        let x, y = Pair.map (interpret s) deq in
        if Value.equal x y then []
        else
          let symheap' = Heap.del_deq symheap deq in
          [([{red with Reduction.symheap= symheap'}], "!=")]
    in
    mk_infrule rl

  let discharge_pto =
    let rl red =
      let s, h, symheap, remainder = Reduction.dest red in
      if not (all_vars_free symheap) then []
      else if not (Uf.is_empty symheap.Heap.eqs) then []
      else if not (Deqs.is_empty symheap.Heap.deqs) then []
      else if Ptos.is_empty symheap.Heap.ptos then []
      else if
        Int.( < ) (Ptos.cardinal symheap.Heap.ptos) 2
        && Tpreds.is_empty symheap.Heap.inds
      then []
      else
        let pto = Ptos.choose symheap.Heap.ptos in
        let symheap' = Heap.mk_pto pto in
        let symheap'' = Heap.del_pto symheap pto in
        let cut_heap = ref None in
        [ ( [ {red with Reduction.symheap= symheap'; remainder= cut_heap}
            ; {red with Reduction.symheap= symheap''; heap= cut_heap} ]
          , "*->" ) ]
    in
    mk_infrule rl

  let discharge_ind =
    let rl red =
      let s, h, symheap, remainder = Reduction.dest red in
      if not (all_vars_free symheap) then []
      else if not (Uf.is_empty symheap.Heap.eqs) then []
      else if not (Deqs.is_empty symheap.Heap.deqs) then []
      else if not (Ptos.is_empty symheap.Heap.ptos) then []
      else if Int.( < ) (Tpreds.cardinal symheap.Heap.inds) 2 then []
      else
        let p = Tpreds.choose symheap.Heap.inds in
        let symheap' = Heap.mk_ind p in
        let symheap'' = Heap.del_ind symheap p in
        let cut_heap = ref None in
        [ ( [ {red with Reduction.symheap= symheap'; remainder= cut_heap}
            ; {red with Reduction.symheap= symheap''; heap= cut_heap} ]
          , "*P" ) ]
    in
    mk_infrule rl

  let eliminate_eq =
    let rl red =
      let s, h, symheap, remainder = Reduction.dest red in
      if all_vars_free symheap then []
      else
        let allvars = Heap.vars symheap in
        let existvars = Term.Set.filter Term.is_exist_var allvars in
        if Uf.is_empty symheap.Heap.eqs then []
        else
          let eqs = Uf.bindings symheap.Heap.eqs in
          let cvalued (x, y) =
            Term.Set.mem x existvars && not (Term.Set.mem y existvars)
          in
          let cvalued (x, y) = cvalued (x, y) || cvalued (y, x) in
          let eq = Blist.find_opt cvalued eqs in
          if Option.is_none eq then []
          else
            let x, y = Option.get eq in
            let x, y = if Term.is_exist_var x then (x, y) else (y, x) in
            let z = Term.fresh_fvar allvars in
            let theta = Subst.singleton x z in
            (* NB we skip removing the equality over x because the UF structure *)
            (* will simply ignore it. *)
            let symheap' = Heap.subst theta symheap in
            let yvalue = interpret s y in
            let s' = Var.Map.add (Var.of_term z) yvalue s in
            [([{red with Reduction.symheap= symheap'; stack= s'}], "exists=")]
    in
    mk_infrule rl

  let eliminate_pto =
    let rl red =
      let s, h, symheap, remainder = Reduction.dest red in
      if all_vars_free symheap then []
      else
        let allvars = Heap.vars symheap in
        let existvars = Term.Set.filter Term.is_exist_var allvars in
        if Ptos.is_empty symheap.Heap.ptos then []
        else
          let in_existvars x = Term.Set.mem x existvars in
          let cvalued (y, xs) =
            (not (Term.Set.mem y existvars)) && Blist.exists in_existvars xs
          in
          let pto = Ptos.find_opt cvalued symheap.Heap.ptos in
          if Option.is_none pto then []
          else
            let ((y, xs) as pto) = Option.get pto in
            let yvalue = interpret s y in
            if not (is_location yvalue) then []
            else
              let yloc = get_location yvalue in
              if not (Location.Map.mem yloc h) then []
              else
                let cell = Location.Map.find yloc h in
                let x = Option.get (Blist.find_opt in_existvars xs) in
                let xindex = Blist.find_index (Term.equal x) xs in
                let z = Term.fresh_fvar allvars in
                let theta = Subst.singleton x z in
                let symheap' = Heap.subst theta symheap in
                let xvalue = Blist.nth cell xindex in
                let s' = Var.Map.add (Var.of_term z) xvalue s in
                [ ( [{red with Reduction.symheap= symheap'; stack= s'}]
                  , "exists->" ) ]
    in
    mk_infrule rl

  let defs = ref Defs.empty

  let select_rule values rules =
    let arity = Indrule.arity (Blist.hd rules) in
    let allvars =
      Term.Set.union_of_list (Blist.map Indrule.vars rules)
    in
    let newformals = Term.fresh_fvars allvars arity in
    let s =
      Var.Map.of_list (Blist.combine (Blist.map Var.of_term newformals) values)
    in
    let freshen_rule r =
      let formals = Indrule.formals r in
      let theta = Term.Map.of_list (Blist.combine formals newformals) in
      Indrule.subst theta r
    in
    let freshrules = Blist.map freshen_rule rules in
    let bodies = Blist.map Indrule.body freshrules in
    let projbodies =
      Blist.map (fun b -> Heap.project b newformals) bodies
    in
    let satisfied =
      Blist.find_opt
        (fun sh -> Stack.satisfies (sh.Heap.eqs, sh.Heap.deqs) s)
        projbodies
    in
    if Option.is_none satisfied then None
    else
      let satisfied = Option.get satisfied in
      let satisfied_index =
        Blist.find_index (Heap.equal satisfied) projbodies
      in
      Some (Blist.nth rules satisfied_index)

  let unfold =
    let rl red =
      let s, h, symheap, remainder = Reduction.dest red in
      if not (all_vars_free symheap) then []
      else if not (Uf.is_empty symheap.Heap.eqs) then []
      else if not (Deqs.is_empty symheap.Heap.deqs) then []
      else if not (Ptos.is_empty symheap.Heap.ptos) then []
      else if not (Int.( = ) (Tpreds.cardinal symheap.Heap.inds) 1) then
        []
      else
        let p = Tpreds.choose symheap.Heap.inds in
        let rules = Defs.get_def (Tpred.predsym p) !defs in
        let args = Tpred.args p in
        let values = Blist.map (interpret s) args in
        let rule = select_rule values rules in
        if Option.is_none rule then []
        else
          let rule = Option.get rule in
          let formals = Indrule.formals rule in
          let s' =
            Var.Map.of_list
              (Blist.combine (Blist.map Var.of_term formals) values)
          in
          [ ( [{red with Reduction.symheap= Indrule.body rule; stack= s'}]
            , "unfold " ^ Predsym.to_string (Indrule.predsym rule) ) ]
    in
    mk_infrule rl

  let axioms = Rule.first [emp_axiom; points_to_axiom]

  let rules =
    Rule.first
      [ discharge_eq
      ; discharge_deq
      ; discharge_pto
      ; discharge_ind
      ; eliminate_eq
      ; eliminate_pto
      ; unfold ]
end

module IntSigModelChecker = Make (Mc_core.IntSig)
module Prover = Prover.Make (IntSigModelChecker.Reduction)

let check_model intuitionistic defs (sh, (stk, h)) =
  let defs = Defs.relevant_defs defs (Ord_constraints.empty, [sh]) in
  assert (Defs.deterministic defs) ;
  assert (Defs.constructively_valued defs) ;
  IntSigModelChecker.defs := defs ;
  let () = debug (fun _ -> Defs.to_string defs) in
  let () = debug (fun _ -> IntSigModelChecker.Stack.to_string stk) in
  let () = debug (fun _ -> IntSigModelChecker.ConcreteHeap.to_string h) in
  let red = IntSigModelChecker.Reduction.mk (stk, h, sh, None) in
  let res =
    Prover.idfs max_int max_int IntSigModelChecker.axioms
      IntSigModelChecker.rules red
  in
  Option.is_some res
  && ( intuitionistic
     || IntSigModelChecker.Location.Map.is_empty
          (Option.get !(red.IntSigModelChecker.Reduction.remainder)) )
