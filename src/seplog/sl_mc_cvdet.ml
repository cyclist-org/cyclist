open Lib
open Util
open Symbols
open MParser

module Make (Sig : Sl_mc_core.ValueSig) = 
  struct
    module GenModelChecker = Sl_mc_core.Make(Sig)
    module Value = GenModelChecker.Value
    module Location = GenModelChecker.Location
    module Var = Sl_mc_core.Var
    module Stack = GenModelChecker.Stack
    module Heap = GenModelChecker.Heap
        
    module Reduction =
      struct
        type t = 
          { 
            stack : Stack.t;
            heap : Heap.t option ref; 
            symheap : Sl_heap.t;
            remainder : Heap.t option ref
          }
          
        let dest r = (r.stack, Option.get !(r.heap), r.symheap, r.remainder)
        let mk (s,h,sh,rem) = 
          { 
            stack = s;
            heap = ref (Some h); 
            symheap = sh;
            remainder = ref rem
          }


        let equal r r' =
          Stack.equal r.stack r'.stack &&
          Sl_heap.equal r.symheap r'.symheap && 
          match (!(r.heap), !(r'.heap)) with
          | (None, None) -> true
          | (Some h, Some h') -> Heap.equal h h'
          | _ -> false
          && 
          match (!(r.remainder), !(r'.remainder)) with
          | (None, None) -> true
          | (Some h, Some h') -> Heap.equal h h'
          | _ -> false 
        
        let equal_upto_tags = equal
        let tags _ = Tags.empty
        let pp fmt r =
          Format.fprintf fmt "@[s, h %s %a -> h':%a@]"
            (* Stack.pp r.stack  *)
            (* (Option.pp Heap.pp) !(r.heap)  *)
            symb_turnstile.str Sl_heap.pp r.symheap  
            (Option.pp Heap.pp) !(r.remainder) 
        let to_string r = mk_to_string pp r 
        let to_melt _ = failwith "N/A" 
      end

    module Rule = Proofrule.Make(Reduction)
    
    let set_metavar rem h = assert (Option.is_none !rem) ; rem := Some h  

    let emp_axiom =
      let f r = 
        let (_, h, symheap, remainder) = Reduction.dest r in
        Sl_heap.is_empty symheap &&
        ( set_metavar remainder h ; true ) in
      Rule.mk_axiom (fun r -> Option.mk (f r) "emp")

    let interpret stack trm =
      if Sl_term.is_nil trm then Value.nil else
      let var = Var.of_term trm in
      Var.Map.find var stack
    
    let is_location = function
      | Value.Location _ -> true
      | _ -> false
    
    let get_location = function
      | Value.Location l -> l
      | _ -> assert false
    
    let all_vars_free h = 
      not (Sl_term.Set.exists Sl_term.is_exist_var (Sl_heap.vars h))
    
    let points_to_axiom =
      let f r =
        let (s, h, symheap, remainder) = Reduction.dest r in
        all_vars_free symheap &&
        Sl_uf.is_empty symheap.Sl_heap.eqs &&
        Sl_deqs.is_empty symheap.Sl_heap.deqs &&
        Sl_tpreds.is_empty symheap.Sl_heap.inds &&
        Sl_ptos.cardinal symheap.Sl_heap.ptos = 1 &&
        let (l,rs) = Sl_ptos.choose symheap.Sl_heap.ptos in
        let (l,rs) = (interpret s l, Blist.map (interpret s) rs) in
        is_location l &&
        let lloc = get_location l in
        Location.Map.mem lloc h &&
        let heap_rs = Location.Map.find lloc h in
        Value.FList.equal rs heap_rs &&
        ( set_metavar remainder (Location.Map.remove lloc h) ; true ) in
      Rule.mk_axiom (fun r -> Option.mk (f r) "|->")
    
    let mk_infrule r = 
      let r' red = 
        Blist.map 
          (fun (subgoals, descr) ->
            let subgoals' = 
              Blist.map 
                (fun g -> (g, TagPairs.empty, TagPairs.empty)) 
                subgoals in 
            (subgoals', descr)) 
            (r red) in
      Rule.mk_infrule r'
    
    let discharge_eq =
      let rl red =
        let (s, h, symheap, remainder) = Reduction.dest red in
        if not (all_vars_free symheap) then [] else
        if Sl_uf.is_empty symheap.Sl_heap.eqs then [] else
        let eqs = Sl_uf.bindings symheap.Sl_heap.eqs in
        let (eq, tl) = Blist.decons eqs in
        let (x,y) = Pair.map (interpret s) eq in
        if not (Value.equal x y) then [] else       
        let symheap' = Sl_heap.with_eqs symheap (Sl_uf.of_list tl) in
        [ ([ { red with Reduction.symheap = symheap' } ], "=") ] in
      mk_infrule rl
    
    let discharge_deq =
      let rl red =
        let (s, h, symheap, remainder) = Reduction.dest red in
        if not (all_vars_free symheap) then [] else
        if not (Sl_uf.is_empty symheap.Sl_heap.eqs) then [] else
        if Sl_deqs.is_empty symheap.Sl_heap.deqs then [] else
        let deq = Sl_deqs.choose symheap.Sl_heap.deqs in
        let (x,y) = Pair.map (interpret s) deq in
        if Value.equal x y then [] else
        let symheap' = Sl_heap.del_deq symheap deq in
        [ ([ { red with Reduction.symheap = symheap' } ], "!=") ] in
      mk_infrule rl
    
    let discharge_pto =
      let rl red =
        let (s, h, symheap, remainder) = Reduction.dest red in
        if not (all_vars_free symheap) then [] else
        if not (Sl_uf.is_empty symheap.Sl_heap.eqs) then [] else
        if not (Sl_deqs.is_empty symheap.Sl_heap.deqs) then [] else
        if Sl_ptos.is_empty symheap.Sl_heap.ptos then [] else
        if 
          Sl_ptos.cardinal symheap.Sl_heap.ptos < 2 && 
          Sl_tpreds.is_empty symheap.Sl_heap.inds 
        then [] else
        let pto = Sl_ptos.choose symheap.Sl_heap.ptos in
        let symheap' = Sl_heap.mk_pto pto in
        let symheap'' = Sl_heap.del_pto symheap pto in
        let cut_heap = ref None in
        [
          ([ 
            { red with Reduction.symheap = symheap' ; remainder = cut_heap }; 
            { red with Reduction.symheap = symheap'' ; heap = cut_heap } 
           ], "*->")
        ] in
      mk_infrule rl
    
    let discharge_ind =
      let rl red =
        let (s, h, symheap, remainder) = Reduction.dest red in
        if not (all_vars_free symheap) then [] else
        if not (Sl_uf.is_empty symheap.Sl_heap.eqs) then [] else
        if not (Sl_deqs.is_empty symheap.Sl_heap.deqs) then [] else
        if not (Sl_ptos.is_empty symheap.Sl_heap.ptos) then [] else
        if Sl_tpreds.cardinal symheap.Sl_heap.inds < 2 then [] else
        let p = Sl_tpreds.choose symheap.Sl_heap.inds in
        let symheap' = Sl_heap.mk_ind p in
        let symheap'' = Sl_heap.del_ind symheap p in
        let cut_heap = ref None in
        [
          ([ 
            { red with Reduction.symheap = symheap' ; remainder = cut_heap }; 
            { red with Reduction.symheap = symheap'' ; heap = cut_heap } 
           ], "*P")
        ] in
      mk_infrule rl

    let eliminate_eq =
      let rl red = 
        let (s, h, symheap, remainder) = Reduction.dest red in
        if all_vars_free symheap then [] else
        let allvars = Sl_heap.vars symheap in
        let existvars = Sl_term.Set.filter Sl_term.is_exist_var allvars in
        if Sl_uf.is_empty symheap.Sl_heap.eqs then [] else
        let eqs = Sl_uf.bindings symheap.Sl_heap.eqs in
        let cvalued (x,y) = 
          Sl_term.Set.mem x existvars && not (Sl_term.Set.mem y existvars) in
        let cvalued (x,y) = cvalued (x,y) || cvalued (y,x) in
        let eq = Blist.find_first cvalued eqs in
        if Option.is_none eq then [] else
        let (x,y) = Option.get eq in
        let (x,y) = if Sl_term.is_exist_var x then (x,y) else (y,x) in 
        let z = Sl_term.fresh_fvar allvars in
        let theta = Sl_subst.singleton x z in
        (* NB we skip removing the equality over x because the UF structure *)
        (* will simply ignore it. *)
        let symheap' = Sl_heap.subst theta symheap in
        let yvalue = interpret s y in
        let s' = Var.Map.add (Var.of_term z) yvalue s in
        [
          ([ 
            { red with Reduction.symheap = symheap' ; stack = s' }; 
           ], "exists=")
        ] in
      mk_infrule rl
        
    let eliminate_pto =
      let rl red = 
        let (s, h, symheap, remainder) = Reduction.dest red in
        if all_vars_free symheap then [] else
        let allvars = Sl_heap.vars symheap in
        let existvars = Sl_term.Set.filter Sl_term.is_exist_var allvars in
        if Sl_ptos.is_empty symheap.Sl_heap.ptos then [] else
        let in_existvars x = Sl_term.Set.mem x existvars in 
        let cvalued (y,xs) = 
          not (Sl_term.Set.mem y existvars) && 
          Blist.exists in_existvars xs in
        let pto = Sl_ptos.find_opt cvalued symheap.Sl_heap.ptos in
        if Option.is_none pto then [] else
        let (y,xs) as pto = Option.get pto in
        let yvalue = interpret s y in
        if not (is_location yvalue) then [] else
        let yloc = get_location yvalue in
        if not (Location.Map.mem yloc h) then [] else
        let cell = Location.Map.find yloc h in
        let x = Option.get (Blist.find_first in_existvars xs) in
        let xindex = Blist.find_index (Sl_term.equal x) xs in
        let z = Sl_term.fresh_fvar allvars in
        let theta = Sl_subst.singleton x z in
        let symheap' = Sl_heap.subst theta symheap in
        let xvalue = Blist.nth cell xindex in
        let s' = Var.Map.add (Var.of_term z) xvalue s in
        [
          ([ 
            { red with Reduction.symheap = symheap' ; stack = s' }; 
           ], "exists->")
        ] in
      mk_infrule rl

    let defs = ref Sl_defs.empty  
    
    let select_rule values rules =
      let arity = Sl_indrule.arity (Blist.hd rules) in
      let allvars = 
        Sl_term.Set.union_of_list (Blist.map Sl_indrule.vars rules) in
      let newformals = Sl_term.fresh_fvars allvars arity in
      let s = 
        Var.Map.of_list (Blist.combine (Blist.map Var.of_term newformals) values) in
      let freshen_rule r = 
        let formals = Sl_indrule.formals r in
        let theta = Sl_term.Map.of_list (Blist.combine formals newformals) in
        Sl_indrule.subst theta r in 
      let freshrules = Blist.map freshen_rule rules in
      let bodies = Blist.map Sl_indrule.body freshrules in
      let projbodies = Blist.map (fun b -> Sl_heap.project b newformals) bodies in
      let satisfied = 
        Blist.find_first 
          (fun sh -> Stack.satisfies (sh.Sl_heap.eqs, sh.Sl_heap.deqs) s) 
          projbodies in
      if Option.is_none satisfied then None else
      let satisfied = Option.get satisfied in
      let satisfied_index = Blist.find_index (Sl_heap.equal satisfied) projbodies in
      Some (Blist.nth rules satisfied_index)
       
    
    let unfold =
      let rl red = 
        let (s, h, symheap, remainder) = Reduction.dest red in
        if not (all_vars_free symheap) then [] else
        if not (Sl_uf.is_empty symheap.Sl_heap.eqs) then [] else
        if not (Sl_deqs.is_empty symheap.Sl_heap.deqs) then [] else
        if not (Sl_ptos.is_empty symheap.Sl_heap.ptos) then [] else
        if not (Sl_tpreds.cardinal symheap.Sl_heap.inds = 1) then [] else
        let p = Sl_tpreds.choose symheap.Sl_heap.inds in
        let rules = Sl_defs.get_def (Sl_tpred.predsym p) !defs in
        let args = Sl_tpred.args p in
        let values = Blist.map (interpret s) args in
        let rule = select_rule values rules in
        if Option.is_none rule then [] else
        let rule = Option.get rule in
        let formals = Sl_indrule.formals rule in
        let s' = 
          Var.Map.of_list (Blist.combine (Blist.map Var.of_term formals) values) in
        [
          ([ 
            { red with Reduction.symheap = Sl_indrule.body rule ; stack = s' }; 
           ], "unfold " ^ (Sl_predsym.to_string (Sl_indrule.predsym rule)))
        ] in
      mk_infrule rl
  
    let axioms = Rule.first [ emp_axiom ; points_to_axiom ]
    
    let rules = 
      Rule.first [
        discharge_eq ;
        discharge_deq ;
        discharge_pto ;
        discharge_ind ;
        eliminate_eq ;
        eliminate_pto ;
        unfold
      ] ;
  
  end
  
module IntSigModelChecker = Make(Sl_mc_core.IntSig)
module Prover = Prover.Make(IntSigModelChecker.Reduction)

let check_model intuitionistic defs (sh, (stk, h)) =
  let defs = Sl_defs.relevant_defs defs [sh] in
  assert (Sl_defs.deterministic defs) ;
  assert (Sl_defs.constructively_valued defs) ;
  IntSigModelChecker.defs := defs ;
  let () = debug (fun _ -> Sl_defs.to_string defs) in
  let () = debug (fun _ -> IntSigModelChecker.Stack.to_string stk) in
  let () = debug (fun _ -> IntSigModelChecker.Heap.to_string h) in
  let red = IntSigModelChecker.Reduction.mk (stk,h,sh,None) in
  let res = 
    Prover.idfs 
      max_int 
      max_int 
      IntSigModelChecker.axioms 
      IntSigModelChecker.rules 
      red in
  Option.is_some res &&
  (intuitionistic || 
  IntSigModelChecker.Location.Map.is_empty 
    (Option.get !(red.IntSigModelChecker.Reduction.remainder))
  )
    
  


