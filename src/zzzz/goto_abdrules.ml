open Lib
open Util
open Symheap
open Goto_program

module Rule = Proofrule.Make(Goto_program.Seq)
module Seqtactics = Seqtactics.Make(Goto_program.Seq)
module Abdrule = Abdrule.Make(Goto_program.Seq)(Goto_program.Defs)

(* let latex_defs d =                 *)
(*   let t = !split_heaps in          *)
(*   let () = split_heaps := false in *)
(*   let res = Defs.to_latex d in     *)
(*   split_heaps := t ; res           *)

let dest_sh_seq = Goto_rules.dest_sh_seq

let last_pred = ref 0
let get_fresh_ident () = Printf.sprintf "I%.3d" (incr last_pred ; !last_pred)

let pick_ind c prod = 
  Inds.min_elt (Inds.filter (has_ident c) prod.inds)

let ex_subst_defs defs =
  let ex_subst_heap h = 
    let (ex_eqs, non_ex_eqs) = 
      Blist.partition (fun (x,_) -> Term.is_exist_var x) (UF.bindings h.eqs) in
      if ex_eqs=[] then h else
      (* NB order of subst is reversed so that *)
      (* the greater variable replaces the lesser *)
      (* this maintains universal vars *)
      Sl_heap.subst (Term.Map.of_list ex_eqs) { h with eqs=UF.of_list non_ex_eqs } in
  let ex_subst_case c = 
    let (p,h) = Case.dest c in
    let p' = Sl_heap.fixpoint ex_subst_heap p in
    Case.mk p' h in
  Blist.map (fun (l,i) -> (Blist.map ex_subst_case l, i)) defs
  

let empify defs = 
  let empify_ c = 
    let (p,h) = Case.dest c in 
    let inds = Inds.filter (fun pred -> Defs.is_defined pred defs) p.inds in
    Case.mk {p with inds=inds} h in
  Blist.map (fun (l,i) -> (Blist.map empify_ l, i)) defs

let inline defs =
  try 
    let (p,h) as q = 
      Blist.find 
        begin fun (p,h) -> 
          (Blist.length p)=1 && 
          let (f,_) = Case.dest (Blist.hd p) in
          let idents = 
            Inds.map_to Strng.Set.add Strng.Set.empty (fun (_,(id,_)) -> id) f.inds in
          not (Strng.Set.mem h idents)
        end (Blist.but_last defs) in
    let defs = Blist.filter ((!=)q) defs in
    let unf = Goto_rules.gen_left_rules_f (p,h) in
    let f orig =
      let (p',h') = Case.dest orig in 
      let first_unfold f =
        let apps = unf ([f],0) in
        if apps=[] then f else 
          let ((f',_), _, _) = Blist.hd (fst (Blist.hd apps)) in 
          Blist.hd f' in
          (* print_endline (Sl_heap.to_string f'') ; f'' in             *)
      let p'' = Sl_heap.fixpoint first_unfold p' in
      Case.mk p'' h' in
    Blist.map (fun (l,i) -> (Blist.map f l, i)) defs
  with Not_found -> defs


let used_only_recursively (heap, (ident, params)) pos = 
  let var = Blist.nth params pos in
  let heap' = { heap with inds=Inds.empty } in
  if Term.Set.mem var (Sl_heap.vars heap') then false else
  Inds.for_all  
    begin fun (_,(ident', params')) ->
      if ident<>ident' then 
        Blist.for_all (fun var' -> not (Term.equal var var')) params'
      else
        Blist.for_all2 
          (fun var' pos' -> pos=pos' || not (Term.equal var var')) 
          params' 
          (Blist.indexes params')
    end 
    heap.inds  

(* for all predicates *)
(* for all integers up to the arity of the predicate *)
(* if the position is unused in all clauses of the definition*)
(* then stop and report predicate and position *)
(* when all are exhausted return None *)
let find_unused_arg defs =
  let check_def (clauses, ident) =
    if clauses = [] then None else
    let check_pos pos = 
      if Blist.for_all 
        begin fun case ->
          let ((heap, (_, params)) as p) = Case.dest case in
          not (Term.Set.mem (Blist.nth params pos) (Sl_heap.vars heap)) ||
          used_only_recursively p pos
        end
        clauses 
      then 
        Some (ident, pos) 
      else 
        None in
    Blist.find_some check_pos (Blist.range 0 (snd (snd (Case.dest (Blist.hd clauses))))) in
  if Blist.length defs=1 then 
    None 
  else 
    Blist.find_some check_def (Blist.but_last defs)

let eliminate (ident, pos) defs =
  let elim_clause case =
    let (heap, (ident', params)) = Case.dest case in
    let elim_pred heap =
      { heap with 
          inds = Inds.endomap 
            begin fun (t, (ident'', params')) ->
              (t, (ident'', if ident=ident'' then Blist.remove_nth pos params' else params')) 
            end 
            heap.inds
      } in
    if ident<>ident' then 
      Case.mk (elim_pred heap) (ident', params)
    else
      Case.mk (elim_pred heap) (ident', Blist.remove_nth pos params) in 
  Blist.map (fun (cl, ident') -> (Blist.map elim_clause cl, ident')) defs      

let elim_dead_vars defs = 
  match find_unused_arg defs with
    | None -> defs
    | Some (ident, pos) -> eliminate (ident, pos) defs

let self_recursive_case case = 
  let (heap, (ident, _)) = Case.dest case in
  Inds.exists (has_ident ident) heap.inds

let elim_inconsistency defs =
  let is_inconsistent_case c =
    let (heap, (ident, _)) = Case.dest c in
    Sl_heap.inconsistent heap ||
    Blist.for_all self_recursive_case (fst (Defs.get_def ident defs)) ||
    Inds.exists
      begin fun (_,(ident', _)) ->
        Defs.mem ident' defs &&
        [] = fst (Defs.get_def ident' defs) 
      end
      heap.inds in
  let elim_inconsistent_cases (clauses, ident) =
    (Blist.filter (fun d -> not (is_inconsistent_case d)) clauses, ident) in
  Blist.map elim_inconsistent_cases defs

let simplify_defs defs =
  Defs.fixpoint  
    begin fun d -> 
      let d = elim_inconsistency d in
      let d = ex_subst_defs d in
      let d = inline d in
      elim_dead_vars d
    end
    (empify defs)

let is_base_case c = let (p,_) = Case.dest c in Inds.is_empty p.inds

let is_possibly_consistent defs = Defs.consistent (empify defs)

let ex_falso_axiom = Abdrule.lift Goto_rules.ex_falso_axiom 
let lhs_disj_to_symheaps = Abdrule.lift Goto_rules.lhs_disj_to_symheaps
let eq_subst_ex = Abdrule.lift (Rule.mk_infrule Goto_rules.eq_subst_ex_f)

(* symbolic execution rules *)
let symex_stop_axiom = Abdrule.lift Goto_rules.symex_stop_axiom
let symex_load_rule = Abdrule.lift Goto_rules.symex_load_rule
let symex_store_rule = Abdrule.lift Goto_rules.symex_store_rule
let symex_free_rule = Abdrule.lift Goto_rules.symex_free_rule
let symex_new_rule = Abdrule.lift Goto_rules.symex_new_rule
let symex_goto_rule = Abdrule.lift Goto_rules.symex_goto_rule
let symex_skip_rule = Abdrule.lift Goto_rules.symex_skip_rule
let symex_non_det_if_rule = Abdrule.lift Goto_rules.symex_non_det_if_rule



let symex_assign_rule_f seq =
  try
    let (f,i) = dest_sh_seq seq in
    let cmd = get_cmd i in
    let (x,e) = Cmd.dest_assign cmd in
    (* if is_fresh_in x f then [] else *)
    let fv = fresh_evar (Sl_heap.vars f) in
    let theta = Term.singleton_subst x fv in
    let f' = Sl_heap.subst theta f in
    let e' = Term.subst theta e in
    let f' = { f' with eqs=UF.add (e',x) f'.eqs } in
    [ [ (([f'], i+1), Sl_heap.tag_pairs f, TagPairs.empty) ], "Assign" ]
  with WrongCmd | Not_symheap -> []
  
let symex_assign_rule = Abdrule.lift (Rule.mk_infrule symex_assign_rule_f)

let post_abd_assign_rule_f seq =
  try
    let (f,i) = dest_sh_seq seq in
    let cmd = get_cmd i in
    let (x,e) = Cmd.dest_assign cmd in
    if not (Sl_heap.is_fresh_in x f) then [] else
     (* exist var is on the left, right is x *)
    let feqs = UF.bindings f.eqs in
    let ((y,_) as eq) = Blist.find (fun (y,z) -> 
      Term.is_exist_var y && Sl_heap.equates f e z 
      ) feqs in
    let neweqs = Blist.filter ((!=)eq) feqs in
    let f' = { f with eqs=UF.add (e,x) f.eqs } in
    let f'' = Sl_heap.subst (Term.singleton_subst y x) 
      { f with eqs=UF.of_list neweqs } in
    [ 
      [ (([f'], i+1), Sl_heap.tag_pairs f, TagPairs.empty) ], "Post Abd Assign";
      [ (([f''], i+1), Sl_heap.tag_pairs f, TagPairs.empty) ], "Post Abd Assign"
    ]
  with WrongCmd | Not_symheap | Not_found -> []
 
let post_abd_assign_rule = Abdrule.lift (Rule.mk_infrule post_abd_assign_rule_f) 

(*let generalisation_f, generalisation =                                           *)
(*  let rl (seq:Seq.t) =                                                           *)
(*    try                                                                          *)
(*      let (l,i) = dest_sh_seq seq in                                             *)
(*      let (eqs, rest) =                                                          *)
(*        Blist.partition                                                           *)
(*          (fun p -> Pair.conj (Pair.map (fun z -> not (Term.is_exist_var z)) p)) *)
(*          (UF.bindings l.eqs) in                                                 *)
(*      if eqs=[] then [] else                                                     *)
(*      let f ((x,y) as eq) =                                                      *)
(*        let new_eqs = (removeq eq eqs) @ rest in                                 *)
(*        let l' = Sl_heap.norm { l with eqs=UF.of_list new_eqs } in                  *)
(*        [ (([l'], i), Sl_heap.tag_pairs l, TagPairs.empty) ] in                     *)
(*      Blist.map f eqs                                                             *)
(*    with Not_symheap -> [] in                                                    *)
(*  rl, Apr.mk_inf_rule rl "Gen"                                                   *)
  
let simplify =
  Abdrule.lift (Rule.mk_infrule (Seqtactics.first [ Goto_rules.eq_subst_ex_f ]))
let wrap r = Abdrule.compose r (Abdrule.attempt simplify)


(*let symex_assign_rule_f, symex_assign_rule =                  *)
(*  let rl seq =                                                *)
(*    try                                                       *)
(*      let (f,i) = dest_sh_seq seq in                          *)
(*      let cmd = get_cmd i in                                  *)
(*      let (x,e) = Cmd.dest_assign cmd in                      *)
(*      let (x,e) = Pair.map (fun z -> UF.find z f.eqs) (x,e) in*)
(*      if Term.equal x e then                                  *)
(*        [ [ (([f], i+1), Sl_heap.tag_pairs f, TagPairs.empty) ] ]*)
(*      else []                                                 *)
(*    with Not_symheap | WrongCmd -> [] in                      *)
(*  rl, Apr.mk_inf_rule rl "Assign"                             *)
  

let symex_det_if_rule =
  let rl seq =
    try
      let (f,i) = dest_sh_seq seq in
      let (c,j) = Cmd.dest_if (get_cmd i) in
      if Cmd.is_non_det c then [] else
      let (x,y) = Cmd.dest_cond c in
      let (x,y) = Pair.map (fun z -> UF.find z f.eqs) (x,y) in
      let t = Sl_heap.tag_pairs f in
      match (Cmd.is_deq c, Sl_heap.equates f x y) with
        | (false , true) ->
          (* cmd wants equality and formula provides it so take the branch *) 
          [ [ (([f], j), t, TagPairs.empty) ], "If(det)" ]
        | (false, false) -> 
          (* cmd wants equality *)
          if Sl_heap.disequates f x y then
            (* and formula forbids it so take other branch *) 
            [ [ (([f], i+1), t, TagPairs.empty) ], "If(det)" ] 
          else 
            (* formula allows either fact so fail *) 
            []
        | (true, true) ->
          (* cmd wants disequality and formula forbids it so take other branch *) 
          [ [ (([f], i+1), t, TagPairs.empty) ], "If(det)" ]
        | (true, false) -> 
          (* cmd wants disequality *)
          if Sl_heap.disequates f x y then
            (* formula provides it so take branch *) 
            [ [ (([f], j), t, TagPairs.empty) ], "If(det)" ] 
          else 
            (* otherwise don't know so fail *)
            []
    with Not_symheap | WrongCmd -> [] in
  Abdrule.lift (Rule.mk_infrule rl) 

let standard_rules = 
  [  
    lhs_disj_to_symheaps ;
(*    matches ;*)
    simplify ;
    (* wrap symex_assign_rule; *)
    wrap symex_load_rule ;
    wrap symex_store_rule ;
    wrap symex_free_rule ;
    wrap symex_new_rule ;
    wrap symex_goto_rule ;
    wrap symex_skip_rule ;
    wrap symex_det_if_rule ;
    wrap symex_non_det_if_rule
  ]

let std_funcs = 
  [
    Goto_rules.lhs_disj_to_symheaps_f; 
    Goto_rules.eq_subst_ex_f ;
    Goto_rules.symex_load_rule_f; 
    Goto_rules.symex_store_rule_f;
    Goto_rules.symex_free_rule_f;
    Goto_rules.symex_new_rule_f;
    Goto_rules.symex_goto_rule_f ;
    Goto_rules.symex_skip_rule_f ;
    Goto_rules.symex_non_det_if_rule_f; 
(*    Goto_rules.symex_assign_rule_f;*)
    symex_assign_rule_f
  ] 
  
let can_fire_std_rules seq = Blist.exists (fun rl -> (rl seq)<>[]) std_funcs



(* abduction rules*)

let abd_assign =
  let rl seq defs =
    debug (fun () -> "Abd assign starts.") ;
    try
      let (f,i) = dest_sh_seq seq in
      let cmd = get_cmd i in
      let (x,e) = Cmd.dest_assign cmd in
      if not (Sl_heap.is_fresh_in x f)(* || not (Program.is_local_var x)*) then [] else
      let inds = Inds.elements f.inds in
      (* filter formula predicates by undefinedness *)
      let inds = Blist.filter (fun pred -> not (Defs.is_defined pred defs)) inds in
      (* further filter by having e in parameter list, *)
      (* unless either is a const(nil) *)
      let inds = if Term.equal e Term.nil then inds else
        Blist.filter (fun (_,(_,params)) -> 
          Blist.exists (fun z -> Sl_heap.equates f e z) params) inds in 
      let fresh_ident = get_fresh_ident () in
      let f (_,(ident, params)) =
        let outerparams = fresh_uvars (Term.Set.empty) (Blist.length params) in 
        (* allocate one more parameter for x *)
        let x' = fresh_evar (Term.Set.of_list outerparams) in
        let newparams = outerparams @ [ x' ] in
        let head = (ident, outerparams) in
        let e' = if Term.equal e Term.nil then e else 
          Blist.nth newparams (Blist.find_index (fun t -> Sl_heap.equates f e t) params) in
        (* let x' = Blist.nth newparams ((Blist.length newparams)-1) in *)
        let clause =
          { Sl_heap.empty with
            eqs=UF.of_list [(x', e')];
            inds=Inds.singleton (0, (fresh_ident, newparams))
          } in
        ( [Case.mk clause head], ident )::defs in
      let res = Blist.map f inds in
      debug (fun () -> "Abd assign " ^ (if res=[] then "fails." else "succeeds.")) ; res
    with Not_symheap | WrongCmd -> [] in
  Abdrule.mk_abdinfrule rl
 
let abd_deref =
  let rl seq defs =
    debug (fun () -> "Abd deref") ; 
(*    debug (fun () -> string_of_defs defs) ;*)
    try
      if can_fire_std_rules seq then raise Not_symheap else
      let (f,i) = dest_sh_seq seq in
      let x = Cmd.dest_deref (get_cmd i) in
      let y = UF.find x f.eqs in
      if Option.is_some (Sl_heap.find_lval y f) then [] else
      let inds = Inds.elements f.inds in
      (* filter formula predicates by undefinedness *)
      let inds = Blist.filter (fun pred -> not (Defs.is_defined pred defs)) inds in
      (* further filter by having y in parameter list *)
      let inds = Blist.filter 
        (fun (_,(_,params)) -> 
          Blist.exists (fun y' -> Sl_heap.equates f y' y) params) inds in
      let fresh_ident = get_fresh_ident () in 
      let f (_,(ident, params)) =
        let newparams = fresh_uvars (Term.Set.empty) (Blist.length params) in
        let head = (ident, newparams) in
        let pto_params = 
          fresh_evars 
            (Term.Set.of_list newparams) 
            (Blist.length (fst !Goto_program.program)) in
        let newy = 
          Blist.nth newparams (Blist.find_index (fun t -> Sl_heap.equates f y t) params) in
        let clause =  
          { 
            eqs=UF.empty; 
            deqs=Deqs.empty; 
            ptos=Ptos.singleton (newy, pto_params); 
            inds=Inds.singleton (0, (fresh_ident, newparams @ pto_params))
          } in
        ( [Case.mk clause head], ident )::defs in 
      Blist.map f inds
    with Not_symheap | WrongCmd -> [] in
  Abdrule.mk_abdinfrule rl

      
let abd_det_if =
  let rl seq defs =
    debug (fun () -> "Abd if starts.") ; 
(*    debug (fun () -> string_of_defs defs) ;*)
    try
      if can_fire_std_rules seq then raise Not_symheap else
      let (f,i) = dest_sh_seq seq in
      let (c,j) = Cmd.dest_if (get_cmd i) in
      if Cmd.is_non_det c then [] else
      let (x,y) = Cmd.dest_cond c in
(*      let () = print_endline ((Term.to_string x) ^ "," ^ (Term.to_string y)) in*)
      let (x',y') = Pair.map (fun z -> UF.find z f.eqs) (x,y) in
(*      let () = print_endline ((Term.to_string x') ^ "," ^ (Term.to_string y')) in*)
      (* refuse to do anything if normal sym ex can fire *)
      if Cmd.is_deq c && Sl_heap.disequates f x' y' then [] else
      if not (Cmd.is_deq c) && Term.equal x' y' then [] else
      let inds = Inds.elements f.inds in
      (* filter formula predicates by undefinedness *)
      let inds = Blist.filter (fun pred -> not (Defs.is_defined pred defs)) inds in
      (* further filter by having x *and* y in parameter list, unless either is a const(nil) *)
      let inds_filter z inds_ = 
        if Term.is_var z then
          Blist.filter (fun (_,(_,params)) -> Blist.exists (fun t -> Sl_heap.equates f z t) params) inds_ 
        else
          inds_ in
      let inds = inds_filter x inds in
      let inds = inds_filter y inds in
      let (fresh_ident, fresh_ident') = (get_fresh_ident (), get_fresh_ident ()) in
      let f (_,(ident, params)) =
        let newparams = fresh_uvars (Term.Set.empty) (Blist.length params) in
        let head = (ident, newparams) in
        let (newx, newy) = Pair.map  
          (fun z -> 
            if Term.is_var z then 
              Blist.nth newparams (Blist.find_index (fun t -> Sl_heap.equates f z t) params) 
            else z) 
          (x,y) in
        let clause_eq = 
          Sl_heap.norm 
            { Sl_heap.empty with
              eqs=UF.of_list [(newx, newy)] ;
              inds=Inds.singleton (0, (fresh_ident, newparams))
            } in
        let clause_deq =  
          { Sl_heap.empty with
            deqs=Deqs.singleton (newx, newy) ;
            inds=Inds.singleton (0, (fresh_ident', newparams))
          } in
        ( [Case.mk clause_eq head; Case.mk clause_deq head], ident )::defs in 
      Blist.map f inds
    with Not_symheap | WrongCmd -> [] in
  Abdrule.mk_abdinfrule rl

let abd_back_rule = 
  let rl s1 s2 defs = 
    debug (fun () -> "Abd back") ; 
(*    debug (fun () -> string_of_defs defs) ;*)
    try 
      if can_fire_std_rules s1 then raise Not_symheap else
      let ((l1,i1),(l2,i2)) = Pair.map dest_sh_seq (s1,s2) in
      if 
        i1<>i2 ||
        Deqs.cardinal l1.deqs < Deqs.cardinal l2.deqs ||
        Ptos.cardinal l1.ptos < Ptos.cardinal l2.ptos ||
        (* refuse if backlink applies already *)
        Goto_rules.is_subsumed s1 s2
      then 
        (debug (fun () -> "Abd back early exit.");
        []) 
      else
      (* find set of identifiers of ind preds in s1/s2 *)
      let (inds1,inds2) = Pair.map Sl_heap.get_idents (l1,l2) in
      let (inds1,inds2) = Pair.map (Strng.MSet.map_to Strng.Set.add Strng.Set.empty Fun.id) (inds1,inds2) in
      (* find fresh ones in s1 *)
      let candidates = Strng.Set.filter (fun ident -> not (Defs.mem ident defs)) inds1 in
      (* discard those that already exist in s2 *)
      let candidates = Strng.Set.filter (fun i -> not (Strng.Set.mem i inds2)) candidates in
      (* for each candidate there must exists one in s2 which *)
      (* if it replaces the candidate in s1, makes inds2 a subset of inds1 *)
      (* this is to overapproximate subsumption *)
      let cp = Blist.cartesian_product (Strng.Set.elements candidates) (Strng.Set.elements inds2) in
      let cp = Blist.filter 
        (fun (c,c') -> Strng.Set.subset inds2 (Strng.Set.add c' (Strng.Set.remove c inds1))) 
        cp in
      let fresh_ident = get_fresh_ident () in
      let base_clause =  
        { 
          eqs=UF.empty; 
          deqs=Deqs.empty; 
          ptos=Ptos.empty; 
          inds=Inds.empty
        } in
      let res = Blist.bind
        (fun (c,c') -> 
          let (_,(_,params)) = pick_ind c l1 in
          let (_,(_,params')) = pick_ind c' l2 in
          let newparams = fresh_uvars (Term.Set.empty) (Blist.length params) in
          let base_clause =  
            { base_clause with inds=Inds.singleton (0, (fresh_ident, newparams)) } in
            (* FIXME why choose and not unify *)
          let combinations = 
            Blist.choose (Blist.repeat newparams (Blist.length params')) in
          let head = (c, newparams) in
          Blist.map 
            (fun comb ->
              let cl = 
                { base_clause with inds=Inds.add (0,(c',comb)) base_clause.inds } in
              ( [Case.mk cl head], c )::defs
            ) 
            combinations
         )
         cp in
      debug (fun () -> "Abd back ends, results=" ^ (string_of_bool (res<>[]))) ; 
      res 
    with Not_symheap -> [] in
  Abdrule.mk_abdbackrule Rule.all_nodes rl


let matches = Abdrule.lift Goto_rules.matches

let deref_tac = 
  Abdrule.first [symex_load_rule; symex_store_rule; symex_free_rule]

let gen_left_rule_fun seq defs =
  let rls = Blist.map Goto_rules.gen_left_rules_f defs in
  let rl = Seqtactics.choice rls in
  let apps = rl seq in
  Blist.map (fun app -> (app,defs)) apps

let unfold = Abdrule.mk_abdgenrule gen_left_rule_fun 
  
let ruleset =
  let axioms = [ ex_falso_axiom ; symex_stop_axiom ] in 
  let abdtacs = [ 
    Abdrule.compose abd_back_rule (Abdrule.compose unfold matches);
    Abdrule.compose abd_deref (Abdrule.compose unfold deref_tac);
    Abdrule.compose abd_det_if (Abdrule.compose unfold symex_det_if_rule);
    Abdrule.compose 
        abd_assign (Abdrule.compose unfold post_abd_assign_rule);
    wrap symex_assign_rule
  ] in
  Abdrule.choice (axioms @ standard_rules @ [ unfold ] @ abdtacs) 

