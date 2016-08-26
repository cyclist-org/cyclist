open Lib
open Util
open Asl_while_program

module SH = Asl_heap
exception Not_symheap = Asl_form.Not_symheap

module Rule = Proofrule.Make(Asl_while_program.Seq)
module Seqtactics = Seqtactics.Make(Asl_while_program.Seq)
module Proof = Proof.Make(Asl_while_program.Seq)
module Node = Proofnode.Make(Asl_while_program.Seq)

let tagpairs s = Seq.tagpairs_one

(* following is for symex only *)

let dest_sh_seq (l,cmd) = (Asl_form.dest l, cmd)


(* axioms *)
let ex_falso_axiom = 
  Rule.mk_axiom (fun (f,_) -> Option.mk (Asl_form.inconsistent f) "Ex Falso")

let symex_stop_axiom =
  Rule.mk_axiom (fun (_,cmd) -> Option.mk (Cmd.is_stop cmd) "Stop")

let symex_empty_axiom =
  Rule.mk_axiom (fun (_,cmd) -> Option.mk (Cmd.is_empty cmd) "Empty")


(* simplification rules *)
let eq_subst_ex_f ((l,cmd) as s) =
  let l' = Asl_form.subst_existentials l in
  let l' = Asl_form.simplify_terms l' in
  if Asl_form.equal l l' then [] else
  [ [ ((l', cmd), tagpairs s, TagPairs.empty) ], "Eq. subst. ex" ]

let simplify_rules = [ eq_subst_ex_f ]

let simplify_seq_rl = 
  Seqtactics.relabel "Simplify" 
    (Seqtactics.repeat (Seqtactics.first  simplify_rules))
let simplify = Rule.mk_infrule simplify_seq_rl  

let wrap r =
  Rule.mk_infrule (Seqtactics.compose r (Seqtactics.attempt simplify_seq_rl))


(* break LHS disjunctions *)
let lhs_disj_to_symheaps =
  let rl (l,cmd) =
    if Blist.length l < 2 then [] else
    [ Blist.map 
        (fun sh -> let s' = ([sh],cmd) in (s', tagpairs s', TagPairs.empty ) ) 
        l,
      "L.Or"
    ] in
  Rule.mk_infrule rl
  
(* FOR SYMEX ONLY *)
let fix_tps l = 
  Blist.map 
    (fun (g,d) -> Blist.map (fun s -> (s, tagpairs s, Seq.tagpairs_one )) g, d) l 

let mk_symex f = 
  let rl ((_,cmd) as seq) =
    let cont = Cmd.get_cont cmd in
    fix_tps 
		  (Blist.map (fun (g,d) -> Blist.map (fun h' -> ([h'], cont)) g, d) (f seq)) in
  wrap rl
  
(* symbolic execution rules *)
let symex_assign_rule =
  let rl seq =
    try
      let (f,cmd) = dest_sh_seq seq in
      let (x,e) = Cmd.dest_assign cmd in
      let fv = fresh_evar (Asl_heap.vars f) in
      let theta = Asl_subst.singleton x fv in
      let f' = Asl_heap.subst theta f in
      let e' = Asl_term.subst theta e in
      [[ SH.add_eq f' (e',x) ], "Assign"]
    with WrongCmd | Not_symheap -> [] in
  mk_symex rl

(* Find array that starts at location v or equivalent according to ∏ *)
let find_arr_on f v = 
	Asl_arrays.find (fun (t1,t2) -> Asl_heap.equates f t1 v) f.SH.arrays

(* Join contiguous arrays: find possible array next to arr and join them. *)
let rec join_arrays f arr =
  let contiguous (f, arr) =
    try
      let next = Asl_term.mk_add((Pervasives.snd arr), Asl_term.mk_const(1)) in
      let nextarr = find_arr_on f next in
      let newheap = SH.del_arr (SH.del_arr f arr) nextarr in
      (* retain info about end of arr *)
      let newheap = SH.add_lt newheap (Pervasives.snd arr, Pervasives.snd nextarr) in
      let newarr = (Pervasives.fst arr, Pervasives.snd nextarr) in
      Some (SH.add_arr newheap newarr, newarr)
    with Not_found -> None
  in match contiguous (f, arr) with
  | Some (f, arr) -> join_arrays f arr
  | None -> f


(* When constructing the z3 fmla, all variables are forced to be natural (>=0) as according to ASL definition (this is added at
  the beginning of the gamma funciton).
  Implicitly, therefore, program variables are not allowed to be negative. If this was to be changed, the prover might
  not be able to execute Load and Store commands anymore as most have the form of

  i := 0;
  while i < n do
    Load/Store command involving a[i];
    i := i+1;
  od

  The Generalise While Rule is applied "forgetting" 'i=0' in the precondition in order to be able to form the backlink
  at the end of the loop ('i' cannot be substituted since it is a program variable: if 'i=0' was present it would be substituted
  with an existential variable during the assignment 'i := i+1' and the backlink would not be possible).
  However, without 'i=0' in the precondition and without assuming i>0, the prover would not be able to assess that i is indeed
  within array bounds: the prover cannot find an appropriate loop invariant. *)
let add_or_lt h (a, b) (c, d) = Some (Fopl.And (Asl_heap.pure_to_fopl h,
                                               Fopl.Or(Fopl.PF (Fopl.Lt(a, b)),
                                                       Fopl.PF (Fopl.Lt(c, d)))))

let add_or_le h (a, b) (c, d) = Some (Fopl.And (Asl_heap.pure_to_fopl h,
                                               Fopl.Or(Fopl.PF (Fopl.Le(a, b)),
                                                       Fopl.PF (Fopl.Lt(c, d)))))

(* free(a, n) frees n cells for the array starting at a.
 * Find array starting at a and possibly join arrays contiguous to it. Let b be the end of the joint array. Memory safe iff
 * ∏ |= a < a+n <= b+1 <=> ∏ |= not (a < a+n /\ a+t <= b+1) <=> ∏ /\ (a+n<=a \/ b+1<a+n) is unsat   *)
let symex_free_rule =
  let rl seq =
    try
      let (f,cmd) = dest_sh_seq seq in
      let (a, n) = Cmd.dest_free cmd in
      let f = join_arrays f (find_arr_on f a) in
      let arr = find_arr_on f a in
      let e = Pervasives.snd arr in
      let darr = SH.del_arr f arr in
      let sum = Asl_term.mk_add(a, n) in
      (* a+n<=a *)
      let leq = (sum, a) in
      (* b+1<a+n *)
      let lt = (Asl_term.mk_add(e, Asl_term.mk_const(1)), sum) in
      let fopl = add_or_le f leq lt in
      let darr = if (Asl_heap.equates f sum (Asl_term.mk_add(e, Asl_term.mk_const(1))))
        then darr else SH.add_arr darr (sum, e) in
      if (Asl_sat.is_unsat (Asl_heap.satisfiable_z3 ~pure:fopl f))
      then [[ darr ], "Free"]
      else []
    with Not_symheap | WrongCmd | Not_found -> [] in
  mk_symex rl

(* Similarly to free. y[e] := x. Find array starting at y and let w be the end of the array. Memory safe iff
 * ∏ |= y<=y+e<=w <=> ∏ /\ (y+e<y \/ w<y+e) is unsat  *)
let symex_store_rule =
  let rl seq =
    try
      let (f, cmd) = dest_sh_seq seq in
      let (y, e, x) =  Cmd.dest_store cmd in
      let arr =  find_arr_on f y in
      let sum = Asl_term.mk_add(y, e) in
      let lt = (sum, y) in
      let lt' = (Pervasives.snd arr, sum) in
      let fopl = add_or_lt f lt lt' in
      if (Asl_sat.is_unsat (Asl_heap.satisfiable_z3 ~pure:fopl f)) then
      [[ f ], ("Store: $\\Pi$ $\\models$ " ^ Asl_term.to_string y ^ " $\\leq$ " ^
        Asl_term.to_string sum ^ " $<$ " ^ Asl_term.to_string (Pervasives.snd arr))] else []
    with Not_symheap | WrongCmd | Not_found -> [] in
  mk_symex rl

(* x := y[e]. Same as store rule. *)
let symex_load_rule =
  let rl seq =
    try
      let (f, cmd) = dest_sh_seq seq in
      let (x, y, e) = Cmd.dest_load cmd in
      let fv = fresh_evar (Asl_heap.vars f) in
      let theta = Asl_subst.singleton x fv in
      let f' = Asl_heap.subst theta f in
      let arr = find_arr_on f y in
      let sum = Asl_term.mk_add(y, e) in
      let lt = (sum, y) in
      let lt' = (Pervasives.snd arr, sum) in
      let fopl = add_or_lt f lt lt' in
      if (Asl_sat.is_unsat (Asl_heap.satisfiable_z3 ~pure:fopl f)) then
      [[ f' ], ("Load: $\\Pi$ $\\models$ " ^ Asl_term.to_string y ^ " $\\leq$ " ^
        Asl_term.to_string sum ^ " $<$ " ^ Asl_term.to_string (Pervasives.snd arr))] else []
    with Not_symheap | WrongCmd | Not_found -> [] in
  mk_symex rl

(* {∏[x'/x] * (y'+1)=(x+E[x'/x]) : array(x, y') * S[x'/x]} |- C
 * ------------------------------------------------------------ 
 *                {∏:S} |- x := new array[E]; C
 * 
 * E determines the size of the new array. y'=x+E-1.             *)
let symex_new_rule =
  let rl seq =
    try
      let (f,cmd) = dest_sh_seq seq in
      let (x, i) = Cmd.dest_new cmd in
      let fv = fresh_evar (Asl_heap.vars f) in
      let theta = Asl_subst.singleton x fv in
      let f' = Asl_heap.subst theta f in
      let (x, i) = (x, Asl_term.subst theta i) in
      let x' = fresh_evar (Asl_heap.vars f') in
      let eq = (Asl_term.mk_add(x', Asl_term.mk_const(1)), Asl_term.mk_add (x, i)) in
			let f'' = Asl_heap.add_eq (Asl_heap.mk_arr (x, x')) eq  in
      [[ Asl_heap.star f' f'' ], "New"]
    with Not_symheap | WrongCmd-> [] in
  mk_symex rl

let symex_skip_rule =
  let rl seq =
    try
      let (f,cmd) = dest_sh_seq seq in 
      let () = Cmd.dest_skip cmd in [[f], "Skip"]
    with Not_symheap | WrongCmd -> [] in
  mk_symex rl

let symex_if_rule =
  let rl seq =
    try
      let (f,cmd) = dest_sh_seq seq in
      let (c,cmd') = Cmd.dest_if cmd in
      let cont = Cmd.get_cont cmd in
      let (f',f'') = Cond.fork f c in 
      fix_tps [[ ([f'], Cmd.mk_seq cmd' cont) ; ([f''], cont) ], "If"]
    with Not_symheap | WrongCmd -> [] in
  wrap rl

let symex_ifelse_rule =
  let rl seq =
    try
      let (f,cmd) = dest_sh_seq seq in
      let (c,cmd1,cmd2) = Cmd.dest_ifelse cmd in
      let cont = Cmd.get_cont cmd in
      let (f',f'') = Cond.fork f c in
      fix_tps 
        [[ ([f'], Cmd.mk_seq cmd1 cont) ; ([f''], Cmd.mk_seq cmd2 cont) ],
         "IfElse"
        ]
    with Not_symheap | WrongCmd -> [] in
  wrap rl

let symex_while_rule =
  let rl seq =
    try
      let (f,cmd) = dest_sh_seq seq in
      let (c,cmd') = Cmd.dest_while cmd in
      let cont = Cmd.get_cont cmd in
      let (f',f'') = Cond.fork f c in
      fix_tps [[ ([f'], Cmd.mk_seq cmd' cmd) ; ([f''], cont) ], "While"]
    with Not_symheap | WrongCmd -> [] in
  wrap rl

let matches ((l,cmd) as seq) ((l',cmd') as seq') =
  try
    if not (Cmd.equal cmd cmd') then [] else
    let (l,l') = Pair.map Asl_form.dest (l,l') in
    let sub_check = Asl_subst.combine_checks [
        Asl_subst.basic_lhs_down_check ;
        Asl_subst.avoids_replacing_check (!program_vars) ;
      ] in
    let cont =
      Asl_unifier.mk_verifier
        (fun (theta, tagpairs) -> 
          let subst_seq = Seq.subst theta seq' in
          (* let () = debug (fun _ -> "term substitution: " ^ ((Format.asprintf " %a" Asl_subst.pp theta))) in
          let () = debug (fun _ -> "source seq: " ^ (Seq.to_string seq)) in
          let () = debug (fun _ -> "target seq: " ^ (Seq.to_string seq')) in
          let () = debug (fun _ -> "substituted target seq: " ^ (Seq.to_string subst_seq)) in *)
          Seq.subsumed seq subst_seq) in
    Asl_unifier.backtrack 
      (Asl_heap.unify_partial ~tagpairs:true)
      ~sub_check
      ~cont
      l' l
  with Not_symheap -> []

(*    seq'     *)
(* ----------  *)
(* seq'[theta] *)
(* where seq'[theta] = seq *)
let subst_rule theta seq' seq = 
  if Seq.equal (Seq.subst theta seq') seq 
    then 
      [ [(seq', Seq.tag_pairs seq', TagPairs.empty)], "Subst" ]
    else 
      []

let frame seq' seq = 
  if Seq.subsumed seq seq' then
    [ [(seq', Seq.tag_pairs seq', TagPairs.empty)], "Frame" ]
  else
    []

(* if there is a backlink achievable through substitution and classical *)
(* weakening then make the proof steps that achieve it explicit so that *)
(* actual backlinking can be done on Seq.equal sequents *) 
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
    let subst_seq = Seq.subst theta targ_seq in
    Rule.sequence [
      if Seq.equal src_seq subst_seq
        then Rule.identity
        else Rule.mk_infrule (frame subst_seq);
        
      if Asl_term.Map.for_all Asl_term.equal theta
        then Rule.identity
        else Rule.mk_infrule (subst_rule theta targ_seq);
         
      Rule.mk_backrule 
        false 
        (fun _ _ -> [targ_idx]) 
        (fun s s' -> 
          [(Seq.tagpairs_one), "Backl"])
    ] in
    Rule.first (Blist.map f apps) idx prf

(* Needed in order to make the backlink possible in some while loops. E.g.:
 * i := 0;
 * while i < n do
 *   do something;
 *   i := i+1;
 * od
 * 
 * i=0 needs to be removed from the pure part before executing the symbolic execution, otherwise it will
 * never be able to perform the backlink as program variables cannot be substituted.
 * Remove parts involving program variables modified inside the loop.
 * In particular try all possible subsets of these variables. *)
let generalise_while_rule idx prf =
  let generalise m h =
    let avoid = ref (Asl_heap.vars h) in
    let gen_term t =
    if Asl_term.Set.mem t m then
      (let r = fresh_evar !avoid in avoid := Asl_term.Set.add r !avoid ; r)
    else t in
    let gen_arr (x,y) =
      (gen_term x, gen_term y) in
      SH.mk 
        (Asl_term.Set.fold Asl_uf.remove m h.SH.eqs)
        (Asl_neqs.filter
          (fun p -> Pair.conj (Pair.map (fun z -> not (Asl_term.Set.mem z m)) p))
          h.SH.neqs)
        (Asl_leqs.filter
          (fun p -> Pair.conj (Pair.map (fun z -> not (Asl_term.Set.mem z m)) p))
          h.SH.leqs)
        (Asl_lts.filter
          (fun p -> Pair.conj (Pair.map (fun z -> not (Asl_term.Set.mem z m)) p))
          h.SH.lts)
        (Asl_arrays.endomap gen_arr h.SH.arrays) in
    let rl seq =
      try
        let label = "Gen.While" in
        let (f,cmd) = dest_sh_seq seq in
        let (_,cmd') = Cmd.dest_while cmd in
        let m = Asl_term.Set.inter (Cmd.modifies cmd') (Asl_heap.vars f) in
        let () = debug (fun _ -> "m in generalise: " ^ Asl_term.Set.to_string m) in
        let subs = Blist.filter (fun x -> not (Asl_term.Set.is_empty x)) (Asl_term.Set.subsets m) in
        (* If previous node is a Gen.While then do not apply again *)
        if label == (Pervasives.snd (Node.dest (Proof.find (idx-1) prf))) then [] else
        Option.list_get (Blist.map
          begin fun m' ->
            let f' = generalise m' f in
            if Asl_heap.equal f f' then None else
            let s' = ([f'], cmd) in
            Some ([ (s', tagpairs s', TagPairs.empty) ], label)
          end
          subs)
    with Not_symheap | WrongCmd -> [] in
  Rule.mk_infrule rl idx prf


let axioms = 
  ref (Rule.first [ex_falso_axiom; symex_stop_axiom; symex_empty_axiom])

let rules = ref Rule.fail

let symex =       
  Rule.first [
    symex_skip_rule ;
    symex_assign_rule ;
    symex_store_rule ;
    symex_load_rule ;
    symex_free_rule ;
    symex_new_rule ;
    symex_if_rule ;
    symex_ifelse_rule ;
    symex_while_rule ;
  ] 

let setup () =
  rules := Rule.first [ 
    lhs_disj_to_symheaps ;
    simplify ;

    Rule.choice [
      dobackl ;
      symex ;
      generalise_while_rule ;
    ]
  ]
