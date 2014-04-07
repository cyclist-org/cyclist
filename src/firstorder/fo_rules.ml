open Lib
open Util
open Firstorder

module FOP = Prover.Make(Firstorder.Seq)
module Proof = Proof.Make(Firstorder.Seq)
module Rule = Proofrule.Make(Firstorder.Seq)
module Seqtactics = Seqtactics.Make(Firstorder.Seq)

let product_subsumed_modulo_tags p1 p2 =
  Prod.subsumed_wrt_tags Tags.empty p1 p2

(* axioms *)
let ex_falso_axiom =
  Rule.mk_axiom
    begin fun (l,_) ->
      Option.mk 
        (Form.for_all 
          begin fun p ->
            Blist.exists (Fun.uncurry Term.equal) (Prod.get_deqs p)  
            ||
            Blist.exists
              (Pair.perm
                (fun x y -> 
                  Term.is_zero x && (Term.is_succ y || Term.is_cons y)))
                (Prod.get_eqs p)
          end
          l
        )
        "Ex Falso"
    end

let id_axiom =
  Rule.mk_axiom
    begin fun (l,r) ->
      Option.mk
        (
          Form.mem Prod.empty r ||
          Form.for_all 
            (fun p -> Form.exists (product_subsumed_modulo_tags p) l) r
        )
        "Id"
    end

let axioms = ref (Rule.first [ ex_falso_axiom ; id_axiom ])

(* inference rules *)

(* simplification rules *)

(* substitute one equality between a var and a term from LHS into sequent *)
let eq_subst_rule seq =
  debug (fun () -> "Eq_subst") ;
  try
    let l = Form.dest (fst seq) in
    let eqs = Prod.get_eqs l in
    let (x,y) = Blist.find 
      (fun (x',y') -> 
        not (Term.equal x' y') && ((Term.is_var x') || (Term.is_var y'))) eqs in
    let (x,y) = if Term.is_var y then (x,y) else (y,x) in
    let theta = Term.singleton_subst y x in 
    [ [ ((Seq.subst theta seq), Seq.tag_pairs seq, TagPairs.empty) ], "" ]
  with Not_product | Not_found -> [] 

let simplify_eqs seq =
  debug (fun () -> "simpl_eqs") ;
  let progress = ref false in
  let non_triv_eq a = 
    let r = not (Atom.is_eq a) ||
      let (x,y) = Atom.dest_eq a in not (Term.equal x y) in
    (if not r then progress := true else ()) ; r in
  let seq' = Pair.map (Form.endomap (Prod.filter non_triv_eq)) seq in
  debug (fun () -> "simpl_eqs done") ;
  if not !progress then [] else
  [ [ (seq', Seq.tag_pairs seq, TagPairs.empty) ], "" ] 

let bijections = Strng.Set.of_list ["s"; "cons"]

(* simplify equalities on known one-to-one functions *)
let bij_eqs (l,r) = 
  debug (fun () -> "bij = simpl") ;
  let f p eq =
    if not (Atom.is_eq eq) then None else
    let (x,y) = Atom.dest_eq eq in
    if not (Term.is_fun x) || not (Term.is_fun y) then None else
    let ((f,args), (f',args')) = Pair.map Term.dest_fun (x,y) in
    if not (Strng.equal f f') || not (Strng.Set.mem f bijections) then None else
    let p' = Prod.remove eq p in
    let new_eqs = Blist.map2 (fun w z -> Atom.mk_eq w z) args args' in
    let p' = Prod.union p' (Prod.of_list new_eqs) in
    let l' = Form.add p' (Form.remove p l) in
    Some l' in
  let g p = Prod.find_map (f p) p in
  match Form.find_map g l with
    | None -> []
    | Some l' -> [ [ ((l', r), Form.tag_pairs l, TagPairs.empty) ], "" ] 

(* cut using x=y |- f(x)=f(y) on the rhs *)
let func_eqs (l,r) = 
  debug (fun () -> "fun = simpl") ;
  let f p eq =
    try 
      let (x,y) = Atom.dest_eq eq in
      let ((f,args), (f',args')) = Pair.map Term.dest_fun (x,y) in
      if not (Strng.equal f f') then None else
      let p' = Prod.remove eq p in
      let new_eqs = Blist.map2 (fun w z -> Atom.mk_eq w z) args args' in
      let p' = Prod.union p' (Prod.of_list new_eqs) in
      let r' = Form.add p' (Form.remove p r) in
      Some r' 
    with Invalid_argument _ -> None in
  let g p = Prod.find_map (f p) p in
  match Form.find_map g r with
    | None -> [] 
    | Some r' -> [ [ ((l, r'), Form.tag_pairs l, TagPairs.empty) ], "" ] 

(* substitute one equality between an exist var and a term in RHS *)
let eq_ex_subst_rule (l,r) =
  debug (fun () -> "Eq_ex_subst") ;
  let eq_ex_subst_product rp =
    let eqs = Prod.get_eqs rp in
    try 
      let (x,y) = 
        Blist.find (fun (x',y') -> 
          Term.is_exist_var x' || Term.is_exist_var y') 
          eqs in
      let (x,y) = if Term.is_exist_var y then (x,y) else (y,x) in
      let theta = Term.singleton_subst y x in 
        Some (Form.add (Prod.subst theta rp) (Form.remove rp r))
    with Not_found -> None in
  match Form.find_map eq_ex_subst_product r with
    | None -> []
    | Some r' -> [ [ ((l, r'), Form.tag_pairs l, TagPairs.empty) ], "" ] 
  
(* remove all RHS atoms that can be discharged *)
let simpl_rhs (l,r) =
  debug (fun () -> "Simpl rhs") ;
  try
    let lp = Form.dest l in
    let is_in_lhs a =
      if Atom.is_ipred a then 
        Prod.exists (Atom.ipred_eq_mod_tags a) lp else
        Prod.mem a lp in
    let prog = ref false in
    let r' = 
      Form.endomap 
        (Prod.filter 
          (fun a -> 
            let res = not (is_in_lhs a) in
            if not res then prog := true ; res  
          )) r in
    if not !prog then [] else 
    [ [ ((l, r'), Prod.tag_pairs lp, TagPairs.empty) ], "" ]
  with Not_product -> [] 

let simplify_rules = [ 
  simplify_eqs ;
  eq_subst_rule ;
  eq_ex_subst_rule ; 
  simpl_rhs ;
  bij_eqs ;
  func_eqs ;
  ]

let simplify_seq = 
  Seqtactics.repeat (Seqtactics.first simplify_rules)

let simplify = Rule.mk_infrule simplify_seq

let wrap r =
  Rule.compose 
    (Rule.mk_infrule (Seqtactics.compose r (Seqtactics.attempt simplify_seq)))
    (Rule.attempt !axioms)

(* break LHS disjunctions *)
let lhs_disj_to_products =
  wrap
    begin fun (l,r) ->  
      if Form.cardinal l < 2 then [] else 
        [ Form.map_to_list 
          (fun p -> 
            ( (Form.singleton p, r), TagPairs.mk (Prod.tags p), TagPairs.empty) ) 
          l,
          "L.Or" 
        ]
    end

(* break RHS conjunction *)
let rhs_conj_to_atoms =
  wrap
    begin fun (l,r) ->
      try
        let rp = Form.dest r in
        if Prod.cardinal rp < 2 then [] else
        let rp = Prod.elements rp in
        let ex_vars_ls = 
          Blist.map 
            (fun a -> Term.Set.filter Term.is_exist_var (Atom.vars a)) rp in
        let chs = Blist.cartesian_hemi_square ex_vars_ls in
        if Blist.exists 
          (fun (l1,l2) -> not (Term.Set.is_empty (Term.Set.inter l1 l2))) chs
        then [] else
        let t = Form.tag_pairs l in
        [ Blist.map 
          (fun at -> 
            ( (l, Form.singleton (Prod.singleton at)), t, TagPairs.empty ) ) rp,
          "R.And" 
        ] 
      with Not_product -> []
    end


(* remove existential vars by instantiating them *)
(* to some universal variable. Clearly, this could extend to arbitrary terms *)
(* but is not for performance reasons. *)
let instantiate_ex = 
  wrap
    begin fun seq ->
    let (uvars, exvars) = 
      Pair.map 
        Term.Set.elements 
          (Term.Set.partition Term.is_univ_var (Seq.vars seq)) in
    let cp = Blist.cartesian_product exvars uvars in
    let t = Seq.tag_pairs seq in
    Blist.map 
      (fun (exv,trm) -> 
        [ (Seq.subst (Term.singleton_subst exv trm) seq, t, TagPairs.empty) ], 
        "Inst. ex"
      ) cp
    end


let matches_ident ident a = 
  let (_,ident',_) = Atom.dest_pred a in ident=ident'

let args_of_ipred a = let (_,_,args) = Atom.dest_pred a in args

let gen_right_rules (ident,def) =
  (* no attempt at unification *)
  let std_ruf_pred_in_prod (f, vs) rp p =
    let vs' = args_of_ipred p in
    let eqs = Blist.combine vs vs' in
    let rp = Prod.remove p rp in
    let f' = Prod.union f 
      (Prod.of_list (Blist.map (fun (x,y) -> Atom.mk_eq x y) eqs)) in
    Some (Prod.union rp f') in
  (* with unification *)
  let uni_ruf_pred_in_prod (f, vs) rp p =
    let vs' = args_of_ipred p in
    let res = Term.multi_unify_args vs vs' in
    if Option.is_none res then None else
    let (theta,eqs) = Option.get res in
    let rp = Prod.remove p rp in
    let f' = Prod.union f 
      (Prod.of_list (Blist.map (fun (x,y) -> Atom.mk_eq x y) eqs)) in
    let f' = Prod.subst theta f' in
    Some (Prod.union rp f') in
  let ruf_pred_in_prod uni case rp p =
    (if uni then uni_ruf_pred_in_prod else std_ruf_pred_in_prod) case rp p in
  let ruf_product uni case seq rp = 
    let rinds = Prod.filter Atom.is_ipred rp in
    let preds = Prod.filter (matches_ident ident) rinds in
    let res = 
      Option.list_get 
        (Blist.map (ruf_pred_in_prod uni case rp) (Prod.elements preds)) in
    res in
  let ruf_prod_in_formula uni case ((l,r) as seq) rp = 
    let r' = Form.remove rp r in
    Blist.map (fun newp -> Form.add newp r') (ruf_product uni case seq rp) in
  let ruf_formula uni case ((l,r) as seq) = 
    Blist.bind (ruf_prod_in_formula uni case seq) (Form.elements r) in
  let right_rule case =
    begin fun ((l,r) as seq) ->
      let case = Case.freshen (Seq.vars seq) case in
      let tag_pairs = Seq.tag_pairs seq in
        Blist.map 
          (fun r' -> [ ((l,r'), tag_pairs, TagPairs.empty) ], (ident ^ " R.Unf.") ) 
          ( (ruf_formula true (Case.dest case) seq) (*@ (ruf_formula true case seq)*) )
      end in
  Blist.map wrap (Blist.map right_rule def)
        

 
let gen_left_rules (ident, def) =
  let left_rule ((l,r) as seq) =
    try
      let l' = Form.dest l in
      let linds = Prod.filter Atom.is_ipred l' in
      let preds = Prod.filter (matches_ident ident) linds in
      if Prod.is_empty preds then [] else
      let tags = Seq.tags seq in
      let tag_pairs = Seq.tag_pairs seq in
      let new_tag = 1 + (try Tags.max_elt tags with Not_found -> 0) in
      let left_unfold contr p =
        let id = Option.get (Atom.tag p) in
        let pvs = args_of_ipred p in
        let l'' = 
          Prod.union 
            (if contr then 
              (Prod.repl_tags new_tag (Prod.singleton p))
             else
              Prod.empty
            ) 
            (Prod.remove p l') in
        let do_case case =
          let (f', vs') = Case.dest (Case.freshen (Seq.vars seq) case) in
          let eqs = Prod.of_list (Blist.map2 (fun x y -> Atom.mk_eq x y) pvs vs') in
          let f' = Prod.union eqs f' in
          let f' = Prod.univ (Seq.vars seq) f' in
          let f' = Prod.repl_tags id f' in
          let l'' = Prod.union f' l'' in
          ((Form.singleton l'', r), 
            (if contr then (TagPairs.add (id,new_tag) tag_pairs) else tag_pairs), 
            TagPairs.singleton (id, id))
        in Blist.map do_case def, (ident ^ " L.Unf.") 
      in (*(Blist.map (left_unfold false) (Prod.elements preds)) 
          @ *)
          (Blist.map (left_unfold true) (Prod.elements preds))
    with Not_product -> [] in
  wrap left_rule 


let matches_fun s1 s2 =
  let tags = Tags.inter (Seq.tags s1) (Seq.tags s2) in
  if Tags.is_empty tags then [] else
  let res = Seq.uni_subsumption s1 s2 in
  if Option.is_none res then [] else
  let theta = Option.get res in
  let s2' = Seq.subst theta s2 in
  let tags' = Tags.fold
    (fun t acc ->
      let new_acc = Tags.add t acc in
      if Seq.subsumed_wrt_tags new_acc s1 s2' then new_acc else acc
    ) tags Tags.empty in
  let () = assert (not (Tags.is_empty tags')) in
  [ ((TagPairs.mk tags', "Backl"), theta) ]


(*    seq'     *)
(* ----------  *)
(* seq'[theta] *)
(* where seq'[theta] = seq *)
let subst_rule theta seq' seq = 
  if Seq.equal (Seq.subst theta seq') seq 
    then 
        [ [(seq', TagPairs.mk (Seq.tags seq'), TagPairs.empty)], "Subst" ]
    else 
        []

(*   F |- G * Pi'  *)
(* --------------- *)
(*   Pi * F |- G   *)
(* where seq' = F |- G * Pi' and seq = Pi * F |- G *)     
let weaken seq' seq = 
  if Seq.subsumed_wrt_tags Tags.empty seq seq' then
    [ [(seq', TagPairs.mk (Tags.inter (Seq.tags seq) (Seq.tags seq')), TagPairs.empty)], "Weaken" ]
  else
    []

(* if there is a backlink achievable through substitution and classical *)
(* weakening then make the proof steps that achieve it explicit so that *)
(* actual backlinking can be done on Seq.equal sequents *) 
let dobackl idx prf =
    let src_seq = Proof.get_seq idx prf in
    let targets = Rule.all_nodes idx prf in
    let apps = 
        Blist.map (fun idx' -> matches_fun src_seq (Proof.get_seq idx' prf)) targets in
    let f targ_idx (p, theta) =
        let targ_seq = Proof.get_seq targ_idx prf in
    let subst_seq = Seq.subst theta targ_seq in
    Rule.sequence [
      if Seq.equal src_seq subst_seq
        then Rule.identity
        else Rule.mk_infrule (weaken subst_seq);
        
      if Term.Map.for_all Term.equal theta
        then Rule.identity
        else Rule.mk_infrule (subst_rule theta targ_seq);
         
      Rule.mk_backrule 
        true 
        (fun _ _ -> [targ_idx]) 
        (fun s s' -> if Seq.equal s s' then [p] else [])
    ] in
    Rule.first 
      (Blist.map2 
          (fun idx' l -> Rule.first (Blist.map (f idx') l)) 
            targets 
            apps) idx prf

let fold (ident,defs) =
  let fold_rl ((l,r) as seq) = 
    try 
      let lp = Form.dest l in
      let tags = Seq.tags seq in
      let freshtag = 1 + (try Tags.max_elt tags with Not_found -> 0) in 
      let do_case case =
        let (f,vs) = Case.dest case in 
        (* skip empty base cases *)
        if Prod.is_empty f then [] else
        (* let (f, vs) = (Prod.univ f, Blist.map Term.univ vs) in  *)
        let results : Term.substitution list ref = ref [] in
        let hook sub = results := sub :: !results ; None in 
        let () = ignore (Prod.left_subsumption hook Term.empty_subst f lp) in
        let process_sub theta = 
          let (f, vs) = (Prod.subst theta f, Blist.map (Term.subst theta) vs) in
          let (fpreds, fnonpreds) = Prod.partition Atom.is_ipred f in
          let lp' = Prod.diff lp fnonpreds in
          let lp' = Prod.filter 
            (fun a -> Prod.for_all 
               (fun a' -> not (Atom.ipred_eq_mod_tags a a')) fpreds) lp' in
          let newpred = Atom.mk_ipred freshtag ident vs in
          let lp' = Prod.add newpred lp' in
          let l' = Form.singleton lp' in
          let seq' = (l',r) in
            [(
              seq', 
              TagPairs.mk (Tags.inter tags (Seq.tags seq')), 
              TagPairs.empty 
            )], (ident ^ " Fold")  in
        Blist.map process_sub !results in
      Blist.bind do_case defs
    with Not_product -> [] in
  Rule.mk_infrule fold_rl 


let rules = ref Rule.fail

let setup defs = 
  let ruf = Rule.choice (Blist.bind gen_right_rules (Defs.bindings defs)) in
  let luf = Rule.choice (Blist.map gen_left_rules (Defs.bindings defs)) in
  let folds = Rule.choice (
    Blist.map 
      (* (fun c -> FOP.Proof_tacs.then_tac (up_to_n 1 (fold c)) matches) *)
      (fun c -> Rule.compose (fold c) dobackl)
      (* fold *)
      (Defs.bindings defs)
    ) in
  let clever = Rule.compose luf ruf in
  rules := Rule.first [ 
    ex_falso_axiom ; id_axiom ; simplify ;
    
    Rule.choice [
      dobackl ; (*matches ;*)
      lhs_disj_to_products ;
      instantiate_ex ;
      rhs_conj_to_atoms;
      ruf; clever; luf; folds
    ] 
  ]
