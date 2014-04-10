open Lib
open Util
open Symbols

type term =
  | Const of int
  | Var of int
  | Fun of string * term list

module rec TermT : BasicType with type t = term =
  struct
    type t = term

    module TermList = MakeFList(TermT)

    let compare t t' = match (t,t') with
      | (Var(n), Var(n')) | (Const(n), Const(n')) -> Int.compare n n'
      | (Fun(f, l), Fun(f', l')) ->
        (match Strng.compare f f' with
          | 0 -> TermList.compare l l'
          | n -> n)
      | (Fun _, _) | (_, Const _) -> 1
      | (Const _, _) | (_, Fun _) -> -1

    let equal t t' = (compare t t')=0
    let hash = Hashtbl.hash

    let to_string = function
      | Const(i) -> string_of_int i
      | Var(v) -> Var.to_string v
      | Fun(f, tl) -> f ^ "(" ^ (TermList.to_string tl) ^ ")"

    let rec pp fmt = function
      | Const(i) -> Format.fprintf fmt "@[%i@]" i
      | Var(v) -> Format.fprintf fmt "@[%s@]" (Var.to_string v)
      | Fun(f, args) ->
        Format.fprintf fmt "@[%s(%a)@]" f (Blist.pp pp_comma pp) args
  end

module Term =
  struct
    include MakeComplexType(TermT)

    let mk_const i = Const(i)
    let zero = mk_const 0
    let is_zero x = equal zero x
    let mk_var i = require (fun () -> i<>0) ; Var(i)
    let mk_univ_var name = mk_var (Var.mk_univ_var name)
    let mk_exist_var name = mk_var (Var.mk_exist_var name)
    let mk_fun ident args = Fun(ident, args)

    let dest_var = function
      | Var(v) -> v
      | _ -> invalid_arg "Term.dest_var"
    let dest_const = function
      | Const(c) -> c
      | _ -> invalid_arg "Term.dest_const"
    let dest_fun = function
      | Fun(f,args) -> (f,args)
      | _ -> invalid_arg "Term.dest_fun"

    let is_var = function
      | Var _ -> true
      | _ -> false
    let is_exist_var = function
      | Var(v) when Var.is_exist_var v -> true
      | _ -> false
    let is_univ_var = function
      | Var(v) when Var.is_univ_var v -> true
      | _ -> false
    let is_fun = function
      | Fun _ -> true
      | _ -> false
    let is_succ = function
      | Fun("s", _) -> true
      | _ -> false
    let is_cons = function
      | Fun("cons", _) -> true
      | _ -> false

    let filter_vars s = Set.filter is_var s

    let rec terms = function
      | Fun(_, tl) as t -> Set.add t (terms_of_list tl)
      | any -> Set.singleton any
    and terms_of_list tl = Set.union_of_list (Blist.map terms tl)

    let vars trm = filter_vars (terms trm)

    let vars_of_list tl = Set.union_of_list (Blist.map vars tl)

    let latex_of_var v =
      (if Var.is_exist_var v then "e" else "a") ^
      "_{" ^ (string_of_int (abs v)) ^ "}"

    let rec to_melt = function
      | Const(i) -> ltx_math (string_of_int i)
      | Var(v) -> ltx_math (Var.to_string v)
      | Fun(f, tl) ->
        ltx_mk_math
          (Latex.concat
            ([ Latex.mathit (Latex.text f); symb_lp.melt;
            ltx_comma (Blist.map to_melt tl); symb_rp.melt ]))

    type substitution = t Map.t
    let empty_subst = Map.empty

    let singleton_subst x y = Map.add x y empty_subst

    let rec subst theta = function
      | Var _ as v when Map.mem v theta -> Map.find v theta
      | Var _ as v -> v
      | Fun(s, tl) -> Fun(s, subst_list theta tl)
      | c -> c
    and subst_list theta l = Blist.map (subst theta) l

    (* find extension theta' of theta such that *)
    (* t[theta'] = t' *)
    let rec unify theta t t' =
      if Map.mem t theta then
        if equal (Map.find t theta) t' then Some theta else None
      else
      match t with
      | Const _ when equal t t' -> Some theta
      | Const _ -> None
      | Var _ when is_exist_var t ->
        if
          not (is_exist_var t') ||
          (* avoid capture *)
          Map.exists (fun _ t'' -> equal t' t'') theta
        then
          None
        else
          Some (Map.add t t' theta)
      | Var _ -> Some (Map.add t t' theta)
      | Fun(f, args) when is_fun t' ->
        let (f', args') = dest_fun t' in
        if
          not (Strng.equal f f') ||
          Blist.length args <> Blist.length args'
        then
          None
        else
          unify_list theta args args'
      | Fun _ -> None
    and unify_list theta args args' = match (args,args') with
      | ([], []) -> Some theta
      | (_,  []) | ([], _) -> None
      | (hd::tl, hd'::tl') ->
        match unify theta hd hd' with
          | None -> None
          | Some theta' -> unify_list theta' tl tl'

    let unify_ordered_pairs theta (x,y) (x', y') =
      match unify theta x x' with
        | None -> None
        | Some theta' -> unify theta' y y'

    let unify_pairs theta p p' =
      Option.list_get [
        unify_ordered_pairs theta p p';
        unify_ordered_pairs theta p (Pair.swap p')
      ]

    (* unifies two terms, computing a substitution s  such that s(t1) = t2 and *)
    (* only substitutes variables in t1 with terms of t2 *)
    (* thus t1 is in the formula to be unfolded and t2 is in the ind. def. *)
    (* the result is a list of pairs of terms of t2 to substitute over the variables of t1*)
    (* find extension theta' of theta such that *)
    (* t[theta'] = t' *)
    let def_add theta x y =
      let to_add =
        if Map.mem x theta then
          Set.add y (Map.find x theta)
        else
          Set.singleton y in
      Map.add x to_add theta

    let rec multi_unify theta t t' = match t with
      | Const _ when equal t t' -> Some theta
      | Const _ -> None
      | Var _ -> Some (def_add theta t t')
      | Fun(f, args) when is_fun t' ->
        let (f', args') = dest_fun t' in
        if
          not (Strng.equal f f') ||
          Blist.length args <> Blist.length args'
        then
          None
        else
          multi_unify_list theta args args'
      | Fun _ -> None
    and multi_unify_list theta args args' = match (args,args') with
      | ([], []) -> Some theta
      | (_,  []) | ([], _) -> None
      | (hd::tl, hd'::tl') ->
        match multi_unify theta hd hd' with
          | None -> None
          | Some theta' -> multi_unify_list theta' tl tl'

    let multi_unify_args ts ts' =
      let res = multi_unify_list Map.empty ts ts' in
      if Option.is_none res then None else
      let m = Option.get res in
      let theta = Map.map (fun v -> Set.choose v) m in
      let set_to_eqs s =
        if Set.cardinal s < 2 then [] else
        let first = Set.choose s in
        let s = Set.remove first s in
        Set.map_to_list (fun el -> (first,el)) s in
      let eqs = Blist.bind (fun (_,v) -> set_to_eqs v) (Map.bindings m) in
      Some (theta, eqs)

    let intset_of_varset s = Set.map_to Int.Set.add Int.Set.empty dest_var s

    let fresh_uvar s = mk_var (Var.fresh_uvar (intset_of_varset s))
    let fresh_evar s = mk_var (Var.fresh_evar (intset_of_varset s))

    let fresh_uvars s i =
      Blist.map mk_var (Var.fresh_uvars (intset_of_varset s) i)
    let fresh_evars s i =
      Blist.map mk_var (Var.fresh_evars (intset_of_varset s) i)
  end

type eq_atom = term * term
type pred_atom = string * term list
type ind_pred_atom = int * pred_atom

type atom =
  | Eq of eq_atom
  | Deq of eq_atom
  | IndPred of ind_pred_atom

module AtomT =
  struct
    type t = atom

    let compare a a' = match (a,a') with
      | (Eq(x,y), Eq(x',y')) | (Deq(x,y), Deq(x',y')) ->
        (match Term.compare x x' with
          | 0 -> Term.compare y y'
          | n -> n)
      | (IndPred(tag,(id,terms)), IndPred(tag',(id',terms'))) ->
        (match Int.compare tag tag' with
          | 0 -> (match Strng.compare id id' with
            | 0 -> Term.FList.compare terms terms'
            | n -> n)
          | m -> m)
      | (Eq _ ,_) | (_, IndPred _) -> -1
      | (_, Eq _) | (IndPred _, _) -> 1

    let equal a a' = (compare a a')=0
    let hash = Hashtbl.hash

    let to_string = function
      | Eq eq -> let (sx,sy) = Pair.map Term.to_string eq in sx ^ "=" ^ sy
      | Deq eq -> let (sx,sy) = Pair.map Term.to_string eq in sx ^ "!=" ^ sy
      | IndPred(t,(ident,tl)) ->
        ident ^ "_" ^ (string_of_int t) ^ "(" ^ (Term.FList.to_string tl) ^ ")"

    let pp fmt = function
      | Eq(x,y) -> Format.fprintf fmt "@[%a=%a@]" Term.pp x Term.pp y
      | Deq(x,y) -> Format.fprintf fmt "@[%a!=%a@]" Term.pp x Term.pp y
      | IndPred(t,(ident,args)) ->
        Format.fprintf fmt "@[%s_%i(%a)@]"
          ident t (Blist.pp pp_comma Term.pp) args

  end

module Atom =
  struct
    include MakeComplexType(AtomT)

    let mk_eq x y = Eq(x,y)
    let mk_deq x y = Deq(x,y)
    let mk_ipred t ident tl = IndPred(t,(ident, tl))

    let dest_eq = function
      | Eq(x,y) -> (x,y)
      | _ -> invalid_arg "Atom.dest_eq"
    let dest_deq = function
      | Deq(x,y) -> (x,y)
      | _ -> invalid_arg "Atom.dest_deq"
    let dest_pred = function
      | IndPred(t,(ident,tl)) -> (t, ident, tl)
      | _ -> invalid_arg "Atom.dest_pred"

    let is_eq = function
      | Eq _ -> true
      | _ -> false
    let is_deq = function
      | Deq _ -> true
      | _ -> false
    let is_ipred = function
      | IndPred _ -> true
      | _ -> false

    let ipred_eq_mod_tags a a' = match (a,a') with
      | (IndPred(_,(ident,tl)), IndPred(_,(ident',tl'))) ->
        Strng.equal ident ident' && Term.FList.equal tl tl'
      | (_,_) -> false

    let terms = function
      | Eq(sx,sy) | Deq(sx,sy) -> Term.Set.union (Term.terms sx) (Term.terms sy)
      | IndPred(_,(_,tl)) -> Term.terms_of_list tl

    let vars a = Term.filter_vars (terms a)

    let tag = function
      | Eq _ | Deq _ -> None
      | IndPred(i, _) -> Some i

    let to_melt = function
      | Eq eq ->
        let (sx,sy) = Pair.map Term.to_melt eq in
        ltx_mk_math (Latex.concat [sx; symb_eq.melt; sy])
      | Deq eq -> let (sx,sy) = Pair.map Term.to_melt eq in
        Latex.concat [sx; symb_deq.melt; sy]
      | IndPred(t,(ident,tl)) ->
        ltx_mk_math
          (Latex.concat
            ([ Latex.index
                (Latex.mathit (Latex.text ident)) (Latex.text (string_of_int t));
              symb_lp.melt; ltx_comma (Blist.map Term.to_melt tl); symb_rp.melt ]))

    let repl_tags t = function
      | IndPred(t',(ident,tl)) -> IndPred(t,(ident, tl))
      | any -> any

    let strip_tags = function
      | IndPred(_,pred) -> pred
      | _ -> invalid_arg "Atom.strip_tags"

    let subst theta = function
      | Eq p -> Eq (Pair.map (fun t -> Term.subst theta t) p)
      | Deq p -> Deq (Pair.map (fun t -> Term.subst theta t) p)
      | IndPred(t, (ident, tl)) -> IndPred(t, (ident, Term.subst_list theta tl))

    let unify_ipreds theta a a' =
      let ((_, ident, args),(_, ident', args')) = Pair.map dest_pred (a,a') in
      if
        not (Strng.equal ident ident') ||
        Blist.length args <> Blist.length args'
      then
        None
      else
        Term.unify_list theta args args'

    let unify_deqs theta a a' =
      let (p,p') = Pair.map dest_deq (a,a') in Term.unify_pairs theta p p'

    let unify_eqs theta a a' =
      let (p,p') = Pair.map dest_eq (a,a') in Term.unify_pairs theta p p'

  end


module Prod =
  struct
    include Atom.Set

    let tags p =
      let add_tag a s = match Atom.tag a with
        | None -> s
        | Some i -> Tags.add i s in
      fold add_tag p Tags.empty

    let to_string f =
      if is_empty f then "T" else
        Blist.to_string " & " Atom.to_string (elements f)

    let to_melt f =
      ltx_mk_math
        (if is_empty f then symb_true.melt else
          Latex.concat
            (Latex.list_insert symb_and.melt (Blist.map Atom.to_melt (elements f))))

    let pp_conj fmt () =
      Format.pp_print_string fmt " /\\" ; Format.pp_print_space fmt ()

    let pp fmt p =
      if is_empty p then
        Format.fprintf fmt "@[T@]"
      else
        Format.fprintf fmt "@[%a@]" (Blist.pp pp_conj Atom.pp) (elements p)

    let terms f = map_to Term.Set.union Term.Set.empty Atom.terms f
    let vars p = Term.filter_vars (terms p)
    let repl_tags t f = endomap (Atom.repl_tags t) f
    let get_eqs p = map_to_list Atom.dest_eq (filter Atom.is_eq p)
    let get_deqs p = map_to_list Atom.dest_deq (filter Atom.is_deq p)

    let tag_of_ipred ip = Option.get (Atom.tag ip)

    module IndSubf = MakeComplexType(PairTypes(Strng.T)(Term.FList))
    module IndSubfs = IndSubf.Set
    let subsumed_wrt_tags tags p1 p2 =
      let ((p1_ips, p1_nips), (p2_ips, p2_nips)) =
        Pair.map (partition Atom.is_ipred) (p1,p2) in
      let ((p1_tips, p1_ips),(p2_tips, p2_ips)) =
        Pair.map (partition (fun p -> Tags.mem (tag_of_ipred p) tags))
        (p1_ips, p2_ips) in
      let (p1_ips, p2_ips) =
        Pair.map
          (fun p ->
            map_to IndSubfs.add IndSubfs.empty Atom.strip_tags p) (p1_ips, p2_ips) in
      subset p1_nips p2_nips &&
      subset p1_tips p2_tips &&
      IndSubfs.subset p1_ips p2_ips

    let hash = Hashtbl.hash
    let tag_pairs f = TagPairs.mk (tags f)

    let filter_by_kind a p =
      let f = match a with
        | Eq _ -> Atom.is_eq
        | Deq _ -> Atom.is_deq
        | IndPred _ -> Atom.is_ipred in
      filter f p

    (* finds an extension of theta, theta' such that *)
    (* p[theta'] is a subset of p' if left=true and  *)
    (* p is a subset of p'[theta'] if left=false *)
    (* p' |- p[theta] if left=true *)
    (* p'[theta] |- p if left=false *)
    let rec uni_subsumption left hook theta p p' =
      if is_empty p then hook theta else
      let direct f theta' a a' = if left then f theta' a a' else f theta' a' a in
      let a = choose p in
      let p = remove a p in
      let to_match = elements (filter_by_kind a p') in
      let g theta' = uni_subsumption left hook theta' p p' in
      let f a' = match a with
        | Eq _ -> Blist.find_some g (direct Atom.unify_eqs theta a a')
        | Deq _ -> Blist.find_some g (direct Atom.unify_deqs theta a a')
        | IndPred _ ->
          match direct Atom.unify_ipreds theta a a' with
            | None -> None
            | Some theta' -> g theta' in
      Blist.find_some f to_match

    let left_subsumption hook theta p p' = uni_subsumption true hook theta p p'

    let subst theta p = endomap (Atom.subst theta) p

    let univ s f =
      let vs = vars f in
      let evs = Term.Set.filter Term.is_exist_var vs in
      let n = Term.Set.cardinal evs in
      if n=0 then f else
      let uvs = Term.fresh_uvars (Term.Set.union s vs) n in
      let theta = Term.Map.of_list (Blist.combine (Term.Set.elements evs) uvs) in
      subst theta f
  end

module ProdC = MakeComplexType(Prod)

exception Not_product
module Form =
  struct
    include ProdC.Set

    let dest f = if cardinal f <> 1 then raise Not_product else min_elt f
    let tags f = Tags.union_of_list (Blist.map Prod.tags (elements f))
    let to_string d =
      if is_empty d then "F" else
        Blist.to_string " \\/ " Prod.to_string (elements d)

    let to_melt d =
      ltx_mk_math
        (if is_empty d then symb_false.melt else
          Latex.concat
            (Latex.list_insert symb_or.melt (Blist.map Prod.to_melt (elements d))))

    let pp_disj fmt () =
      Format.pp_print_string fmt " \\/" ; Format.pp_print_space fmt ()
    let pp fmt f =
      if is_empty f then
        Format.fprintf fmt "@[F@]"
      else
        Format.fprintf fmt "@[%a@]" (Blist.pp pp_disj Prod.pp) (elements f)

    let terms f = map_to Term.Set.union Term.Set.empty Prod.terms f
    let tag_pairs f = TagPairs.mk (tags f)

    (* f2 |- f1 *)
    let subsumed_wrt_tags t f1 f2 =
      for_all (fun d2 ->
        exists (fun d1 ->
          Prod.subsumed_wrt_tags t d1 d2) f1) f2

    let rec uni_subsumption left fhook theta f f' =
      if is_empty f' then fhook theta else
      let p' = choose f' in
      let f' = remove p' f' in
      let hook' theta' =
        uni_subsumption left fhook theta' f f' in
      let g p = Prod.uni_subsumption left hook' theta p p' in
      Blist.find_some g (elements f)

    let left_subsumption fhook theta f f' =
      uni_subsumption true fhook theta f f'
    let right_subsumption fhook theta f f' =
      uni_subsumption false fhook theta f f'

    let subst theta f = endomap (Prod.subst theta) f
    
    let is_prod f = cardinal f = 1
  end


module Seq =
  struct
    type t = Form.t * Form.t

    let equal (l,r) (l',r') = Form.equal l l' && Form.equal r r'
    let dest s = Pair.map Form.dest s
    let tags seq = Form.tags (fst seq)
    let to_string (l,r) = (Form.to_string l) ^ " |- " ^ (Form.to_string r)
    let to_melt (l,r) =
      ltx_mk_math
        (Latex.concat [Form.to_melt l; symb_turnstile.melt; Form.to_melt r])
    let pp fmt (l,r) = Format.fprintf fmt "@[%a |-@ %a@]" Form.pp l Form.pp r
    let terms s = Term.Set.union (Form.terms (fst s)) (Form.terms (snd s))
    let vars s = Term.filter_vars (terms s)
    let tag_pairs f = TagPairs.mk (tags f)
    (* s2 entails s1 *)
    let subsumed_wrt_tags t s1 s2 =
      Form.subsumed_wrt_tags t (fst s2) (fst s1) &&
      Form.subsumed_wrt_tags Tags.empty (snd s1) (snd s2)

    let subst theta s = Pair.map (Form.subst theta) s

    (*  s' *)
    (* ___ *)
    (*  s  *)
    let uni_subsumption s s' =
      let ((l,r),(l',r')) = (s,s') in
      let tags = Tags.inter (tags s) (tags s') in
      let valid theta' =
        let s'' = subst theta' s' in
        let tags' = Tags.fold
          ( fun t acc ->
            let new_acc = Tags.add t acc in
            if subsumed_wrt_tags new_acc s s'' then new_acc else acc
          ) tags Tags.empty in
        if not (Tags.is_empty tags') then
          Some theta' else None in
      let hook theta' = Form.right_subsumption valid theta' r r' in
      Form.left_subsumption hook Term.Map.empty l' l


  end

module Case =
  struct
    type t = Prod.t * (term list)
    let mk p pa = (p,pa)
    let dest c = c
    let vars (f,vs) =
      Term.filter_vars (Term.Set.union (Term.terms_of_list vs) (Prod.terms f))
    let subst theta (f, vs) =
      (Prod.subst theta f, Term.subst_list theta vs)

    let freshen varset case =
      let casevars = vars case in
      let allvars = Term.Set.union varset casevars in
      let (exist_vars, univ_vars) =
        Pair.map
          Term.Set.elements (Term.Set.partition Term.is_exist_var casevars) in
      let fresh_u_vars = Term.fresh_uvars allvars (Blist.length univ_vars) in
      let fresh_e_vars = Term.fresh_evars allvars (Blist.length exist_vars) in
      let theta = Term.Map.of_list
        ((Blist.combine univ_vars fresh_u_vars) @
         (Blist.combine exist_vars fresh_e_vars)) in
      subst theta case
  end

(* association lists to lists are used because the order in which predicates *)
(* and cases are added is v. important for performance and a tree-map or *)
(* hash-map would disregard that order *)
module Defs =
  struct
    type t = (string * Case.t list) list

    let bindings s = s
    let empty = []

    let to_string defs =
      let string_of_case (ident, cls) =
        let string_of_clause (f, params) =
          (Prod.to_string f) ^ " => " ^ ident ^
          (bracket (Blist.to_string "," Term.to_string params)) in
        ident ^ " {\n" ^ (Blist.to_string " |\n" string_of_clause cls) ^ "\n}" in
      Blist.to_string " ;\n\n" string_of_case (bindings defs)

    let to_melt d = ltx_text (to_string d) 
    let pp fmt d = Format.fprintf fmt "%s" (to_string d)

    let add ident case defs =
      if Blist.mem_assoc ident defs then
        Blist.map
          begin fun (ident', cases) ->
            if ident=ident' then
              (ident', cases @ [case])
            else
              (ident', cases)
          end
        defs
      else
        defs @ [ (ident, [case]) ]
  end