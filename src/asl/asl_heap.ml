open Lib
open Util
open Symbols
open MParser
module F = Fopl
module L = List
module T = Asl_term

let split_heaps = ref true

type abstract1 = Asl_term.Set.t option

type symheap =
  {
    eqs : Asl_uf.t;
    neqs : Asl_neqs.t;
    leqs : Asl_leqs.t;
    lts : Asl_lts.t;
    arrays : Asl_arrays.t;
    mutable _terms : Asl_term.Set.t option;
    mutable _vars : Asl_term.Set.t option;
  }

type t = symheap

(* accessors *)

let equal h h' =
  h == h' ||
  Asl_uf.equal h.eqs h'.eqs &&
  Asl_neqs.equal h.neqs h'.neqs &&
  Asl_leqs.equal h.leqs h'.leqs &&
  Asl_lts.equal h.lts h'.lts &&
  Asl_arrays.equal h.arrays h'.arrays

let equal_upto_tags h h' = equal h h'

include Fixpoint(struct type t = symheap let equal = equal end)

let compare f g =
  if f == g then 0 else
    match Asl_uf.compare f.eqs g.eqs with
    | n when n <>0 -> n
    | _ -> match Asl_neqs.compare f.neqs g.neqs with
        | n when n <>0 -> n
        | _ -> match Asl_leqs.compare f.leqs g.leqs with
          | n when n <>0 -> n
          | _ -> match Asl_lts.compare f.lts g.lts with
            | n when n <>0 -> n
            | _ -> Asl_arrays.compare f.arrays g.arrays

(* custom hash function so that memoization fields are ignored when hashing *)
(* so that the hash invariant is preserved [a = b => hash(a) = hash(b)] *)
(* FIXME: memoize hash as well? *)
let hash h = 
  genhash
    (genhash 
      (genhash 
        (genhash 
          (Asl_arrays.hash h.arrays)
          (Asl_neqs.hash h.neqs))
        (Asl_uf.hash h.eqs))
      (Asl_leqs.hash h.leqs))
    (Asl_lts.hash h.lts)

let terms f =
  match f._terms with
  | Some trms -> trms
  | None ->
    let trms = 
      Asl_term.Set.union_of_list
        [ Asl_uf.terms f.eqs; 
          Asl_neqs.terms f.neqs; 
          Asl_leqs.terms f.leqs; 
          Asl_lts.terms f.lts; 
          Asl_arrays.terms f.arrays] in
    f._terms <- Some trms;
    trms

let vars f = 
  match f._vars with 
  | Some v -> v
  | None ->
    let v = Asl_term.filter_vars (terms f) in
    f._vars <- Some v;
    v

let to_string f =
  let res = String.concat symb_star.sep
      ((Asl_uf.to_string_list f.eqs) @ (Asl_neqs.to_string_list f.neqs) @
         (Asl_leqs.to_string_list f.leqs) @ (Asl_lts.to_string_list f.lts) @
        (Asl_arrays.to_string_list f.arrays)) in
  if res = "" then keyw_emp.str else res

let to_melt f =
  let sep = if !split_heaps then Latex.text " \\\\ \n" else symb_star.melt in
  let content = Latex.concat (Latex.list_insert sep
          (Blist.filter (fun l -> not (Latex.is_empty l))
              [Asl_uf.to_melt f.eqs; Asl_neqs.to_melt f.neqs;
              Asl_leqs.to_melt f.leqs; Asl_lts.to_melt f.lts;
              Asl_arrays.to_melt f.arrays])) in
  let content = if !split_heaps then
      Latex.concat
        [
        ltx_newl;
        Latex.environment
          (* ~opt: (Latex.A, Latex.text "b") *)
          (* ~args:[(Latex.A, Latex.text "l")] *)
          "gathered" (Latex.M, content) Latex.M;
        ltx_newl
        ]
    else
      content in
  ltx_mk_math content

let pp fmt h =
  let l =
    ((Asl_uf.to_string_list h.eqs) @ (Asl_neqs.to_string_list h.neqs) @
      (Asl_leqs.to_string_list h.leqs) @ (Asl_lts.to_string_list h.lts) @
      (Asl_arrays.to_string_list h.arrays)) in
  Format.fprintf fmt "@[%a@]" (Blist.pp pp_star Format.pp_print_string) 
    (if l<>[] then l else [keyw_emp.str])

let subsumed ?(total=true) h h' = 
  Asl_uf.subsumed h.eqs h'.eqs &&
  Asl_neqs.subsumed h'.eqs h.neqs h'.neqs &&
  Asl_leqs.subsumed h'.eqs h.leqs h'.leqs &&
  Asl_lts.subsumed h'.eqs h.lts h'.lts &&
  Asl_arrays.subsumed ~total h'.eqs h.arrays h'.arrays


let subsumed_upto_tags ?(total=true) h h' = subsumed ~total h h'

  
(* Constructors *)

let mk eqs neqs leqs lts arrays = { eqs; neqs; leqs; lts; arrays; _terms=None; _vars=None }
  
let dest h = (h.eqs, h.neqs, h.leqs, h.lts, h.arrays)
  
let empty = mk Asl_uf.empty Asl_neqs.empty Asl_leqs.empty Asl_lts.empty Asl_arrays.empty

let is_empty h = equal h empty

let subst theta h =
  { eqs = Asl_uf.subst theta h.eqs;
    neqs = Asl_neqs.subst theta h.neqs;
    leqs = Asl_leqs.subst theta h.leqs;
    lts = Asl_lts.subst theta h.lts;
    arrays = Asl_arrays.subst theta h.arrays;
    _terms = None;
    _vars = None
  }

let with_eqs h eqs = { h with eqs = eqs; _terms=None; _vars=None }
let with_neqs h neqs = { h with neqs = neqs; _terms=None; _vars=None }
let with_leqs h leqs = { h with leqs = leqs; _terms=None; _vars=None }
let with_lts h lts = { h with lts = lts; _terms=None; _vars=None }
let with_arrays h arrs = { h with arrays = arrs; _terms=None; _vars=None }

(* TODO: add counterparts for leqs and lts *)
let del_neq h neq = with_neqs h (Asl_neqs.remove neq h.neqs)
let del_arr h arr = with_arrays h (Asl_arrays.remove arr h.arrays)


let mk_arr arr = 
  { empty with arrays = Asl_arrays.singleton arr; _terms=None; _vars=None }
let mk_eq p = 
  { empty with eqs = Asl_uf.add p Asl_uf.empty; _terms=None; _vars=None }
let mk_neq p = 
  { empty with neqs = Asl_neqs.singleton p; _terms=None; _vars=None }
let mk_leq p = 
  { empty with leqs = Asl_leqs.singleton p; _terms=None; _vars=None }
let mk_lt p = 
  { empty with lts = Asl_lts.singleton p; _terms=None; _vars=None }


(* star two formulae together *)
let star f g =
  (* computes all neqs due to a list of arrs:
   * for each pair of arrays array(a, b), array(c, d)
   * add a != c, a != d, b != c, b != d *)
  (*let explode_neqs arrs =
    let cp = Blist.cartesian_hemi_square arrs in
    let diff_ends arr1 arr2 = Asl_neqs.of_list [(fst arr1, fst arr2); (fst arr1, snd arr2); (snd arr1, fst arr2); (snd arr1, snd arr2)] in
    (Blist.fold_left (fun s p -> Asl_neqs.union (diff_ends (fst p) (snd p)) s) Asl_neqs.empty cp) in
  (* computes all leqs due to a list of arrs:
   * for each array array(a, b)
   * add a <= b *)
  let explode_leqs arrs =
    (Blist.fold_left (fun s p -> Asl_leqs.add (fst p, snd p) s) Asl_leqs.empty arrs) in*)
  let newarrs = Asl_arrays.union f.arrays g.arrays in
  mk 
    (Asl_uf.union f.eqs g.eqs)
    (Asl_neqs.union_of_list [f.neqs; g.neqs])
    (Asl_leqs.union_of_list [f.leqs; g.leqs])
    (Asl_lts.union f.lts g.lts)
    newarrs

let parse_atom st =
  ( attempt (parse_symb keyw_emp >>$ empty) <|>
    attempt (Asl_uf.parse |>> mk_eq) <|>
    attempt (Asl_neqs.parse |>> mk_neq) <|>
    attempt (Asl_leqs.parse |>> mk_leq) <|>
    attempt (Asl_lts.parse |>> mk_lt) <|>
    (Asl_array.parse |>> mk_arr) <?> "atom"
  ) st

let parse st =
  (sep_by1 parse_atom (parse_symb symb_star) >>= (fun atoms ->
          return (Blist.foldl star empty atoms)) <?> "symheap") st

let of_string s =
  handle_reply (MParser.parse_string parse s ())

let add_eq h eq = 
  { h with eqs = Asl_uf.add eq h.eqs; _terms=None; _vars=None }
let add_neq h neq = 
  { h with neqs = Asl_neqs.add neq h.neqs; _terms=None; _vars=None }
let add_leq h leq = 
  { h with leqs = Asl_leqs.add leq h.leqs; _terms=None; _vars=None }
let add_lt h lt = 
  { h with lts = Asl_lts.add lt h.lts; _terms=None; _vars=None }
let add_arr h arr = star h (mk_arr arr) 

let subst_existentials h =
  let aux h' =
    let (ex_eqs, non_ex_eqs) =
      Blist.partition
        (fun p' -> Pair.disj (Pair.map Asl_term.is_exist_var p')) 
        (Asl_uf.bindings h'.eqs) in
    if ex_eqs =[] then h' else
    let ex_eqs = 
      Blist.map (fun (x,y) ->
        if Asl_term.is_exist_var x then (x, y)
          else (y,x)) ex_eqs in
      let h'' = 
        { h' with eqs = Asl_uf.of_list non_ex_eqs; _terms=None; _vars=None } in
      subst (Asl_term.Map.of_list ex_eqs) h'' in
  fixpoint aux h

let simplify_terms h =
  let theta = Asl_term.Map.of_list (Blist.foldl (fun acc x -> let s = (Asl_term.simplify x) in
                           if not (Asl_term.equal s x) then (x, s)::acc else acc) [] (Asl_term.Set.elements (terms h)))
  in subst theta h

let norm h =
  { h with
    neqs = Asl_neqs.norm h.eqs h.neqs ;
    leqs = Asl_leqs.norm h.eqs h.leqs ;
    lts = Asl_lts.norm h.eqs h.lts ;
    arrays = Asl_arrays.norm h.eqs h.arrays ;
    _terms = None ; 
    _vars = None ;
  }

let unify_partial ?(tagpairs=false) 
    ?(sub_check=Asl_subst.trivial_check)
    ?(cont=Asl_unifier.trivial_continuation)
    ?(init_state=Asl_unifier.empty_state) h h' =
  let f1 theta' = Asl_uf.unify_partial ~sub_check ~cont ~init_state:theta' h.eqs h'.eqs in
  let f2 theta' = Asl_neqs.unify_partial ~sub_check ~cont:f1 ~init_state:theta' h.neqs h'.neqs in
  let f3 theta' = Asl_leqs.unify_partial ~sub_check ~cont:f2 ~init_state:theta' h.leqs h'.leqs in
  let f4 theta' = Asl_lts.unify_partial ~sub_check ~cont:f3 ~init_state:theta' h.lts h'.lts in
  Asl_arrays.unify ~total:false ~sub_check ~cont:f4 ~init_state h.arrays h'.arrays


let exist_vars heap = Asl_term.Set.filter Asl_term.is_exist_var (vars heap)

let free_vars heap = Asl_term.Set.filter Asl_term.is_free_var (vars heap)

(* First part of gamma: AND all the pure parts together *)
let pure_to_fopl h = F.And(F.And(Asl_uf.to_fopl h.eqs, Asl_neqs.to_fopl h.neqs),
                              F.And(Asl_leqs.to_fopl h.leqs, Asl_lts.to_fopl h.lts))

(* 'gamma' is a function defined in the paper, used for both
 * satisfaction and entailment
 * NB: As highlighted in asl_while_rules, ALL vars are set to be >= 0. Indipendently whether they are
 * locations or not. To change this behaviour, apply all_natural to (Asl_arrays.vars h.arrays) instead of (vars h).
 *)
let gamma ?(pure=None) h =
  (* This part is in the paper implicitly; all vars are >= 0. *)
  let rec all_natural vars =
    let constraint_on_var s = F.PF (F.Le (Asl_term.mk_const(0), s))
    in Asl_term.Set.fold (fun v acc -> F.And(constraint_on_var v, acc)) vars F.trivially_true
  (* First part of gamma: AND all the pure parts together *)
  in let pure =
    match pure with
    | Some f -> f
    | None -> pure_to_fopl h
  (* Second part of gamma: start of array comes before its end *)
  in let rec begin_end_array_constraints =
    let spacial_to_formula = function
      | (t1, t2) -> F.PF (F.Le (t1, t2))
    in function
      | []      -> F.trivially_true
      | [h]     -> spacial_to_formula h
      | h :: t  -> F.And(spacial_to_formula h,
                      begin_end_array_constraints t)
  (* Third part of gamma: no two arrays overlap. *)
  in let non_overlapping_arrays_constraints spacial_terms =
    (* Constraints for one particular array: comparing its indices
     * with those of all other arrays *)
    let rec compare_with_others arr =
      let compare_two_arrays (a1, b1) (a2, b2) =
        F.Or(F.PF(F.Lt(b1, a2)), F.PF(F.Lt(b2, a1)))
      in function
        | []      -> F.trivially_true
        | [h]     -> compare_two_arrays arr h
        | h :: t  -> F.And((compare_two_arrays arr h),
                      compare_with_others arr t)
    (* Now apply compare_with_others to each array in the list. We do
     * NOT want to compare an array with itself (this will result in
     * an invalid constraint), unless the user specified two or more
     * identical arrays (in which case the invalidity is correct
     * because the arrays overlap).
     * Thus, 'all_arrays_minus arr' removes a SINGLE copy of arr from
     * all_arrays.
     *)
    in let rec constraints all_arrays array_list =
      let all_arrays_minus arr =
          let rec all_arrays_minus arr = function
          | [] -> failwith "Impossible, there should be at least one\
                            copy of arr in the list of all arrays."
          | h :: t -> if h = arr
                      then t
                      else h :: (all_arrays_minus arr t)
          in all_arrays_minus arr all_arrays
      in match array_list with
        | []      -> F.trivially_true
        | [h]     -> compare_with_others h (all_arrays_minus h)
        | h :: t  -> F.And(
                      (compare_with_others h (all_arrays_minus h)),
                      constraints t t)
    in let array_idxs = List.map (fun a -> match a with
      | (a, b) -> (a, b)
    ) spacial_terms
    in constraints array_idxs array_idxs
  in F.And(all_natural (vars h),
          F.And(pure,
                F.And(begin_end_array_constraints (Asl_arrays.to_list h.arrays),
                      non_overlapping_arrays_constraints (Asl_arrays.to_list h.arrays)))
  )

let satisfiable_z3 ?(pure=None) h =
  let fopl = F.Exists (free_vars h, (gamma ~pure h))
  in F.to_z3 fopl


(* ∏ |= (x=y) <=> ∏ /\ (x!=y) is unsat *)
let equates h x y =
  if Asl_uf.simple_equates h.eqs x y then true else
  let all_free = Asl_term.Set.union (Asl_term.filter_vars (Asl_term.Set.singleton y))
    (Asl_term.Set.union (Asl_term.filter_vars (Asl_term.Set.singleton x)) (free_vars h))
  in Asl_sat.is_unsat (F.to_z3 (F.Exists (all_free, F.And (pure_to_fopl  h, F.PF(F.Ne(x, y))))))

(* ∏ |= (x!=y) <=> ∏ /\ (x=y) is unsat *)
let disequates h x y = 
  let all_free = Asl_term.Set.union (Asl_term.filter_vars (Asl_term.Set.singleton y))
    (Asl_term.Set.union (Asl_term.filter_vars (Asl_term.Set.singleton x)) (free_vars h))
  in Asl_sat.is_unsat (F.to_z3 (F.Exists (all_free, F.And (pure_to_fopl  h, F.PF(F.Eq(x, y))))))

(* ∏ |= (x<y) <=> ∏ /\ (y<=x) is unsat *)
let entail_lt h x y =
  let all_free = Asl_term.Set.union (Asl_term.filter_vars (Asl_term.Set.singleton y))
    (Asl_term.Set.union (Asl_term.filter_vars (Asl_term.Set.singleton x)) (free_vars h))
  in Asl_sat.is_unsat (F.to_z3 (F.Exists (all_free, F.And (pure_to_fopl  h, F.PF(F.Le(y, x))))))

(* ∏ |= (x<=y) <=> ∏ /\ (y<x) is unsat *)
let entail_le h x y =
  let all_free = Asl_term.Set.union (Asl_term.filter_vars (Asl_term.Set.singleton y))
    (Asl_term.Set.union (Asl_term.filter_vars (Asl_term.Set.singleton x)) (free_vars h))
  in Asl_sat.is_unsat (F.to_z3 (F.Exists (all_free, F.And (pure_to_fopl  h, F.PF(F.Lt(y, x))))))

    
    