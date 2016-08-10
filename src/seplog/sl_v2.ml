open Hashcons
open Symbols
open MParser
open Lib
open Util

type _t = node hash_consed
and node =
  | True
  | Emp
  | Eq of Sl_tpair.t
  | Deq of Sl_tpair.t
  | Pto of Sl_pto.t
  | Pred of Sl_tpred.t
  | Star of _t * _t
  | Or of _t * _t
  | Exists of Sl_term.t * _t

module Form = struct
  type t = node
  
  let equal x y = match (x,y) with
    | True, True
    | Emp, Emp -> true  
    | Eq p, Eq p' 
    | Deq p, Deq p' -> Sl_tpair.equal p p'
    | Pto(pto), Pto(pto') -> Sl_pto.equal pto pto'
    | Pred(pred), Pred(pred') -> Sl_tpred.equal pred pred'
    | Star(f, g), Star(f', g') 
    | Or(f, g), Or(f', g') -> f==f' && g==g'
    | Exists(x, f), Exists(x', f') -> Sl_term.equal x x' && f==f'
    | _, _ -> false
  
  let hash = function
    | True -> 9
    | Emp -> 11
    | Eq p -> Sl_tpair.hash p
    | Deq p -> abs (19 * (Sl_tpair.hash p) + 1)
    | Pto pto -> abs (19 * (Sl_pto.hash pto) + 2)
    | Pred pred -> abs (19 * (Sl_tpred.hash pred) + 3)
    | Star(f, g) -> abs (19 * (19 * f.hkey + g.hkey) + 4)  
    | Or(f, g) -> abs (19 * (19 * f.hkey + g.hkey) + 5)
    | Exists(x, f) -> abs (19 * (19 * (Sl_term.hash x) + f.hkey) + 6)
    
  end
  
module HForm = Make(Form)  

let ft = HForm.create 251

let compare f g = Pervasives.compare f.hkey g.hkey
let equal f g = f==g 
let hash f = f.Hashcons.hkey

let mk_true () = HForm.hashcons ft (True)
let mk_emp () =  HForm.hashcons ft (Emp)
let mk_eq p =    HForm.hashcons ft (Eq p)
let mk_deq p =   HForm.hashcons ft (Deq p)
let mk_pto pto = HForm.hashcons ft (Pto pto)
let mk_pred pred = HForm.hashcons ft (Pred pred)
let mk_star f g = HForm.hashcons ft (Star(f, g))
let mk_or f g = HForm.hashcons ft (Or(f, g))
let mk_exists x f = 
  if not (Sl_term.is_var x) then failwith "mk_exists nil" 
  else HForm.hashcons ft (Exists(x, f))

let is_true f = match f.node with
  | True -> true
  | _ -> false 
let is_emp f = match f.node with
  | Emp -> true
  | _ -> false 
let is_eq f = match f.node with
  | Eq _ -> true
  | _ -> false 
let is_deq f = match f.node with
  | Deq _ -> true
  | _ -> false 
let is_pto f = match f.node with
  | Pto _ -> true
  | _ -> false 
let is_pred f = match f.node with
  | Pred _ -> true
  | _ -> false 
let is_star f = match f.node with
  | Star _ -> true
  | _ -> false 
let is_or f = match f.node with
  | Or _ -> true
  | _ -> false 
let is_exists f = match f.node with
  | Exists _ -> true
  | _ -> false 

let is_atom f = match f.node with
  | True | Emp | Eq _ | Deq _ | Pto _ | Pred _ -> true
  | _ -> false 

let dest_true f = match f.node with
  | True -> ()
  | _ -> failwith "dest_true"
let dest_emp f = match f.node with
  | Emp -> ()
  | _ -> failwith "dest_emp"
let dest_eq f = match f.node with
  | Eq p -> p
  | _ -> failwith "dest_eq" 
let dest_deq f = match f.node with
  | Deq p -> p
  | _ -> failwith "dest_deq" 
let dest_pto f = match f.node with
  | Pto p -> p
  | _ -> failwith "dest_pto" 
let dest_pred f = match f.node with
  | Pred p -> p
  | _ -> failwith "dest_pred" 
let dest_star f = match f.node with
  | Star(f, g) -> (f, g)
  | _ -> failwith "dest_star"
let dest_or f = match f.node with
  | Or(f, g) -> (f, g)
  | _ -> failwith "dest_or" 
let dest_exists f = match f.node with
  | Exists(x, f) -> (x, f)
  | _ -> failwith "dest_exist"

let rec pp fmt f = 
  let paren p fmt f =
    if is_atom f || is_star f || p f then 
      pp fmt f 
    else 
      Format.fprintf fmt "@[%s%a%s@]" symb_lp.str pp f symb_rp.str 
  in
  match f.node with
  | True -> Format.pp_print_string fmt keyw_true.str
  | Emp -> Format.pp_print_string fmt symb_emp.str
  | Eq(x,y) -> 
    Format.fprintf fmt "@[%a%s%a@]" Sl_term.pp x symb_eq.str Sl_term.pp y  
  | Deq(x,y) -> 
    Format.fprintf fmt "@[%a%s%a@]" Sl_term.pp x symb_deq.str Sl_term.pp y  
  | Pto p -> 
    Sl_pto.pp fmt p
  | Pred p ->
    Sl_tpred.pp fmt p
  | Exists(x, f) ->
    Format.fprintf fmt "@[%s %a%s %a@]" 
      keyw_exists.str Sl_term.pp x symb_dot.str (paren is_exists) f
  | Star(f, g) ->
    Format.fprintf fmt "@[%a%s%a@]"
      (paren (fun _ -> false)) f symb_star.sep (paren (fun _ -> false)) g 
  | Or(f, g) ->
    Format.fprintf fmt "@[%a%s%a@]"
      (paren is_or) f symb_or.sep (paren is_or) g 

let to_string = mk_to_string pp 

let parse =
  let parse_infix_pair symb st = 
    pair Sl_term.parse (parse_symb symb >> Sl_term.parse) st
  in
  let connectives = 
    [ (* abstraction is necessary to avoid capture to a fixed poly-type *)
      [ Infix  ((fun st -> (parse_symb symb_star >>$ mk_star) st), Assoc_right) ];
      [ Prefix (fun st -> (parse_symb keyw_exists >> Sl_term.parse >>= 
                 (fun x -> parse_symb symb_dot >>$ mk_exists x)) st) ];
      [ Infix  ((fun st -> (parse_symb symb_or >>$ mk_or) st), Assoc_right) ]
    ] 
  in
  let rec paren_atom st =
    ( attempt (parse_symb symb_lp >> parse << parse_symb symb_rp) <|>
      attempt (parse_symb keyw_true >>$ mk_true ()) <|>
      attempt (parse_symb keyw_emp >>$ mk_emp ()) <|>
      attempt (parse_infix_pair symb_eq |>> mk_eq) <|> 
      attempt (parse_infix_pair symb_deq |>> mk_deq) <|>
      attempt (Sl_pto.parse |>> mk_pto) <|>
              (Sl_tpred.parse |>> mk_pred)
    ) st
  and parse st = expression connectives paren_atom st 
  in  parse

let rec terms f = match f.node with 
  | True | Emp -> Sl_term.Set.empty
  | Eq(x,y) | Deq(x,y) -> Sl_term.Set.add y (Sl_term.Set.singleton x)
  | Pto p -> Sl_pto.terms p
  | Pred pred -> Sl_tpred.terms pred
  | Star(f, g) | Or(f, g) -> Sl_term.Set.union (terms f) (terms g) 
  | Exists(x, f) -> Sl_term.Set.add x (terms f)

let rec _vars free f = match f.node with 
  | True | Emp -> Sl_term.Set.empty
  | Eq(x,y) | Deq(x,y) -> 
    Sl_term.filter_vars (Sl_term.Set.add y (Sl_term.Set.singleton x))
  | Pto p -> Sl_pto.vars p
  | Pred pred -> Sl_tpred.vars pred
  | Star(f, g) | Or(f, g) -> Sl_term.Set.union (_vars free f) (_vars free g) 
  | Exists(x, f) -> 
    let v = _vars free f in
    (if free then Sl_term.Set.remove else Sl_term.Set.add) x v

let free_vars f = _vars true f

let rec tags f = match f.node with 
  | True | Emp | Eq _ | Deq _ | Pto _ -> Tags.empty
  | Pred pred -> Sl_tpred.tags pred
  | Star(f, g) | Or(f, g) -> Tags.union (tags f) (tags g) 
  | Exists(_, f) -> tags f

(* capture avoiding substitution *)
let rec subst theta f = match f.node with
  | True | Emp -> f
  | Eq p -> mk_eq (Sl_tpair.subst theta p)
  | Deq p -> mk_deq (Sl_tpair.subst theta p)
  | Pto p -> mk_pto (Sl_pto.subst theta p)
  | Pred p -> mk_pred (Sl_tpred.subst theta p)
  | Star(f, g) -> mk_star (subst theta f) (subst theta g)
  | Or(f, g) -> mk_or (subst theta f) (subst theta g) 
  | Exists(x, f) -> mk_exists x (subst (Sl_term.Map.remove x theta) f)

(* enfore the variable convention *)
(* i.e. all quantifies bind distinct variables and these *)
(* are in addition distinct to all free variables *)
let enforce_vc f = 
  let rec alphanorm a v = match a.node with
    | True | Emp | Eq _ | Deq _ | Pto _ | Pred _ -> (a, v)
    | Star(f, g) -> 
      let (f', v') = alphanorm f v in
      let (g', v'') = alphanorm g v' in 
      (mk_star f' g', v'') 
    | Or(f, g) ->  
      let (f', v') = alphanorm f v in
      let (g', v'') = alphanorm g v' in 
      (mk_or f' g', v'') 
    | Exists(x, f) ->
      if Sl_term.Set.mem x v then
        let y = Sl_term.fresh_fvar v in
        let f' = subst (Sl_subst.singleton x y) f in
        let (f', v') = alphanorm f' (Sl_term.Set.add y v) in
        (mk_exists y f', v')
      else 
        let (f', v') = alphanorm f (Sl_term.Set.add x v) in
        (mk_exists x f', v')
  in
  fst (alphanorm f (free_vars f))

let is_symheap f =
  let rec body h = match h.node with
    | Star(f, g) -> is_atom f && body g
    | True | Emp | Eq _ | Deq _ | Pto _ | Pred _ -> true
    | Or _ | Exists _ -> false
  in
  let rec exists h = match h.node with 
    | Exists(_, k) -> exists k
    | True | Emp | Eq _ | Deq _ | Pto _ | Pred _ | Star _ -> body h
    | Or _ -> false
  in
  exists f
    
let is_dsh f =
  let rec disj h = match h.node with
    | Or(f, g) -> is_symheap f && disj g
    | _ -> is_symheap h
  in
  is_symheap f || disj f     


let to_dsh f =
  let rec norm a = match a.node with
    (* (f \/ g) \/ h = f \/ (g \/ h) *)
    | Or({node=Or(f, g)}, h) -> mk_or f (mk_or g h)
    
    (* (f * g) * h = f * (g * h) *)
    | Star({node=Star(f, g)}, h) ->  mk_star f (mk_star g h)
    
    (* \exists x. (f \/ g) = (\exists x.f) \/ (\exists x.g) *)
    | Exists(x, {node=Or(f,  g)}) -> 
      mk_or (mk_exists x f) (mk_exists x g)
      
    (* f * (g \/ h) = (f*g) \/ (f*h) *)
    | Star(f, {node=Or(g, h)}) -> 
      mk_or (mk_star f g) (mk_star f h)
    (* (f \/ g) * h) = (f*h) \/ (g*h) *)
    | Star({node=Or(f, g)}, h) -> 
      mk_or (mk_star f h) (mk_star g h)
    
    (* (\exists x.f) * f' = \exists x .(f*f') *)
    (* whenever x \notin free_vars f' *)
    | Star({node=Exists(x, f)}, f') ->
      if Sl_term.Set.mem x (free_vars f') then a else 
        mk_exists x (mk_star f f')
    | Star(f, {node=Exists(x, f')}) ->
      if Sl_term.Set.mem x (free_vars f) then a else 
        mk_exists x (mk_star f f')
    
    (* | Exists(x, {node=Exists(y,f)}) ->           *)
    (*   if (Sl_term.compare x y) <= 0 then a else  *)
    (*     mk_exists y (mk_exists x f)              *)
    
    | Exists(x, f) -> mk_exists x (norm f)
    | Or(f, g) -> mk_or (norm f) (norm g)
    | Star(f, g) -> mk_star (norm f) (norm g)
    
    (* | Eq(x,y) ->                                             *)
    (*   if (Sl_term.compare x y) <= 0 then a else mk_eq (y,x)  *)
    (* | Deq(x,y) ->                                            *)
    (*   if (Sl_term.compare x y) <= 0 then a else mk_deq (y,x) *)
    | _ -> a
  in
  let res = Lib.fixpoint equal norm (enforce_vc f) in
  assert (is_dsh res) ; res 

let to_symheap f =
  if not (is_symheap f) then failwith "to_symheap" else f

let rec fold_atoms fn g a =
  match g.node with
    | True | Emp | Eq _ | Deq _ | Pto _ | Pred _ -> fn g a
    | Star(l, r) | Or(l, r) -> fold_atoms fn r (fold_atoms fn l a) 
    | Exists(_, f) -> fold_atoms fn f a 

let rec find_atoms pred g = 
  match g.node with
    | True | Emp | Eq _ | Deq _ | Pto _ | Pred _ -> Option.pred pred g
    | Star(l, r) | Or(l, r) ->
      let l' = find_atoms pred l in
      if Option.is_some l' then l' else find_atoms pred r
    | Exists(_, f) -> find_atoms pred f 

let rec exists_atoms pred g =
  match g.node with
    | True | Emp | Eq _ | Deq _ | Pto _ | Pred _ -> pred g
    | Star(l, r) | Or(l, r) -> exists_atoms pred l || exists_atoms pred r
    | Exists(_, f) -> exists_atoms pred f 

let rec map_atoms fn g =
  match g.node with
    | True | Emp | Eq _ | Deq _ | Pto _ | Pred _ -> fn g
    | Star(l, r) -> mk_star (map_atoms fn l) (map_atoms fn r) 
    | Or(l, r) -> mk_or (map_atoms fn l) (map_atoms fn r)
    | Exists(x, f) -> mk_exists x (map_atoms fn f) 
  

(* type path =                                 *)
(*   | Top                                     *)
(*   | StarL of path * _t                      *)
(*   | StarR of _t * path                      *)
(*   | OrL of path * _t                        *)
(*   | OrR of _t * path                        *)
(*   | ExistsD of Sl_term.t * path             *)

(* type location = _t * path                   *)

(* let change (_, p) f = (f, p)                *)
(* let rec zip (f, p) = match p with           *)
(*   | Top -> f                                *)
(*   | StarL(q, r) -> zip (mk_star f r, q)     *)
(*   | StarR(l, q) -> zip (mk_star l f, q)     *)
(*   | OrL(q, r) -> zip (mk_or f r, q)         *)
(*   | OrR(l, q) -> zip (mk_or l f, q)         *)
(*   | ExistsD(x, q) -> zip (mk_exists x f, q) *)

(* let fold f g a =                                                        *)
(*   let rec aux f ((g, path) as loc) a =                                  *)
(*     match g.node with                                                   *)
(*       | True | Emp | Eq _ | Deq _ | Pto _ | Pred _ -> f loc a           *)
(*       | Star(l, r) ->                                                   *)
(*         aux f (r, StarR(l, path)) (f loc (aux f (l, StarL(path, r)) a)) *)
(*       | Or(l, r) ->                                                     *)
(*         aux f (r, OrR(l, path)) (f loc (aux f (l, OrL(path, r)) a))     *)
(*       | Exists(x, g) ->                                                 *)
(*         f loc (aux f (g, ExistsD(x, path)) a)                           *)
(*   in                                                                    *)
(*   aux f (g, Top) a                                                      *)

(* let iter f g =                                                        *)
(*   let rec aux f ((g, path) as loc) =                                  *)
(*     match g.node with                                                 *)
(*       | True | Emp | Eq _ | Deq _ | Pto _ | Pred _ -> f loc           *)
(*       | Star(l, r) ->                                                 *)
(*         aux f (l, StarL(path, r)) ; aux f (r, StarR(l, path)) ; f loc *)
(*       | Or(l, r) ->                                                   *)
(*         aux f (l, OrL(path, r)) ; aux f (r, OrR(l, path)) ; f loc     *)
(*       | Exists(x, g) -> aux f (g, ExistsD(x, path)) ; f loc           *)
(*   in                                                                  *)
(*   aux f (g, Top)                                                      *)


(* let find p f =                                             *)
(*   let rec search pred ((f, p) as loc) =                    *)
(*     if pred f then Some loc else                           *)
(*     match f.node with                                      *)
(*       | True | Emp | Eq _ | Deq _ | Pto _ | Pred _ -> None *)
(*       | Star(l, r) ->                                      *)
(*         let res = search pred (l, StarL(p, r)) in          *)
(*         if Option.is_some res then res else                *)
(*         search pred (r, StarR(l, p))                       *)
(*       | Or(l, r) ->                                        *)
(*         let res = search pred (l, OrL(p, r)) in            *)
(*         if Option.is_some res then res else                *)
(*         search pred (r, OrR(l, p))                         *)
(*       | Exists(x, g) -> search pred (g, ExistsD(x, p))     *)
(*   in                                                       *)
(*   search p (f, Top)                                        *)

module CommonImplementation = 
  struct
    type t = _t
    
    let hash = hash
    let equal = equal
    let compare = compare
    let pp = pp
    let to_string = to_string
        
    let mk_true = mk_true
    let mk_emp = mk_emp
    let mk_eq = mk_eq
    let mk_deq = mk_deq
    let mk_pto = mk_pto
    let mk_pred = mk_pred
    
    let is_true = is_true
    let is_emp = is_emp
    let is_eq = is_eq
    let is_deq = is_deq
    let is_pto = is_pto
    let is_pred = is_pred
  
    let dest_true = dest_true
    let dest_emp = dest_emp
    let dest_eq = dest_eq
    let dest_deq = dest_deq
    let dest_pto = dest_pto
    let dest_pred = dest_pred
    
    let terms = terms
    let free_vars = free_vars
    let subst = subst
    let tags = tags
    let tag_pairs f = TagPairs.mk (tags f)
  
  end

module Atom =
  struct
    include CommonImplementation
    
    let parse st =
      (parse >>= 
        (fun f -> if not (is_atom f) then fail "Not an atom" else return f))
        st  
    let of_string = mk_of_string parse
  
  end

module SymHeap =
  struct
    include CommonImplementation
    
    let parse st =
      (parse >>= 
        (fun f -> if not (is_symheap f) then fail "Not a symheap" else return f))
        st  
    let of_string = mk_of_string parse
    
    let bound_vars sh =
      let rec loop vs n = match n.node with
        | Exists (x, f) -> loop (Sl_term.Set.add x vs) f
        | _ -> vs in
      loop Sl_term.Set.empty sh
    
    let is_atom = is_atom
    
    let dest_atom f = 
      if is_atom f then f else failwith "Not an atoms"
      
    let of_atom f = f
    
    let fold = fold_atoms
    
    let find = find_atoms
    
    let exists = exists_atoms
    
    let _build_uf f =
        fold 
          (fun atom a -> if is_eq atom then Sl_uf.add (dest_eq atom) a else a)
          f
          Sl_uf.empty
     
    let equates f x y = 
      Sl_uf.equates (_build_uf f) x y
    
    let disequates f x y =
      exists
        (fun atom -> 
          not (is_deq atom) ||
          let (w, z) = dest_deq atom in
          equates f x w && equates f y z
          || 
          equates f x z && equates f y w 
        )
        f
    
    let find_lval x f =
      Option.map
        dest_pto 
        (find_atoms 
          (fun atom -> is_pto atom && equates f x (fst (dest_pto atom))) 
          f)
    
    let idents f =
      fold 
        (fun atom a -> 
          if is_pred atom then 
            Sl_predsym.MSet.add (Sl_tpred.predsym (dest_pred atom)) a
          else
            a
        ) 
        f
        Sl_predsym.MSet.empty

    let inconsistent f = 
      exists
        (fun atom -> is_deq atom && let (x,y) = dest_eq atom in equates f x y) 
        f

    let memory_consuming f =
      not (exists is_pred f) || exists is_pto f 
        
    let constructively_valued h =
      let freevars = free_vars h in
      let existvars = bound_vars h in
      let is_cvalued cvalued v =
        Sl_term.Set.exists (equates h v) cvalued ||
        exists 
          (fun atom ->
            is_pto atom &&
            let (y,zs) = dest_pto atom in 
            Sl_term.Set.mem y cvalued && Blist.exists (Sl_term.equal v) zs) 
          h in
      let rec aux cvalued rest =
        let new_cvalued = Sl_term.Set.filter (is_cvalued cvalued) rest in
        if Sl_term.Set.is_empty new_cvalued 
        then
          Sl_term.Set.is_empty rest 
        else
          aux 
            (Sl_term.Set.union cvalued new_cvalued) 
            (Sl_term.Set.diff rest new_cvalued) in
      aux freevars existvars
    
    let subst_tags tagpairs f =
      map_atoms
        (fun g -> 
          if is_pred g then 
            mk_pred (Sl_tpred.subst_tag tagpairs (dest_pred g))
          else
            g
        )
        f
    
    (* let norm f =                                       *)
    (*   let uf = _build_uf f in                          *)
    (*   let rec loop f =                                 *)
    (*     match f.node with                              *)
    (*     | True | Emp -> f                              *)
    (*     | Eq p -> mk_eq (Sl_tpair.order p)             *)
    (*     | Deq p -> mk_deq (Sl_tpair.order p)           *)
    (*     | Pto pto -> mk_pto (Sl_pto.norm uf pto)       *)
    (*     | Pred pred -> mk_pred (Sl_tpred.norm uf pred) *)
    (*     | Star(f, g) -> mk_star (loop f) (loop g)      *)
    (*     | Or(f, g) -> mk_or (loop f) (loop g)          *)
    (*     | Exists(x, f) -> mk_exists x (loop f)         *)
    (*   in                                               *)
    (*   loop f                                           *)
  end


module type CommonInterface = 
  sig
    include Util.BasicType
    val parse : (t, 'a) MParser.parser
    val of_string : string -> t
    
    val mk_true : unit -> t
    val mk_emp : unit -> t
    val mk_eq : Sl_tpair.t -> t
    val mk_deq : Sl_tpair.t -> t
    val mk_pto : Sl_pto.t -> t 
    val mk_pred  : Sl_tpred.t -> t
  
    val is_true : t -> bool
    val is_emp : t -> bool
    val is_eq : t -> bool
    val is_deq : t -> bool
    val is_pto : t -> bool  
    val is_pred  : t -> bool
  
    val dest_true : t -> unit
    val dest_emp : t -> unit
    val dest_eq : t -> Sl_tpair.t
    val dest_deq : t -> Sl_tpair.t
    val dest_pto : t -> Sl_pto.t
    val dest_pred  : t -> Sl_tpred.t
    
    val terms : t -> Sl_term.Set.t
    val free_vars : t -> Sl_term.Set.t
    val subst : Sl_subst.t -> t -> t
    val tags : t -> Tags.t
    val tag_pairs : t -> Util.TagPairs.t
  end
    