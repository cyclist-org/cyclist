open Hashcons
open Symbols
open MParser

type t = node hash_consed
and node =
  | True
  | Emp
  | Eq of Sl_tpair.t
  | Deq of Sl_tpair.t
  | Pto of Sl_pto.t
  | Pred of Sl_tpred.t
  | Star of t * t
  | Or of t * t
  | Exists of Sl_term.t * t

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


let rec to_string f = 
  let paren p f = 
    let lp,rp = 
      if is_atom f || is_star f || p f then "","" 
      else symb_lp.str,symb_rp.str in
    lp ^ (to_string f) ^ rp in
  match f.node with
  | True -> keyw_true.str
  | Emp -> symb_emp.str
  | Eq(x,y) -> 
    (Sl_term.to_string x) ^ symb_eq.str ^ (Sl_term.to_string y)
  | Deq(x,y) -> 
    (Sl_term.to_string x) ^ symb_deq.str ^ (Sl_term.to_string y)
  | Pto p -> 
    Sl_pto.to_string p
  | Pred p ->
    Sl_tpred.to_string p
  | Exists(x, f) ->
    keyw_exists.str ^ " " ^ (Sl_term.to_string x) ^ symb_dot.str ^ " " ^ 
    paren is_exists f
  | Star(f, g) ->
    (paren (fun _ -> false) f) ^ symb_star.sep ^ (paren (fun _ -> false) g) 
  | Or(f, g) ->
    (paren is_or f) ^ symb_or.sep ^ (paren is_or g) 



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
  let rec atom st = 
    ( attempt (parse_symb symb_lp >> parse << parse_symb symb_rp) <|>
      attempt (parse_symb keyw_true >>$ mk_true ()) <|>
      attempt (parse_symb keyw_emp >>$ mk_emp ()) <|>
      attempt (parse_infix_pair symb_eq |>> mk_eq) <|> 
      attempt (parse_infix_pair symb_deq |>> mk_deq) <|>
      attempt (Sl_pto.parse |>> mk_pto) <|>
              (Sl_tpred.parse |>> mk_pred)                       
    ) st  
  and parse st = expression connectives atom st 
  in  parse

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

let freevars f = _vars true f
(* let vars f = _vars false f *)

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
        let y = Sl_term.fresh_uvar v in
        let f' = subst (Sl_term.singleton_subst x y) f in
        let (f', v') = alphanorm f' (Sl_term.Set.add y v) in
        (mk_exists y f', v')
      else 
        let (f', v') = alphanorm f (Sl_term.Set.add x v) in
        (mk_exists x f', v')
  in
  fst (alphanorm f (freevars f))

let rec dsh_iter fn frm = match frm.node with
  | Or(f, g) -> fn f ; dsh_iter fn g
  | _ -> fn frm

let rec symheap_iter fn frm = match frm.node with
  | Exists(_, g) -> symheap_iter fn g
  | True | Emp | Eq _ | Deq _ | Pto _ | Pred _ -> fn frm 
  | Star(f, g) -> fn f ; symheap_iter fn g
  | _ -> failwith "symheap_iter -- not symheap"

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
    (* whenever x \notin freevars f' *)
    | Star({node=Exists(x, f)}, f') ->
      if Sl_term.Set.mem x (freevars f') then a else 
        mk_exists x (mk_star f f')
    | Star(f, {node=Exists(x, f')}) ->
      if Sl_term.Set.mem x (freevars f) then a else 
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

type path = 
  | Top
  | StarL of path * t
  | StarR of t * path
  | OrL of path * t
  | OrR of t * path
  | ExistsD of Sl_term.t * path

type location = t * path

let change (_, p) f = (f, p)
let rec zip (f, p) = match p with
  | Top -> f
  | StarL(q, r) -> zip (mk_star f r, q)
  | StarR(l, q) -> zip (mk_star l f, q)
  | OrL(q, r) -> zip (mk_or f r, q)
  | OrR(l, q) -> zip (mk_or l f, q)
  | ExistsD(x, q) -> zip (mk_exists x f, q)

let fold f g a =
  let rec aux f ((g, path) as loc) a = 
    match g.node with
      | True | Emp | Eq _ | Deq _ | Pto _ | Pred _ -> f loc a
      | Star(l, r) ->
        aux f (r, StarR(l, path)) (f loc (aux f (l, StarL(path, r)) a))
      | Or(l, r) ->
        aux f (r, OrR(l, path)) (f loc (aux f (l, OrL(path, r)) a)) 
      | Exists(x, g) -> 
        f loc (aux f (g, ExistsD(x, path)) a) 
  in
  aux f (g, Top) a

let iter f g =
  let rec aux f ((g, path) as loc) = 
    match g.node with
      | True | Emp | Eq _ | Deq _ | Pto _ | Pred _ -> f loc 
      | Star(l, r) ->
        aux f (l, StarL(path, r)) ; aux f (r, StarR(l, path)) ; f loc
      | Or(l, r) ->
        aux f (l, OrL(path, r)) ; aux f (r, OrR(l, path)) ; f loc
      | Exists(x, g) -> aux f (g, ExistsD(x, path)) ; f loc 
  in
  aux f (g, Top)


let find p f =
  let rec search pred ((f, p) as loc) = 
    if pred f then Some loc else
    match f.node with
      | True | Emp | Eq _ | Deq _ | Pto _ | Pred _ -> None
      | Star(l, r) ->
        let res = search pred (l, StarL(p, r)) in
        if Option.is_some res then res else
        search pred (r, StarR(l, p))
      | Or(l, r) ->
        let res = search pred (l, OrL(p, r)) in
        if Option.is_some res then res else
        search pred (r, OrR(l, p))
      | Exists(x, g) -> search pred (g, ExistsD(x, p)) 
  in
  search p (f, Top)


let mk str = Lib.handle_reply (MParser.parse_string parse str ());;

let f = mk "(x->z \\/ y->z) * true *  (Ex x . x=y * y->z) \\/ x!=nil";; 

(* print_endline (to_string f);;              *)

(* print_endline (to_string (enforce_vc f));; *)

(* assert (equal (mk (to_string f)) f);;      *)

let g = to_dsh f;;

print_endline (to_string g);;

assert (is_dsh g);;

print_newline ();;

(* iter                                   *)
(*   (fun (g,p) ->                        *)
(*     print_newline ();                  *)
(*     print_endline (to_string g) ;      *)
(*     print_endline "--->";              *)
(*     let g' = zip (g,p) in              *)
(*     assert (equal f g') ;              *)
(*     (* print_endline (to_string g') *) *)
(*   )                                    *)
(*   f;;                                  *)





type 'a formula = t
type dsh
type symheap













