open Lib
open Symbols
open MParser
open Fun
open Generic
open Seplog

type t =
  | Atom of Sl_heap_rho.t * int
  | Next of t * int
  | G of Tags.elt * t * int
  | F of t * int
  | And of t list * int
  | Or of t list * int

let depth = function
  | Atom(_,d) | Next(_,d) | G(_,_,d) | F(_,d) | And(_,d) | Or(_,d) -> d

let hash = Hashtbl.hash

let is_atom = function
  | Atom _ -> true
  | _ -> false
let is_next = function
  | Next _ -> true
  | _ -> false
let is_g = function
  | G _ -> true
  | _ -> false
let is_f = function
  | F _ -> true
  | _ -> false
let is_and = function
  | And _ -> true
  | _ -> false
let is_or = function
  | Or _ -> true
  | _ -> false

let dest_atom = function
  | Atom(x,_) -> x
  | _ -> invalid_arg "dest_atom"
let dest_next = function
  | Next(x,_) -> x
  | _ -> invalid_arg "dest_next"
let dest_g = function
  | G(ti,x,_) -> (ti,x)
  | _ -> invalid_arg "dest_g"
let dest_f = function
  | F(x,_) -> x
  | _ -> invalid_arg "dest_ef"
let dest_and = function
  | And(x,_) -> x
  | _ -> invalid_arg "dest_and"
let dest_or = function
  | Or(x,_) -> x
  | _ -> invalid_arg "dest_or"

let _mk_atom a n = Atom(a, n)
let _mk_next f n = Next(f, n)
let _mk_g t f n = G(t, f, n)
let _mk_f f n = F(f, n)
let _mk_and fs n = And(fs, n)
let _mk_or fs n =
  Or(fs, n)

let mk_atom a = _mk_atom a 1
let mk_next f = _mk_next f (1 + depth f)
let mk_g t f = _mk_g t f (1 + depth f)
let mk_f f = _mk_f f (1 + depth f)
let mk_and fs =
  _mk_and fs (1 + (List.fold_left (fun d f -> max (depth f) d) 0 fs))
let mk_or fs =
  _mk_or fs (1 + (List.fold_left (fun d f -> max (depth f) d) 0 fs))

let unfold_g f = match f with
  | G(_,inner,_) -> (inner,mk_next f)
  | _ -> invalid_arg "unfold_g"

let unfold_f f = match f with
  | F(inner,_) -> (inner, mk_next f)
  | _ -> invalid_arg "unfold_f"

let unfold_or f = match f with
  | Or(fs,i) -> begin match (List.length fs) with
    | 0
    | 1 -> invalid_arg "unfold_or"
    | 2 -> (List.hd fs, List.hd (List.tl fs))
    | _ -> (List.hd fs, mk_or (List.tl fs))
    end
  | _ -> invalid_arg "unfold_or"

let unfold_and f = match f with
  | And(fs,i) -> begin match (List.length fs) with
      | 0
      | 1 -> invalid_arg "unfold_or"
      | 2 -> (List.hd fs, List.hd (List.tl fs))
      | _ -> (List.hd fs, mk_and (List.tl fs))
      end
  | _ -> invalid_arg "unfold_or"

let rec fold f acc g = match g with
  | Atom _ -> f g acc
  | Next(h,_) | G(_,h,_) | F(h,_) -> f g (f h acc)
  | And(fs,_) | Or(fs,_) ->
      f g (List.fold_left (fun acc h -> fold f acc h) acc fs)

let for_all f g =
  fold (fun g' acc -> acc && f g') true g

let exists f g =
  fold (fun g' acc -> acc || f g') false g

let rec _to_string f =
  (if is_and f || is_or f then bracket else id) (to_string f)
and to_string = function
  | Atom(a,_) -> Sl_heap_rho.to_string a
  | Next(f,_) -> "X" ^ (_to_string f)
  | G(tag, f, _) -> "G_" ^ (Tags.Elt.to_string tag) ^ (_to_string f)
  | F(f, _) -> "F" ^ (_to_string f)
  | And(fs,_) ->
Blist.to_string
  " & "
  (fun g -> (if is_or g then bracket else id) (to_string g))
  fs
  | Or(fs,_) ->
Blist.to_string " | " to_string fs

(* let rec to_melt f =
  match f with
  | Atom(a,_) -> Sl_heap_rho.to_melt a
  | Next(f,_) -> Latex.concat [symb_next.melt; to_melt f]
  | G(_,f,_) -> Latex.concat [symb_g.melt; to_melt f]
  | F(f,_) -> Latex.concat [symb_f.melt; to_melt f]
  | And(fs,_) -> Latex.concat (Latex.list_insert symb_and.melt (Blist.map to_melt fs))
  | Or(fs,_) -> Latex.concat (Latex.list_insert symb_or.melt (Blist.map to_melt fs)) *)

let pp fmt f = Format.fprintf fmt "@[%s@]" (to_string f)

(* atoms must be minimal to be considered first after unfolding *)
let rec compare f g =
  let r = Int.compare (depth f) (depth g) in
  if r<>0 then r else
match (f,g) with
| Atom(a, _), Atom(b, _) -> Sl_heap_rho.compare a b
| Next(a,_), Next(b,_)
| F(a,_), F(b,_)-> compare a b
| G (atag, a, _), G(btag, b, _) ->
  begin
    match Tags.Elt.compare atag btag with
    | 0 -> compare a b
    | n -> n
  end
| And(a,_), And(b,_) | Or(a,_), Or(b,_) ->
    let rec compare_list al bl =
      begin
        match (al,bl) with
        | ([], []) -> 0
        | ([], _) -> -1
        | (_, []) -> 1
        | (hd::tl, hd'::tl') ->
            begin
        match compare hd hd' with
        | 0 -> compare_list tl tl'
        | n -> n
            end
      end
    in compare_list a b
| Atom _, _ | _, Or _-> -1
| Or _, _ | _, Atom _ -> 1
| _, _ -> 1

let equal f g = (compare f g)=0

(* atoms must be minimal to be considered first after unfolding *)
let rec compare_upto_tags f g =
  let r = Int.compare (depth f) (depth g) in
  if r<>0 then r else
match (f,g) with
| Atom(a, _), Atom(b, _) -> Sl_heap_rho.compare a b
| Next(a,_), Next(b,_)
| F(a,_), F(b,_)
| G (_, a, _), G(_, b, _) -> compare a b
| And(a,_), And(b,_) | Or(a,_), Or(b,_) ->
    let rec compare_list a b =
      begin
        match (a,b) with
        | ([], []) -> 0
        | ([], _) -> -1
        | (_, []) -> 1
        | (hd::tl, hd'::tl') ->
            begin
        match compare hd hd' with
        | 0 -> compare_list tl tl'
            | n -> n
            end
      end
    in compare_list a b
| Atom _, _ | _, Or _-> -1
| Or _, _ | _, Atom _ -> 1
| _, _ -> 1

let equal_upto_tags f g = (compare_upto_tags f g)=0

let is_slformula f =
  for_all (fun g -> is_atom g || is_or g) f

let is_checkable f =
  match f with
  | Atom _ -> true
  | Or (l,_) -> List.fold_left (fun acc x -> acc || is_atom x) false l
  | _ -> false
  (*exists (fun g -> is_atom g) f *)

(* let rec norm = function *)
(*   (\* flatten consecutive occurences of operators *\) *)
(*   (\* | Diamond(f,_) when is_diamond f ->  *\) *)
(*   (\*    norm (mk_diamond (dest_diamond f)) *\) *)
(*   (\* (\\* | Box(i, f, _) when is_box f && i = fst (dest_box f) -> *\\) *\) *)
(*   (\* (\\*    norm (mk_box i (snd (dest_box f))) *\\) *\) *)
(*   (\* | Box(f, _) when is_box f -> *\) *)
(*   (\*    norm (mk_box (dest_box f)) *\) *)
(*   | And(conj,_) when List.exists is_and conj -> *)
(*    let h = List.find is_and conj in *)
(*    let newconj = FormSet.union (FormSet.remove h conj) (dest_and h) in  *)
(*    norm (mk_and newconj) *)
(*   | Or(disj,_) when FormSet.exists is_or disj -> *)
(*    let h = FormSet.find is_or disj in *)
(*    let newdisj = FormSet.union (FormSet.remove h disj) (dest_or h) in *)
(*    norm (mk_or newdisj) *)

(*   (\* identities about booleans -- unsure if these should be used *\) *)
(*   (\* | Circle(f,_) when is_and f ->                                *\) *)
(*   (\*   norm (mk_and (FormSet.endomap mk_circle (dest_and f)))  *\) *)
(*   (\* | Circle(f,_) when is_or f ->                                 *\) *)
(*   (\*   norm (mk_or (FormSet.endomap mk_circle (dest_or f)))    *\) *)
(*   (\* | Diamond(f,_) when is_or f ->                                *\) *)
(*   (\*   norm (mk_or (FormSet.endomap mk_diamond (dest_or f)))   *\) *)
(*   (\* | Box(i, f, _) when is_and f ->                               *\) *)
(*   (\*   norm (mk_and (FormSet.endomap (mk_box i) (dest_and f))) *\) *)

(*   (\* pass through *\) *)
(*   | Atom _ as a -> a *)
(*   | And(fs,_) -> mk_and (FormSet.endomap norm fs)  *)
(*   | Or(fs,_) -> mk_or (FormSet.endomap norm fs) *)
(*   | Circle(f,_) -> mk_circle (norm f) *)
(*   | Diamond(f,_) -> mk_diamond (norm f) *)
(*   | Box(f, _) -> mk_box (norm f) *)
(*   | AG(tag,f,_) -> mk_ag tag (norm f) *)
(*   | EG(tag,f,_) -> mk_eg tag (norm f) *)
(*   | AF(f,_) -> mk_af (norm f) *)
(*   | EF(f,_) -> mk_ef (norm f) *)

(* include Fixpoint(struct type t = Form.t let equal = equal end) *)

(* let norm f =  *)
(*   let f' = fixpoint norm f in *)
(*   if is_or f' then f' else mk_or (FormSet.singleton f')   *)

let rec to_slformula t =
  match t with
  | Atom(g,_) -> [g]
  | Or(fs,_) -> Blist.flatten (List.map to_slformula fs)
  | And(fs,i) -> invalid_arg ((string_of_int i) ^"-AND- to_slformula" ^ to_string t)
  | Next(fs,i) -> invalid_arg ((string_of_int i) ^"-NEXT- to_slformula" ^ to_string t)
  | G(_,fs,i) -> invalid_arg ((string_of_int i) ^"-G- to_slformula" ^ to_string t)
  | F(fs,i) -> invalid_arg ((string_of_int i) ^"-F- to_slformula" ^ to_string t)

let rec extract_checkable_slformula t =
  match t with
  | Atom(g,_) -> [g]
  | Or(fs,_) -> Blist.flatten (List.map extract_checkable_slformula fs)
  | _ -> []

let rec tags f =
  fold
    begin fun g acc -> match g with
          | Next (f,_)
          | F (f,_) -> tags f
          | G (i,_,_) -> Tags.add i acc
          | Atom _
          | _ -> acc
    end
    Tags.empty
    f

let outermost_tag f =
  match f with
  | Next (G (t,_,_),_)
  | G(t,_,_) -> Tags.singleton t
  | _ -> Tags.empty

let rec subst_tags tagpairs f =
  match f with
  | Atom _ -> f
  | Next (f,d) -> Next (subst_tags tagpairs f, d)
  | F (f,d) -> F (subst_tags tagpairs f, d)
  | And (fs,d) -> And (List.map (subst_tags tagpairs) fs, d)
  | Or (fs,d) -> Or (List.map (subst_tags tagpairs) fs, d)
  | G (tag,f,d) ->
    let (_, tag'') = Tagpairs.find_first (fun (tag',_) -> tag=tag') tagpairs in
    G (tag'',subst_tags tagpairs f, d)


let step tf = match tf with
  | Next (f,_) -> f
  | _ -> let () = debug (fun () -> "step with formula " ^ to_string tf) in
    invalid_arg "step"

let vars f =
  fold
    begin
      fun g acc ->
        match g with
        | Atom(h,_) -> Term.Set.union (Sl_heap_rho.vars h) acc
        | _ -> acc
    end
    Term.Set.empty
    f

let complete_tags avoid f =
let rec _complete_tags avoid f =
  let wrap mk f n avoid =
    let (f, used) = _complete_tags avoid f in
    (mk f n, Tags.union avoid used) in
  let do_list avoid fs =
    let g (fs, avoid) f =
      let (f', used) = _complete_tags avoid f in
      (f::fs, Tags.union avoid used) in
    let (fs, used) = Blist.fold_left g ([], avoid) fs in
    (Blist.rev fs, used) in
  match f with
  | Atom (h, n) ->
    let h = Sl_heap_rho.complete_tags avoid h in
    let used = Tags.union avoid (Sl_heap_rho.tags h) in
    (Atom (h, n), used)
  | Next (f, n) ->
    wrap _mk_next f n avoid
  | F (f, n) ->
    wrap _mk_f f n avoid
  | And (fs, n) -> Pair.map_left (Fun.swap _mk_and n) (do_list avoid fs)
  | Or (fs, n) -> Pair.map_left (Fun.swap _mk_or n) (do_list avoid fs)
  | G (t, f, n) ->
    let t = if Tags.is_anonymous t then Tags.fresh_fvar avoid else t in
    let (f, used) = _complete_tags (Tags.add t avoid) f in
    (G(t, f, n), used) in
Pair.left (_complete_tags (Tags.union avoid (tags f)) f)

let rec parse_atom st =
  (   attempt (Sl_heap_rho.parse |>> (fun sf -> mk_atom sf))
  <|> attempt (parse_symb symb_next >>
    parse_atom|>> (fun inner -> mk_next inner))
  <|> attempt (parse_symb symb_g >>
    Tokens.parens parse_atom|>> (fun inner ->
    (* TODO: Fix up tags: convert all anonymous tags to fresh non-anonymous *)
    mk_g Tags.anonymous inner))
  <|> attempt (parse_symb symb_f >>
    Tokens.parens parse_atom|>> (fun inner -> mk_f inner))
  <|> attempt (parse_symb symb_or >>
  Tokens.parens (sep_by1 parse_atom (parse_symb symb_comma) |>> fun (atoms) ->
    mk_or atoms))
  <|> attempt (parse_symb symb_and >>
  Tokens.parens (sep_by1 parse_atom (parse_symb symb_comma) |>> fun (atoms) ->
    mk_and atoms))
  ) st

let parse st =
  (sep_by1 parse_atom (parse_symb symb_or) >>= (fun atoms ->
            match atoms with
            | [] -> return (mk_atom Sl_heap_rho.empty)
            | [f] -> return f
            | _ -> return (mk_or atoms)
    <?> "tempform")) st

let of_string s =
  handle_reply (MParser.parse_string parse s ())
