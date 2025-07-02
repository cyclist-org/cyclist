open Lib
open MParser
open MParser_RE

type t =
| Atom of string
| NegAtom of string
| Conj of t  * t
| Disj of t * t
| Next of t
| Always of t
| Eventually of t

let hash = Hashtbl.hash

let ord =
  function
  | Atom _ ->
    0
  | NegAtom _ ->
    1
  | Conj (_,_) ->
    2
  | Disj (_,_) ->
    3
  | Next _ ->
    4
  | Always _ ->
    5
  | Eventually _ ->
    6

let rec compare f f' =
  match f, f' with
  | Atom s, Atom s'
  | NegAtom s, NegAtom s' ->
    String.compare s s'
  | Conj (f1, f2), Conj (f1', f2')
  | Disj (f1, f2), Disj (f1', f2') ->
    let r = compare f1 f1' in
    if (Int.equal r 0)
      then compare f2 f2'
      else r
  | Next f, Next f'
  | Always f, Always f'
  | Eventually f, Eventually f' ->
    compare f f'
  | _, _ ->
    (ord f') - (ord f)

let equal f f' =
  Int.equal (compare f f') 0

let rec pp fmt =
  function
  | Atom s ->
    Format.fprintf fmt "%s" s
  | NegAtom s ->
    Format.fprintf fmt "¬%s" s
  | Conj (f1, f2) ->
    Format.fprintf fmt "(%a ∧ %a)" pp f1 pp f2
  | Disj (f1, f2) ->
    Format.fprintf fmt "(%a ∨ %a)" pp f1 pp f2
  | Next f ->
    Format.fprintf fmt "◯ %a" pp f
  | Always f ->
    Format.fprintf fmt "□ %a" pp f
  | Eventually f ->
    Format.fprintf fmt "◇ %a" pp f

let to_string f = mk_to_string pp f

let rec parse st =
  (spaces >> (
   attempt (parse_ident >>= (fun s -> return (Atom s)))
      <|>
    attempt (Tokens.symbol "¬" >> parse_ident >>= (fun s -> return (NegAtom s)))
      <|>
    attempt (Tokens.symbol "◯" >> parse >>= (fun f -> return (Next f)))
      <|>
    attempt (Tokens.symbol "□" >> parse >>= (fun f -> return (Always f)))
      <|>
    attempt (Tokens.symbol "◇" >> parse >>= (fun f -> return (Eventually f)))
      <|>
    Tokens.parens (
      parse >>= (fun f1 ->
      attempt (Tokens.symbol "∧" >> parse >>= (fun f2 -> return (Conj (f1, f2))))
        <|>
      (Tokens.symbol "∨" >> parse >>= (fun f2 -> return (Disj (f1, f2)))))
    ))
  ) st

(* Constructors *)

let mk_atom a =
  Atom a
let mk_negatom a =
  NegAtom a
let mk_disj (f, g) =
  Disj (f, g)
let mk_conj (f, g) =
  Conj (f, g)
let mk_next f =
  Next f
let mk_eventually f =
  Eventually f
let mk_always f =
  Always f

module Operators =
struct
  let at = mk_atom
  let nxt = mk_next
  let ev = mk_eventually
  let alw = mk_always
  let ( || ) f f' = mk_disj (f, f')
  let ( && ) f f' = mk_conj (f, f')
  let _X = mk_next
  let _F = mk_eventually
  let _G = mk_always
end

(* Destructors *)

let dest_atom =
  function
  | Atom s ->
    Option.some s
  | _ ->
    None

let dest_negatom =
  function
  | NegAtom s ->
    Option.some s
  | _ ->
    None

let dest_disj =
  function
  | Disj (f, f') ->
    Some (f, f')
  | _ ->
    None

let dest_conj =
  function
  | Conj (f, f') ->
    Some (f, f')
  | _ ->
    None

let dest_next =
  function
  | Next f ->
    Some f
  | _ ->
    None

let dest_eventually =
  function
  | Eventually f ->
    Some f
  | _ ->
    None

let dest_always =
  function
  | Always f ->
    Some f
  | _ ->
    None

(* Operations *)

let rec neg =
  function
  | Atom p ->
    NegAtom p
  | NegAtom p ->
    Atom p
  | Conj (f, f') ->
    Disj (neg f, neg f')
  | Disj (f, f') ->
    Conj (neg f, neg f')
  | Next f ->
    Next (neg f)
  | Always f ->
    Eventually (neg f)
  | Eventually f ->
    Always (neg f)

(* Predicates *)

let is_disj =
  function
  | Disj (_,_) ->
    true
  | _ ->
    false

let is_conj =
  function
  | Conj (_,_) ->
    true
  | _ ->
    false

let is_next =
  function
  | Next _ ->
    true
  | _ ->
    false

let is_eventually =
  function
  | Eventually _ ->
    true
  | _ ->
    false

let is_always =
  function
  | Always _ ->
    true
  | _ ->
    false

let is_traceable =
  function
  | Always _
  | Next (Always _) ->
    true
  | _ ->
    false