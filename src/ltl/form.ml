open Lib
open MParser
open MParser_RE

type t =
  | Eventually of t
  | Always of t
  | Next of t
  | Disj of t * t
  | Conj of t * t
  | NegAtom of string
  | Atom of string
[@@deriving compare, equal]

let hash = Hashtbl.hash

(* The symbols are printed with an explicit width of 1, as Format counts bytes
   rather than characters before OCaml 5.4, which would break lines differently
   depending on the compiler version. *)
let pp_sym fmt s = Format.pp_print_as fmt 1 s

let rec pp fmt = function
  | Atom s -> Format.fprintf fmt "%s" s
  | NegAtom s -> Format.fprintf fmt "%a%s" pp_sym "¬" s
  | Conj (f1, f2) -> Format.fprintf fmt "(%a %a %a)" pp f1 pp_sym "∧" pp f2
  | Disj (f1, f2) -> Format.fprintf fmt "(%a %a %a)" pp f1 pp_sym "∨" pp f2
  | Next f -> Format.fprintf fmt "%a %a" pp_sym "◯" pp f
  | Always f -> Format.fprintf fmt "%a %a" pp_sym "□" pp f
  | Eventually f -> Format.fprintf fmt "%a %a" pp_sym "◇" pp f

let to_string f = mk_to_string pp f

let rec parse st =
  (spaces
  >> (attempt (parse_ident >>= fun s -> return (Atom s))
     <|> attempt
           (Tokens.symbol "¬" >> parse_ident >>= fun s -> return (NegAtom s))
     <|> attempt (Tokens.symbol "◯" >> parse >>= fun f -> return (Next f))
     <|> attempt (Tokens.symbol "□" >> parse >>= fun f -> return (Always f))
     <|> attempt (Tokens.symbol "◇" >> parse >>= fun f -> return (Eventually f))
     <|> Tokens.parens
           ( parse >>= fun f1 ->
             attempt
               (Tokens.symbol "∧" >> parse >>= fun f2 -> return (Conj (f1, f2)))
             <|> ( Tokens.symbol "∨" >> parse >>= fun f2 ->
                   return (Disj (f1, f2)) ) )))
    st

(* Constructors *)

let atom a = Atom a
let negatom a = NegAtom a
let disj (f, g) = Disj (f, g)
let conj (f, g) = Conj (f, g)
let next f = Next f
let eventually f = Eventually f
let always f = Always f

module Operators = struct
  let at = atom
  let nxt = next
  let ev = eventually
  let alw = always
  let ( || ) f f' = disj (f, f')
  let ( && ) f f' = conj (f, f')
  let _X = next
  let _F = eventually
  let _G = always
end

(* Destructors *)

let dest_atom = function Atom s -> Option.some s | _ -> None
let dest_negatom = function NegAtom s -> Option.some s | _ -> None
let dest_disj = function Disj (f, f') -> Some (f, f') | _ -> None
let dest_conj = function Conj (f, f') -> Some (f, f') | _ -> None
let dest_next = function Next f -> Some f | _ -> None
let dest_eventually = function Eventually f -> Some f | _ -> None
let dest_always = function Always f -> Some f | _ -> None

(* Operations *)

let rec neg = function
  | Atom p -> NegAtom p
  | NegAtom p -> Atom p
  | Conj (f, f') -> Disj (neg f, neg f')
  | Disj (f, f') -> Conj (neg f, neg f')
  | Next f -> Next (neg f)
  | Always f -> Eventually (neg f)
  | Eventually f -> Always (neg f)

(* Predicates *)

let is_disj = function Disj (_, _) -> true | _ -> false
let is_conj = function Conj (_, _) -> true | _ -> false
let is_next = function Next _ -> true | _ -> false
let is_eventually = function Eventually _ -> true | _ -> false
let is_always = function Always _ -> true | _ -> false
let is_traceable = function Always _ | Next (Always _) -> true | _ -> false
