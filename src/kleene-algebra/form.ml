open Lib
open MParser
open MParser_RE

type t =
| Zero
| One
| Letter of Char.t
| Choice of t * t
| Concat of t * t
| Star of t

let hash = Hashtbl.hash

let ord =
  function
  | Zero ->
    0
  | One ->
    1
  | Letter _ ->
    2
  | Choice (_,_) ->
    3
  | Concat (_,_) ->
    4
  | Star _ ->
    5

let rec compare f f' =
  match f, f' with
  | Zero, Zero
  | One, One ->
    0
  | Letter a, Letter b ->
    Char.compare a b
  | Choice (e1, f1), Choice (e2, f2)
  | Concat (e1, f1), Concat (e2, f2) ->
    let result = compare e1 e2 in
    if Int.equal result 0 then
      compare f1 f2
    else
      result
  | Star e, Star f ->
    compare e f
  | _,_ ->
    (ord f') - (ord f)

let equal f f' =
  Int.equal (compare f f') 0

let rec partition =
  function
  | Choice (e, f) ->
    List.append (partition e) (partition f)
  | _ as f ->
    [f]

let rec factorise =
  function
  | Concat (e, f) ->
    List.append (factorise e) (factorise f)
  | _ as f ->
    [f]

let pp fmt =
  let rec pp top fmt =
    function
    | Zero ->
      Format.fprintf fmt "0"
    | One ->
      Format.fprintf fmt "1"
    | Letter a ->
      Format.fprintf fmt "%c" a
    | Choice (_, _) as f ->
      let printer =
        if top then Format.fprintf fmt "%a"
               else Format.fprintf fmt "(%a)" in
      printer
        (Blist.pp (fun fmt () -> Format.pp_print_string fmt " + ") (pp false))
        (partition f)
    | Concat (_, _) as f ->
      let printer =
        if top then Format.fprintf fmt "%a"
               else Format.fprintf fmt "(%a)" in
      printer
        (Blist.pp (fun fmt () -> ()) (pp false))
        (factorise f)
    | Star (Zero as f)
    | Star (One as f)
    | Star f ->
      Format.fprintf fmt "%a*" (pp false) f in
  pp true fmt

let to_string f = mk_to_string pp f

let letter_valid c =
  (Char.compare 'a' c <= 0) && (Char.compare c 'z' <= 0)

let zero = Zero
let one = One

let letter c =
  Letter c

let choice e f =
  Choice (e, f)
let concat e f =
  Concat (e, f)
let star e =
  Star e

let rec either =
  function
  | [] ->
    Zero
  | [e] ->
    e
  | e::es ->
    Choice (e, either es)

let rec concatenate =
  function
  | [] ->
    One
  | [e] ->
    e
  | e::es ->
    Concat(e, concatenate es)

let parse_letter st = (
    spaces >> any_char >>= (fun c ->
    if (letter_valid c) then return c else fail "Invalid letter")
  ) st

let rec parse_aux st = (
    ((Tokens.symbol "0" >>
        ((attempt (Symbols.(parse_symb symb_star) >>$ (Star Zero)))
          <|>
         (return Zero)))
      <|>
    (Tokens.symbol "1" >>
        ((attempt (Symbols.(parse_symb symb_star) >>$ (Star One)))
          <|>
         (return One)))
      <|>
    (attempt
        (parse_letter >>= (fun c ->
        (attempt (Symbols.(parse_symb symb_star) >>$ Star (Letter c)))
          <|>
        (return (Letter c)))) <?> "Letter")
      <|>
    (Tokens.parens parse >>= (fun f ->
      (attempt Symbols.(parse_symb symb_star) >>$ (Star f))
        <|>
      (return f))))
    << spaces
  ) st
and parse st = (
    attempt (sep_by1 parse_aux (Tokens.symbol "+") >>= (function
        | _::_::_ as es -> return (either es)
        | _ -> fail "Require at least two disjuncts"))
      <|>
    (many1 parse_aux |>> concatenate)
  ) st

let of_string = mk_of_string (parse << eof)

module Operators =
struct
    let ( <.> ) = concat
    let ( <+> ) = choice
end

(* Destructors *)

let dest_letter =
  function
  | Letter c ->
    Some c
  | _ ->
    None

let dest_choice =
  function
  | Choice (e, f) ->
    Some (e, f)
  | _ ->
    None

let dest_concat =
  function
  | Concat (e, f) ->
    Some (e, f)
  | _ ->
    None

let dest_star =
  function
  | Star e ->
    Some e
  | _ ->
    None

(* Predicates *)

let is_zero =
  function
  | Zero ->
    true
  | _ ->
    false

let is_one =
  function
  | One ->
    true
  | _ ->
    false

let is_letter =
  function
  | Letter _ ->
    true
  | _ ->
    false

let is_choice =
  function
  | Choice (_,_) ->
    true
  | _ ->
    false

let is_concat =
  function
  | Concat (_,_) ->
    true
  | _ ->
    false

let is_star =
  function
  | Star _ ->
    true
  | _ ->
    false

let is_atom =
  function
  | Zero
  | One
  | Letter _ ->
    true
  | _ ->
    false

let rec contains_empty =
  function
  | One
  | Star _ ->
    true
  | Choice (e, f) ->
    (contains_empty e) || (contains_empty f)
  | Concat (e, f) ->
    (contains_empty e) && (contains_empty f)
  | _ ->
    false

let rec can_start_with c e =
  if (not (letter_valid c)) then
    invalid_arg __FUNCTION__
  else
    match e with
    | Letter c' when Char.equal c c' ->
      true
    | Choice (e, f) ->
      (can_start_with c e) || (can_start_with c f)
    | Concat (e, f) ->
      (can_start_with c e) || (contains_empty e && can_start_with c f)
    | Star e ->
      can_start_with c e
    | _ ->
      false
