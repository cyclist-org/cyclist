open Lib
open MParser
open MParser_RE

type letter = Char.t

let letter_to_string = Format.asprintf "%c"

type t =
| Zero
| One
| Letter of letter
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

let rec pp fmt =
  function
  | Zero ->
    Format.fprintf fmt "0"
  | One ->
    Format.fprintf fmt "1"
  | Letter a ->
    Format.fprintf fmt "%c" a
  | Choice (_, _) as f ->
    Blist.pp (fun fmt () -> Format.pp_print_string fmt " + ") pp fmt
      (partition f)
  | Concat (_, _) as f ->
    Blist.pp (fun fmt () -> ()) bracket_choice fmt
      (factorise f)
  | Star (Concat (_,_) as f)
  | Star (Choice (_,_) as f) ->
    Format.fprintf fmt "(%a)*" pp f
  | Star f ->
    Format.fprintf fmt "%a*" pp f
and bracket_choice fmt =
  function
  | Choice (_, _) as f ->
    Format.fprintf fmt "(%a)" pp f
  | _ as f ->
    pp fmt f

let rec pp_full fmt =
  function
  | Zero ->
    Format.fprintf fmt "0"
  | One ->
    Format.fprintf fmt "1"
  | Letter a ->
    Format.fprintf fmt "%c" a
  | Choice (e, f) ->
    print_binary `Choice fmt e f
  | Concat (e, f) ->
    print_binary `Concat fmt e f
  | Star (Concat (_,_) as f)
  | Star (Choice (_,_) as f) ->
    Format.fprintf fmt "(%a)*" pp f
  | Star f ->
    Format.fprintf fmt "%a*" pp f
and print_binary op fmt e f =
  let print_e =
    match op, e with
    | `Choice, Choice (_,_)
    | `Concat, Concat (_,_)
    | `Concat, Choice (_,_) ->
      fun fmt -> Format.fprintf fmt "(%a)" pp_full
    | _ ->
      pp_full in
  let print_f =
    match op, f with
    | `Choice, Choice (_,_)
    | `Concat, Concat (_,_)
    | `Concat, Choice (_,_) ->
      fun fmt -> Format.fprintf fmt "(%a)" pp_full
    | _ ->
      pp_full in
  let print_op =
    match op with
    | `Choice ->
      fun fmt () -> Format.pp_print_string fmt " + "
    | `Concat ->
      fun fmt () -> () in
  Format.fprintf fmt "%a%a%a"
    print_e e
    print_op ()
    print_f f

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
    if (letter_valid c) then return (Letter c) else fail "Invalid letter")
  ) st

let rec parse_aux st = (
    ((Tokens.symbol "0" >>
        ((attempt (many Symbols.(parse_symb symb_star) |>>
          (List.fold_left (fun f _ -> Star f) Zero)))
            <|>
          (return Zero)))
      <|>
    (Tokens.symbol "1" >>
        ((attempt (many Symbols.(parse_symb symb_star) |>>
          (List.fold_left (fun f _ -> Star f) One)))
            <|>
          (return One)))
      <|>
    (attempt
        (parse_letter >>= (fun c ->
        ((attempt (many Symbols.(parse_symb symb_star) |>>
          (List.fold_left (fun f _ -> Star f) c)))
            <|>
          (return c)))) <?> "Letter")
      <|>
    (Tokens.parens parse >>= (fun f ->
        many Symbols.(parse_symb symb_star) |>>
          (List.fold_left (fun f _ -> Star f) f))))
    << spaces
  ) st
and parse_concat st =
  (many1 parse_aux |>> concatenate) st
and parse st = (
    attempt (sep_by1 parse_concat (Tokens.symbol "+") >>= (function
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

let rec contains_zero =
  function
  | Zero ->
    true
  | One | Letter _ ->
    false
  | Choice (e, f) | Concat (e, f) ->
    (contains_zero e) || (contains_zero f)
  | Star e ->
    contains_zero e

let rec matches_empty =
  function
  | One
  | Star _ ->
    true
  | Choice (e, f) ->
    (matches_empty e) || (matches_empty f)
  | Concat (e, f) ->
    (matches_empty e) && (matches_empty f)
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
      (can_start_with c e) || (matches_empty e && can_start_with c f)
    | Star e ->
      can_start_with c e
    | _ ->
      false

let get_alphabet f =
  let rec get_alphabet =
    function
    | Zero
    | One ->
      []
    | Letter c ->
      [c]
    | Concat (e, f)
    | Choice (e, f) ->
      List.append (get_alphabet e) (get_alphabet f)
    | Star e ->
      get_alphabet e in
  List.sort_uniq Char.compare (get_alphabet f)