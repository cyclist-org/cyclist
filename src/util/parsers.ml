open Lib
open MParser
open MParser_PCRE

let try_prefix p continue_with =
  ( (look_ahead p) >> continue_with p )

let expect_before p p' msg st =
  ( (followed_by p' "") >> fail msg
      <|> p ) st

let parse_pair p1 p2 st =
  ( Tokens.parens (
    p1 >>= (fun v1 ->
    spaces >> Tokens.comma >> spaces >>
    p2 |>>
    (fun v2 -> (v1, v2)))) ) st

let parse_list p st =
  ( Tokens.squares (Tokens.semi_sep_end p) ) st
