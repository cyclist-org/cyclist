open KleeneAlg.Form

(* Test equal *)

let%test _ =
  equal zero zero

let%test _ =
  equal one one

let%test _ =
  equal (letter 'a') (letter 'a')

let%test _ =
  let a = letter 'a' in
  equal (star a) (star a)

let%test _ =
  let a = letter 'a' in
  let star_a = star a in
  equal (concat a star_a) (concat a star_a)

let%test _ =
  let a = letter 'a' in
  let aa = concat a a in
  let star_aa = star aa in
  not (equal (concat aa star_aa) (concat a star_aa))

(* Test of_string / parse *)

let%expect_test _ =
  let parsed = of_string "a" in
  print_endline (to_string parsed) ;
  [%expect{| a |}]

let%test _ =
  let parsed = of_string "a" in
  let expected = letter 'a' in
  parsed = expected

let%test _ =
  let parsed = of_string "a*" in
  let expected = star (letter 'a') in
  parsed = expected

let%test _ =
  let parsed = of_string "(ab)" in
  let expected = concat (letter 'a') (letter 'b') in
  parsed = expected

let%test _ =
  let parsed = of_string "(ab)*" in
  let expected = star (concat (letter 'a') (letter 'b')) in
  parsed = expected

let%test _ =
  let parsed = of_string "(abc)" in
  let expected = concatenate [(letter 'a'); (letter 'b'); (letter 'c')] in
  parsed = expected

let%test _ =
  let parsed = of_string "(a + b)*" in
  let expected = star (choice (letter 'a') (letter 'b')) in
  parsed = expected

let%test _ =
  let parsed = of_string "(a + b)**" in
  let expected = star (star (choice (letter 'a') (letter 'b'))) in
  parsed = expected

let%test _ =
  let parsed = of_string "a + b + c" in
  let expected = either [(letter 'a'); (letter 'b'); (letter 'c')] in
  parsed = expected

let%test _ =
  let parsed = of_string "(a + b + c)" in
  let expected = either [(letter 'a'); (letter 'b'); (letter 'c')] in
  parsed = expected

let%test _ =
  let parsed = of_string "a**" in
  let expected = star (star (letter 'a')) in
  parsed = expected

let%test _ =
  let parsed = of_string "(a*)*" in
  let expected = star (star (letter 'a')) in
  parsed = expected

let%test _ =
  let parsed = of_string "(a*a)" in
  let expected = concat (star (letter 'a')) (letter 'a') in
  parsed = expected

let%test _ =
  let parsed = of_string "a*a" in
  let expected = concat (star (letter 'a')) (letter 'a') in
  parsed = expected

let%test _ =
  let parsed = of_string "aaa" in
  let expected = concat (letter 'a') (concat (letter 'a') (letter 'a')) in
  parsed = expected

let%test _ =
  let parsed = of_string "a(aa)" in
  let expected = concat (letter 'a') (concat (letter 'a') (letter 'a')) in
  parsed = expected

let%test _ =
  let parsed = of_string "(aa)a" in
  let expected = concat (concat (letter 'a') (letter 'a')) (letter 'a') in
  parsed = expected

let%test _ =
  let parsed = of_string "aa + aa" in
  let expected =
    let aa = concat (letter 'a')  (letter 'a') in
    either [aa; aa] in
  parsed = expected

let%test _ =
  let parsed = of_string "(a + a)a + aa" in
  let expected =
    let open Operators in
    let a = letter 'a' in
    ((a <+> a) <.> a) <+> (a <.> a) in
  parsed = expected

(* Test pp *)

let%expect_test _ =
  let parsed = of_string "a(a + a)" in
  pp Format.std_formatter parsed ;
  [%expect{| a(a + a) |}]

let%expect_test _ =
  let parsed = of_string "(a(a + a))a" in
  pp Format.std_formatter parsed ;
  [%expect{| a(a + a)a |}]

let%expect_test _ =
  let parsed = of_string "a((a + a)a)" in
  pp Format.std_formatter parsed ;
  [%expect{| a(a + a)a |}]

(* Test pp_full *)

let%expect_test _ =
  let parsed = of_string "a(aa)" in
  pp_full Format.std_formatter parsed ;
  [%expect{| a(aa) |}]

let%expect_test _ =
  let parsed = of_string "aaa" in
  pp_full Format.std_formatter parsed ;
  [%expect{| a(aa) |}]

let%expect_test _ =
  let parsed = of_string "(aa)a" in
  pp_full Format.std_formatter parsed ;
  [%expect{| (aa)a |}]

let%expect_test _ =
  let parsed = of_string "aaa + b + b((a**a*)b)a" in
  pp_full Format.std_formatter parsed ;
  [%expect{| a(aa) + (b + b(((a**a*)b)a)) |}]

let%expect_test _ =
  let parsed = of_string "a(a + a)" in
  pp_full Format.std_formatter parsed ;
  [%expect{| a(a + a) |}]

(* Test factorise *)

let%test _ =
  let factorised = factorise (of_string "a(a(bb + (a* + a**)))") in
  let expected =
    let open Operators in
    let a = letter 'a' in
    let b = letter 'b' in
    [a; a; either [b <.> b; star a; star (star a)]] in
  factorised = expected

