open KleeneAlg.Form

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
  let parsed = of_string "(a + b + c)" in
  let expected = either [(letter 'a'); (letter 'b'); (letter 'c')] in
  parsed = expected

let%test _ =
  let parsed = of_string "(a*)*" in
  let expected = star (star (letter 'a')) in
  parsed = expected
