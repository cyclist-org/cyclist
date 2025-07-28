open KleeneAlg
open KleeneAlg.Seq

(* Helper functions *)
let seq_of_letters es fs =
  of_lists (List.map Form.letter es, List.map Form.letter fs)

(* remove_at *)

let%test _ =
  let seq = seq_of_letters ['a'] ['b'] in
  try
    let _ = remove_at (-1) seq in
    false
  with Invalid_argument _ ->
    true

let%expect_test _ =
  let seq = seq_of_letters ['a'] ['b'] in
  print_endline (to_string (remove_at 0 seq));
  [%expect{| ⊢ b |}]

let%test _ =
  let seq = seq_of_letters ['a'] ['b'] in
  try
    let _ = remove_at 1 seq in
    false
  with Invalid_argument _ ->
    true

let%expect_test _ =
  let seq = seq_of_letters ['a'] ['b'] in
  print_endline (to_string (remove_at 2 seq));
  [%expect{| a ⊢ |}]

let%test _ =
  let seq = seq_of_letters ['a'] ['b'] in
  try
    let _ = remove_at 3 seq in
    false
  with Invalid_argument _ ->
    true

let%expect_test _ =
  let seq = seq_of_letters ['a';'b'] ['c';'d'] in
  print_endline (to_string (remove_at 0 seq));
  [%expect{| b ⊢ c, d |}]

let%expect_test _ =
  let seq = seq_of_letters ['a';'b'] ['c';'d'] in
  print_endline (to_string (remove_at 1 seq));
  [%expect{| a ⊢ c, d |}]

let%test _ =
  let seq = seq_of_letters ['a';'b'] ['c';'d'] in
  try
    let _ = remove_at 2 seq in
    false
  with Invalid_argument _ ->
    true

let%expect_test _ =
  let seq = seq_of_letters ['a';'b'] ['c';'d'] in
  print_endline (to_string (remove_at 3 seq));
  [%expect{| a, b ⊢ d |}]

let%expect_test _ =
  let seq = seq_of_letters ['a';'b'] ['c';'d'] in
  print_endline (to_string (remove_at 4 seq));
  [%expect{| a, b ⊢ c |}]

let%test _ =
  let seq = seq_of_letters ['a';'b'] ['c';'d'] in
  try
    let _ = remove_at 5 seq in
    false
  with Invalid_argument _ ->
    true

(* remove_many_at *)

let%expect_test _ =
  let seq = seq_of_letters ['a';'b';'c';] ['d';'e';'f';] in
  print_endline (to_string (remove_many_at 0 1 seq));
  [%expect{| b, c ⊢ d, e, f |}]

let%expect_test _ =
  let seq = seq_of_letters ['a';'b';'c';] ['d';'e';'f';] in
  print_endline (to_string (remove_many_at 1 1 seq));
  [%expect{| a, c ⊢ d, e, f |}]

let%expect_test _ =
  let seq = seq_of_letters ['a';'b';'c';] ['d';'e';'f';] in
  print_endline (to_string (remove_many_at 2 1 seq));
  [%expect{| a, b ⊢ d, e, f |}]

let%expect_test _ =
  let seq = seq_of_letters ['a';'b';'c';] ['d';'e';'f';] in
  print_endline (to_string (remove_many_at 3 1 seq));
  [%expect{| a, b, c ⊢ e, f |}]

let%expect_test _ =
  let seq = seq_of_letters ['a';'b';'c';] ['d';'e';'f';] in
  print_endline (to_string (remove_many_at 4 1 seq));
  [%expect{| a, b, c ⊢ e, f |}]

let%expect_test _ =
  let seq = seq_of_letters ['a';'b';'c';] ['d';'e';'f';] in
  print_endline (to_string (remove_many_at 5 1 seq));
  [%expect{| a, b, c ⊢ d, f |}]

let%expect_test _ =
  let seq = seq_of_letters ['a';'b';'c';] ['d';'e';'f';] in
  print_endline (to_string (remove_many_at 6 1 seq));
  [%expect{| a, b, c ⊢ d, e |}]

let%expect_test _ =
  let seq = seq_of_letters ['a';'b';'c';] ['d';'e';'f';] in
  print_endline (to_string (remove_many_at 7 1 seq));
  [%expect{| a, b, c ⊢ d, e, f |}]

let%expect_test _ =
  let seq = seq_of_letters ['a';'b';'c';] ['d';'e';'f';] in
  print_endline (to_string (remove_many_at 0 2 seq));
  [%expect{| c ⊢ d, e, f |}]

let%expect_test _ =
  let seq = seq_of_letters ['a';'b';'c';] ['d';'e';'f';] in
  print_endline (to_string (remove_many_at 1 2 seq));
  [%expect{| a ⊢ d, e, f |}]

let%expect_test _ =
  let seq = seq_of_letters ['a';'b';'c';] ['d';'e';'f';] in
  print_endline (to_string (remove_many_at 2 2 seq));
  [%expect{| a, b ⊢ e, f |}]

let%expect_test _ =
  let seq = seq_of_letters ['a';'b';'c';] ['d';'e';'f';] in
  print_endline (to_string (remove_many_at 3 2 seq));
  [%expect{| a, b, c ⊢ f |}]

let%expect_test _ =
  let seq = seq_of_letters ['a';'b';'c';] ['d';'e';'f';] in
  print_endline (to_string (remove_many_at 4 2 seq));
  [%expect{| a, b, c ⊢ f |}]

let%expect_test _ =
  let seq = seq_of_letters ['a';'b';'c';] ['d';'e';'f';] in
  print_endline (to_string (remove_many_at 5 2 seq));
  [%expect{| a, b, c ⊢ d |}]

let%expect_test _ =
  let seq = seq_of_letters ['a';'b';'c';] ['d';'e';'f';] in
  print_endline (to_string (remove_many_at 6 2 seq));
  [%expect{| a, b, c ⊢ d, e |}]

let%expect_test _ =
  let seq = seq_of_letters ['a';'b';'c';] ['d';'e';'f';] in
  print_endline (to_string (remove_many_at 7 2 seq));
  [%expect{| a, b, c ⊢ d, e, f |}]

(* find_index_right *)

let%test _ =
  let open Form in
  let seq = of_lists ([], [one; zero; zero;]) in
  (find_index_right is_one seq) = Some 1

let%test _ =
  let open Form in
  let seq = of_lists ([], [zero; one; zero;]) in
  (find_index_right is_one seq) = Some 2

let%test _ =
  let open Form in
  let seq = of_lists ([], [zero; zero; one;]) in
  (find_index_right is_one seq) = Some 3

let%test _ =
  let open Form in
  let seq = of_lists ([ zero; ], [one; zero; zero;]) in
  (find_index_right is_one seq) = Some 2

let%test _ =
  let open Form in
  let seq = of_lists ([ zero; ], [zero; one; zero;]) in
  (find_index_right is_one seq) = Some 3

let%test _ =
  let open Form in
  let seq = of_lists ([ zero; ], [zero; zero; one;]) in
  (find_index_right is_one seq) = Some 4

let%test _ =
  let open Form in
  let seq = of_lists ([ one; ], [one; zero; zero;]) in
  (find_index_right is_one seq) = Some 2

let%test _ =
  let open Form in
  let seq = of_lists ([ one; ], [zero; one; zero;]) in
  (find_index_right is_one seq) = Some 3

let%test _ =
  let open Form in
  let seq = of_lists ([ one; ], [zero; zero; one;]) in
  (find_index_right is_one seq) = Some 4

(* filter_right *)

let%expect_test _ =
  let result =
    let open Form in
    let _a = letter 'a' in
    let star_a = star _a in
    let seq = of_lists ([], [one; concat _a star_a]) in
    filter_right is_one seq in
  print_endline (to_string result) ;
  [%expect{|  ⊢ 1 |}]

(* drop_right *)

let%expect_test _ =
  let result = drop_right 1 (of_lists ([], [Form.one])) in
  print_endline (to_string result) ;
  [%expect{|  ⊢ |}]
