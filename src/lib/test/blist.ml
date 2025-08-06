open Lib.Blist

(* Auxiliary functions for output *)

let pp_print_list pp fmt =
  Format.fprintf fmt "[%a]"
    (Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.pp_print_string fmt "; ")
        pp)

let print_pair pp_fst pp_snd fmt (fst, snd) =
  Format.fprintf fmt "(%a, %a)"
    pp_fst fst
    pp_snd snd

let print_int_list_pair_list =
  pp_print_list
    (print_pair
      (pp_print_list Format.pp_print_int)
      (pp_print_list Format.pp_print_int))

let print_int_list_list =
  pp_print_list (pp_print_list Format.pp_print_int)

let output pp res =
  print_endline
    (Format.asprintf "%a" pp res)

(* Test sublist_index *)

let%test _ =
  (sublist_index [] []) = Some 0

let%test _ =
  (sublist_index [] [ 0 ]) = Some 0

let%test _ =
  (sublist_index [] [ 0; 1; 2; 3 ]) = Some 0

let%test _ =
  (sublist_index [ 0; 1; 2; 3 ] [ 0; 1; 2; 3 ]) = Some 0

let%test _ =
  (sublist_index [ 1; 2; 3 ] [ 0; 1; 2; 3 ]) = Some 1

let%test _ =
  (sublist_index [ 2; 3 ] [ 0; 1; 2; 3 ]) = Some 2

let%test _ =
  (sublist_index [ 3 ] [ 0; 1; 2; 3 ]) = Some 3

let%test _ =
  (sublist_index [ 0 ] [ 0; 1; 2; 3 ]) = Some 0

let%test _ =
  (sublist_index [ 1 ] [ 0; 1; 2; 3 ]) = Some 1

let%test _ =
  (sublist_index [ 2 ] [ 0; 1; 2; 3 ]) = Some 2

let%test _ =
  (sublist_index [ 0 ] [ 0; 1; 1; 0 ]) = Some 0

let%test _ =
  (sublist_index [ 1 ] [ 0; 1; 1; 0 ]) = Some 1

let%test _ =
  (sublist_index [ 1 ] [ 0; 1; 1; 1; 0 ]) = Some 1

let%test _ =
  (sublist_index [ 1; 1 ] [ 0; 1; 1; 0 ]) = Some 1

let%test _ =
  (sublist_index [ 1; 1 ] [ 0; 1; 1; 1; 0 ]) = Some 1

let%test _ =
  (sublist_index [ 1; 1 ] [ 0; 1; 1; 1; 1; 0 ]) = Some 1

(* Test sublist_last_index *)

let%test _ =
  (sublist_last_index [] []) = Some 0

let%test _ =
  (sublist_last_index [] [ 0 ]) = Some 1

let%test _ =
  (sublist_last_index [] [ 0; 1; 2; 3 ]) = Some 4

let%test _ =
  (sublist_last_index [ 0; 1; 2; 3 ] [ 0; 1; 2; 3 ]) = Some 0

let%test _ =
  (sublist_last_index [ 1; 2; 3 ] [ 0; 1; 2; 3 ]) = Some 1

let%test _ =
  (sublist_last_index [ 2; 3 ] [ 0; 1; 2; 3 ]) = Some 2

let%test _ =
  (sublist_last_index [ 3 ] [ 0; 1; 2; 3 ]) = Some 3

let%test _ =
  (sublist_last_index [ 0 ] [ 0; 1; 2; 3 ]) = Some 0

let%test _ =
  (sublist_last_index [ 1 ] [ 0; 1; 2; 3 ]) = Some 1

let%test _ =
  (sublist_last_index [ 2 ] [ 0; 1; 2; 3 ]) = Some 2

let%test _ =
  (sublist_last_index [ 0 ] [ 0; 1; 1; 0 ]) = Some 3

let%test _ =
  (sublist_last_index [ 1 ] [ 0; 1; 1; 0 ]) = Some 2

let%test _ =
  (sublist_last_index [ 1 ] [ 0; 1; 1; 1; 0 ]) = Some 3

let%test _ =
  (sublist_last_index [ 1; 1 ] [ 0; 1; 1; 0 ]) = Some 1

let%test _ =
  (sublist_last_index [ 1; 1 ] [ 0; 1; 1; 1; 0 ]) = Some 2

let%test _ =
  (sublist_last_index [ 1; 1 ] [ 0; 1; 1; 1; 1; 0 ]) = Some 3

(* Test all_splits *)

let%test _ =
  (all_splits []) = [([], [])]

let%test _ =
  (all_splits [ 0 ]) = [([], [0]); ([0], [])]

let%test _ =
  (all_splits [ 0; 1; ]) = [([], [0; 1]); ([0], [1]); ([0; 1], [])]

let%test _ =
  (all_splits [ 0; 1; 2; ]) =
    [([], [0; 1; 2]); ([0], [1; 2]); ([0; 1], [2]); ([0; 1; 2], [])]

let%test _ =
  (all_splits ~include_empty:false []) = []

let%test _ =
  (all_splits ~include_empty:false [ 0 ]) = []

let%test _ =
   (all_splits ~include_empty:false [ 0; 1; ]) = [([0], [1])]

let%test _ =
  (all_splits ~include_empty:false [ 0; 1; 2; ]) = [([0], [1; 2]); ([0; 1], [2])]

let%test _ =
  (all_splits ~include_empty:false [ 0; 1; 2; 3; ]) =
    [([0], [1; 2; 3]); ([0; 1], [2; 3]); ([0; 1; 2], [3])]

(* Test all_combs *)

let%test _ =
  (all_combs []) = []

let%test _ =
  (all_combs ~include_empty:true []) = [[]]

let%test _ =
  (all_combs [1]) = []

let%test _ =
  (all_combs ~include_empty:true [1]) = [[]]

let%test _ =
  (all_combs [1; 2]) = [[1]; [2]]

let%test _ =
  (all_combs ~include_empty:true [1; 2]) = [[]; [1]; [2]]

let%test _ =
  (all_combs [1; 2; 3]) = [[1]; [2]; [3]; [1; 2]; [1; 3]; [2; 3]]

let%test _ =
  (all_combs ~include_empty:true [1; 2; 3]) =
    [[]; [1]; [2]; [3]; [1; 2]; [1; 3]; [2; 3]]

let%test _ =
  (all_combs [1; 2; 3; 4]) =
    [[1]; [2]; [3]; [4];
     [1; 2]; [1; 3]; [1; 4]; [2; 3]; [2; 4]; [3; 4];
     [1; 2; 3]; [1; 2; 4]; [1; 3; 4]; [2; 3; 4]]

let%test _ =
  (all_combs ~include_empty:true [1; 2; 3; 4]) =
    [[];
     [1]; [2]; [3]; [4];
     [1; 2]; [1; 3]; [1; 4]; [2; 3]; [2; 4]; [3; 4];
     [1; 2; 3]; [1; 2; 4]; [1; 3; 4]; [2; 3; 4]]
