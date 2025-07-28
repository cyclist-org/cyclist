open Lib.Blist

(* Auxiliary functions for output *)

let pp_print_list pp fmt =
  Format.fprintf fmt "[%a]"
    (Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
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

let output pp res =
  print_endline
    (Format.asprintf "%a" pp res)

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
  (all_splits ~allow_empty:false []) = []

let%test _ =
  (all_splits ~allow_empty:false [ 0 ]) = []

let%test _ =
   (all_splits ~allow_empty:false [ 0; 1; ]) = [([0], [1])]

let%test _ =
  (all_splits ~allow_empty:false [ 0; 1; 2; ]) = [([0], [1; 2]); ([0; 1], [2])]

let%test _ =
  (all_splits ~allow_empty:false [ 0; 1; 2; 3; ]) =
    [([0], [1; 2; 3]); ([0; 1], [2; 3]); ([0; 1; 2], [3])]

