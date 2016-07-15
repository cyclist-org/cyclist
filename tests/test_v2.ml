let f = mk "(x->z \\/ y->z) * true *  (Ex x . x=y * y->z) \\/ x!=nil";; 

(* print_endline (to_string f);;              *)

(* print_endline (to_string (enforce_vc f));; *)

(* assert (equal (mk (to_string f)) f);;      *)

let g = to_dsh f;;

print_endline (to_string g);;

assert (is_dsh g);;

print_newline ();;

(* iter                                   *)
(*   (fun (g,p) ->                        *)
(*     print_newline ();                  *)
(*     print_endline (to_string g) ;      *)
(*     print_endline "--->";              *)
(*     let g' = zip (g,p) in              *)
(*     assert (equal f g') ;              *)
(*     (* print_endline (to_string g') *) *)
(*   )                                    *)
(*   f;;                                  *)


