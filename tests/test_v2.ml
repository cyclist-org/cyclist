open Lib
open Sl_v2

let () =
  runtest 
    "Tests for new SL core." 
    (fun () ->
      let f = SymHeap.of_string "x!=y * x=y * x->z * P(y,z)" in
      print_endline (SymHeap.to_string f) ; 
      ()
      (* let f =                                                            *)
      (*   of_string                                                        *)
      (*     "(x->z \\/ y->z) * true *  (Ex x . x=y * y->z) \\/ x!=nil" in  *)
      (* (* assert (equal (of_string (to_string f)) f) ; *)                 *)
      (* let g = to_dsh f in                                                *)
      (* assert (is_dsh g) ;                                                *)
      (* iter                                    *)
      (*   (fun (g,p) ->                         *)
      (*     (* print_newline ();             *) *)
      (*     (* print_endline (to_string g) ; *) *)
      (*     (* print_endline "--->";         *) *)
      (*     let g' = zip (g,p) in               *)
      (*     assert (equal f g') ;               *)
      (*     (* print_endline (to_string g') *)  *)
      (*   )                                     *)
      (*   f                                     *)
    )


