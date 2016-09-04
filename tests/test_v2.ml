open Lib
open Sl_v2

let subst_tests () =
  let f = SymHeap.of_string "x!=y * x=y * x->z" in
  let g = SymHeap.of_string "a!=b * a=b * a->c" in
  assert (not (SymHeap.equal f g)) ;
  let theta = Sl_subst.of_list 
    [ (Sl_term.of_string "x", Sl_term.of_string "a") ; 
      (Sl_term.of_string "y", Sl_term.of_string "b") ;
      (Sl_term.of_string "z", Sl_term.of_string "c")
    ] in
  let f' = SymHeap.subst theta f in
  assert (SymHeap.equal f' g)
  


let unification_tests () = 
  let a, a' = Atom.of_string "a=b", Atom.of_string "x=y" in
  let theta = fst (Option.get (Atom.unify a a')) in
  (* Format.eprintf "%a@." Sl_subst.pp theta ; *)
  assert (Atom.equal_upto_tags (Atom.subst theta a) a') ;      
  
  let f = SymHeap.of_string "x!=y * x=y * x->z * P(y,z)" in
  let g = SymHeap.of_string "a!=b * a=b * a->c * P(b,c)" in
  let theta = fst (Option.get (SymHeap.unify f g)) in
  (* Format.eprintf "%a@." Sl_subst.pp theta  ; *)
  let f' = SymHeap.subst theta f in
  (* Format.eprintf "f' = %a@." SymHeap.pp f'; *)
  (* Format.eprintf " g = %a@." SymHeap.pp g;  *)
  assert (SymHeap.equal_upto_tags f' g)

let () =
  runtest 
    "Tests for new SL core." 
    (fun () ->
      subst_tests () ;
      unification_tests () ;
      let f = SymHeap.of_string "x!=y * x=y * x->z * P(y,z)" in
      print_endline (SymHeap.to_string f) ; 
      let g = SymHeap.of_string "a!=b * a=b * a->c * P(b,c)" in
      Format.eprintf "%a@." (Option.pp Sl_unifier.pp_state) (SymHeap.unify f g) ;
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


