open Generic
open Ltl
open   Form

module Proof = Proof.Make(Seq)(Lib.Strng)
module Prover = Prover.Make(Seq)(Lib.Strng)
module Frontend = Frontend.Make(Seq)(Lib.Strng)

let () =
  Tags.alphabet := Lib.VarManager.arabic_digits

let prove = Frontend.prove_seq !Rules.axioms !Rules.rules

let run_test seq =
  let () = Format.printf "Running test: %a @?" Seq.pp seq in
  let (_, res, _, depth) = prove seq in
  match res with
  | `NOT_FOUND ->
    Format.printf "(Not proved)@.Search depth was %i@." depth
  | `SUCCESS prf ->
    Format.printf "(Proved)@." ;
    Frontend.Prover.pp_proof_stats Format.std_formatter prf ;
    Format.printf "Search depth was %i@." depth
  | `TIMEOUT ->
    Format.printf "(Timed out)@.Last search depth was %i@." depth

(* Define some atoms, for convenience *)
let p = mk_atom "p"
let not_p = mk_negatom "p"

;;

Rules.use_cut false

;;

open Operators

;;

(* Theorem *)
run_test
  (Seq.singleton
    (p || not_p))

;;

(* Non-theorem *)
run_test
  (Seq.singleton
    (p && not_p))

;;

(* Theorem *)
run_test
  (Seq.singleton
    (not_p || ev (p && nxt not_p) || alw p))

;;

(* non-theorem *)
run_test
  (Seq.singleton
    (ev (alw p) || ev (alw not_p)))

;;

(* Theorem *)
run_test
  (Seq.singleton
    (ev (alw p) || ev (alw not_p) || (alw (ev p) && alw (ev not_p))))

;;

(* Theorem *)
run_test
  (Seq.singleton
    (ev (alw p) || alw (ev not_p)))

;;

(* Theorem *)
run_test
  (Seq.singleton
    (ev (alw p) || ev (alw not_p) || alw (ev p)))

;;

(* Theorem *)
run_test
  (Seq.singleton
    (ev p || alw not_p))

;;

(* Theorem *)
run_test
  (Seq.singleton
    (ev (alw p) || ev (alw not_p) || alw ((ev p) && alw (ev not_p))))

    (* ;;

run_test
  (Seq.singleton
    ()) *)

