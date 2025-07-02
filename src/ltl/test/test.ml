open Generic
open Ltl
open   Form

module Prover = Prover.Make(Seq)
module Frontend = Frontend.Make(Prover)

let () =
  Tags.alphabet := Lib.VarManager.arabic_digits

let prove = Frontend.idfs !Rules.axioms !Rules.rules

let run_test seq =
  let () = Format.printf "Running test: %a " Seq.pp seq in
  begin match (prove seq) with
  | None ->
    Format.printf "(Not proved)@."
  | Some prf ->
    Format.printf "(Proved)@.%a" Prover.Proof.pp prf
  end

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

(* ;;

run_test
  (Seq.singleton
    ()) *)

