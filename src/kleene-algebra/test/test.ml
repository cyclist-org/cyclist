open Generic
open KleeneAlg
open KleeneAlg.Form.Operators

module Proof = Proof.Make(Seq)(Rules.Infrule)
module Prover = Prover.Make(Seq)(Rules.Infrule)
module Frontend = Frontend.Make(Seq)(Rules.Infrule)

let () =
  Tags.alphabet := Lib.VarManager.greek_alphabet

let prove = Frontend.idfs !Rules.axioms !Rules.rules

let run_test seq =
  let () = Format.printf "Running test: %a " Seq.pp_no_tags seq in
  begin match (prove seq) with
  | None ->
    Format.printf "(Not proved)@."
  | Some prf ->
    (* Format.printf "(Proved)@.%a" Proof.pp prf *)
    Format.printf "(Proved)@."
  end

(* The tests *)

let _ =
  let seq = " ⊢ " in
  run_test (Seq.of_string seq)

let _ =
  let seq = "a ⊢ a" in
  run_test (Seq.of_string seq)

let _ =
  let seq = "a ⊢ b" in
  run_test (Seq.of_string seq)

let _ =
  let seq = "a ⊢ a*" in
  run_test (Seq.of_string seq)

let _ =
  let seq = "a* ⊢ b*" in
  run_test (Seq.of_string seq)

let _ =
  let seq = "a* ⊢ a*, a*" in
  run_test (Seq.of_string seq)

let _ =
  let seq = "a* ⊢ a*a*" in
  run_test (Seq.of_string seq)

let _ =
  let seq = "a*a* ⊢ a*" in
  run_test (Seq.of_string seq)

let _ =
  let seq = "a* ⊢ (a*)*" in
  run_test (Seq.of_string seq)

let _ =
  let seq = "(a*)* ⊢ a*" in
  run_test (Seq.of_string seq)

let _ =
  let seq = "a*(a*)* ⊢ a*" in
  run_test (Seq.of_string seq)

let _ =
  let seq = "a* ⊢ (a+b)*" in
  run_test (Seq.of_string seq)

let _ =
  let seq = "b* ⊢ (a+b)*" in
  run_test (Seq.of_string seq)

let _ =
  let seq = "(a+b)* ⊢ a*" in
  run_test (Seq.of_string seq)

let _ =
  let seq = "(a+b)* ⊢ a*, b*" in
  run_test (Seq.of_string seq)

let _ =
  let seq = "aa* ⊢ a*a" in
  run_test (Seq.of_string seq)

let _ =
  let seq = "(a+b)* ⊢ a*(ba*)*" in
  run_test (Seq.of_string seq)

let _ =
  let seq = "a* ⊢ (aa)*, a(aa)*" in
  run_test (Seq.of_string seq)

let _ =
  let seq = "a* ⊢ (aa)*, (aa)*a" in
  run_test (Seq.of_string seq)

let _ =
  let seq = "a* ⊢ 1, (aa)*a, a(aa)*a" in
  run_test (Seq.of_string seq)
