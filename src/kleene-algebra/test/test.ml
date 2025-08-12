open Generic
open KleeneAlg
open KleeneAlg.Form.Operators

module Proof = Proof.Make(Seq)(Rules.Infrule)
module Frontend = Frontend.Make(Seq)(Rules.Infrule)

let () = Frontend.timeout := 10

let prove = Frontend.prove_seq !Rules.axioms !Rules.rules

let run_test seq =
  let () = Format.printf "Running test: %a @?" Seq.pp_no_tags seq in
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

let _ =
  let seq = "b,b,((a+b)(b+c))* ⊢ (a+b)(b+c)((a+b)(b+c))*" in
  run_test (Seq.of_string seq)

let _ =
  let seq = "b,b,((a+b)(d+b))* ⊢ ((a+b)(d+b))*" in
  run_test (Seq.of_string seq)

let _ =
  let seq = "b* ⊢ ((a+b)(b+c))*, b((a+b)(b+c))*" in
  run_test (Seq.of_string seq)

let _ =
  let seq = "b* ⊢ ((a+b)(b+c))*, ((a+b)(b+c))*b" in
  run_test (Seq.of_string seq)

let _ =
  let seq = "b* ⊢ ((a+b)(d+b))*, b((a+b)(d+b))*" in
  run_test (Seq.of_string seq)

let _ =
  let seq = "b* ⊢ ((a+b)(d+b))*, ((a+b)(d+b))*b" in
  run_test (Seq.of_string seq)
