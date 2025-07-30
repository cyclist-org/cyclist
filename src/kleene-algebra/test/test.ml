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
  let () = Format.printf "Running test: %a " Seq.pp seq in
  begin match (prove seq) with
  | None ->
    Format.printf "(Not proved)@."
  | Some prf ->
    (* Format.printf "(Proved)@.%a" Proof.pp prf *)
    Format.printf "(Proved)@."
  end

(* Some atomic formulas *)
let a = Form.letter 'a'
let b = Form.letter 'b'

(* The tests *)

let _ =
  run_test
    (Seq.of_lists (
        [],
        []
      ))

let _ =
  run_test
    (Seq.of_lists (
        [a],
        [a]
      ))

let _ =
  run_test
    (Seq.of_lists (
        [a],
        [b]
      ))

let _ =
  let open Form in
  run_test
    (Seq.of_lists (
        [a],
        [star a]
      ))

let _ =
  let open Form in
  run_test
    (Seq.of_lists (
        [star a],
        [star b]
      ))

let _ =
  let open Form in
  run_test
    (Seq.of_lists (
        [star a],
        [star a; star a]
      ))

let _ =
  let open Form in
  run_test
    (Seq.of_lists (
        [star a],
        [(star a) <.> (star a)]
      ))

let _ =
  let open Form in
  run_test
    (Seq.of_lists (
        [(star a) <.> (star a)],
        [star a]
      ))

let _ =
  let open Form in
  run_test
    (Seq.of_lists (
        [star a],
        [star (star a)]
      ))

let _ =
  let open Form in
  run_test
    (Seq.of_lists (
        [star (star a)],
        [star a]
      ))

let _ =
  let open Form in
  run_test
    (Seq.of_lists (
        [star a; star (star a)],
        [star a]
      ))

let _ =
  let open Form in
  run_test
    (Seq.of_lists (
        [star a],
        [star (a <+> b)]
      ))

let _ =
  let open Form in
  run_test
    (Seq.of_lists (
        [star b],
        [star (a <+> b)]
      ))

let _ =
  let open Form in
  run_test
    (Seq.of_lists (
        [star (a <+> b)],
        [star a]
      ))

let _ =
  let open Form in
  run_test
    (Seq.of_lists (
        [star (a <+> b)],
        [star a; star b]
      ))

let _ =
  let open Form in
  run_test
    (Seq.of_lists (
        [star a],
        [star (a <.> a); a <.> star (a <.> a)]
      ))

let _ =
  let open Form in
  run_test
    (Seq.of_lists (
        [concat a (star a)],
        [concat (star a) a]
      ))

let _ =
  let open Form in
  run_test
    (Seq.of_lists (
        [star (a <+> b)],
        [(star a) <.> (star (b <.> (star a)))]
      ))

let _ =
  let open Form in
  run_test
    (Seq.of_lists (
        [star a],
        [star (a <.> a); (star (a <.> a)) <.> a]
      ))

let _ =
  let open Form in
  run_test
    (Seq.of_lists (
        [star a],
        [one; (star (a <.> a)) <.> a; a <.> (star (a <.> a)) <.> a]
      ))
