open Lib
open Sl_term

let mk str = handle_reply (MParser.parse_string parse str ())

let () =
  runtest 
    "Fresh existential var must not be nil." 
    (fun () -> 
      let x = mk "x" in
      let xprime = fresh_evar (Set.singleton x) in
      assert (not (is_nil xprime))
    )  