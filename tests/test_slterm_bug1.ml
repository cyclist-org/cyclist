open Lib
open Sl_term
open Test

let mk str = handle_reply (MParser.parse_string parse str ())


let () =
  run 
    "fresh existential is existential" 
    (fun () -> 
      let xprime = fresh_evar (Set.empty) in
      assert (is_exist_var xprime)
    )  