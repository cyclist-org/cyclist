open Lib
open Sl_term

let () =
  runtest 
    "Empty set of variables must not raise exception in fresh_evar." 
    (fun () -> 
      let _ = fresh_evar (Set.empty) in ()
    )  