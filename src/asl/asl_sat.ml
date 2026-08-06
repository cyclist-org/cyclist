open Printf

let z3_path = ref "z3"
let timeout = ref 2
let times = ref 0

let check_satisfiability str =
  let () = times := !times + 1 in
  let cmd = sprintf "%s -in -T:%i" !z3_path !timeout in
  let stdout, stdin = Unix.open_process cmd in
  output_string stdin str;
  close_out stdin;
  let output = input_line stdout in
  Stdlib.ignore (Unix.close_process (stdout, stdin));
  output

let is_sat s =
  try match check_satisfiability s with "sat" -> true | _ -> false
  with _ -> false

let is_unsat s =
  try match check_satisfiability s with "unsat" -> true | _ -> false
  with _ -> false
