(* Generates a family of inductive definitions Phi_n for an
 * inductive predicate P_n such that |base(P_n)| \in \Omega(2^n).
 *)

open Lib

let def_separator = " ;\n\n"

let pzero = "zero"

let pone = "one"

let pbool = "bool"

let pand = "and"

let pxor = "xor"

let pnot = "not"

let psucc = "succ"

let pp = "P"

let pq = "Q"

let pbitvector = "bitvector"

let circ_suffix = "circuit"

let rec_suffix = "rec"

(* succ{k}circuit_*)
let psucc_circuit k = psucc ^ string_of_int k ^ circ_suffix

(* succ{k}rec*)
let psucc_rec k = psucc ^ string_of_int k ^ rec_suffix

let pzero_def = pzero ^ " {\n  x=nil => " ^ pzero ^ "(x)\n}"

let pone_def = pone ^ " {\n  x!=nil => " ^ pone ^ "(x)\n}"

let pbool_def =
  pbool ^ " {\n  " ^ pzero ^ "(x) => " ^ pbool ^ "(x) |\n  " ^ pone ^ "(x) => "
  ^ pbool ^ "(x)\n}"

let pand_def =
  pand ^ " {\n  " ^ pzero ^ "(x) * " ^ pzero ^ "(z) => " ^ pand
  ^ "(x,y,z) |\n  " ^ pzero ^ "(y) * " ^ pzero ^ "(z) => " ^ pand
  ^ "(x,y,z) |\n  " ^ pone ^ "(x) * " ^ pone ^ "(y) * " ^ pone ^ "(z) => "
  ^ pand ^ "(x,y,z)\n}"

let pxor_def =
  pxor ^ " {\n  " ^ pzero ^ "(x) * " ^ pzero ^ "(y) * " ^ pzero ^ "(z) => "
  ^ pxor ^ "(x,y,z) |\n  " ^ pzero ^ "(x) * " ^ pone ^ "(y) * " ^ pone
  ^ "(z) => " ^ pxor ^ "(x,y,z) |\n  " ^ pone ^ "(x) * " ^ pzero ^ "(y) * "
  ^ pone ^ "(z) => " ^ pxor ^ "(x,y,z) |\n  " ^ pone ^ "(x) * " ^ pone
  ^ "(y) * " ^ pzero ^ "(z) => " ^ pxor ^ "(x,y,z)\n}"

(* Note that a more succinct representation of xor via e.g. *)
(*   x != y * one(z) => xor(x,y,z) would not behave as desired: *)
(* then also xor(2,3,1) would hold, even though the only z s.t. *)
(* xor(2,3,z) holds should be z = nil. *)

let pnot_def =
  pnot ^ " {\n  " ^ pzero ^ "(x) * " ^ pone ^ "(y) => " ^ pnot ^ "(x,y) |\n  "
  ^ pone ^ "(x) * " ^ pzero ^ "(y) => " ^ pnot ^ "(x,y)\n}"

let circuit_def = Blist.rev [pzero_def; pone_def; pand_def; pxor_def; pnot_def]

(* Helper: [lower..upper] in Haskell. *)
let rec from_to lower upper =
  if Int.( < ) upper lower then [] else lower :: from_to (lower + 1) upper

(* Helper to generate argument lists: E.g. param_m_to_n "x" 2 4 = "x2,x3,x4". *)
let param_m_to_n x m n =
  String.concat ","
    (Blist.map (( ^ ) x) (Blist.map string_of_int (from_to m n)))

(* Helper to generate argument lists: E.g. param_1_to_n "x" 3 = "x1,x2,x3". *)
let param_1_to_n x n = param_m_to_n x 1 n

let condlist_to_string conds = String.concat " * " conds

(* Returns an inductive definition for a predicate succ{n}circuit. *)
let psucc_circuit_def (n : int) : string =
  let psucc_circuit_n = psucc_circuit n in
  let rec conds k =
    if Int.( <= ) k 1 then [pnot ^ "(x1,y1)"]
    else if Int.( = ) k 2 then conds 1 @ [pxor ^ "(x1,x2,y2)"]
    else if Int.( = ) k 3 then
      conds 2 @ [pand ^ "(x1,x2,z3)"; pxor ^ "(z3,x3,y3)"]
    else
      let kp = string_of_int (k - 1) in
      let kk = string_of_int k in
      conds (k - 1)
      @ [ pand ^ "(z" ^ kp ^ ",x" ^ kp ^ ",z" ^ kk ^ ")"
        ; pxor ^ "(x" ^ kk ^ ",y" ^ kk ^ ",z" ^ kk ^ ")" ]
  in
  psucc_circuit_n ^ " {\n  "
  ^ condlist_to_string (conds n)
  ^ " => " ^ psucc_circuit_n
  ^ Lib.bracket (param_1_to_n "x" n ^ "," ^ param_1_to_n "y" n)
  ^ "\n}"

let pp_def n =
  let param_list = param_1_to_n "x" n in
  let first_conds =
    Blist.map (fun i -> pone ^ "(x" ^ string_of_int i ^ ")") (from_to 1 n)
  in
  let last_cond = [pq ^ Lib.bracket param_list] in
  let conds = first_conds @ last_cond in
  pp ^ " {\n  " ^ condlist_to_string conds ^ " => " ^ pp
  ^ Lib.bracket param_list ^ "\n}"

let pq_def n psucc_name =
  let x_param_list = param_1_to_n "x" n in
  let y_param_list = param_1_to_n "y" n in
  let conds_def1 =
    Blist.map (fun i -> pzero ^ "(y" ^ string_of_int i ^ ")") (from_to 1 n)
  in
  let conds_def2 =
    [ psucc_name ^ Lib.bracket (x_param_list ^ "," ^ y_param_list)
    ; pq ^ Lib.bracket x_param_list ]
  in
  let clause_head_with_arrow = " => " ^ pq ^ Lib.bracket y_param_list in
  pq ^ " {\n  "
  ^ condlist_to_string conds_def1
  ^ clause_head_with_arrow ^ " |\n  "
  ^ condlist_to_string conds_def2
  ^ clause_head_with_arrow ^ "\n}"

let succ_circuit_def n =
  [pp_def n; pq_def n (psucc_circuit n); psucc_circuit_def n]

let overall_circuit_def n =
  String.concat def_separator (succ_circuit_def n @ circuit_def)

(* Returns inductive definitions for a predicate succ{n}rec along with *)
(* succ1rec .. succ{n-1}rec. *)
let rec psucc_rec_def (n : int) : string list =
  let psucc_rec_n = psucc_rec n in
  let clause_head_with_arrow =
    " => " ^ psucc_rec_n
    ^ Lib.bracket (param_1_to_n "x" n ^ "," ^ param_1_to_n "y" n)
  in
  let first_conds k =
    let eqs =
      Blist.map
        (fun i -> "x" ^ string_of_int i ^ "=y" ^ string_of_int i)
        (from_to 2 k)
    in
    let zero_one = [pzero ^ "(x1)"; pone ^ "(y1)"] in
    eqs @ zero_one
  in
  let first_clause k =
    condlist_to_string (first_conds k) ^ clause_head_with_arrow
  in
  if Int.( <= ) n 1 then [psucc_rec_n ^ " {\n  " ^ first_clause n ^ "\n}"]
  else
    (* we need 2 clauses AND a recursive call to the generator *)
    let second_conds k =
      [ psucc_rec (n - 1)
        ^ Lib.bracket (param_m_to_n "x" 2 k ^ "," ^ param_m_to_n "y" 2 k)
      ; pone ^ "(x1)"
      ; pzero ^ "(y1)" ]
    in
    let second_clause k =
      condlist_to_string (second_conds k) ^ clause_head_with_arrow
    in
    ( psucc_rec_n ^ " {\n  " ^ first_clause n ^ " |\n  " ^ second_clause n
    ^ "\n}" )
    :: psucc_rec_def (n - 1)

let overall_rec_def n =
  String.concat def_separator
    ( [pp_def n; pq_def n (psucc_rec n)]
    @ psucc_rec_def n @ [pzero_def; pone_def] )

let pbitvector_def (n : int) : string =
  let conds =
    Blist.map
      (fun i -> pbool ^ Lib.bracket ("x" ^ string_of_int i))
      (from_to 1 n)
  in
  pbitvector ^ " {\n  " ^ condlist_to_string conds ^ " => " ^ pbitvector
  ^ Lib.bracket (param_1_to_n "x" n)
  ^ "\n}"

let overall_bitvector_def n =
  String.concat def_separator [pzero_def; pone_def; pbool_def; pbitvector_def n]

;;
let param = Sys.argv.(1) in
let n = Scanf.sscanf param "%d" (fun x -> x) in
let output =
  (*overall_circuit_def n*) overall_rec_def n
  (* overall_bitvector_def n *)
in
print_endline output
