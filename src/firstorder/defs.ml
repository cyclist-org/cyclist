open Lib
open   Symbols

open Generic

open MParser

(* association lists to lists are used because the order in which predicates
   and cases are added is very important for performance and a tree-map or
   hash-map would disregard that order *)

type t = (string * Case.t list) list

let bindings s = s
let empty = []

let to_string defs =
  let def_to_string (ident, cls) =
    ident ^ " {\n" ^ 
      (Blist.to_string " |\n" (Case.to_string ident) cls) ^ "\n}" in
  Blist.to_string " ;\n\n" def_to_string defs

let pp fmt d = Format.fprintf fmt "%s" (to_string d)

let add ident case defs =
  if Blist.mem_assoc ident defs then
    Blist.map
      begin fun (ident', cases) ->
        if String.equal ident ident' then
          (ident', cases @ [case])
        else
          (ident', cases)
      end
    defs
  else
    defs @ [ (ident, [case]) ]

(* ind_def:                                                      *)
(*     | p = IDENT; inds = ind_cases { (inds, p) }               *)
let parse_def st =
  ( parse_ident >>= (fun p ->
    Case.parse_cases >>= (fun inds ->
    return (inds,p))) <?> "Def") st

(* ind_def_set:                                                  *)
(*     | ids = separated_nonempty_list(SEMICOLON, ind_def); EOF  *)
(*   {                                                           *)
(*     let f defs (c,ident)= FO.Defs.add ident c defs in         *)
(*     let g defs cases = List.fold_left f defs cases in         *)
(*     let h defs (cases,_) = g defs cases in                    *)
(*     List.fold_left h FO.Defs.empty ids                        *)
(*   }                                                           *)
let parse st =
  ( sep_by1 parse_def (parse_symb symb_semicolon) >>= (fun ids ->
    eof >>
    let f defs (c,ident)= add ident c defs in
    let g defs cases = Blist.fold_left f defs cases in
    let h defs (cases,_) = g defs cases in
    return (List.fold_left h empty ids) <?> "Defs")) st

let of_channel c =
  handle_reply (parse_channel parse c ())
