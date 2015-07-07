open Util
open Lib
open MParser
open Symbols

module List = Blist

open Sl_mc_core

module IntSigModelChecker = Make(IntSig)
open IntSigModelChecker

module IntSigParser =
  struct
    let parse_location st =
      (Tokens.hexadecimal |>> (fun v ->
        if v == 0 then (failwith "0x0 is not a location!") else v)) st
    let parse_scalar st =
      ( attempt (Tokens.hexadecimal |>> (fun v ->
        if v == 0 then Value.zero else (Value.mk_loc_val v)))
        <|>
        attempt (Tokens.integer |>> (fun v -> Value.mk_scalar_val v))
        <|>
        attempt (Tokens.skip_symbol "false" >>$ Value.zero)
        <|>
        attempt (Tokens.skip_symbol "true" >>$ Value.mk_scalar_val 1)
        <|>
        (fail "Cannot parse this as a scalar value!")
      ) st
  end

module StackParser = Stack.MakeParser(IntSigParser)
module HeapParser = Heap.MakeParser(IntSigParser)

let model_parser st = (mk_model_parser (StackParser.parse, HeapParser.parse)) st

let defs_path = ref "examples/sl.defs"
let str_model = ref ""
let str_symheap = ref ""
let cvdet = ref false

let usage =
  (
    "usage: " ^
    Sys.argv.(0) ^
    " [-D <file>] -M <string> -F <string>"
    )

let speclist = [
    ("-D", Arg.Set_string defs_path,
      ": read inductive definitions from <file>, default is " ^ !defs_path);
    ("-d", Arg.Set do_debug,": print debug messages");
    ("-s", Arg.Set Stats.do_statistics,": print statistics");
    ("-M", Arg.Set_string str_model, ": <string> model to be checked");
    ("-F", Arg.Set_string str_symheap, ": <string> symbolic heap to check against");
    ("-CVDET", Arg.Set cvdet, ": apply CV+DET algorithm");
  ]

let die msg =
  print_endline msg ;
  print_endline (Arg.usage_string speclist usage) ;
  exit 1

let () =
  gc_setup () ;
  Format.set_margin (Sys.command "exit $(tput cols)") ;
  Arg.parse speclist (fun _ -> raise (Arg.Bad "Stray argument found.")) usage ;
  if !str_model="" then die "-M must be specified." ;
  if !str_symheap="" then die "-F must be specified." ;
  let sh = Sl_heap.of_string !str_symheap in
  (* TODO: Need to check that all predicate instances in sh match the arity in defs *)
  let defs = Sl_defs.of_channel (open_in !defs_path) in
  let ((s, h) as model) = model_of_string model_parser !str_model in
  let () = print_endline("Heap size: " ^ (Int.to_string (Heap.size h))) in
  begin
    Stats.reset () ;
    Stats.Gen.call () ;
    let call () = 
      if !cvdet then 
        Sl_mc_cvdet.check_model defs (sh, model) 
      else 
        check_model defs (sh, model) in
    let res = call () in
    Stats.Gen.end_call () ;
    if !Stats.do_statistics then Stats.gen_print ();
    if res then
      print_endline("Model verified")
    else
      print_endline("Not a satisfying model!")
  end

