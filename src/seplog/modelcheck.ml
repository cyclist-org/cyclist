open Lib
open Symbols
open Generic
open MParser
module List = Blist
module MCGen = Mc_core.Make (Mc_core.IntSig)

module IntSigParser = struct
  let parse_location st =
    ( Tokens.hexadecimal |>> fun v ->
      if v == 0 then failwith "0x0 is not a location!" else v )
      st

  let parse_scalar st =
    (attempt
       ( Tokens.hexadecimal |>> fun v ->
         if v == 0 then MCGen.Value.zero else MCGen.Value.mk_loc_val v )
    <|> attempt (Tokens.integer |>> fun v -> MCGen.Value.mk_scalar_val v)
    <|> attempt (Tokens.skip_symbol "false" >>$ MCGen.Value.zero)
    <|> attempt (Tokens.skip_symbol "true" >>$ MCGen.Value.mk_scalar_val 1)
    <|> fail "Cannot parse this as a scalar value!")
      st
end

module StackParser = MCGen.Stack.MakeParser (IntSigParser)
module HeapParser = MCGen.ConcreteHeap.MakeParser (IntSigParser)

let model_parser st =
  (MCGen.mk_model_parser (StackParser.parse, HeapParser.parse)) st

let default_defs_path = "examples/sl.defs"

let run defs_path str_model str_symheap cvdet intuitionistic max_hashset_size ()
    =
  gc_setup ();
  Option.iter (fun n -> MCGen.max_hashset_size := n) max_hashset_size;
  let sh = Heap.of_string ~allow_tags:false str_symheap in
  (* TODO: Need to check that all predicate instances in sh match the arity in defs *)
  let defs = Defs.of_channel (open_in defs_path) in
  let ((_s, h) as model) = MCGen.model_of_string model_parser str_model in
  let () =
    print_endline ("Heap size: " ^ Int.to_string (MCGen.ConcreteHeap.size h))
  in
  Stats.reset ();
  Stats.Gen.call ();
  let call () =
    if cvdet then Mc_cvdet.check_model intuitionistic defs (sh, model)
    else MCGen.check_model intuitionistic defs (sh, model)
  in
  let res = call () in
  Stats.Gen.end_call ();
  if !Stats.do_statistics then Stats.gen_print ();
  if res then print_endline "Model verified"
  else print_endline "Not a satisfying model!"

let cmd =
  let open Cmdliner in
  let defs =
    Arg.(
      value & opt file default_defs_path
      & info [ "D"; "defs" ] ~docv:"FILE"
          ~doc:"Read inductive definitions from $(docv).")
  in
  let model =
    Arg.(
      required
      & opt (some string) None
      & info [ "M"; "model" ] ~docv:"MODEL" ~doc:"The model to be checked.")
  in
  let formula =
    Arg.(
      required
      & opt (some string) None
      & info [ "F"; "formula" ] ~docv:"FORMULA"
          ~doc:"The symbolic heap to check the model against.")
  in
  let cvdet =
    Arg.(value & flag & info [ "cvdet" ] ~doc:"Apply the CV+DET algorithm.")
  in
  let intuitionistic =
    Arg.(
      value & flag
      & info [ "i"; "intuitionistic" ] ~doc:"Use intuitionistic checking.")
  in
  let max_hashset_size =
    Arg.(
      value
      & opt (some int) None
      & info [ "max-hashset-size" ] ~docv:"INT"
          ~doc:
            "Maximum size for internal hashset creation, default is 15,485,863.")
  in
  Cmd.v
    (Cmd.info "modelcheck"
       ~doc:"Check a model against a separation logic symbolic heap."
       ~exits:Cmd.Exit.defaults)
    Term.(
      const run $ defs $ model $ formula $ cvdet $ intuitionistic
      $ max_hashset_size $ Frontend.debug_term)
