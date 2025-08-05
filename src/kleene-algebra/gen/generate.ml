open Cmdliner
open Feat
open KleeneAlg
open KleeneAlgGen
open Lib
open Regextkit

let rec to_re e =
  if Form.is_zero e then
    Tree.Empty
  else if Form.is_one e then
    Tree.Epsilon
  else if Form.is_letter e then
    Tree.Literal (Format.asprintf "%c" (Option.get (Form.dest_letter e)))
  else if Form.is_concat e then
    let (e', e'') = Option.get (Form.dest_concat e) in
    Tree.Concat (to_re e', to_re e'')
  else if Form.is_choice e then
    let (e', e'') = Option.get (Form.dest_choice e) in
    Tree.Union (to_re e', to_re e'')
  else if Form.is_star e then
    Tree.Star (to_re (Option.get (Form.dest_star e)))
  else
  invalid_arg __FUNCTION__

let to_dfa alphabet e =
  let dfa = Dfa.re_to_dfa (to_re e) in
  Dfa.create
    (Dfa.get_states dfa)
    (List.map Form.letter_to_string alphabet)
    (Dfa.get_transitions dfa)
    (Dfa.get_start dfa)
    (Dfa.get_accepting dfa)

let init_random randomise seed =
  match seed with
  | Some i ->
    Random.init i
  | None ->
    if randomise then Random.self_init ()

let get_range min_size max_size =
  match (min_size, max_size) with
  | None, None ->
    (1, 2)
  | None, Some max ->
    (0, max)
  | Some min, None ->
    (min, Stdlib.Int.succ min)
  | Some min, Some max when min < max ->
    (min, max)
  | _ ->
    invalid_arg "Minimum size must be less than maximum size"

let get_alphabet alphabet_size =
  List.init
    alphabet_size
    (fun i -> Char.chr (i + (Char.code 'a')))

let gen_exprs
      include_zero include_one alphabet_size num_per_size min_size max_size =
  let alphabet = get_alphabet alphabet_size in
  let (min_size, max_size) = get_range min_size max_size in
  let enum = Enumerate.expressions1 ~include_zero ~include_one alphabet in
  let formulas =
    Enum.sample num_per_size enum min_size max_size (Stdlib.Seq.empty) in
  Format.pp_print_seq
    ~pp_sep:Format.pp_force_newline Form.pp
    Format.std_formatter
    formulas ;
  Format.pp_force_newline Format.std_formatter ()

let gen_entailments
      include_zero include_one alphabet_size
      num_per_size min_size max_size show_invalid =
  let alphabet = get_alphabet alphabet_size in
  let (min_size, max_size) = get_range min_size max_size in
  let enum = Enumerate.expressions1 ~include_zero ~include_one alphabet in
  let formulas =
    Enum.sample num_per_size enum min_size max_size (Stdlib.Seq.empty) in
  let with_dfas =
    Stdlib.Seq.map
      (fun f -> let dfa = to_dfa alphabet f in (f, dfa, Dfa.complement dfa))
      (formulas) in
  let entailments =
    let product_empty dfa1 dfa2 =
      Dfa.is_empty (Dfa.product_intersection dfa1 dfa2) in
    Stdlib.Seq.filter_map
      (fun ((e, e_dfa, _), (f, _, f_comp)) ->
        if (e = f) then
          None
        else
          let included = product_empty e_dfa f_comp in
          if show_invalid
            then Option.mk (not (included)) (e, f)
            else Option.mk (included) (e, f))
      (Stdlib.Seq.product with_dfas with_dfas) in
  let pp_entailment fmt (e, f) =
    Format.fprintf fmt "%a %a %a"
      Form.pp e Symbols.pp_symb Symbols.symb_turnstile_unicode Form.pp f in
  Format.pp_print_seq
    ~pp_sep:Format.pp_force_newline pp_entailment
    Format.std_formatter
    entailments ;
  Format.pp_force_newline Format.std_formatter ()


(* Utility term combinator *)
let do_sequence = Term.const (fun () () -> ())

(* Command line argument terms *)

let include_zero =
  let info =
    let doc = "Exclude 0 (the empty language) in expressions" in
    Arg.info ~doc ["without-zero"] in
  Term.map not (Arg.value (Arg.flag info))

let include_one =
  let info =
    let doc = "Exclude 1 (the empty string language) in expressions" in
    Arg.info ~doc ["without-one"] in
  Term.map not (Arg.value (Arg.flag info))

let alphabet_size =
  let info =
    let doc = "The size of the alphabet for generated expressions" in
    Arg.info ~doc ["alphabet-size"] in
  Arg.value (Arg.opt Arg.int 1 info)

let num_per_size =
  let info =
    let doc = "The number of expressions to generate per size" in
    Arg.info ~doc ["per-size"] in
  Arg.value (Arg.opt Arg.int 1 info)

let min_size =
  let info =
    let doc = "The minimum size (inclusive) of expressions to generate" in
    Arg.info ~doc ["min-size"] in
  Arg.value (Arg.opt (Arg.some' ~none:1 Arg.int) None info)

let max_size =
  let info =
    let doc = "The maximum size (exclusive) of expressions to generate" in
    Arg.info ~doc ["max-size"] in
  let none = "One more than the minimum size" in
  Arg.value (Arg.opt (Arg.some ~none Arg.int) None info)

let show_invalid =
  let info =
    let doc = "Generate invalid entailments, instead of valid ones" in
    Arg.info ~doc ["show-invalid"] in
  Arg.value (Arg.flag info)

let randomise =
  let info =
    let doc =
      "Initialise the random number generator with a system-dependent \
       random seed" in
    Arg.info ~doc ["randomise"] in
  Arg.value (Arg.flag info)

let seed =
  let info =
    let doc =
      "Initialise the random number generator with the given seed; \
       when present, this option overrides --randomise" in
    Arg.info ~doc ["seed"] in
  let none = "Default system seed" in
  Arg.value (Arg.opt (Arg.some ~none Arg.int) None info)

let cmd_gen_exprs =
  let term =
    Term.(do_sequence
      $ ((const init_random) $ randomise $ seed)
      $ ((const gen_exprs)
            $ include_zero
            $ include_one
            $ alphabet_size
            $ num_per_size
            $ min_size
            $ max_size)) in
  let doc = "Generate a sequence of random expressions" in
  Cmd.(v (info ~doc "expressions") term)

let cmd_gen_entailments =
  let term =
    Term.(do_sequence
      $ ((const init_random) $ randomise $ seed)
      $ ((const gen_entailments)
            $ include_zero
            $ include_one
            $ alphabet_size
            $ num_per_size
            $ min_size
            $ max_size
            $ show_invalid)) in
  let doc =
    "Generate a sequence of random entailments. \
     This is done by first generating a sequence of random expressions, \
     and then constructing potential entailments by considering every pair of \
     distinct expressions" in
  Cmd.(v (info ~doc "entailments") term)

let entry =
  let doc = "Generate random expressions and entailments of Kleene Algebra" in
  Cmd.(group (info "generate" ~doc)) [
      cmd_gen_exprs ;
      cmd_gen_entailments
    ]

let () =
    Cmd.(exit (eval entry))