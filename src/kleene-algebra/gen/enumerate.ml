open Feat.Enum
open Fix.Memoize.Int
open KleeneAlg

(* An internal data type for expressions, which is more convenient for building
   enumerations. *)
type t =
  | Zero
  | One
  | Letter of Form.letter
  | Choice of t list
  | Concat of t list
  | Star of t

let merge_choice =
  let rec merge_choice acc =
    function
    | [] ->
      List.rev acc
    | (Choice xs)::ys ->
      merge_choice (List.rev_append xs acc) ys
    | x::xs ->
      merge_choice (x::acc) xs in
  merge_choice []

let merge_concat =
  let rec merge_concat acc =
    function
    | [] ->
      List.rev acc
    | (Concat xs)::ys ->
      merge_concat (List.rev_append xs acc) ys
    | x::xs ->
      merge_concat (x::acc) xs in
  merge_concat []

let rec normalise =
  function
  | Zero
  | One
  | Letter _ as f ->
    f
  | Star f ->
    Star (normalise f)
  | Choice [] ->
    Zero
  | Concat [] ->
    One
  | Choice [f]
  | Concat [f] ->
    normalise f
  | Choice fs ->
    Choice (merge_choice (List.map normalise fs))
  | Concat fs ->
    Concat (merge_concat (List.map normalise fs))

(* collapses adjacent levels of choices and concatenations; i.e. the result is
   a [t] value in which there are no immediate nestings of choices within
   choices or concatenations within contatenations. *)
let rec canonicalise =
  function
  | Zero
  | One
  | Letter _ as f ->
    f
  | Star f ->
    Star (canonicalise f)
  | Choice [] ->
    Zero
  | Concat [] ->
    One
  | Choice [f]
  | Concat [f] ->
    canonicalise f
  | Concat fs ->
    Concat (merge_concat (List.map canonicalise fs))
  | Choice fs ->
    Choice
      (List.sort_uniq
        (Stdlib.compare)
        (merge_choice (List.map canonicalise fs)))

(* Produces canonical concatenation expressions, provided the input
   sub-expressions are also canonical *)
let concatenate_canonical fs =
  Concat (merge_concat fs)

(* Produces canonical choice expressions, provided the input sub-expressions
   are also canonical *)
let choice_canonical fs =
  Choice (List.sort_uniq (Stdlib.compare) (merge_choice fs))

let rec convert =
  function
  | Zero ->
    Form.zero
  | One ->
    Form.one
  | Letter c ->
    Form.letter c
  | Choice fs ->
    Form.either (List.map convert fs)
  | Concat fs ->
    Form.concatenate (List.map convert fs)
  | Star f ->
    Form.star (convert f)

let atoms ?(include_zero=false) ?(include_one=true) letters =
  let atoms = List.map (fun c -> Letter c) letters in
  let atoms = if include_one then One :: atoms else atoms in
  let atoms = if include_zero then Zero :: atoms else atoms in
  finite atoms

let list_of_two_or_more enum =
  map
    (fun (x, (y, rest)) -> x::y::rest)
    (enum ** pay (enum ** pay (list enum)))

(* This enumeration generates concatenations of concatenations of etc. and also
  choices of choices of choices of etc. However, it canonicalises formulas on
  construction, so different equivalent ways of combining several levels of
  choice/concatenation are mapped to equal formulas. Thus, the enumeration
  does not guarantee non-repetition. *)
let enum1 ?(include_zero=false) ?(include_one=false) letters =
  fix
    (fun enum ->
      (atoms ~include_zero ~include_one letters)
        ++ pay (map concatenate_canonical (list_of_two_or_more (enum)))
        ++ pay (map choice_canonical (list_of_two_or_more (enum)))
        ++ pay (map (fun x -> Star x) (enum)))

let expressions1 ?(include_zero=false) ?(include_one=false) letters =
  map convert (enum1 ~include_zero ~include_one letters)

(* This enumeration tries to be smarter by constructing three mutually dependent
  enumerations, two of which contain no top-level choices and concatenations,
  respectively, that restrict concatenations to be constructed only out of
  formulas with no top-level concatenations, and choices to be constructed
  only out of formulas with no top-level choices.
  Again, this enumeration is constructed to filter out duplicate formulas
  within choices, and so the enumeration does not guarantee non-repetition.
  But it aims to have less repetition that the [enum1] enumeration above.
  Some preliminary experiments show that the performance of this enumeration
  scales much worse than the more naive [enum1] defined above. *)
let enum2 ?(include_zero=false) ?(include_one=false) letters =
  let atoms = atoms ~include_zero ~include_one letters in
  let all enums = (fun part -> let (enum, _, _) = enums part in enum) in
  let no_choice enums = (fun part -> let (_, enum, _) = enums part in enum) in
  let no_concat enums = (fun part -> let (_, _, enum) = enums part in enum) in
  let def enums =
    let enum_all =
      atoms
        ++ pay
            (map
              (fun fs -> Choice (List.sort_uniq (Stdlib.compare) fs))
              (list_of_two_or_more (no_choice enums)))
        ++ pay
            (map
              (fun fs -> Concat fs)
              (list_of_two_or_more (no_concat enums)))
        ++ pay (map (fun f -> Star f) (all enums)) in
    let enum_no_choice =
      atoms
        ++ pay
            (map
              (fun fs -> Concat fs)
              (list_of_two_or_more (no_concat enums)))
        ++ pay (map (fun f -> Star f) (all enums)) in
    let enum_no_concat =
      atoms
        ++ pay
            (map
              (fun fs -> Choice (List.sort_uniq (Stdlib.compare) fs))
              (list_of_two_or_more (no_choice enums)))
        ++ pay (map (fun f -> Star f) (all enums)) in
    (fun part -> (enum_all part, enum_no_choice part, enum_no_concat part))
  in
  all (fix def)

let expressions2 ?(include_zero=false) ?(include_one=false) letters =
  map convert (enum2 ~include_zero ~include_one letters)
