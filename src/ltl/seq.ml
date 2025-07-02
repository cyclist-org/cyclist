open Lib
open Generic
open MParser
open MParser_RE

module Formula : (BasicType with type t = Tags.Elt.t * Form.t) =
struct

  type t = Tags.Elt.t * Form.t

  let hash (_, f) = Form.hash f
  let compare (_, f) (_, f') = Form.compare f f'
  let equal (_, f) (_, f') = Form.equal f f'
  let pp fmt (t, f) =
    if Tags.is_anonymous t then
      Form.pp fmt f
    else
      Format.fprintf fmt "%a: %a" Tags.Elt.pp t Form.pp f
  let to_string = mk_to_string pp

end

(*
 * The comparison of tagged formulas ignores the tags.
 * Invariants of sets of formulas that we maintain:
 *   - Formulas [f] for which [Seq.is_traceable f = true] are tagged with
 *     Free tags.
 *   - Formulas [f] for which [Seq.is_traceable f = false] are tagged with
 *     [Tags.anonymous].
 *   - Free tags are unique within a set of formulas
 *)
module FormulaSet = Treeset.Make(Formula)

type t = FormulaSet.t

let pp = FormulaSet.pp

let to_string = FormulaSet.to_string

let equal_upto_tags s = FormulaSet.equal s

let equal s s' =
  FormulaSet.for_all
    (fun (t, f) ->
      FormulaSet.exists
        (fun (t', f') -> Tags.Elt.equal t t' && Form.equal f f')
        s')
    s

let tags s =
  let tags =
    FormulaSet.fold
      (fun (t, _) tags -> Tags.add t tags)
      s
      Tags.empty in
  Tags.remove Tags.anonymous tags

let of_list fs =
  let tags = Tags.fresh_fvars Tags.empty (List.length fs) in
  List.fold_left2
    (fun seq t f ->
      let formula =
        if (Form.is_traceable f) then (t, f) else (Tags.anonymous, f) in
      let () =
        if (FormulaSet.mem formula seq) then
          debug
            (fun () ->
              Format.asprintf "Ignoring duplicate formula: %a" Form.pp f) in
      FormulaSet.add formula seq)
    FormulaSet.empty
    tags
    fs

let to_list s =
  List.map (fun (_, f) -> f) (FormulaSet.to_list s)

let get_tag f s =
  Option.map
    (fun (t, _) -> t)
    (FormulaSet.find_opt (Tags.anonymous, f) s)

let add f s =
  let tag =
    if (Form.is_traceable f) then
      Tags.fresh_fvar (tags s)
    else
      Tags.anonymous in
FormulaSet.add (tag, f) s

let singleton f =
  add f FormulaSet.empty

let add_all fs s =
  let tags = Tags.fresh_fvars (tags s) (List.length fs) in
  List.fold_left2
    (fun s t f ->
      let t = if (Form.is_traceable f) then t else Tags.anonymous in
      FormulaSet.add (t, f) s)
    s
    tags
    fs

let remove f s =
  (* Since comparion of Formula.t values ignores the tag,
     we can use Tags.anonymous as a dummy. *)
  FormulaSet.remove (Tags.anonymous, f) s

let subset = FormulaSet.subset

(* To maintain the tag uniqueness invariant, we rely on the implementation of
   the underlying operation only returning elements from one of the input sets *)
let inter = FormulaSet.inter

let diff = FormulaSet.diff

let exists p s =
  FormulaSet.exists (fun (_, f) -> p f) s

let find_suchthat_opt p s =
  Option.map
    (fun (_, f) -> f)
    (FormulaSet.find_suchthat_opt (fun (_, f) -> p f) s)

let fold_with_tags = FormulaSet.fold
let fold f s acc =
  FormulaSet.fold (fun (_, form) acc -> f form acc) s acc

let parse st =
  let () = debug (fun () -> "Parsing sequent") in
  let res =
    (Tokens.comma_sep Form.parse |>> of_list) st in
  let () = debug (fun () -> "Finished parsing sequent") in
  res

let of_string s =
  mk_of_string parse s

let is_empty = FormulaSet.is_empty
let is_axiomatic s =
  FormulaSet.exists
    (fun (_, f) ->
      Option.pred_dest
        (fun p ->
          FormulaSet.exists
            (fun (_, f') -> Option.pred_dest (String.equal p) (Form.dest_negatom f'))
            s)
        (Form.dest_atom f))
    s