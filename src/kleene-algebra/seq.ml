open Lib
open Lib.Symbols
open Generic
open MParser
open MParser_RE

module Formula =
struct

  type t = Tags.Elt.t * Form.t

  let hash (_, f) = Form.hash f
  let compare_upto_tags (_, f) (_, f') = Form.compare f f'
  let compare (t, f) (t', f') =
    let result = Form.compare f f' in
    if Int.equal result 0 then Tags.Elt.compare t t' else result
  let equal (tag, f) (tag', f') = Tags.Elt.equal tag tag' && Form.equal f f'
  let equal_upto_tags (_, f) (_, f') =
    Form.equal f f'
  let pp fmt (t, f) =
    if Tags.is_anonymous t then
      Form.pp fmt f
    else
      Format.fprintf fmt "{%a: %a}" Tags.Elt.pp t Form.pp f
  let pp_no_tags fmt (_, f) =
    Form.pp fmt f
  let to_string = mk_to_string pp

end

(*
 * Invariants of lists of formulas that we maintain:
 *   - [Formula.t] values [f] for which [Form.is_star f = true] are tagged with
 *     Free tags.
 *   - [Formula.t] values [f] for which [Form.is_star f = false] are tagged with
 *     [Tags.anonymous].
 *   - Free tags are unique within the list of [Formula.t] values
 *)
type t = Formula.t list * Form.t list

let pp_aux print_tags fmt (es, fs) =
  let pp_formula = if print_tags then Formula.pp else Formula.pp_no_tags in
  Format.fprintf fmt "%a ⊢ %a"
    (Blist.pp (fun fmt () -> Format.fprintf fmt ", ") pp_formula)
    es
    (Blist.pp (fun fmt () -> Format.fprintf fmt ", ") Form.pp)
    fs
let pp = pp_aux true
let pp_no_tags = pp_aux false

let to_string = mk_to_string pp

let equal (es, fs) (es', fs') =
  List.equal Formula.equal es es'
    && List.equal Form.equal
        (List.sort Form.compare fs)
        (List.sort Form.compare fs')
let equal_upto_tags (es, fs) (es', fs') =
  List.equal Formula.equal_upto_tags es es'
    && List.equal Form.equal
        (List.sort Form.compare fs)
        (List.sort Form.compare fs')

let tags (es, _) =
  List.fold_left
    (fun tags (t, _) -> if Tags.is_anonymous t then tags else Tags.add t tags)
    (Tags.empty)
    (es)

let of_lists (es, fs) =
  let tags = Tags.fresh_fvars Tags.empty (List.length es) in
  let es' =
    List.fold_left2
      (fun es e tag ->
        let e' = if Form.is_star e then (tag, e) else (Tags.anonymous, e) in
        e' :: es)
      []
      es
      tags in
  (List.rev es', fs)

let parse st =
  (
    sep_by Form.parse Tokens.comma >>= (fun es ->
    parse_symb symb_turnstile >>
    sep_by Form.parse Tokens.comma >>= (fun fs ->
    return (of_lists (es, fs))))
  ) st

let of_string = mk_of_string (parse << eof)

let with_consequent fs (es, _) = (es, fs)

let antecedent (es, _) =
  List.map (fun (_, f) -> f) es
let consequent (_, fs) = fs

let right_start (es, _) =
  1 + List.length es

let get_tag idx (es, _) =
  if (idx < 0 || idx >= List.length es) then
    invalid_arg __FUNCTION__
  else
    let (t, _) = List.nth es idx in
    t

let zip_tags (es, _) (fs, _) =
  if Int.equal (List.compare_lengths es fs) 0 then
    List.fold_left2
      (fun tps (t, _) (t', _) ->
        if (Tags.is_anonymous t || Tags.is_anonymous t') then
          tps
        else
          Tagpairs.add (t, t') tps)
      Tagpairs.empty
      es
      fs
  else
    invalid_arg __FUNCTION__

let nth_opt (es, fs) i =
  let left_len = List.length es in
  if (i >= 0 && i < left_len) then
    let (_, e) = List.nth es i in
    Some e
  else if (i > left_len && i < (1 + left_len + List.length fs)) then
    Some (List.nth fs (i - left_len - 1))
  else
    None

let nth seq i =
  match nth_opt seq i with
  | Some f ->
    f
  | None ->
    invalid_arg __FUNCTION__

let remove_at i (es, fs) =
  let left_len = List.length es in
  if (i < 0 || Int.equal i left_len || i >= (1 + left_len + List.length fs))
  then
    invalid_arg __FUNCTION__
  else if (i < left_len) then
    let es' = List.append (List.take i es) (List.drop (i+1) es) in
    (es', fs)
  else
    let j = i - (List.length es) - 1 in
    let fs' = List.append (List.take j fs) (List.drop (j+1) fs) in
    (es, fs')

let remove_many_at i len ((es, fs) as seq) =
  if (i < 0) then
    invalid_arg __FUNCTION__
  else
    let left_len = List.length es in
    let es' =
      if (i < left_len)
        then (List.take i es) @ (List.drop (i + len) es)
        else es in
    let fs' =
      if (i + len < left_len) then
        fs
      else if i < left_len then
        let len = len - left_len + i in
        List.drop len fs
      else
        let right_start = right_start seq in
        let i = (Int.max right_start i) - right_start in
        (List.take i fs) @ (List.drop (i + len) fs)
    in
    (es', fs')

let insert_many xs i ((es, fs) as seq) =
  let left_len = List.length es in
  if (i < 0) then
    invalid_arg __FUNCTION__
  else if (i <= left_len) then
    let tags = Tags.fresh_fvars (tags seq) (List.length xs) in
    let xs =
      List.map2
        (fun tag f -> if Form.is_star f then (tag, f) else (Tags.anonymous, f))
        tags
        xs in
    let es' = List.append (List.append (List.take i es) xs) (List.drop i es) in
    (es', fs)
  else if (i <= 1 + left_len + List.length fs) then
    let j = i - left_len - 1 in
    let fs' = List.append (List.append (List.take j fs) xs) (List.drop j fs) in
    (es, fs')
  else
    invalid_arg __FUNCTION__

let insert f i seq =
  insert_many [f] i seq

let filter_left p (es, fs) =
  let es' = List.filter (fun (_, e) -> p e) es in
  (es', fs)
let filter_right p (es, fs) =
  let fs' = List.filter p fs in
  (es, fs')
let filter p seq =
  filter_right p (filter_left p seq)


let remove_left p seq =
  filter_left (fun f -> not (p f)) seq
let remove_right p seq =
  filter_right (fun f -> not (p f)) seq
let remove p seq =
  remove_right p (remove_left p seq)


let find_index_left p (es, _) =
  List.find_index (fun (_, f) -> p f) es
let find_index_right  p (es, fs) =
  Option.map (( + ) (1 + List.length es))
    (List.find_index p fs)

let find_indices_left p (es, _) =
  let rec aux idx acc =
    function
    | [] ->
      List.rev acc
    | (_, e) :: es ->
      let acc = if (p e) then idx :: acc else acc in
      aux (idx+1) acc es in
  aux 0 [] es

let find_indices_right p ((_, fs) as seq) =
  let rec aux idx acc =
    function
    | [] ->
      List.rev acc
    | f :: fs ->
      let acc = if (p f) then idx :: acc else acc in
      aux (idx+1) acc fs in
  aux (right_start seq) [] fs

let exists_left p (es, _) =
  List.exists (fun (_, f) -> p f) es
let exists_right p (_, fs) = List.exists p fs

let forall_left p (es, _) =
  List.for_all (fun (_, f) -> p f) es
let forall_right p (_, fs) = List.for_all p fs

let take_left n (es, fs) =
  if (n < 0) then
    invalid_arg __FUNCTION__
  else
    (List.take n es, fs)

let take_right n (es, fs) =
  if (n < 0) then
    invalid_arg __FUNCTION__
  else
    (es, List.take n fs)

let drop_left n (es, fs) =
  if (n < 0) then
    invalid_arg __FUNCTION__
  else
    (List.drop n es, fs)

let drop_right n (es, fs) =
  if (n < 0) then
    invalid_arg __FUNCTION__
  else
    (es, List.drop n fs)

let all_left_splits (es, fs) =
  List.map
    (fun (l, r) -> (l, fs), (r, fs))
    (Blist.all_splits es)

let is_axiomatic ?(nonatomic=true) ((es, fs) as seq) =
  match es with
  | [] ->
    List.exists Form.is_one fs
  | [(_, e)] when not (Form.is_zero e) ->
    (Form.is_letter e || nonatomic) && List.exists (Form.equal e) fs
  | _ ->
    exists_left Form.is_zero seq

let is_empty_left (es, _) =
  List.is_empty es
let is_empty_right (_, fs) =
  List.is_empty fs
