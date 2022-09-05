open Lib
open   Symbols

open Generic

open MParser

module SH = Pheap

module Defs = struct
  include Flist.Make (Preddef)

  let mem ident defs =
    Blist.exists
      (fun def -> Predsym.equal ident (Preddef.predsym def))
      defs

  let empty = []

  let to_list d = d

  let add def defs =
    assert (not (mem (Preddef.predsym def) defs)) ;
    def :: defs

  let of_list defs = Blist.foldl (fun d p -> add p d) empty defs

  let to_string defs =
    Blist.to_string (symb_semicolon.sep ^ "\n\n") Preddef.to_string defs

  let pp fmt d = Format.fprintf fmt "%s" (to_string d)

  let is_defined defs (_, (ident, _)) = mem ident defs

  let is_undefined defs pred = not (is_defined defs pred)

  let get_def ident defs =
    Preddef.rules
      (Blist.find
         (fun def -> Predsym.equal ident (Preddef.predsym def))
         defs)

  let unfold ?(gen_tags = true) (vars, tags) ((_, (ident, _)) as pred) defs =
    Blist.map
      (Indrule.unfold ~gen_tags (vars, tags) pred)
      (get_def ident defs)

  let of_formula defs ((_, hs) as f) =
    let counter = ref 0 in
    let get_ident () = Predsym.of_string (Printf.sprintf "P%d" !counter) in
    let () =
      while mem (get_ident ()) defs do
        incr counter
      done
    in
    let predsym = get_ident () in
    let formals =
      Term.Set.to_list
        (Term.Set.filter Term.is_free_var (Form.vars f))
    in
    let pred = (predsym, formals) in
    let rules = Blist.map (fun h -> Indrule.mk h pred) hs in
    let def = Preddef.mk (rules, predsym) in
    def :: defs

  let rule_fold f v defs =
    let f' v' def = Blist.foldl f v' (Preddef.rules def) in
    Blist.foldl f' v defs

  let rule_iter f defs =
    let f' def = Blist.iter f (Preddef.rules def) in
    Blist.iter f' defs

  let relevant_defs all_defs (_, hs) =
    let ident_set ids = Predsym.Set.of_list (Predsym.MSet.to_list ids) in
    let iterate preds =
      let add_preds pred preds =
        let rules = get_def pred all_defs in
        let add_preds_from_rule preds rule =
          let body, _ = Indrule.dest rule in
          let new_preds = ident_set (Pheap.idents body) in
          Predsym.Set.union preds new_preds
        in
        Blist.foldl add_preds_from_rule preds rules
      in
      Predsym.Set.fold add_preds preds preds
    in
    let init_ids =
      let ident_mset =
        Blist.fold_right
          (fun h -> Predsym.MSet.union (Pheap.idents h))
          hs Predsym.MSet.empty
      in
      ident_set ident_mset
    in
    let relevant_preds = Predsym.Set.fixpoint iterate init_ids in
    Predsym.Set.fold
      (fun pred defs -> add (Preddef.mk (get_def pred all_defs, pred)) defs)
      relevant_preds empty

  let check_form_wf defs (_, hs) =
    let check_pred p =
      let predsym = Tpred.predsym p in
      let pname = Predsym.to_string predsym in
      if not (mem predsym defs) then
        invalid_arg ("Cannot find definition for predicate " ^ pname)
      else
        let def = get_def predsym defs in
        if not (Blist.is_empty def) then
          let expected = Indrule.arity (Blist.hd def) in
          let provided = Tpred.arity p in
          if not (Int.equal expected provided) then
            invalid_arg
              ( pname ^ " given " ^ Int.to_string provided
              ^ " arguments when its definition expects "
              ^ Int.to_string expected ^ "!" )
    in
    Blist.iter
      (fun h ->
        let _, _, _, inds = Heap.dest h in
        Tpreds.iter check_pred inds )
      hs

  let check_consistency defs =
    rule_iter
      (fun rl ->
        try check_form_wf defs (Ord_constraints.empty, [Indrule.body rl])
        with Invalid_argument s ->
          failwith
            ( "Error in definition of "
            ^ Predsym.to_string (Indrule.predsym rl)
            ^ ": " ^ s ) )
      defs

  let parse st =
    ( sep_by1 Preddef.parse (parse_symb symb_semicolon)
    >>= (fun preddefs ->
          eof
          >>$
          let defs =
            Blist.rev
              (Blist.fold_left
                 (fun defs d ->
                   let () =
                     debug (fun _ ->
                         "Parsing definition of: "
                         ^ Predsym.to_string (Preddef.predsym d) )
                   in
                   add d defs )
                 empty preddefs)
          in
          let () = check_consistency defs in
          defs )
    <?> "defs" )
      st

  let of_string s = handle_reply (MParser.parse_string parse s ())

  let of_channel c = handle_reply (MParser.parse_channel parse c ())

  let memory_consuming defs = Blist.for_all Preddef.memory_consuming defs

  let constructively_valued defs =
    Blist.for_all Preddef.constructively_valued defs

  let deterministic defs = Blist.for_all Preddef.deterministic defs
end

include Defs
include Fixpoint.Make (Defs)
