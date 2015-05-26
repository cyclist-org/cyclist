open Lib
open Util
open Symbols
open MParser

module SH = Sl_heap

module Defs =
  struct
    include MakeFList(Sl_preddef)
    
    let mem ident defs =
      Blist.exists 
        (fun def -> Sl_predsym.equal ident (Sl_preddef.predsym def)) defs
    
    let empty = []
    let to_list d = d
    let add def defs =
      assert (not (mem (Sl_preddef.predsym def) defs)) ;
      def::defs
    
    let of_list defs = 
      Blist.foldl (fun d p -> add p d) empty defs
      
    let to_string defs =
      Blist.to_string (symb_semicolon.sep ^ "\n\n") Sl_preddef.to_string defs
    
    let to_melt d = ltx_text (to_string d)
    
    let pp fmt d = Format.fprintf fmt "%s" (to_string d)
        
    let parse st =
      (sep_by1 Sl_preddef.parse (parse_symb symb_semicolon) <?> "defs") st

    let of_string s =
      handle_reply (MParser.parse_string parse s ())
    
    let of_channel c =
      handle_reply (MParser.parse_channel parse c ())
    
    
    let is_defined defs (_, (ident, _)) =
      mem ident defs
    
    let is_undefined defs pred = not (is_defined defs pred)
    
    let get_def ident defs =
      Sl_preddef.rules (Blist.find 
        (fun def -> Sl_predsym.equal ident (Sl_preddef.predsym def)) 
        defs)
    
    let unfold vars h ((_, (ident, _)) as pred) defs = 
      Blist.map (Sl_indrule.unfold vars h pred) (get_def ident defs)
    
    let of_formula defs f =
      let counter = ref 0 in
      let get_ident () = Sl_predsym.mk (Printf.sprintf "P%d" !counter) in
      let () = while mem (get_ident ()) defs do incr counter done in
      let predsym = get_ident () in
      let formals = 
        Sl_term.Set.to_list 
          (Sl_term.Set.filter Sl_term.is_univ_var (Sl_form.vars f)) in
      let pred = (predsym, formals) in
      let rules = Blist.map (fun h -> Sl_indrule.mk h pred) f in
      let def = Sl_preddef.mk (rules, predsym) in
      def::defs  
      
    let rule_fold f v defs =
      let f' v' def = Blist.foldl f v' (Sl_preddef.rules def) in
      Blist.foldl f' v defs
      
    let rule_iter f defs =
      let f' def = Blist.iter f (Sl_preddef.rules def) in
      Blist.iter f' defs

    let relevant_defs all_defs f = 
      let ident_set f = Sl_predsym.Set.of_list (Sl_predsym.MSet.to_list (Sl_heap.idents f)) in
      let iterate preds =
        let add_preds pred preds = 
          let rules = get_def pred all_defs in
          let add_preds_from_rule preds rule =
            let (body, _) = Sl_indrule.dest rule in
            let new_preds = ident_set body in
            Sl_predsym.Set.union preds new_preds in
          Blist.foldl add_preds_from_rule preds rules in
        Sl_predsym.Set.fold add_preds preds preds in
      let relevant_preds = Sl_predsym.Set.fixpoint iterate (ident_set f) in
      Sl_predsym.Set.fold
        (fun pred defs -> add (Sl_preddef.mk ((get_def pred all_defs), pred)) defs)
        relevant_preds
        empty

  end
include Defs
include Fixpoint(Defs)


