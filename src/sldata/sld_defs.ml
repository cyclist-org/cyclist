open Lib
open Util
open Symbols
open MParser

module SH = Sld_heap

module Defs =
  struct
    include MakeFList(Sld_preddef)
    
    let mem ident defs =
      Blist.exists 
        (fun def -> Sld_predsym.equal ident (Sld_preddef.predsym def)) defs
    
    let empty = []
    let to_list d = d
    let add def defs =
      assert (not (mem (Sld_preddef.predsym def) defs)) ;
      def::defs
    
    let of_list defs = 
      Blist.foldl (fun d p -> add p d) empty defs
      
    let to_string defs =
      Blist.to_string (symb_semicolon.sep ^ "\n\n") Sld_preddef.to_string defs
    
    let to_melt d = ltx_text (to_string d)
    
    let pp fmt d = Format.fprintf fmt "%s" (to_string d)
        
    let parse st =
      (sep_by1 Sld_preddef.parse (parse_symb symb_semicolon) <?> "defs") st

    let of_string s =
      handle_reply (MParser.parse_string parse s ())
    
    let of_channel c =
      handle_reply (MParser.parse_channel parse c ())
    
    
    let is_defined defs (_, (ident, _)) =
      mem ident defs
    
    let is_undefined defs pred = not (is_defined defs pred)
    
    let get_def ident defs =
      Sld_preddef.rules (Blist.find 
        (fun def -> Sld_predsym.equal ident (Sld_preddef.predsym def)) 
        defs)
    
    let unfold vars h ((_, (ident, _)) as pred) defs = 
      Blist.map (Sld_indrule.unfold vars h pred) (get_def ident defs)
    
    let of_formula defs f =
      let counter = ref 0 in
      let get_ident () = Sld_predsym.mk (Printf.sprintf "P%d" !counter) in
      let () = while mem (get_ident ()) defs do incr counter done in
      let predsym = get_ident () in
      let formals = 
        Sld_term.Set.to_list 
          (Sld_term.Set.filter Sld_term.is_univ_var (Sld_form.vars f)) in
      let pred = (predsym, formals) in
      let rules = Blist.map (fun h -> Sld_indrule.mk h pred) f in
      let def = Sld_preddef.mk (rules, predsym) in
      def::defs  
      
    let rule_fold f v defs =
      let f' v' def = Blist.foldl f v' (Sld_preddef.rules def) in
      Blist.foldl f' v defs
      
    let rule_iter f defs =
      let f' def = Blist.iter f (Sld_preddef.rules def) in
      Blist.iter f' defs

    let relevant_defs all_defs f = 
      let ident_set ids = Sld_predsym.Set.of_list (Sld_predsym.MSet.to_list ids) in
      let iterate preds =
        let add_preds pred preds = 
          let rules = get_def pred all_defs in
          let add_preds_from_rule preds rule =
            let (body, _) = Sld_indrule.dest rule in
            let new_preds = ident_set (Sld_heap.idents body) in
            Sld_predsym.Set.union preds new_preds in
          Blist.foldl add_preds_from_rule preds rules in
        Sld_predsym.Set.fold add_preds preds preds in
      let init_ids = 
        let ident_mset = Blist.fold_right 
          (fun h -> Sld_predsym.MSet.union (Sld_heap.idents h)) 
          f Sld_predsym.MSet.empty in
        ident_set ident_mset in 
      let relevant_preds = Sld_predsym.Set.fixpoint iterate init_ids in
      Sld_predsym.Set.fold
        (fun pred defs -> add (Sld_preddef.mk ((get_def pred all_defs), pred)) defs)
        relevant_preds
        empty
    
    let check_form_wf defs f =
      let check_pred p = 
        let predsym = Sld_tpred.predsym p in
        let pname = Sld_predsym.to_string predsym in
        if not (mem predsym defs) then
          invalid_arg ("Cannot find definition for predicate " ^ pname)
        else
          let def = get_def predsym defs in
          if not (Blist.is_empty def) then
            let expected = Sld_indrule.arity (Blist.hd def) in
            let provided = Sld_tpred.arity p in 
            if expected <> provided then
              invalid_arg ("Predicate " ^ pname ^ " given " ^ 
                (Int.to_string provided) ^ " arguments when " ^
                (Int.to_string expected) ^ " expected!") in
      Blist.iter
        (fun h -> let (_, _, _, inds) = Sld_heap.dest h in
          Sld_tpreds.iter check_pred inds)
        f
          
  end
include Defs
include Fixpoint(Defs)


