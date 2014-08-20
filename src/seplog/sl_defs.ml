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
        (fun def -> Strng.equal ident (Sl_preddef.predsym def)) defs
    
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
    
    let of_channel c =
      handle_reply (MParser.parse_channel parse c ())
    
    
    let is_defined defs (_, (ident, _)) =
      mem ident defs
    
    let is_undefined defs pred = not (is_defined defs pred)
    
    let get_def ident defs =
      Sl_preddef.rules (Blist.find 
        (fun def -> Strng.equal ident (Sl_preddef.predsym def)) 
        defs)
    
    let unfold vars h ((_, (ident, _)) as pred) defs = 
      Blist.map (Sl_indrule.unfold vars h pred) (get_def ident defs)
  end
include Defs
include Fixpoint(Defs)


