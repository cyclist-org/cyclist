open Lib
open MParser 

module T = Util.Strng

module H = Hashcons.Make(T)

let predtbl = H.create 997

module HT = 
  struct
    type t = T.t Hashcons.hash_consed
    
    let mk s = H.hashcons predtbl s
    let of_string = mk
    let parse st = (parse_ident |>> mk) st
    
    let to_string s = s.Hashcons.node
    let to_melt s = Latex.mathit (Latex.text (to_string s))
    let pp fmt s = T.pp fmt (to_string s)
    
    let compare s s' = Pervasives.compare s.Hashcons.tag s'.Hashcons.tag
    let equal s s' = s==s'
    let hash s = s.Hashcons.hkey
  end

include HT

module Set = Util.MakeTreeSet(HT)
module MSet = Util.MakeMultiset(HT)
