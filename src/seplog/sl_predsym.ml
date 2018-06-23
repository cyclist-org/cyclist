open Lib
open MParser

module T = Strng

module H = Hashcons.Make(T)

let predtbl = H.create 997

module HT =
  struct
    type t = T.t Hashcons.hash_consed

    let parse st = (parse_ident |>> H.hashcons predtbl) st
    let of_string s =
      handle_reply (MParser.parse_string parse s ())

    let to_string s = s.Hashcons.node
    let pp fmt s = T.pp fmt (to_string s)

    let compare s s' = Pervasives.compare s.Hashcons.tag s'.Hashcons.tag
    let equal s s' = s==s'
    let hash s = s.Hashcons.hkey
  end

include HT

module Set = Treeset.Make(HT)
module MSet = Multiset.Make(HT)
module Map = Treemap.Make(HT)
