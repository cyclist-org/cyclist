open Lib

include BasicType

module Set : OrderedContainer with type elt = t

module MSet : OrderedContainer with type elt = t

module Map : OrderedMap with type key = t

val parse : (t, 'a) MParser.parser

val of_string : string -> t
