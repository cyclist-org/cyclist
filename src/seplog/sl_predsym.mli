include Utilsigs.BasicType

module Set : Utilsigs.OrderedContainer with type elt = t

module MSet : Utilsigs.OrderedContainer with type elt = t

module Map : Utilsigs.OrderedMap with type key = t

val parse : (t, 'a) MParser.parser

val of_string : string -> t
