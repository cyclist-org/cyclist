type 'a formula

val parse : ('a formula, 'b) MParser.parser

type dsh
type symheap

val to_dsh : 'a formula -> dsh formula
val to_symheap : dsh formula -> symheap formula

val symheap_iter : (symheap formula -> unit) -> symheap formula -> unit