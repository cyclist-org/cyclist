open MParser
module Tokens = MParser_PCRE.Tokens

type symbol = {str: string; sep: string}

(* str fields *cannot* have whitespace *)
(* surrounding them as this trips the lexer *)

let make_symb s = {str= s; sep= " " ^ s ^ " "}

let mk_keyw s = {str= s; sep= " " ^ s ^ " "}

let symb_nullstr = {str= ""; sep= ""}

let symb_false = make_symb "F"

let symb_true = make_symb "T"

let symb_or = make_symb "\\/"

let symb_and = make_symb "/\\"

let symb_ampersand = make_symb "&"

let symb_emp = make_symb "emp"

let symb_star = make_symb "*"

let symb_pointsto = make_symb "->"

let symb_eq = make_symb "="

let symb_deq = make_symb "!="

let symb_lt = make_symb "<"

let symb_leq = make_symb "<="

let symb_lp = make_symb "("

let symb_rp = make_symb ")"

let symb_lb = make_symb "{"

let symb_rb = make_symb "}"

let symb_semicolon = make_symb ";"

let symb_colon = make_symb ":"

let symb_comma = {str= ","; sep= ", "}

let symb_turnstile = make_symb "|-"

let symb_turnstile_underscore = make_symb "|-_"

let symb_ind_implies = make_symb "=>"

let symb_ind_sep = make_symb "|"

let symb_assign = make_symb ":="

let symb_bang = make_symb "!"

let symb_caret = make_symb "^"

let symb_mapsto = make_symb "|->"

let symb_final = make_symb "final"

let symb_box = make_symb "[]"

let symb_diamond = make_symb "<>"

let symb_circle = make_symb "()"

let symb_af = make_symb "AF"

let symb_ag = make_symb "AG"

let symb_ef = make_symb "EF"

let symb_eg = make_symb "EG"

let symb_next = make_symb "X"

let symb_f = make_symb "F"

let symb_g = make_symb "G"

let symb_fld_sel = make_symb "."

let symb_dot = symb_fld_sel

let keyw_exists = mk_keyw "Ex"

let keyw_final = mk_keyw "final"

let keyw_true = mk_keyw "true"

let keyw_emp = mk_keyw "emp"

let keyw_free = mk_keyw "free"

let keyw_new = mk_keyw "new"

let keyw_goto = mk_keyw "goto"

let keyw_if = mk_keyw "if"

let keyw_fi = mk_keyw "fi"

let keyw_then = mk_keyw "then"

let keyw_else = mk_keyw "else"

let keyw_stop = mk_keyw "stop"

let keyw_return = mk_keyw "return"

let keyw_nil = mk_keyw "nil"

let keyw_fields = mk_keyw "fields"

let keyw_judgement = mk_keyw "judgement"

let keyw_precondition = mk_keyw "precondition"

let keyw_postcondition = mk_keyw "postcondition"

let keyw_property = mk_keyw "property"

let keyw_proc = mk_keyw "proc"

let keyw_skip = mk_keyw "skip"

let keyw_while = mk_keyw "while"

let keyw_do = mk_keyw "do"

let keyw_od = mk_keyw "od"

let keyw_assert = mk_keyw "assert"

let parse_symb s st = (spaces >> Tokens.skip_symbol s.str >> spaces) st
