let ltx_mk_math l = Latex.mode Latex.M l
let ltx_mk_text l = Latex.mode Latex.T l
let ltx_math s = ltx_mk_math (Latex.text s)
let ltx_text s = ltx_mk_text (Latex.text s)
let ltx_newl = Latex.text "\n"

type symbol = { str: string; sep:string; melt:Latex.t }

(* str fields *cannot* have whitespace *)
(* surrounding them as this trips the lexer *)

let make_symb s m = { str=s; sep=" " ^ s ^ " "; melt=m }
let mk_keyw s =
  {
    str=s;
    sep=" " ^ s ^ " ";
    melt=ltx_mk_math (Latex.texttt (Latex.text s))
  }

let symb_false = make_symb "F" Latex.bot
let symb_true = make_symb "T" Latex.top
let symb_or = make_symb "\\/" Latex.lor_
let symb_and = make_symb "/\\" Latex.land_
let symb_ampersand = make_symb "&" Latex.empty (* FIXME *)
let symb_emp = make_symb "emp" (Latex.texttt (Latex.text "emp"))
let symb_star = make_symb "*" Latex.ast
let symb_pointsto = make_symb "->" Latex.mapsto
let symb_eq = make_symb "=" (ltx_math " = ")
let symb_deq = make_symb "!=" Latex.neq
let symb_lp = make_symb "(" (Latex.text "(")
let symb_rp = make_symb ")" (Latex.text ")")
let symb_lb = make_symb "{" (Latex.text "\\{")
let symb_rb = make_symb "}" (Latex.text "\\}")
let symb_semicolon = make_symb ";" (Latex.text ";")
let symb_colon = make_symb ":" Latex.empty
let symb_comma = { str=","; sep=", "; melt=Latex.text ", " }
let symb_turnstile = make_symb "|-" Latex.vdash
let symb_turnstile_underscore = make_symb "|-_" Latex.empty (* FIXME *)
(* let symb_dturnstile_underscore = make_symb "||-_" Latex.empty (* FIXME *) *)
let symb_ind_implies = make_symb "=>" Latex.empty
let symb_ind_sep = make_symb "|" Latex.empty
let symb_assign = make_symb ":=" (ltx_math ":=")
let symb_bang = make_symb "!" Latex.empty
let symb_caret = make_symb "^" Latex.empty
(* let symb_underscore = make_symb "_" " " Latex.empty (* FIXME *) *)
(* let symb_box = make_symb "[]" " \\Box" Latex.empty (* FIXME *) *)
(* let symb_diamond = make_symb "<>" " \\Diamond " Latex.empty    *)
(* let symb_circle = make_symb "()" " \\bigcirc " Latex.empty     *)
let symb_fld_sel = make_symb "." (Latex.text ".")

let keyw_emp = mk_keyw "emp"
let keyw_free = mk_keyw "free"
let keyw_new = mk_keyw "new"
let keyw_goto = mk_keyw "goto"
let keyw_if = mk_keyw "if"
let keyw_fi = mk_keyw "fi"
let keyw_then = mk_keyw "then"
let keyw_else = mk_keyw "else"
let keyw_stop = mk_keyw "stop"
let keyw_nil = mk_keyw "nil"
let keyw_fields = mk_keyw "fields"
let keyw_judgement = mk_keyw "judgement"
let keyw_precondition = mk_keyw "precondition"
let keyw_postcondition = mk_keyw "postcondition"
let keyw_proc = mk_keyw "proc"
let keyw_skip = mk_keyw "skip"
let keyw_while = mk_keyw "while"
let keyw_do = mk_keyw "do"
let keyw_od = mk_keyw "od"

let ltx_paren l = Latex.concat [symb_lp.melt; l; symb_rp.melt]
let ltx_comma l = Latex.concat (Latex.list_insert symb_comma.melt l)
let ltx_star l = Latex.concat (Latex.list_insert symb_star.melt l)
let ltx_math_space = Latex.text "\\;"

open MParser
let parse_symb s st = Tokens.skip_symbol s.str st

let max_tag = ref 0
let upd_tag tag = 
  max_tag := max !max_tag tag ; tag
let next_tag () =
  incr max_tag ; !max_tag 

let parse_tag st = 
  ( parse_symb symb_caret >> 
    Tokens.integer |>> (fun tag -> assert (tag>0) ; tag) <?> "tag") st
