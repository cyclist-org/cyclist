{
open While_parser
open Symbols
exception Error of string

let lex_error lexbuf =
  raise 
    (Error 
      (Printf.sprintf 
        "At line %d, column %d: unexpected character." 
        (lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum)
        (lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum - 
         lexbuf.Lexing.lex_curr_p.Lexing.pos_bol) 
        )) 

let keywords = 
  [
    (keyw_emp.str, EMP);
    (keyw_free.str, FREE);
    (keyw_new.str, NEW);
    (keyw_if.str, IF);
    (keyw_fi.str, FI);
    (keyw_then.str, THEN);
    (keyw_stop.str, STOP);
    (keyw_nil.str, NIL); 
    (keyw_fields.str, FIELDS);
    (keyw_precondition.str, PRECONDITION);
    (keyw_skip.str, SKIP);
    (keyw_else.str, ELSE);
    (keyw_while.str, WHILE);
    (keyw_do.str, DO);
    (keyw_od.str, OD);
  ]

let symbols =
  [ 
    (symb_or.str, OR); 
    (symb_star.str, STAR); 
    (symb_pointsto.str, POINTS_TO); 
    (symb_eq.str, EQ); 
    (symb_deq.str, DEQ); 
    (symb_lp.str, LP); 
    (symb_rp.str, RP); 
    (symb_lb.str, LB);
    (symb_rb.str, RB);
    (symb_semicolon.str, SEMICOLON);
    (symb_colon.str, COLON);
    (symb_comma.str, COMMA); 
    (* (symb_turnstile.str, TURNSTILE);  *)
    (symb_ind_implies.str, IND_IMPLIES);
    (symb_ind_sep.str, IND_SEP);
    (symb_assign.str, ASSIGN);
    (* (symb_bang.str, BANG);                *)
    (symb_underscore.str, UNDERSCORE);
    (symb_fld_sel.str, FLD_SEL);
  ]

let parse_string lexbuf = 
  try
    List.assoc (Lexing.lexeme lexbuf) symbols
  with Not_found -> lex_error lexbuf

}

let ident = ['a'-'z' 'A'-'Z']+'''?
let onechar_symb = [ '*' '-' '+' '=' '!' '(' ')' '{' '}' ';' ':' ',' '|' '_' '[' ']' '.' ]
let twochar_symb = "\\/" | "|-" | "=>" | ":=" | "->" | "!=" 
let threechar_symb = "|-_" 
let fourchar_symb = "||-_" 
 
rule token = parse
  | [' ' '\t' ]+ { token lexbuf }
  | '\n' { Lexing.new_line lexbuf ; token lexbuf }
  | ident 
    {
      let s = Lexing.lexeme lexbuf in
      try
        List.assoc s keywords 
      with Not_found -> 
        if Var.is_exist_name s then EIDENT s else IDENT s
    }
  | onechar_symb { parse_string lexbuf }
  | twochar_symb { parse_string lexbuf }
  | threechar_symb { parse_string lexbuf }
  | fourchar_symb { parse_string lexbuf }
  | ['0'-'9']+ as n { NUM (int_of_string n) }
  | eof { EOF }
  | _ { lex_error lexbuf }

