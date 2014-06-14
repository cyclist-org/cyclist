{
	open Fo_parser
  exception Error of string
}

rule token = parse
  | [' ' '\t' '\n'] { token lexbuf }
  | "true" { TRUE }
	| "\\/" { OR }
	| '&' { AND }
	| '=' { EQ }
	| "!=" { DEQ }
	| '(' { LP }
	| ')' { RP }
	| '{' { LB }
	| '}' { RB }
	| ';' { SEMICOLON }
	| ',' { COMMA }
	| "|-" { TURNSTILE }
	| "=>" { IND_IMPLIES }
	| '|' { IND_SEP }
	| '_' { UNDERSCORE }
  | '\'' { APOSTROPHE }
  | ['0'-'9']+ as n { NUM (int_of_string n) }
  | ['a'-'z' 'A'-'Z']+ as s { IDENT s }
	| eof { EOF }
  | _ 
    { raise 
      (Error 
        (Printf.sprintf 
          "At offset %d: unexpected character.\n" 
          (Lexing.lexeme_start lexbuf))) 
    }

{

}