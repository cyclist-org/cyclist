%{
open Lib
module FO = Firstorder
%}

%token <int> NUM
%token <string> IDENT
%token OR
%token AND
%token EQ
%token DEQ
%token LP
%token RP
%token LB
%token RB
%token COMMA
%token TURNSTILE
%token IND_IMPLIES
%token IND_SEP
%token SEMICOLON
%token UNDERSCORE
%token TRUE
%token APOSTROPHE
%token EOF

%start sequent
%type <Firstorder.Seq.t> sequent 

%start ind_def_set
%type <Firstorder.Defs.t> ind_def_set
%%

/* FO sequents stuff */
term:
	| n = NUM { FO.Term.mk_const n }
  | a = IDENT { FO.Term.mk_univ_var a }
	| e = IDENT ; APOSTROPHE { FO.Term.mk_exist_var (e^"'") }
	| f = IDENT; ts = terms { FO.Term.mk_fun f ts }

terms:
  | LP; ts = separated_nonempty_list(COMMA, term); RP { ts }

atom:
  | t1 = term; EQ; t2 = term { FO.Atom.mk_eq t1 t2 }
  | p = IDENT; UNDERSCORE; n = NUM; ts = terms { FO.Atom.mk_ipred n p ts }
	| t1 = term; DEQ; t2 = term { FO.Atom.mk_deq t1 t2 }

product:
	| TRUE { FO.Prod.empty }
	| p = separated_nonempty_list(AND, atom) { FO.Prod.of_list p }

formula:
	| f = separated_nonempty_list(OR, product) { FO.Form.of_list f }

sequent: f1 = formula; TURNSTILE; f2 = formula; EOF { (f1, f2) }

/* defs stuff */
ind_case:
	| prod = product; IND_IMPLIES; pred = IDENT; ts = terms 
  { (FO.Case.mk prod ts, pred) }
 
ind_cases:
	| LB; inds = separated_nonempty_list(IND_SEP, ind_case); RB 
  { inds }

ind_def:
	| p = IDENT; inds = ind_cases { (inds, p) }

ind_def_set: 
	| ids = separated_nonempty_list(SEMICOLON, ind_def); EOF 
  {
    let f defs (c,ident)= FO.Defs.add ident c defs in
    let g defs cases = List.fold_left f defs cases in
    let h defs (cases,_) = g defs cases in
    List.fold_left h FO.Defs.empty ids
  }

%%
