%{
open Lib
open Symbols
 
module S = Symheap
module P = Goto_program
(* module TL = Tempform *)
  
let pred_cnt = ref 0
  
%}

%token EMP NIL COMMA CARET
%token <string> IDENT EIDENT
%token <int> NUM
%token LP RP LB RB
%token OR STAR 
/* AND */
%token POINTS_TO EQ DEQ
/* %token BOX DIAMOND CIRCLE */

%token TURNSTILE TURNSTILE_ 
/* DTURNSTILE_ */
%token IND_IMPLIES IND_SEP
%token SEMICOLON COLON BANG
%token FIELDS JUDGEMENT FREE NEW GOTO IF STOP ASSIGN SKIP
%token EOF

/* %left OR */
/* %left AND *./
/* %nonassoc BOX DIAMOND CIRCLE */

%start sequent
%type <Sl_heap.Seq.t> sequent 

%start formula
%type <Sl_heap.Form.t> formula 

%start ind_def_set
%type <Sl_heap.Defs.t> ind_def_set

%start program
%type <Goto_program.Seq.t * Goto_program.program_t> program


/*
%start tl_formula
%type <Tempform.Form.t> tl_formula

%start tl_sequent
%type <Tempform.Seq.t> tl_sequent

%start tl_program
%type <Tempform.Seq.t * Program.program_t> tl_program
*/
%%

/* SL sequents stuff */
avar: a = IDENT { S.Term.mk_univ_var a }
evar: e = EIDENT { S.Term.mk_exist_var e }
const: NIL { S.Term.nil }

var:
  | a = avar { a }
  | e = evar { e }
 
term:
  | v = var { v }
  | c = const { c }

terms:  ts = separated_nonempty_list(COMMA, term) { ts }
paren_terms: ts = delimited(LP, terms, RP) { ts }

atom: 
  | EMP { S.Heap.empty }
  | t1 = term; EQ; t2 = term { S.Heap.mk_eq t1 t2 }
  | t1 = term; DEQ; t2 = term { S.Heap.mk_deq t1 t2 }
  | t1 = term; POINTS_TO; ts = terms { S.Heap.mk_pto t1 ts }
  | id = IDENT; ts = paren_terms 
    { S.Heap.mk_ind (incr pred_cnt; !pred_cnt) id ts }
  | id = IDENT; CARET; n = NUM; ts = paren_terms 
    { pred_cnt := max !pred_cnt n ; S.Heap.mk_ind n id ts }

product: ps = separated_nonempty_list(STAR, atom) 
  { List.fold_left S.Heap.star S.Heap.empty ps}

formula_: fs = separated_nonempty_list(OR, product) { fs }

formula: f = formula_; EOF { f }

sequent_: p = separated_pair(formula_, TURNSTILE, formula_) { pred_cnt:=0 ; p }

sequent: s = sequent_; EOF { s }

/* defs stuff */
ind_case: f = product; IND_IMPLIES; id = IDENT; ts = paren_terms 
  { S.Case.mk f (id,ts) }

ind_cases: inds = separated_nonempty_list(IND_SEP, ind_case) { inds }

ind_def: p = IDENT; inds = delimited(LB, ind_cases, RB) { (inds, p) }

ind_def_set: ids = separated_nonempty_list(SEMICOLON, ind_def); EOF { ids }

/* program stuff */
condition:
	| t1 = term; EQ; t2 = term { P.Cmd.mk_eq t1 t2 }
	|	t1 = term; DEQ; t2 = term { P.Cmd.mk_deq t1 t2 }
	| STAR { P.Cmd.mk_non_det () }

command:
	| v = var; ASSIGN; t = term { P.Cmd.mk_assign v t }
	| v1 = var; ASSIGN; v2 = var; POINTS_TO; id = IDENT { P.Cmd.mk_load v1 v2 id }
	| v = var; POINTS_TO; id = IDENT; ASSIGN; t = term { P.Cmd.mk_store v id t }
	| FREE; ts = paren_terms
    { assert (List.length ts = 1) ; P.Cmd.mk_free (List.hd ts) }
	| v = var; ASSIGN; NEW; LP; RP { P.Cmd.mk_new v }
	| GOTO; n = NUM { P.Cmd.mk_goto n }
	| IF; c = condition; GOTO; n = NUM { P.Cmd.mk_if c n }
	| STOP { P.Cmd.mk_stop } 
  | SKIP { P.Cmd.mk_skip }

lcommand: p = separated_pair(NUM, COLON, command) { p } 

lcommands: lcmds = separated_nonempty_list(SEMICOLON, lcommand) { lcmds }

fields: FIELDS; COLON; ils = separated_nonempty_list(COMMA, IDENT); SEMICOLON 
	{ Blist.rev (Blist.combine ils (Blist.range 0 ils)) }


judgement: JUDGEMENT; COLON; f = formula_; TURNSTILE_; n = NUM; BANG { (f, n) }  

program:
	f = fields; j = judgement; l = lcommands; EOF { (j, (f, l)) }
	
/*
tl_formula_:
  | f = product { TL.Form.mk_atom f }
  | LP; f = tl_formula_; RP { f }
  | CIRCLE; f = tl_formula_; { TL.Form.mk_circle f }
  | DIAMOND; f = tl_formula_; { TL.Form.mk_diamond f }
  | BOX; f = tl_formula_; { TL.Form.mk_box (incr pred_cnt; !pred_cnt) f }
  | a = tl_formula_; AND; b = tl_formula_
    { TL.Form.mk_and (TL.FormSet.add b (TL.FormSet.singleton a)) }
  | a = tl_formula_; OR; b = tl_formula_
    { TL.Form.mk_or (TL.FormSet.add b (TL.FormSet.singleton a)) }

tl_formula: f = tl_formula_; EOF { f }

tl_sequent_: l = formula_; DTURNSTILE_; i = NUM; r = tl_formula_ 
  { (l,i, TL.Form.mk_or (TL.FormSet.singleton r)) }

tl_sequent: s = tl_sequent_; EOF { s }
  
tl_judgement: JUDGEMENT; COLON; s = tl_sequent_ { s }  

tl_program:
  f = fields; j = tl_judgement; l = lcommands; EOF { (j,(f,l)) }
*/
%%

