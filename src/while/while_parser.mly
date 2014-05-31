%{
open Lib
open Symbols
 
module S = Symheap
module P = While_program
  
let pred_cnt = ref 0
  
%}

%token EMP NIL COMMA CARET
%token <string> IDENT EIDENT
%token <int> NUM
%token LP RP LB RB
%token OR STAR 
%token POINTS_TO EQ DEQ

%token IND_IMPLIES IND_SEP
%token SEMICOLON COLON 
%token FIELDS PRECONDITION 
%token FLD_SEL
%token FREE NEW IF FI THEN STOP ASSIGN SKIP WHILE DO OD ELSE
%token EOF

%start ind_def_set
%type <Symheap.Defs.t> ind_def_set

%start program
%type <Symheap.Form.t * While_program.Cmd.t> program
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

heap: ps = separated_nonempty_list(STAR, atom) 
  { List.fold_left S.Heap.star S.Heap.empty ps}

formula: fs = separated_nonempty_list(OR, heap) { fs }

/* defs stuff */
ind_case: f = heap; IND_IMPLIES; id = IDENT; ts = paren_terms 
  { S.Case.mk f (id,ts) }

ind_cases: inds = separated_nonempty_list(IND_SEP, ind_case) { inds }

ind_def: p = IDENT; inds = delimited(LB, ind_cases, RB) { (inds, p) }

ind_def_set: ids = separated_nonempty_list(SEMICOLON, ind_def); EOF { ids }

/* program stuff */
fields: FIELDS; COLON; ils = separated_nonempty_list(COMMA, IDENT); SEMICOLON 
    { List.iter P.Field.add ils }

precondition: PRECONDITION; COLON; f = formula; SEMICOLON { f }

condition:
	| t1 = term; EQ; t2 = term { P.Cond.mk_eq t1 t2 }
	|	t1 = term; DEQ; t2 = term { P.Cond.mk_deq t1 t2 }
	| STAR { P.Cond.mk_non_det () }

basic_command:
  | STOP { P.Cmd.mk_stop } 
  | SKIP { P.Cmd.mk_skip }
	| v = var; ASSIGN; t = term { P.Cmd.mk_assign v t }
	| v1 = var; ASSIGN; v2 = var; FLD_SEL; fld = IDENT 
    { P.Cmd.mk_load v1 v2 fld }
	| v = var; FLD_SEL; fld = IDENT; ASSIGN; t = term 
    { P.Cmd.mk_store v fld t }
	| FREE; ts = paren_terms
    { assert (List.length ts = 1) ; P.Cmd.mk_free (List.hd ts) }
	| v = var; ASSIGN; NEW; LP; RP { P.Cmd.mk_new v }
  | IF; cond = condition; THEN; cmd = command; FI { P.Cmd.mk_if cond cmd }
  | IF; cond = condition; THEN; cmd1 = command; ELSE; cmd2 = command; FI { P.Cmd.mk_ifelse cond cmd1 cmd2 }
  | WHILE; cond = condition; DO; cmd = command; OD { P.Cmd.mk_while cond cmd }

command:
  | cmds = separated_nonempty_list(SEMICOLON, basic_command) { P.Cmd.mk_from_list cmds }

program:
	fields; p = precondition; cmd = command; EOF { (p, cmd) }
%%


