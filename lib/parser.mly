%{
open Types
open Util
%}

%token <int> INT
%token <string> ID
%token PLUS
%token LPAREN
%token RPAREN
%token LET
%token FUNC
%token EQUALS
%token IN
%token EOF
%token OPEN_CURLY CLOSE_CURLY

%nonassoc IN
%left PLUS
%nonassoc LPAREN
%start <Types.expr_t> prog

%%

prog:
	| e = expr; EOF { e }
	;

expr:
  | i = INT
    { make_node (EXPN_literal(VAL_i32(i))) $startpos }
  | x = ID
    { make_node (EXPN_var(x)) $startpos }
  | proc_expr = expr; LPAREN; arg = expr; RPAREN;
     { make_node (EXPN_call(proc_expr, arg)) $startpos }
  | LPAREN; e = expr; RPAREN
     { e }
  | e1 = expr; PLUS; e2 = expr
    { make_node (EXPN_add(e1, e2)) $startpos }
  | LET; id = ID; EQUALS; value_exp = expr; IN; body_exp = expr
    { make_node (EXPN_let (id, value_exp, body_exp)) $startpos }
   (*TODO: is it possible to *not* require the func body to be wrapped in { }?  Without them
    there is a shift/reduce conflict I can't seem to solve, but that might be because I'm
    a noob.
    *)
  | FUNC; LPAREN; var_name = ID; RPAREN; OPEN_CURLY; body = expr; CLOSE_CURLY;
    { make_node (EXPN_func(var_name, body)) $startpos }
  ;
