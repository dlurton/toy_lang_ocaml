%{
open Types
open Util
%}

(*
TODO: I recall seeing somewhere these tokens having their definition as well as declaration here too.
Can this be utilized to reduce the complexity of lexer.mll or eliminate it entirely?
It looked something like: `%token LET "let"`
 *)
%token <int> INT
%token <string> ID
%token TRUE FALSE
%token ADD SUB MUL DIV MOD EQUALS
%token LPAREN RPAREN
%token LET REC
%token IF THEN ELSE
%token FUNC
%token IN ARROW
%token EOF

(****
  The order of tokens listed here specifies their precedence.
  Tokens appearing later have a higher precedence than those appearing earlier.
 ****)

%nonassoc IN ELSE ARROW
%left EQUALS
%left ADD SUB
(*
  TODO: verify that MUL, DIV & MOD operators below should have left associativity,
  which seems right to me but I read somewhere recently that they should
  have right associativity instead.
*)
%left MUL DIV MOD
%nonassoc LPAREN

%start <Types.expr_t> prog

%%

prog:
	| e = expr; EOF { e }
	;

expr:

  (* literals *)
  | TRUE     { make_node (EXPN_literal(VAL_bool(true))) $startpos}
  | FALSE    { make_node (EXPN_literal(VAL_bool(false))) $startpos }
  | i = INT  { make_node (EXPN_literal(VAL_i32(i))) $startpos }

  (* variable reference *)
  | x = ID { make_node (EXPN_var(x)) $startpos }

  (* function call *)
  | proc_expr = expr; LPAREN; arg = expr; RPAREN;
    { make_node (EXPN_call(proc_expr, arg)) $startpos }

  (* precedence override *)
  | LPAREN; e = expr; RPAREN { e }

  (* binary expressions *)
  | e1 = expr; ADD; e2 = expr
    { make_node (EXPN_binary(OP_add, e1, e2)) $startpos }
  | e1 = expr; SUB; e2 = expr
    { make_node (EXPN_binary(OP_sub, e1, e2)) $startpos }
  | e1 = expr; MUL; e2 = expr
    { make_node (EXPN_binary(OP_mul, e1, e2)) $startpos }
  | e1 = expr; DIV; e2 = expr
    { make_node (EXPN_binary(OP_div, e1, e2)) $startpos }
  | e1 = expr; MOD; e2 = expr
    { make_node (EXPN_binary(OP_mod, e1, e2)) $startpos }
  | e1 = expr; EQUALS; e2 = expr
    { make_node (EXPN_binary(OP_equals, e1, e2)) $startpos }

  (* if expression *)
  | IF; cond_exp = expr; THEN; then_exp = expr; ELSE; else_exp = expr;
    { make_node (EXPN_if (cond_exp, then_exp, else_exp)) $startpos }

  (* let & let rec expressions *)
  | LET; id = ID; EQUALS; value_exp = expr; IN; body_exp = expr
    { make_node (EXPN_let (id, false, value_exp, body_exp)) $startpos }
  | LET; REC; id = ID; EQUALS; value_exp = expr; IN; body_exp = expr
    { make_node (EXPN_let (id, true, value_exp, body_exp)) $startpos }

  (* function constructor expression *)
  | FUNC; var_name = ID; ARROW; body = expr;
    { make_node (EXPN_func(var_name, body)) $startpos }
  ;
