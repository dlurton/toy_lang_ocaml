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
%token LET
%token IF THEN ELSE
%token FUNC
%token IN
%token EOF
%token OPEN_CURLY CLOSE_CURLY

(****
  The order of tokens listed here specifies their precedence.
  Tokens appearing later have a higher precedence than those appearing earlier.
 ****)

%nonassoc IN ELSE
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

  (* let expression *)
  | LET; id = ID; EQUALS; value_exp = expr; IN; body_exp = expr
    { make_node (EXPN_let (id, value_exp, body_exp)) $startpos }
   (*TODO: is it possible to *not* require the func body to be wrapped in { }?  Without them
    there is a shift/reduce conflict I can't seem to solve, but that might be because I'm
    a noob.
    *)

  (* function constructor expression *)
  | FUNC; LPAREN; var_name = ID; RPAREN; OPEN_CURLY; body = expr; CLOSE_CURLY;
    { make_node (EXPN_func(var_name, body)) $startpos }
  ;
