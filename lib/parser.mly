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
%token ADD SUB MUL DIV MOD EQUALS GT GTE LT LTE
%token LAND LOR (* logical and and logical or *)
%token LPAREN RPAREN
%token LET REC AND
%token IF THEN ELSE
%token FUNC
%token IN ARROW
%token EOF
%token COMMA

(****
  The order of tokens listed here specifies their precedence.
  Tokens appearing later have a higher precedence than those appearing earlier.
 ****)

%nonassoc IN ELSE ARROW
%left LOR
%left LAND
%left GT GTE LT LTE
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
%type <var_def_t> var_def
%%

prog:
	| e = expr; EOF { e }
	;

var_def:
  | id = ID; EQUALS; value_exp = expr;
    { (id, value_exp) }
  ;

expr:
 (* Note the odd use of (op; $startpos(op))--this is needed instead of
     simply $(startpos(op)) because Menhir generates an unused variable
     which generates a compiler warning which dune forces to be an
     error. *)

  (* literals *)
  | TRUE     { make_node (EXPN_literal(VAL_bool(true))) $startpos}
  | FALSE    { make_node (EXPN_literal(VAL_bool(false))) $startpos }
  | i = INT  { make_node (EXPN_literal(VAL_i32(i))) $startpos }

  (* variable reference *)
  | x = ID { make_node (EXPN_var(x)) $startpos }

  (* function call *)
  | proc_expr = expr; lp = LPAREN; args = separated_list(COMMA, expr); RPAREN;
    { make_node (EXPN_call(proc_expr, args)) (lp; $startpos(lp)) }

  (* precedence override *)
  | LPAREN; e = expr; RPAREN { e }

  (* binary expressions *)
  | e1 = expr; op = ADD; e2 = expr
    { make_node (EXPN_binary(OP_add, e1, e2)) (op; $startpos(op)) }
  | e1 = expr; op = SUB; e2 = expr
    { make_node (EXPN_binary(OP_sub, e1, e2)) (op; $startpos(op)) }
  | e1 = expr; op = MUL; e2 = expr
    { make_node (EXPN_binary(OP_mul, e1, e2)) (op; $startpos(op)) }
  | e1 = expr; op = DIV; e2 = expr
    { make_node (EXPN_binary(OP_div, e1, e2)) (op; $startpos(op)) }
  | e1 = expr; op = MOD; e2 = expr
    { make_node (EXPN_binary(OP_mod, e1, e2)) (op; $startpos(op)) }
  | e1 = expr; op = EQUALS; e2 = expr
    { make_node (EXPN_binary(OP_eq, e1, e2)) (op; $startpos(op)) }
  | e1 = expr; op = GT; e2 = expr
    { make_node (EXPN_binary(OP_gt, e1, e2)) (op; $startpos(op)) }
  | e1 = expr; op = GTE; e2 = expr
    { make_node (EXPN_binary(OP_gte, e1, e2)) (op; $startpos(op)) }
  | e1 = expr; op = LT; e2 = expr
    { make_node (EXPN_binary(OP_lt, e1, e2)) (op; $startpos(op)) }
  | e1 = expr; op = LTE; e2 = expr
    { make_node (EXPN_binary(OP_lte, e1, e2)) (op; $startpos(op)) }
  | e1 = expr; op = LAND; e2 = expr
    { make_node (EXPN_logical(LOP_and, e1, e2)) (op; $startpos(op)) }
  | e1 = expr; op = LOR; e2 = expr
    { make_node (EXPN_logical(LOP_or, e1, e2)) (op; $startpos(op)) }

  (* if expression *)
  | IF; cond_exp = expr; THEN; then_exp = expr; ELSE; else_exp = expr;
    { make_node (EXPN_if (cond_exp, then_exp, else_exp)) $startpos }

  (* let & let rec expressions *)
  | LET; id = ID; EQUALS; value_exp = expr; IN; body_exp = expr
    { make_node (EXPN_let (id, value_exp, body_exp)) $startpos }
  | LET; REC; var_decls = separated_nonempty_list(AND, var_def);
    IN; body_exp = expr
    { make_node (EXPN_let_rec (var_decls, body_exp)) $startpos }

  (* function constructor expression *)
  | FUNC; param_names = list(ID); ARROW; body = expr;
    { make_node (EXPN_func(param_names, body)) $startpos }
  ;
