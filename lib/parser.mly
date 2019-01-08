%{
open Types
open Util
let none_to_unknown (ty_option: type_t option): type_t =
  match ty_option with
  | Some(ty) -> ty
  | None -> TY_unknown

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
%token FUNC INT_TYPE BOOL_TYPE
%token IN ARROW
%token EOF
%token COMMA COLON

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
%left MUL DIV MOD
%nonassoc LPAREN

%start <Types.expr_node_t> prog
%type <var_def_t> var_def
%type <type_t> type_spec
%type <param_def_t> param_def
%type <type_t> var_type_suffix
%type <type_t> ret_type_suffix
%%

prog:
	| e = expr; EOF { e }
	;

type_spec:
  | INT_TYPE;
    { TY_int }
  | BOOL_TYPE;
    { TY_bool }
  | LPAREN; arg_types = separated_list(COMMA, type_spec); ARROW; ret_type = type_spec; RPAREN;
    { TY_func(arg_types, ret_type) }
  ;

var_type_suffix:
  | COLON; ty = type_spec;
    { ty }

ret_type_suffix:
  | ARROW; ty = type_spec;
    { ty }

var_def:
  | id = ID; ty = var_type_suffix?; EQUALS; value_exp = expr;
    { (id, (none_to_unknown ty), value_exp) }
  ;

param_def:
  | arg_id = ID; ty = var_type_suffix?
    { (arg_id, (none_to_unknown ty)) }
  ;

expr:
 (* Note the odd use of (op; $startpos(op))--this is needed instead of
     simply $(startpos(op)) because Menhir generates an unused variable
     which generates a compiler warning which dune forces to be an
     error. *)

  (* literals *)
  | TRUE     { make_node (EXP_literal(VAL_bool(true))) $startpos}
  | FALSE    { make_node (EXP_literal(VAL_bool(false))) $startpos }
  | i = INT  { make_node (EXP_literal(VAL_int(i))) $startpos }

  (* variable reference *)
  | x = ID { make_node (EXP_var(x)) $startpos }

  (* function call *)
  | proc_expr = expr; lp = LPAREN; args = separated_list(COMMA, expr); RPAREN;
    { make_node (EXP_call(proc_expr, args)) (lp; $startpos(lp)) }

  (* precedence override *)
  | LPAREN; e = expr; RPAREN { e }

  (* binary expressions *)
  | e1 = expr; op = ADD; e2 = expr
    { make_node (EXP_binary(OP_add, e1, e2)) (op; $startpos(op)) }
  | e1 = expr; op = SUB; e2 = expr
    { make_node (EXP_binary(OP_sub, e1, e2)) (op; $startpos(op)) }
  | e1 = expr; op = MUL; e2 = expr
    { make_node (EXP_binary(OP_mul, e1, e2)) (op; $startpos(op)) }
  | e1 = expr; op = DIV; e2 = expr
    { make_node (EXP_binary(OP_div, e1, e2)) (op; $startpos(op)) }
  | e1 = expr; op = MOD; e2 = expr
    { make_node (EXP_binary(OP_mod, e1, e2)) (op; $startpos(op)) }
  | e1 = expr; op = EQUALS; e2 = expr
    { make_node (EXP_binary(OP_eq, e1, e2)) (op; $startpos(op)) }
  | e1 = expr; op = GT; e2 = expr
    { make_node (EXP_binary(OP_gt, e1, e2)) (op; $startpos(op)) }
  | e1 = expr; op = GTE; e2 = expr
    { make_node (EXP_binary(OP_gte, e1, e2)) (op; $startpos(op)) }
  | e1 = expr; op = LT; e2 = expr
    { make_node (EXP_binary(OP_lt, e1, e2)) (op; $startpos(op)) }
  | e1 = expr; op = LTE; e2 = expr
    { make_node (EXP_binary(OP_lte, e1, e2)) (op; $startpos(op)) }
  | e1 = expr; op = LAND; e2 = expr
    { make_node (EXP_logical(LOP_and, e1, e2)) (op; $startpos(op)) }
  | e1 = expr; op = LOR; e2 = expr
    { make_node (EXP_logical(LOP_or, e1, e2)) (op; $startpos(op)) }

  (* if expression *)
  | IF; cond_exp = expr; THEN; then_exp = expr; ELSE; else_exp = expr;
    { make_node (EXP_if (cond_exp, then_exp, else_exp)) $startpos }

  (* let & let rec expressions *)
  | LET; id = ID; ty = var_type_suffix?; EQUALS; value_exp = expr; IN; body_exp = expr
    { make_node (EXP_let ((id, (none_to_unknown ty), value_exp), body_exp)) $startpos }
  | LET; REC; var_decls = separated_nonempty_list(AND, var_def); IN; body_exp = expr
    { make_node (EXP_let_rec (var_decls, body_exp)) $startpos }

  (* function constructor expression *)
  | FUNC; LPAREN; func_type = separated_list(COMMA, param_def); ret_type = ret_type_suffix?; RPAREN; ARROW; body = expr;
    { make_node (EXP_func(func_type, (none_to_unknown ret_type), body)) $startpos }
  ;
