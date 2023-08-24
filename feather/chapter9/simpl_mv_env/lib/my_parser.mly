%{
open Ast


(** [make_apply e [e1; e2; ...]] makes the application  
    [e e1 e2 ...]).  Requires: the list argument is non-empty. *)
let rec make_apply e = function
  | [] -> failwith "precondition violated"
  | [e'] -> App (e, e')
  | h :: ((_ :: _) as t) -> make_apply (App (e, h)) t
%}

%token <int> INT
%token <string> ID
%token TRUE FALSE
%token LEQ
%token TIMES  
%token PLUS
%token LPAREN RPAREN
%token LET EQUALS IN
%token IF THEN ELSE
%token FUN RARROW
%token SEMICOLON DSEMICOLON
%token EOF
// to do
%token COLON  
%token INT_TYPE BOOL_TYPE

%nonassoc IN
%nonassoc ELSE

%left LEQ   /* lower precedence */
%left PLUS  
%left TIMES /* higher precedence */

%start <Ast.expr> prog
%type <Ast.expr> expr
%%

prog :
	| e = expr; EOF { e }
	;

expr:
	| e = expr; DSEMICOLON { e }
	| e = simpl_expr { e }
	| e = simpl_expr; es = simpl_expr+ { make_apply e es }
	| e1 = expr; LEQ; e2 = expr { Binop (Leq, e1, e2) }
	| e1 = expr; TIMES; e2 = expr  { Binop (Mult, e1, e2) }
	| e1 = expr; PLUS; e2 = expr  { Binop (Add, e1, e2) }
	| LET; x = ID; COLON; t = typ;  EQUALS; e1 = expr; IN; e2 = expr;
					{ Let (x, t, e1, e2) }   
	| IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If (e1, e2, e3) }
	| FUN; x = ID; RARROW; e = expr { Fun (x, e) }
	//  to do let x = 0 without in ;; 
	// and  fun x y .. -> expr (multiple parameters)
	// (e1, e2) fst snd  impl tuple
	// match e with Left x1 -> e1 | Right x2 -> e2
	// type checking type inferring
	;

simpl_expr:
	| i = INT { Int i }
	| x = ID { Var x }
	| TRUE { Bool true }
	| FALSE { Bool false }
	| LPAREN; e = expr; RPAREN { e }	
	;

typ:
	| INT_TYPE { TInt }
	| BOOL_TYPE { TBool}