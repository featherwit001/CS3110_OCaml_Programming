%token <int> INT
%token TIMES
%token PLUS
%token LPAREN
%token RPAREN
%token EOF

// %right PLUS
%left PLUS  /* lower precedence */
%left TIMES /* higher precedence */

%start <Ast.expr> prog
%type <Ast.expr> expr
%%

prog :
	| e = expr; EOF { e }
	;

expr:
	| i = INT { Int i }
	| e1 = expr; TIMES; e2 = expr  { Binop (Mult, e1, e2) }
	| e1 = expr; PLUS; e2 = expr  { Binop (Add, e1, e2) }
	| LPAREN; e = expr; RPAREN { e }
	;