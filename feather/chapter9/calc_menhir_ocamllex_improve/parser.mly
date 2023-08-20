%token <int> INT
%token <float> FLOAT
%token TIMES
%token PLUS
%token DIV
%token MINUS
%token LPAREN
%token RPAREN
%token EOF

// %right PLUS
%left PLUS MINUS /* lowest precedence */
%left TIMES DIV /* medium precedence */
%nonassoc UMINUS /* highest precedence*/

%start <Ast.expr> prog
%type <Ast.expr> expr
%%

prog :
	| e = expr; EOF { e }
	;

expr:
	| f = FLOAT { Float f }
	| i = INT { Int i }
	| e1 = expr; TIMES; e2 = expr  { Binop (Mul, e1, e2) }
	| e1 = expr; PLUS; e2 = expr  { Binop (Add, e1, e2) }
	| e1 = expr; MINUS; e2 = expr  { Binop (MINUS, e1, e2) }
	| e1 = expr; DIV; e2 = expr  { Binop (DIV, e1, e2) }
	| LPAREN; e = expr; RPAREN { e }
	| MINUS; f = FLOAT; %prec UMINUS { Float (-.f) }
	| MINUS; i = INT; %prec UMINUS { Int (-i) }
	;