%token <int> INT
%token PLUS
%token TIMES
%token LPAREN
%token RPAREN
%token EOF

// %right PLUS
%left PLUS  /* lower precedence */
%left TIMES /* higher precedence */

%start prog   /* the entry point */
%type <Ast.expr> prog

%%

prog :
	| expr EOF { $1 }
	;

expr:
	| INT { Int $1 }
	| expr TIMES expr  { Binop (Mult, $1, $3) }
	| expr PLUS expr  { Binop (Add, $1, $3) }
	| LPAREN expr RPAREN { $2 }
	;



// %token <int> INT
// %token EOF

// //intro
// %start prog
// %type <unit> prog

// %start <Ast.expr> prog

// prog:
//   | e = expr; EOF { e }
//   ;

// expr:
//   | i = INT { Int i }
//   ;