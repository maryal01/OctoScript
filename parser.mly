/* Ocamlyacc parser for OctoScript */

%{
open Ast
%}

%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK 
%token COMMA SEMI DOT
%token PLUS MINUS TIMES DIVIDE POW LOG ASSIGN 
%token NOT EQ NEQ LT LEQ GT GEQ AND OR
%token FARROW LARROW
%token RETURN IF ELSE WHILE PRINT BREAK
%token INT BOOL FLOAT NONE STRING LAMBDA 
%token TABLE LIST TUPLE
%token <int> LITERAL
%token <bool> BLIT
%token <string> ID
%token <float> FLIT
%token EOF

%start program
%type <Ast.program> program

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT

%%

program:
  decls EOF { $1 }

decls:
   /* nothing */ { []  }
 | decls statement_list  { $1 }
 
fdecl:
    ID LPAREN formals_opt RPAREN FARROW rtype LBRACE statement_list RBRACE
        { ($1, $3, $6, List.rev $8) }

formals_opt:
   /* nothing */  { [] }
  | formal_list   { $1 }

formal_list:
   rtype ID                   { [($1, $2)]     }
 | formal_list COMMA rtype ID { ($3, $4) :: $1 }


rtype_list:
    rtype                { [$1] }
  | rtype_list COMMA rtype { $3 :: $1 }


statement_list:
    /* nothing */  { [] }
  | statement_list statement { $2 :: $1 }

statement:
    expr SEMI                                                { Expr $1               }
  | RETURN expr_opt SEMI                                     { Return $2             }
  | IF LPAREN expr RPAREN statement_list %prec NOELSE        { If($3, $5, [])        }
  | IF LPAREN expr RPAREN statement_list ELSE statement_list { If($3, $5, $7)        }
  | WHILE LPAREN expr RPAREN statement_list                  { While($3, $5)         }
  | BREAK SEMI                                               { Break                 }
  | ID ASSIGN expr SEMI                                      { Assign($1, $3)        }
  | PRINT expr                                               { Print($2)             }

rtype:
    INT    { INT    }
  | BOOL   { BOOLEAN   }
  | FLOAT  { FLOAT  }
  | STRING { STRING }
  | LAMBDA { LAMBDA }
  | NONE   { NONE   }
  | TABLE   { TABLE   }
  | TUPLE   { TUPLE   }
  | LIST   { LIST   }



expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

args_opt:
    /* nothing */ { [] }
  | args_list  { List.rev $1 }

args_list:
    expr                    { [$1] }
  | args_list COMMA expr { $3 :: $1 }
  


expr:
    LITERAL              { PrimLit(Int($1))          }
  | FLIT	               { PrimLit(Float($1))        }
  | BLIT                 { PrimLit(Boolean($1))      }
  | ID                   { PrimLit(String($1))       } 
  | expr PLUS   expr     { Binop($1, Add,   $3)      }
  | expr MINUS  expr     { Binop($1, Sub,   $3)      }
  | expr TIMES  expr     { Binop($1, Mul,   $3)      }
  | expr DIVIDE expr     { Binop($1, Div,   $3)      }
  | expr POW expr        { Binop($1, Pow,   $3)      }
  | LOG expr expr        { Binop($2, Log,   $3)      }
  | expr EQ     expr     { Binop($1, EQ,    $3)      }
  | expr NEQ    expr     { Binop($1, NEQ,   $3)      }
  | expr LT     expr     { Binop($1, LT,    $3)      }
  | expr LEQ    expr     { Binop($1, LTE,   $3)      }
  | expr GT     expr     { Binop($1, GT,    $3)      }
  | expr GEQ    expr     { Binop($1, GTE,   $3)      }
  | expr AND    expr     { Binop($1, AND,   $3)     }
  | expr OR     expr     { Binop($1, OR,    $3)     }
  | MINUS expr %prec NOT { Unop(NEG, $2)             }
  | NOT expr             { Unop(NOT, $2)             }
  | ID LPAREN args_opt RPAREN { Call($1, $3) }
  | expr DOT ID LPAREN args_opt RPAREN { Apply($1, $3, $5)}

