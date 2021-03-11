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
%token INT BOOL FLOAT VOID NONE STRING
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
   /* nothing */ { ([], [])               }
 | decls statement  { (($2 :: fst $1), snd $1) }
 | decls fdecl { (fst $1, ($2 :: snd $1)) }

fdecl:
    ID LPAREN formals_opt RPAREN FARROW return_type LBRACE statement_list RBRACE
        { { typ = $6;
        fname = $1;
        formals = List.rev (fst $3);
        types = snd $3;
        body = List.rev $8 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { $1 }

formal_list:
   /* nothing */                                    { ([], None)                     }
 | formal_list COMMA return_type ID                 { ((($3, $4) :: fst $1), snd $1) }
 | formal_list COMMA LBRACK return_type_list RBRACK { (fst $1, Some $4)              }


return_type_list:
    return_type                { [$1] }
  | return_type_list COMMA return_type { $3 :: $1 }


statement_list:
    /* nothing */  { [] }
  | statement_list statement { $2 :: $1 }

statement:
    expr SEMI                                                { Expr $1               }
  | RETURN expr_opt SEMI                                     { Return $2             }
  | IF LPAREN expr RPAREN statement_list %prec NOELSE        { If($3, $5, [])        }
  | IF LPAREN expr RPAREN statement_list ELSE statement_list { If($3, $5, $7)        }
  | WHILE LPAREN expr RPAREN statement_list                  { While($3, $5)         }
  | BREAK SEMI                                               { Break()               }
  | ID ASSIGN expr SEMI                                      { Assign($1, $3)        }
  | PRINT expr                                               { Print(#2)             }

return_type:
    INT    { Int    }
  | BOOL   { Bool   }
  | FLOAT  { Float  }
  | STRING { String }
  | VOID   { Void   }



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
  | ID                   { Var($1))                  } 
  | expr PLUS   expr     { Binop($1, Add,   $3)      }
  | expr MINUS  expr     { Binop($1, Sub,   $3)      }
  | expr TIMES  expr     { Binop($1, Mul,   $3)      }
  | expr DIVIDE expr     { Binop($1, Div,   $3)      }
  | expr POW expr        { Binop($1, Pow,   $3)      }
  | LOG expr expr        { Binop($2, Log,   $3)      }
  | expr EQ     expr     { Comop($1, EQ,    $3)      }
  | expr NEQ    expr     { Comop($1, NEQ,   $3)      }
  | expr LT     expr     { Comop($1, LT,    $3)      }
  | expr LEQ    expr     { Comop($1, LTE,   $3)      }
  | expr GT     expr     { Comop($1, GT,    $3)      }
  | expr GEQ    expr     { Comop($1, GTE,   $3)      }
  | expr AND    expr     { Boolop($1, AND,   $3)     }
  | expr OR     expr     { Boolop($1, OR,    $3)     }
  | MINUS expr %prec NOT { Unop(NEG, $2)             }
  | NOT expr             { Unop(NOT, $2)             }
  | LPAREN formals_opt RPAREN LARROW return_type LBRACE statement_list RBRACE 
                         { Lambda(func($2, $7, $5))}
  | ID LPAREN args_opt RPAREN { FuncCall($1, $3) }
  | expr DOT ID LPAREN args_opt RPAREN { Apply($1, $3, $5)}





