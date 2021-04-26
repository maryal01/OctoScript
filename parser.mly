/* Ocamlyacc parser for OctoScript */
%{ open Ast %}

%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK 
%token COMMA SEMI DOT COLON
%token PLUS MINUS TIMES DIVIDE POW LOG MOD ASSIGN 
%token OP_NOT OP_EQ OP_NEQ OP_LT OP_LEQ OP_GT OP_GEQ OP_AND OP_OR
%token FARROW LARROW FUNC
%token RETURN IF ELSE WHILE BREAK
%token TYP_INT TYP_BOOL TYP_FLOAT TYP_NONE TYP_STRING TYP_LAMBDA 
%token TYP_TABLE TYP_LIST TYP_TUPLE

%token <int> ILIT
%token <bool> BLIT
%token <string> SLIT
%token <string> ID
%token <float> FLIT
%token EOF

%start program
%type <Ast.program> program

%nonassoc NOELSE
%nonassoc ELSE
%nonassoc TYP_TUPLE
%nonassoc ARGS

%right ASSIGN
%left OP_OR OP_AND 
%left OP_EQ OP_NEQ OP_LT OP_GT OP_LEQ OP_GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right POW LOG
%right OP_NOT
%left DOT

%%

program:
  decls EOF { (fst $1, List.rev (snd $1)) }

decls:
   /* nothing */  { ([], [])  }
 | decls fdecl     { (($2 :: fst $1), snd $1) }
 | decls statement  { (fst $1, ($2 :: snd $1)) }
 

formals_opt:
   /* nothing */  { [] }
  | formal_list   { List.rev $1 }

formal_list:
   typ ID                   { [($1, $2)]     }
 | formal_list COMMA typ ID { ($3, $4) :: $1 }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

args_opt:
    /* nothing */ { [] }
  | args_list  { List.rev $1 }

args_list:
    expr                    { [$1] }
  | args_list COMMA expr    { $3 :: $1 }


fdecl: 
  FUNC ID LPAREN formals_opt RPAREN FARROW typ  LBRACE stmnt_list RBRACE  
    { { typ = $7; fname = $2; formals = $4; body = List.rev $9 } } 

stmnt_list:
    /* nothing */  { [] }
  | stmnt_list statement { $2 :: $1 }


typ:
    TYP_INT    { INT     }
  | TYP_BOOL   { BOOLEAN }
  | TYP_FLOAT  { FLOAT   }
  | TYP_STRING { STRING  }
  | TYP_LAMBDA { LAMBDA  }
  | TYP_NONE   { NONE    }
  | TYP_TABLE  { TABLE   }
  | TYP_TUPLE  { TUPLE   }
  | TYP_LIST   { LIST    }

primitives:
    ILIT                    { Int($1)      }
  | FLIT	                  { Float($1)    }
  | BLIT                    { Boolean($1)  }
  | SLIT                    { String($1)   }

array:
    /* nothing */ { [] } 
  |   primitives  { [$1] }
  |   array COMMA primitives { $3 :: $1 }
 

statement:
    expr SEMI                                                                     { Expr $1                 }
  | RETURN expr_opt SEMI                                                          { Return $2               }
  | LBRACE stmnt_list LBRACE                                                      { Block (List.rev $2)     } 
  | IF LPAREN expr RPAREN LBRACE statement RBRACE %prec NOELSE                    { If($3, $6, Block([]))   }
  | IF LPAREN expr RPAREN LBRACE statement RBRACE ELSE LBRACE statement RBRACE    { If($3, $6, $10)         }
  | WHILE LPAREN expr RPAREN LBRACE statement RBRACE                              { While($3, $6)           }
  | BREAK SEMI                                                                    { Break                   }
  | ID ASSIGN expr SEMI                                                           { Assign($1, $3)          }
  | typ ID ASSIGN expr SEMI                                                       { Declare($1, $2, $4)     }
  | typ ID SEMI                                                                   { Declare($1, $2, Noexpr) }

expr:
    ILIT                                      { PrimLit(Int($1))       }
  | FLIT	                                    { PrimLit(Float($1))     }
  | BLIT                                      { PrimLit(Boolean($1))   }
  | SLIT                                      { PrimLit(String($1))    }                         
  | ID                                        { Var($1)                }
  | expr PLUS   expr                          { Binop($1, Add,   $3)   }
  | expr MINUS  expr                          { Binop($1, Sub,   $3)   }
  | expr TIMES  expr                          { Binop($1, Mul,   $3)   }
  | expr DIVIDE expr                          { Binop($1, Div,   $3)   }
  | expr POW    expr                          { Binop($1, Pow,   $3)   }
  | expr MOD    expr                          { Binop($1, Mod,   $3)   }
  | expr LOG    expr                          { Binop($1, Log,   $3)   }
  | expr OP_EQ     expr                       { Binop($1, EQ,    $3)   }
  | expr OP_NEQ    expr                       { Binop($1, NEQ,   $3)   }
  | expr OP_LT     expr                       { Binop($1, LT,    $3)   }
  | expr OP_LEQ    expr                       { Binop($1, LTE,   $3)   }
  | expr OP_GT     expr                       { Binop($1, GT,    $3)   }
  | expr OP_GEQ    expr                       { Binop($1, GTE,   $3)   }
  | expr OP_AND    expr                       {  Binop($1, AND,   $3)  }
  | expr OP_OR     expr                       { Binop($1, OR,    $3)   }
  | MINUS expr %prec OP_NOT                   { Unop(NEG, $2)          }
  | OP_NOT expr                               { Unop(NOT, $2)          }
  | ID LPAREN  args_opt RPAREN                { Call($1, $3)           }
  | IF LPAREN expr RPAREN  expr %prec NOELSE  { IfExpr($3, $5, Noexpr) }
  | IF LPAREN expr RPAREN  expr  ELSE  expr   { IfExpr($3, $5, $7)     }
  | expr DOT ID LPAREN  args_opt RPAREN       { Apply($1, $3, $5)      }
  | LBRACK  array  RBRACK                     { ListLit(List.rev $2)   } 
  | LPAREN  array  RPAREN                     { TupleLit(List.rev $2)  }
  | FUNC LPAREN formals_opt RPAREN LARROW LBRACE expr RBRACE { Lambda($3, $7)}
