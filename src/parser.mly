%{
  open Type
  open Ast
%}

// Lvar
%token LPAREN            "("
%token RPAREN            ")"
%token LBRACK            "["
%token RBRACK            "]"
%token ADD               "+"
%token SUB               "-"
%token LET               "let"
%token READ              "read"
%token <string> VARIABLE
%token <int> INT

%token EOF

%type <Ast.texp> texp
%start <Ast.def list> start

%%
start:
| body = texp EOF { [{Ast.name = "main"; params = []; retty = Type.Integer; body}] }

texp:
// LVar
| num = INT                                                            { {Ast.exp = Int num; ty = Integer}               }
| "(" "read" ")"                                                       { {Ast.exp = Read; ty = Integer}                  }
| "(" "+"  e1 = texp e2 = texp ")"                                     { {Ast.exp = Add (e1, e2); ty = Integer}          }
| "(" "-"  e1 = texp e2 = texp ")"                                     { {Ast.exp = Sub (e1, e2); ty = Integer}          }
| var = VARIABLE                                                       { {Ast.exp = Var var; ty = Integer}               }
| "(" "let" "(" "[" var = VARIABLE init = texp "]" ")" body = texp ")" { {Ast.exp = Let (var, init, body); ty = Integer} }