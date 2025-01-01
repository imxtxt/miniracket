%{
  open Type
  open Ast

  let rec mk_begin es =
    match es with
    | [] -> failwith "mk_begin"
    | [a] -> [], a
    | h :: t ->
        let tmp, last = mk_begin t in
        (h :: tmp), last
%}

// LVar
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

// LIf
%token TRUE "#t"
%token FALSE "#f"
%token IF "if"
%token EQ "eq?"
%token LT "<"
%token LE "<="
%token GT ">"
%token GE ">="
%token AND "and"
%token OR "or"
%token NOT "not"

// LWhile
%token SET "set!"
%token BEGIN "begin"
%token WHILE "while"
%token VOID "void"

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

// LIf
| "#t"                                       { {Ast.exp = Bool true; ty = Integer}                                         }
| "#f"                                       { {Ast.exp = Bool false; ty = Integer}                                        }
| "(" "if" e1 = texp e2 = texp e3 = texp ")" { {Ast.exp = If (e1, e2, e3); ty = Integer}                                   }
| "(" "eq?" e1 = texp e2 = texp ")"          { {Ast.exp = Cmp (Eq, e1, e2); ty = Integer}                                  }
| "(" "<" e1 = texp e2 = texp ")"            { {Ast.exp = Cmp (Lt, e1, e2); ty = Integer}                                  }
| "(" "<=" e1 = texp e2 = texp ")"           { {Ast.exp = Cmp (Le, e1, e2); ty = Integer}                                  }
| "(" ">" e1 = texp e2 = texp ")"            { {Ast.exp = Cmp (Gt, e1, e2); ty = Integer}                                  }
| "(" ">=" e1 = texp e2 = texp ")"           { {Ast.exp = Cmp (Ge, e1, e2); ty = Integer}                                  }
| "(" "not" e1 = texp ")"                    { {Ast.exp = Not e1; ty = Integer}                                            }
| "(" "and" e1 = texp e2 = texp ")"          { {Ast.exp = If (e1, e2, {Ast.exp = Bool false; ty = Integer}); ty = Integer} }
| "(" "or" e1 = texp e2 = texp ")"           { {Ast.exp = If (e1, {Ast.exp = Bool true; ty = Integer}, e2); ty = Integer}  }

// LWhile
| "(" "set!" var = VARIABLE rhs = texp ")"  { {Ast.exp = SetBang (var, rhs); ty = Integer}                       }
| "(" "begin" es = list(texp) ")"           { let es, e = mk_begin es in {Ast.exp = Begin (es, e); ty = Integer} }
| "(" "while" cnd = texp body = texp ")"    { {Ast.exp = WhileLoop (cnd, body); ty = Integer}                    }
| "(" "void" ")"                            { {Ast.exp = Void; ty = Integer}                                     }