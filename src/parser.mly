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

// LTup
%token VECTOR "vector"
%token VECTORLENGTH "vector-length"
%token VECTORREF "vector-ref"
%token VECTORSET "vector-set!"

// LArray
%token ARRAY "array"
%token ARRAYLENGTH "array-length"
%token ARRAYREF "array-ref"
%token ARRAYSET "array-set!"
%token MUL "*"

// LFun
%token DEFINE "define"
%token COLON ":"
%token ARROW "->"
%token TINTEGER "Integer"
%token TBOOLEAN "Boolean"
%token TVOID "Void"
%token TVECTOR "Vector"
%token TARRAY "Array"

%token EOF

%type <Ast.texp> texp
%type <Ast.def> def
%type <Type.ty> ty
%start <Ast.def list> start

%%
start:
| fs = list(def) EOF { fs }

texp:
// LVar
| num = INT                                                            { {Ast.exp = Int num; ty = Integer}               }
| "(" "read" ")"                                                       { {Ast.exp = Read; ty = Integer}                  }
| "(" "+" e1 = texp e2 = texp ")"                                      { {Ast.exp = Add (e1, e2); ty = Integer}          }
| "(" "-" e1 = texp e2 = texp ")"                                      { {Ast.exp = Sub (e1, e2); ty = Integer}          }
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
| "(" "set!" var = VARIABLE rhs = texp ")" { {Ast.exp = SetBang (var, rhs); ty = Integer}                       }
| "(" "begin" es = list(texp) ")"          { let es, e = mk_begin es in {Ast.exp = Begin (es, e); ty = Integer} }
| "(" "while" cnd = texp body = texp ")"   { {Ast.exp = WhileLoop (cnd, body); ty = Integer}                    }
| "(" "void" ")"                           { {Ast.exp = Void; ty = Integer}                                     }

// LTup
| "(" "vector" es = list(texp) ")"                 { {Ast.exp = Vector es; ty = Integer}             }
| "(" "vector-length" e1 = texp ")"                { {Ast.exp = VectorLength e1; ty = Integer}       }
| "(" "vector-ref" e1 = texp i = INT ")"           { {Ast.exp = VectorRef (e1, i); ty = Integer}     }
| "(" "vector-set!" e1 = texp i = INT e2 = texp")" { {Ast.exp = VectorSet (e1, i, e2); ty = Integer} }

// LArray
| "(" "*" e1 = texp e2 = texp ")"                   { {Ast.exp = Mul (e1, e2); ty = Integer}         }
| "(" "array" len = texp init = texp ")"            { {Ast.exp = Array (len, init); ty = Integer}    }
| "(" "array-length" e1 = texp ")"                  { {Ast.exp = ArrayLength e1; ty = Integer}       }
| "(" "array-ref" e1 = texp i = texp ")"            { {Ast.exp = ArrayRef (e1, i); ty = Integer}     }
| "(" "array-set!" e1 = texp i = texp e2 = texp ")" { {Ast.exp = ArraySet (e1, i, e2); ty = Integer} }

// Lfun
| "(" e1 = texp args = list(texp) ")" { {Ast.exp = Apply (e1, args); ty = Integer} }


// Lfun
def:
| "(" "define" "(" n = VARIABLE ps = list(param) ")" ":" t = ty b = texp ")" { {Ast.name = n; params = ps; retty = t; body = b}        }
| body = texp                                                                { {Ast.name = "main"; params = []; retty = Integer; body} }

param:
| "[" v = VARIABLE ":" t = ty "]" { (v, t) }

ty:
| "Integer"                             { Integer              }
| "Boolean"                             { Boolean              }
| "Void"                                { Void                 }
| "(" "Vector" ts = list(ty) ")"        { Vector ts            }
| "(" "Array" t = ty  ")"               { Array t              }
| "(" pt = list(ty) "->" retty = ty ")" { Function (pt, retty) }