{
  open Parser
}

let lower = ['a'-'z']
let upper = ['A'-'Z']
let digit = ['0'-'9']

let ident_suffix = (lower | upper | digit | '_')

rule token = parse
  | [' ' '\t' '\r' '\n']        { token lexbuf            }
  | '('                         { LPAREN                  }
  | ')'                         { RPAREN                  }
  | '['                         { LBRACK                  }
  | ']'                         { RBRACK                  }
  | '+'                         { ADD                     }
  | '-'                         { SUB                     }
  | "let"                       { LET                     }
  | "read"                      { READ                    }
  | "#t"                        { TRUE                    }
  | "#f"                        { FALSE                   }
  | "if"                        { IF                      }
  | "eq?"                       { EQ                      }
  | "<"                         { LT                      }
  | "<="                        { LE                      }
  | ">"                         { GT                      }
  | ">="                        { GE                      }
  | "and"                       { AND                     }
  | "or"                        { OR                      }
  | "not"                       { NOT                     }
  | "set!"                      { SET                     }
  | "begin"                     { BEGIN                   }
  | "while"                     { WHILE                   }
  | "void"                      { VOID                    }
  | "vector"                    { VECTOR                  }
  | "vector-length"             { VECTORLENGTH            }
  | "vector-ref"                { VECTORREF               }
  | "vector-set!"               { VECTORSET               }
  | lower ident_suffix* as lxm  { VARIABLE lxm            }
  | digit+ as lxm               { INT (int_of_string lxm) }
  | eof                         { EOF                     }