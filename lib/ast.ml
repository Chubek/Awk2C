module NodeType = struct
  type toplvl = 
    | FuncDefn of { name: ident
                  ; formals: ident list
                  ; body: block
                  }
    | RulePair of { patt: patt option
                  ; action: block
                  }
  and patt =
    | Begin
    | End
    | ERE of string
    | ERERange of string * string
    | Expr of expr

  and block = stmt list

  and stmt =
    | Group of stmt list
    | ExprExec of expr
    | IfElse of { base: (expr * stmt)
                ; induct: (expr * stmt) list
                ; fail: stmt
                }
    | ForCount of { init: stmt list
                  ; cond: expr list
                  ; update: expr list
                  }
    | ForIn of { alias: ident
               ; subj: expr
               }
    | While of { cond: expr
               ; body: stmt
               }
    | DoWhile of { cond: expr
                 ; body: stmt
                 }
    | Switch of { discrim: expr
                ; clauses: (const * stmt) list
                ; default: stmt
                }
    | Return of expr
    | Exit of expr
    | Delete of expr
    | Print of expr list
    | Printf of { fmtstr: string
                ; args: expr list
                ; redir: redir_op * expr
                }
    | Getline of { dest: ident option
                 ; redir: redir_op * expr
                 }
    | Continue
    | Next
    | Break
    | Last
    | Null

  and redir =
    | In of expr
    | Out of expr
    | Append of expr
    | PipeIn of expr
    | PipeOut of expr

  and expr = 
    | Group of expr list
    | Const of const
    | LValue of lvalue
    | Unary of { operator: unary_op
               ; operand: expr
               ; post: bool
               }
    | Binary of { loperand: expr
                ; roperand: expr
                ; operator: binary_op
                }
    | Relational of { loperand: expr
                    ; roperan: expr
                    ; operator: relational_op
                    }
    | Logical of { loperand: expr
                 ; roperand: expr
                 ; operator: logical_op
                 }
    | MatchERE of { operand: expr
                  ; operator: string
                  ; negate: bool
                  }
    | Assign of { lhs: lvalue
                ; rhs: expr
                ; inplace: binary_op option
                }
    | Ternary of { base: expr
                 ; induct: expr
                 ; fail: expr
                 }
    | Field of expr
    | Call of { name: ident
              ; params: expr list
              }
    | Concat of expr list

  and unary_op =
    | Negate
    | Complement
    | Invert
    | Increment
    | Decrement

  and binary_op =
    | Add
    | Subtract
    | Times
    | Divide
    | Modulo
    | Exponent

  and relational_op = 
    | Equal
    | NotEqual
    | Greater
    | GreaterEqual
    | Lesser
    | LesserEqual

  and logical_op = 
    | Disjunct
    | Conjunct

  and lvalue = 
    | Singular of ident
    | Associative of ident * expr list 

  and ident = string * Intrin.slot option

  and const =
    | Number of float
    | String of string
end
