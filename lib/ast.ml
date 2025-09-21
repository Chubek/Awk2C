module NodeType = struct
  type toplvl = 
    | FuncDefn of { name: string
                  ; formals: string list
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
    | Unary of unary_op option * expr
    | Binary of binary_op * expr * expr
    | Relop of rel_op * expr * expr
    | EREOp of ere_op * expr * string
    | IfElse of { base: (expr * stmt)
                ; induct: (expr * stmt) list
                ; fail: stmt
                }
    | ForCount of { init: stmt list
                  ; cond: expr list
                  ; update: expr list
                  }
    | ForIn of { alias: string
               ; aarry: string
               }
    | While of { cond: expr
               ; body: stmt
               }
    | DoWhile of { cond: expr
                 ; body: stmt
                 }
    | Return of expr
    | Exit of expr
    | Call of { fnname: string
              ; actuals: expr list
              }
    | Continue
    | Next
    | Break
    | Last

end
