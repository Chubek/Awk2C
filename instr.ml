module Instr = struct
  type t =
    | Expression of expr
    | Statement of stmt

  and expr =
    | ConstNumber of float
    | ConstString of string
    | ConstERE of string
    | Variable of Symtbl.ident
    | Fetch of Symtbl.ident * Environ.t
    | NumericOp of numeric_op
    | StringOp of string_op
    | EffectSequence of stmt * expr
    | CallFn of Symtbl.ident * expr list

  and stmt =
    | Store of Symtbl.ident * expr * Environ.t
    | ExecExpression of expr
    | Jump of expr * label
    | CJump of rel_op * expr * expr * label * label
    | Sequence of stmt * stmt
    | Label of label
    | IO of io_op


  and label = Symtbl.ident
end
