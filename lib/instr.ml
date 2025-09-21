module Instr = struct
  type t =
    | Expression of expr
    | Statement of stmt

  and expr =
    | ConstNumber of float
    | ConstString of string
    | ConstERE of string
    | Variable of temp
    | Fetch of temp * Environ.t
    | NumericOp of numeric_op
    | StringOp of string_op
    | EffectSequence of stmt * expr
    | CallFn of temp * expr list

  and stmt =
    | Store of temp * expr * Environ.t
    | ExecExpression of expr
    | Jump of expr * label
    | CJump of rel_op * expr * expr * label * label
    | Sequence of stmt * stmt
    | Label of label
    | IO of io_op


  and label = Symtbl.ident
  and temp = Symtbl.ident

  let generate_label = fun () -> Symtbl.generate_temp_id ".L_"
  let generate_temp = fun () -> Symtbl.generate_temp_id ".T_"

  module TempSet = Symtbl.IdentSet
end
