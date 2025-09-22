module Instr = struct
  type t =
    | ConstOp of const_op
    | MemoryOp of memory_op
    | ArrayOp of array_op
    | ControlOp of control_op
    | NumericOp of numeric_op
    | StringOp of string_op
    | EREOp of ere_op
    | LogicalOp of logical_op
    | RelationalOp of relational_op
    | FileOp of file_op
    | IOOp of io_op
    | ConvOp of conv_op
    | MathOp of math_op
    | SysOp of sys_op
    | SeqOp of t list

  and const_op =
    | ConstNumber of Runtime.Number.t
    | ConstString of Runtime.String.t
    | ConstBool of Runtime.Bool.t
    | ConstERE of Runtime.ERE.t
    | ConstRecord of Runtime.Record.t
    | ConstField of Runtime.Field.t

  and memory_op =
    | Load of temp
    | Store of temp * t
    | Destroy of temp

  and array_op =
    | MakeArray of int
    | DestroyArray of temp
    | LengthArray of temp
    | IndexArray of t

  and control_op =
    | Goto of label
    | MakeLabel of label
    | JumpZero of temp * label
    | JumpNonZero of temp * label

  and numeric_op =
    | Add of temp list
    | Sub of temp list
    | Mul of temp list
    | Div of temp list
    | Mod of temp list

  and string_op =
    | Concat of temp list
    | Split of temp
    | Length of temp
    | Substr of temp
    | ToLower of temp
    | ToUpper of temp
    | Index of temp
    | Format of temp * temp list

  and ere_op =
    | Match of temp list
    | Sub of temp list
    | GSub of temp list

  and math_op =
    | Rand
    | Round of temp
    | SRand of temp
    | Sqrt of temp
    | Log10 of temp
    | Log2 of temp
    | Exp of temp
    | Sin of temp
    | Cos of temp
    | Atan2 of temp

  and conv_op = 
    | StrToNum of temp
    | NumToStr of temp

  and label = Symtbl.ident
  and temp = Symtbl.ident

  let generate_label = fun () -> Symtbl.generate_temp_id ".L_"
  let generate_temp = fun () -> Symtbl.generate_temp_id ".T_"

  module TempSet = Symtbl.IdentSet
end
