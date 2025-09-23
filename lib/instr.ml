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
    | BitwiseOp of bitwise_op
    | RelationalOp of relational_op
    | FileOp of file_op
    | IOOp of io_op
    | ConvOp of conv_op
    | MathOp of math_op
    | SysOp of sys_op
    | RuntimeOp of runtime_op
    | SeqOp of t list

  and const_op =
    | ConstNumber of Runtime.Number.t
    | ConstString of Runtime.String.t
    | ConstBool of Runtime.Bool.t
    | ConstERE of Runtime.ERE.t
    | ConstField of Runtime.Field.t

  and memory_op =
    | LoadVar of temp
    | StoreVar of temp * t
    | PushArena
    | PopArena
    | ResetArena
    | RequestMemory of temp * int
    | CopyMemory of temp * temp

  and array_op =
    | MakeArray of temp * int
    | DestroyArray of temp
    | SetAt of temp * temp * t
    | GetAt of temp * t
    | LengthArray of temp
    | IndexArray of t
    | SortArray of temp
    | AssocSort of temp
    | IsArray of temp
    | DeleteElement of temp

  and control_op =
    | Goto of label
    | MakeLabel of label
    | JumpIfTrue of temp * label
    | JumpIfFalse of temp * label
    | RecurseIfTrue of temp
    | RecurseIfFalse of temp
    | Exit of temp

  and numeric_op =
    | Add of temp list
    | Subtract of temp list
    | Multiply of temp list
    | Divide of temp list
    | Modolu of temp list
    | Exponentiate of temp list
    | Invert of temp list

  and string_op =
    | MakeString of string
    | DestroyString of temp
    | ConcatString of temp list
    | CompareString of temp list
    | SplitString of temp
    | LengthString of temp
    | Substring of temp
    | ToLower of temp
    | ToUpper of temp
    | IndexString of temp
    | FormatString of temp * temp list

  and ere_op =
    | MakeERE of temp
    | DestroyERE of temp
    | CompileERE of temp
    | Match of temp list
    | Sub of temp list
    | GSub of temp list

  and logical_op =
    | Disjunct of temp list
    | Conjunct of temp list
    | Negate of temp list

  and bitwise_op =
    | BitwiseAnd of temp list
    | InclusiveOr of temp list
    | ExclusiveOr of temp list
    | Complement of temp list
    | ShiftLeft of temp * temp list
    | ShiftRight of temp * temp list

  and relational_op =
    | Equals of temp list
    | NotEquals of temp list
    | GreaterThan of temp list
    | GreaterThanEqual of temp list
    | LesserThan of temp list
    | LesserThanEqual of list

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

  and sys_op =
    | Systime
    | Maketime of temp
    | Strftime of temp * temp list
    | Syscall of temp * temp list
    | ExecCommand of temp * temp

  and file_op =
    | OpenFile of temp * Runtime.IO.FileMode
    | CloseFile of temp
    | OpenPipe of temp
    | ClosePipe of temp
    | OpenSocket of temp
    | CloseSocket of temp

  and io_op =
    | Read of temp
    | Write of temp * temp list
    | Append of temp * temp list
    | GetNextLine of temp

  and runtime_op =
    (* TODO *)

  and label = Symtbl.ident
  and temp = Symtbl.ident

  let generate_label = fun () -> Symtbl.generate_temp_id ".L_"
  let generate_temp = fun () -> Symtbl.generate_temp_id ".T_"

  module TempSet = Symtbl.IdentSet
end
