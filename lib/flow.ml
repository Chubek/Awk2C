type blkid = int

module BasicBlock = struct
  type t = 
    { id: blkid
    ; instrs: Instr.t list
    }

  let compare bb1 bb2 = compare bb1.id bb2.id
  let hash bb = Hashtbl.hash bb.id
  let equal bb1 bb2 = (=) bb1.id bb2.id
end

module ControlTransfer = struct
  type t =
        | OnNonZero of Instr.Expr.t * Instr.Label.t
        | OnZero of Instr.Expr.t * Instr.Label.t
        | Assignment of Instr.Temp.t * Instr.Expr.t
        | Load of Instr.Temp.t * Instr.Mem.t
        | Store of Instr.Mem.t * Instr.Expr.t
        | Empty

  let compare = compare
  let default = Empty
end

module CFG = Graph.Imperative.Digraph.ConcreteLabeled(BasicBlock)(ControlTransfer)

