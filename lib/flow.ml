module BasicBlock = struct
  type t = 
    { id: int
    ; instrs: Instr.t list
    }

  let compare bb1 bb2 = compare bb1.id bb2.id
  let hash bb = Hashtbl.hash bb.id
  let equal bb1 bb2 = bb1.id = bb2.id
end

module ControlTransfer = struct
  type t =
    | ConditionalJump
    | Fallthrough

  let compare = compare
  let default = Fallthrough
end

module DataTransfer = struct
  type t = 
    | Use of Instr.TempSet.t
    | Def of Instr.TempSet.t

  let compare = Instr.TempSet.compare
  let default = Instr.TempSet.empty
end

module CFG = Graph.Imperative.Digraph.ConcreteLabeled(BasicBlock)(ControlTransfer)
module DFG = Graph.Imperative.Digraph.ConcreteLabeled(BasicBlock)(DataTransfer)

