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
    | OnZero
    | Jump of Instr.label
    | Call of Symtbl.ident
    | Fallthrough

  let compare = compare
  let default = Fallthrough
end

module DataTransfer = struct
  type t = 
    | Use of Symtbl.IdentSet.t
    | Def of Symtbl.IdentSet.t

  let compare = Symtbl.IdentSet.compare
  let default = Symtbl.IdentSet.empty
end

module CFG = Graph.Imperative.Digraph.ConcreteLabeled(BasicBlock)(ControlTransfer)
module DFG = Graph.Imperative.Digraph.ConcreteLabeled(BasicBlock)(DataTransfer)

