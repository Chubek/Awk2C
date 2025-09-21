module BasicBlock = struct
  type t = 
    { id: int
    ; instrs: Instr.t list
    }

  let compare bb1 bb2 = compare bb1.id bb2.id
  let hash bb = Hashtbl.hash bb.id
  let equal bb1 bb2 = bb1.id = bb2.id
end

module Transfer = struct
  type t =
    | OnZero
    | Jump of Symtbl.label
    | Call of Symtbl.ident
    | Fallthrough

  let compare = compare
  let default = Fallthrough
end

module CFG = Graph.Imperative.Digraph.ConcreteLabeled(BasicBlock)(Transfer)

