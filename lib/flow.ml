type blkid = int

module BasicBlock = struct
  type t = 
    { id: blkid
    ; instrs: Instr.t list
    ; mutable gen_defs: Instr.TempSet
    ; mutable kill_defs: Instr.TempSet
    ; mutable gen_exprs: Instr.TempSet
    ; mutable kill_exprs: Instr.TempSet
    ; mutable use_vars: Instr.TempSet
    ; mutable def_vars: Instr.TempSet
    ; mutable live_in: Instr.TempSet
    ; mutable live_out: Instr.TempSet
    }

  let compare bb1 bb2 = compare bb1.id bb2.id
  let hash bb = Hashtbl.hash bb.id
  let equal bb1 bb2 = (=) bb1.id bb2.id

  let compute_gen_defs blk =
          (* ??? *)

  let compute_kill_defs blk =
          (* ??? *)

  let compute_gen_exprs blk =
          (* ??? *)

  let compute_kill_exprs blk = 
          (* ??? *)

  let compute_use_vars blk =
          (* ??? *)

  let compute_def_vars blk =
          (* ??? *)

  let compute_live_in blk dfg =
          (* ??? *)

  let compute_live_out blk dfg =
          (* ??? *)
end

module ControlTransfer = struct
  type t =
    | IfTrueJump
    | IfFalseJump
    | Goto
    | Fallthrough

  let compare = compare
  let default = Fallthrough
end

module DataTransfer = struct
  type t = 
    | Use of Instr.TempSet.t
    | Def of Instr.TempSet.t

  let compare (Use _) (Def _) = false
  let compare (Def _) (Use _) = false
  let compare (Use tset1) (Use tset2) = Instr.TempSet.compare tset1 tset2
  let compare (Def tset1) (Def tset2) = Instr.TempSet.compare tset1 tset2

  let default = Use (Instr.TempSet.empty)
end

module CFG = Graph.Imperative.Digraph.ConcreteLabeled(BasicBlock)(ControlTransfer)
module DFG = Graph.Imperative.Digraph.ConcreteLabeled(BasicBlock)(DataTransfer)

