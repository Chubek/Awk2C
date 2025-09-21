module Symtbl = struct
  type ident = string
  module IdentSet = Set.Make(ident)
  module IdentMap = Map.Make(ident)

  type symbol =
    | Global
    | Local
    | Formal
    | Param
    | Extern
    | Static
    | Builtin
    | Function

  type t = symbol IdentMap

  let builtin_varnames: ident list = [ "FS"
                                     ; "NR"
                                     ; "FNR"
                                     ; (* TODO *)
                                     ]
end
