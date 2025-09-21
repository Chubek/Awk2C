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

  type t = symbol IdentMap.t

  let builtin_varnames: ident array = [| "FS"
                                       ; "NR"
                                       ; "FNR"
                                       ; "FS"
                                       ; "OFS"
                                       ; "RS"
                                       ; "ORS"
                                       ; "FILENAME"
                                       ; "ARGV"
                                       ; "ARGC"
                                       ; "ARGIND"
                                       ; "RSTART"
                                       ; "RLENGTH"
                                       ; "SUBSEP"
                                      |]

  let id_is_builtin id = List.mem builtin_varnames id
end
