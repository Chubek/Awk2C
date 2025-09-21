module Symtbl = struct
  type ident = string
  module IdentSet = Set.Make(ident)
  module IdentMap = Map.Make(ident)

  (* TODO *)
  type symbol =
    | Global
    | Local
    | Formal
    | Param
    | Extern
    | Static
    | Builtin
    | Function

  let tbl = ref IdentMap.empty

  let insert_symbol id sym = 
    tbl := IdentMap.add id sym !tbl

  let get_symbol id =
    IdentMap.find id !tbl

  let exists_symbol id =
    Option.is_some (IdentMap.find_opt id !tbl)

  let remove_symbol id =
    tlb := IdentMap.remove id !tbl

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

  let generate_temp_id prefix = prefix ^ "_" ^ (Awk2CUtils.random_string)
end
