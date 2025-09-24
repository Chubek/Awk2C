module Symtbl = struct
  module Symbol = struct
    type t =
      { name: string
      ; uid: int
      ; kind: kind
      ; scope: scope
      ; builtin: bool
      }

    and kind =
      | Scalar
      | Array
      | Function
      | Formal

    and scope =
      | Global
      | Local
      | Parameter
      | Block of int

    let compare s1 s2 = compare s1.uid s2.uid
    let hash s = Hashtbl.hash s.uid
    let equal s1 s2 = (=) s1.uid s2.uid

    let uid_counter = ref 0
    let next_uid () =
      let uid = !uid_counter in
      incr uid_counter;
      uid

    let create name kind scope =
      { name = name
      ; uid = next_uid ()
      ; kind = kind
      ; scope = scope
      ; builtin = Runtime.Builtin.is_builtin name
      }
  end

  module Ident = String
  module IdentMap = Map.Make(Ident)

  type t =
    { filename: string
    ; scopes: (Symbol.t IdentMap.t) list
    }

  let create filename =
    { filename = filename
    ; scopes = [IdentMap.empty]
    }

  let nesting_level tbl =
    (List.length tbl.scopes) - 1

  let current_scope tbl =
    if (nesting_level tbl) = 0 
    then Symbol.Global
    else if (nesting_level tbl) = 1
    then Symbol.Local
    else if (nesting_level tbl) > 1
    then Symbol.Block (nesting_level tbl)

  let enter_scope tbl =
    tbl.scopes <- IdentMap.empty :: tbl.scopes

  let leave_scope tbl =
    match tbl.scopes with
    | [] -> failwith "BUG: Scope list empty"
    | [_] -> ()
    | _ :: tail -> tbl.scopes <- tail

  let install_symbol tbl name kind scope =
    let new_symbol = Symbol.create name kind (if Option.is_none scope 
                                              then current_scope tbl
                                              else Option.get scope) in
    match tbl.scopes with
    | [] -> failwith "BUG: Scope list empty"
    | head :: tail -> tbl.scopes <- (IdentMap.add name new_symbol head) :: tail

  let install_scalar tbl name =
    install_symbol tbl name Symbol.Scalar None

  let install_array tbl name =
    install_symbol tbl name Symbol.Array None

  let install_function tbl name =
    install_symbol tbl name Symbol.Function (Some Symbol.Global)

  let install_param tbl name =
    install_symbol tbl name Symbol.Formal (Some Symbol.Parameter)

  let find_symbol tbl name =
    let rec find_symbol_aux scopes =
      match scopes with
      | [] -> None
      | [scope] -> IdentMap.find_opt name scope
      | head :: tail -> let sym_opt = IdentMap.find_opt name head in
        if Option.is_some sym_opt then sym_opt
        else find_symbol_aux tail
    in
    find_symbol_aux tbl.scopes

  let delete_symbol tbl name =
    let rec delete_symbol_aux scopes acc =
      match scopes with
      | [] -> ()
      | head :: tail ->
        if (IdentMap.mem name head)
        then
          let new_head = IdentMap.remove name head in
          List.rev_append acc (new_head :: tail)
        else
          delete_symbol_aux tail (head :: acc)
    in
    tbl.scopes <- delete_symbol_aux tbl.scopes []
end
