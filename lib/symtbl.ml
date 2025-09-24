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

    let create_scalar name scope =
      create name Scalar scope

    let create_array name scope =
      create name Array scope

    let create_function name =
      create name Function Global

    let create_parameter name =
      create name Local Parameter

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
    | [] -> failwith "No more scopes left"
    | head :: tail -> tbl.scopes <- tail

  let install_scalar tbl name =
    let new_symbol = Symbol.create_scalar name (current_scope tbl) in
    match tbl.scopes with
    | [] -> failwith "Scopes empty"
    | head :: tail -> tbl.scopes <- (IdentMap.add name new_symbol head) :: tail

  let install_array name =
    let new_symbol = Symbol.create_array name (current_scope tbl) in
    match tbl.scopes with
    | [] -> failwith "Scopes empty"
    | head :: tail -> tbl.scopes <- (IdentMap.add name new_symbol head) :: tail

  let install_function name =
    let new_symbol = Symbol.create_function name in
    match tbl.scopes with
    | [] -> failwith "Scopes empty"
    | head :: tail -> tbl.scopes <- (IdentMap.add name new_symbol head) :: tail

  let install_param name =
    let new_symbol = Symbol.create_parameter name in
    match tbl.scopes with
    | [] -> failwith "Scopes empty"
    | head :: tail -> tbl.scopes <- (IdentMap.add name new_symbol head) :: tail

  let find_symbol tbl name =
    let rec find_symbol scopes =
      match scopes with
      | [] -> None
      | [scope] -> IdentMap.find_opt name scope
      | head :: tail -> let sym_opt = IdentMap.find_opt name head in
        if Option.is_some sym_opt then sym_opt
        else find_symbol tail
    in
    find_symbol tbl.scopes

end
