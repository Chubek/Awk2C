module ERE = struct
  module Absyn = struct
    type t =
      | Group of t
      | Literal of char
      | Brack of char list * bool
      | Repeat of t * int * int
      | Concat of t list
      | Alt of t * t
      | Wildcard
      | StartAnchor
      | EndAnchor
  end

  module Parser = struct
    type t =
      { lit: string
      ; mutable cursor: int
      }

    exception End_of_ERE
    exception Syntax_error
    exception Unescaped_operator

    let create lit =
      { lit = lit
      ; cursor = 0
      }

    let chars_remain ere =
      (>) (String.length ere.lit) ere.cursor

    let peek ere =
      if not (chars_remain ere)
      then raise End_of_ERE
      else ere.lit.[ere.cursor]

    let peek_next ere =
      if (>) (String.length ere.lit) (ere.cursor + 1)
      then ere.lit.[ere.cursor + 1]
      else '\x00'

    let peek_opt ere =
      if (chars_remain ere)
      then Some (peek ere)
      else None

    let consume ere =
      let ch = peek ere in
      p.cursor <- p.cursor + 1;
      ch

    let consume_if ere pred =
      match peek_opt ere with
      | Some ch when ch = pred ->
        consume ere |> ignore;
        true
          _ -> false

    let consume_while ere pred =
      let aux acc =
        match (peek_opt ere) with
        | Some ch when (pred ch) -> aux (ch :: acc)
        | _ -> List.rev acc
      in 
      aux []

    let rec parse_ere ere =
      let left_node = parse_concat ere in
      match (peek_opt ere) with
      | Some '|' ->
        consume ere |> ignore;
        let right_node = parse_ere ere in
        Absyn.Alt (left_node, right_node)
      | _ -> left_node

    and parse_concat ere =
      let rec aux acc =
        match (peek_opt ere) with
        | Some ('|' | ')') ->
          acc
        | None ->
          acc
        | Some _ ->
          let next_node = parse_repeat ere in
          aux (next_node :: acc)
      in
      match List.rev (aux []) with
      | [] -> raise Syntax_error
      | [node] -> node
      | nodes -> Absyn.Concat nodes

    and parse_repeat ere =
      let atom_node = parse_atom ere in
      let min, max =
        match (peek_opt ere) with
        | Some '*' ->
          consume ere |> ignore;
          (0, -1)
        | Some '+' ->
          consume ere |> ignore;
          (1, -1)
        | Some '?' ->
          (0, 1)
        | Some '{' ->
          consume ere |> ignore;
          let n = parse_number ere in
          consume_if ere ',';
          let m = parse_number ere in
          consume_if ere '}';
          (n, m)
        | _ -> (-1, -1)
      in
      if (min = -1) && (max = -1)
      then atom_node
      else Absyn.Repeat (atom_node, min. max)

    and parse_atom ere =
      let ch = consume ere in
      match ch with
      | '\\' -> parse_escape ere
      | '^' -> Absyn.StartAnchor
      | '$' -> Absyn.EndAnchor
      | '.' -> Absyn.Wildcard
      | '(' ->
        let sub_expr = parse_ere ere in
        if not (consume_if ere ')')
        then raise Syntax_error;
        Absyn.Group sub_expr
      | '[' -> parse_bracket ere
      | '*' | '+' | '?' | '{' | '|' | ')' -> raise Unescaped_operator
      | _ as ord_char -> Absyn.Literal ord_char

    and parse_number ere =
      let num_chars = consume_while ere (fun ch -> ch >= '0' && ch <= '9') in
      if List.is_empty num_chars
      then -1
      else (Awk2CUtils.string_to_integer 10 (String.of_seq (List.to_seq num_chars)))

    and parse_hex ere =
      let hex_chars = consume_while ere (fun ch -> (ch >= '0' && ch <= '9') 
                                                   && (ch >= 'a' && ch <= 'f')
                                                   && (ch >= 'A' && ch <= 'F')) in
      if List.is_empty hex_chars 
      then 0
      else (Awk2CUtils.string_to_integer 16 (String.of_seq (List.to_seq hex_chars)))

    and parse_bracket ere =
      let negated = consume_if ere '^' in
      let rec aux acc =
        match (peek_opt ere) with
        | Some ':' -> 
          let class_name = consume_while ere (fun ch -> ch >= 'a' && ch <= 'z') in
          match class_name with
          | Some clsnm -> aux ((lookup_classname clsnm) @ acc)
          | _ -> raise Syntax_error
          | Some ']' -> 
            consume ere |> ignore; 
            List.rev acc
          | Some sym1 ->
            consume ere |> ignore;
            (match peek_opt ere with
             | Some '-' when peek_next ere != ']' ->
               consume ere |> ignore;
               let sym2 = consume ere in
               aux ((gen_char_range sym1 sym2) @ acc)
             | _ -> aux (sym1 :: acc))
      in
      Absyn.Brack (aux [], negated)

    and parse_escape ere =
      let esc_char = consume ere in
      match (Char.lowercase_ascii esc_char) with
      | 'n' -> '\n'
      | 'r' -> '\r'
      | 't' -> '\t'
      | '"' -> '"'
      | '\'' -> '\''
      | '\\' -> '\\'
      | 'f' -> '\x0C'
      | 'v' -> '\x08'
      | 'a' -> '\x07'
      | 'x' | 'u' ->
        let hex_num = parse_hex ere in
        match hex_num with
        | Some code -> Char.code code
        | None -> raise Syntax_error

    and lookup_classname clsnm =
      match clsnm with
      | "alnum" -> (gen_char_range 'a' 'z') 
                   @ (gen_char_range 'A' 'Z') 
                   @ (gen_char_range '0' '9')
      | "ascii" -> gen_char_range '\x00' '\x7F'
      | "alpha" -> (gen_char_range 'a' 'z') 
                   @ (gen_char_range 'A' 'Z')
      | "blank" -> [ '\t' ; '\x20' ]
      | "cntrl" -> (gen_char_range '\x00' '\x1F') @ [ '\x7F' ]
      | "digit" -> gen_char_range '0' '9'
      | "graph" -> gen_char_range '\x21' '\x7E'
      | "lower" -> gen_char_range 'a' 'z'
      | "print" -> gen_char_range '\x20' '\x7E'
      | "punct" -> [
          '!'; '"'; '#'; '$'; '%'; '&'; '\''; '('; ')'; '*'; '+'; ','; '-'; '.'; '/';
          ':'; ';'; '<'; '='; '>'; '?'; '@';
          '['; '\\'; ']'; '^'; '_'; '`';
          '{'; '|'; '}'; '~'
        ]
      | "space" -> [ '\x20' ; '\x0C' ; '\n' ; '\r' ; '\t' ; '\x08' ; '\x07' ]
      | "upper" -> gen_char_range 'A' 'Z'
      | "xdigit" -> (gen_char_range '0' '9') 
                    @ (gen_char_range 'A' 'F') 
                    @ (gen_char_range 'a' 'f')
      | _ -> raise Syntax_error

    and gen_char_range fc tc =
      let start_code, end_code = (Char.code fc, (Char.code tc) + 1) in
      let rec aux acc code = 
        if code = end_code
        then List.rev acc
        else aux ((Char.chr code) :: acc) (code + 1)
      in
      aux [] start_code
  end

  module AutomatonState = struct
    type t =
      { uid: int
      ; accepting: bool
      }    

    let compare s1 s2 = compare s1.uid s2.uid
    let hash s = Hashtbl.hash s.uid
    let equal s1 s2 = (=) s1.uid s2.uid

    let uid_counter = ref 0
    let next_uid () =
      let uid = !uid_counter in
      incr uid_counter;
      uid

    let empty =
      { uid = (-1)
      ; accepting = false
      }

    let create ?accepting:(false) () =
      { uid = next_uid ()
      ; accepting = accepting
      }
  end

  module AutomatonTransition = struct
    type t = 
      | Epsilon
      | OnAny
      | OnSymbol of char
      | VirtualStart
      | VirtualEnd

    let compare = compare
    let default = Epsilon
  end

  module NFA = Graph.Imperative.Digraph.ConcreteLabeled(AutomatonState)(AutomatonTransition)
  module DFA = Graph.Imperative.Digraph.ConcreteLabeled(AutomatonState)(AutomatonTransition)

  module NFAConstructor = struct
    type t =
      { mutable start_state: AutomatonState.t
      ; mutable accept_state: AutomatonState.t
      ; graph: NFA.t
      }

    let create () =
      { start_state = AutomatonState.empty
      ; accept_state = AutomatonState.empty
      ; graph = NFA.create ()
      }

    let add_transition nfacon src lbl dst =
      NFA.add_vertex nfacon.graph src;
      NFA.add_vertex nfacon.graph dst;
      NFA.add_edge_e nfacon.graph (src, lbl, dst)

    let build_nfa nfacon node =
      match node with
      | Absyn.Literal ch -> build_literal nfacon ch
      | Absyn.Alt (left, right) -> build_alt nfacon left right
      | Absyn.Repeat (expr, min, max) -> build_repeat nfacon expr min max
      | Absyn.Concat lst -> build_concat nfacon lst
      | Absyn.Group expr -> build_group nfacon expr
      | Absyn.Brack (lst, neg) -> build_brack nfacon lst neg
      | Absyn.Wildcard -> build_wildcard nfacon
      | Absyn.StartAnchor -> build_start_anchor nfacon
      | Absyn.EndAnchor -> build_end_anchor nfacon

  end
end
