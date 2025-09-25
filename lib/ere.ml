module ERE = struct
  module Absyn = struct
    type t =
      | Group of t list
      | Literal of char
      | Brack of char list * bool
      | Repeat of int option * int option
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
    exception Syntax_Errorr
    exception Unescaped_Operator

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

    let peek_next =
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
        let right_node = parse ere in
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
      | [] -> raise "Syntax Error: in ERE"
      | [node] -> node
      | nodes -> Absyn.Concat nodes

    and parse_repeat ere =
      let atom_node = parse_atom ere in
      let min, max =
        match (peek_opt ere) with
        | Some '*' ->
          consume ere |> ignore;
          (Some 0, None)
        | Some '+' ->
          consume ere |> ignore;
          (Some 1, None)
        | Some '?' ->
          (Some 0, Some 1)
        | Some '{' ->
          consume ere |> ignore;
          let n = parse_number ere in
          consume_if ere ',';
          let m = parse_number ere in
          consume_if ere '}';
          (n, m)
        | _ -> (Some -1, None)
      in
      if Option.is_some min && Option.get min = -1
      then atom_node
      else Absyn.Repeat min, max

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
        then raise Syntax_Error
            Absyn.Group sub_expr
      | '[' -> parse_bracket ere
      | '*' | '+' | '?' | '{' | '|' | ')' -> raise Unescaped_Operator
      | _ as ord_char -> Absyn.Literal ord_char

    and parse_number ere =
      let num_chars = consume_while ere (fun ch -> ch >= '0' && ch <= '9') in
      if List.is_empty num_chars
      then None
      else Some (int_of_string (String.of_seq (List.to_seq num_chars)))

    and parse_escape ere =

    and parse_bracket ere =
      let negated = consume_if ere '^' in
      let rec aux acc =
        match (peek_opt ere) with
        | Some ':' -> 
          let class_name = consume_while ere (fun ch -> ch >= 'a' && ch <= 'z') in
          match class_name with
          | Some clsnm -> aux ((lookup_classname clsnm) :: acc)
          | _ -> raise Syntax_Error
          | Some ']' -> 
            consume ere |> ignore; 
            List.rev acc
          | Some sym1 ->
            consume ere |> ignore;
            (match peek_opt ere with
             | Some '-' when peek_next ere != ']' ->
               consume ere |> ignore;
               let sym2 = consume ere in
               aux ((gen_range sym1 sym2) :: acc)
             | _ -> aux (sym1 :: acc))
      in
      Absyn.Brack (aux [], negated)

  end
end
