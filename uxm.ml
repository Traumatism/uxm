exception ParseError of string

type tokenkind =
  | Symbol of char
  | Name of string
  | Int of int
  | Expr of token list

and expr =
  | IntLiteral of int
  | Var of string
  | UnaryOp of char * expr
  | BinaryOp of char * expr * expr
  | Call of string * exprs
  | Group of exprs

and token = tokenkind
and tokens = token list
and exprs = expr list
and env_t = (string * expr) list
and parser_f = tokens -> expr * tokens
and rule = { lhs : expr; rhs : expr }

let rec string_of_expr : expr -> string = function
  | IntLiteral n -> string_of_int n
  | Var name -> name
  | UnaryOp (op, e) -> Printf.sprintf "(%c, %s)" op (string_of_expr e)
  | BinaryOp (op, left, right) ->
      Printf.sprintf "(%s %c %s)" (string_of_expr left) op
        (string_of_expr right)
  | Call (fname, args) ->
      let args_str = String.concat ", " (List.map string_of_expr args) in
      Printf.sprintf "%s(%s)" fname args_str
  | Group exprs ->
      let exprs_str = String.concat " " (List.map string_of_expr exprs) in

      Printf.sprintf "(%s)" exprs_str

and string_of_token : token -> string = function
  | Symbol c -> Printf.sprintf "(Sym %c)" c
  | Name s -> Printf.sprintf "(Name %s)" s
  | Int n -> Printf.sprintf "(Int %d)" n
  | Expr xs -> "[" ^ string_of_tokens xs ^ "]"

and string_of_tokens (xs : tokens) : string =
  String.concat ", " (List.map string_of_token xs)

and tokenize (str : string) : tokens =
  let chars = ref (str |> String.to_seq |> List.of_seq) in
  let rec lex_chars (acc : tokens) : tokens =
    let rec take_while (pred : char -> bool) (acc : string) : string =
      match !chars with
      | c :: tl when pred c ->
          let () = chars := tl in
          take_while pred (acc ^ String.make 1 c)
      | _ -> acc
    and is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
    and is_whitespace = function ' ' | '\t' | '\n' -> true | _ -> false
    and is_digit = function '0' .. '9' -> true | _ -> false in

    match !chars with
    | [] -> acc
    | c :: tl ->
        let () = chars := tl in
        if is_whitespace c then lex_chars acc
        else if is_alpha c then
          let content =
            take_while
              (fun c -> is_digit c || is_alpha c || c = '_')
              (String.make 1 c)
          in
          lex_chars (Name content :: acc)
        else if is_digit c then
          let content = take_while is_digit (String.make 1 c) in
          lex_chars (Int (int_of_string content) :: acc)
        else lex_chars (Symbol c :: acc)
  in

  lex_chars []

and parse_expr : parser_f = function xs -> parse_additive xs

and parse_additive : parser_f = function
  | xs ->
      let lhs, tl = parse_multiplicative xs in
      parse_binary_op lhs tl [ '+'; '-' ] parse_multiplicative

and parse_multiplicative : parser_f = function
  | xs ->
      let lhs, tl = parse_unary xs in
      parse_binary_op lhs tl [ '*'; '/' ] parse_unary

and parse_unary : parser_f = function
  | Symbol ('-' as op) :: tl ->
      let e, tl' = parse_unary tl in
      (UnaryOp (op, e), tl')
  | Symbol '+' :: tl -> parse_unary tl
  | xs -> parse_exponentiation xs

and parse_exponentiation : parser_f = function
  | xs ->
      let lhs, tl = parse_primary xs in
      parse_binary_op lhs tl [ '^' ] parse_primary

and parse_binary_op (lhs : expr) (xs : tokens) (ops : char list)
    (next_parser : parser_f) : expr * tokens =
  match xs with
  | Symbol op :: tl when List.mem op ops ->
      let rhs, tl' = next_parser tl in
      parse_binary_op (BinaryOp (op, lhs, rhs)) tl' ops next_parser
  | _ -> (lhs, xs)

and parse_primary : parser_f = function
  | Int n :: tl -> (IntLiteral n, tl)
  | Name n :: Symbol '(' :: tl ->
      let xs, tl' = parse_args tl in
      (Call (n, xs), tl')
  | Name n :: tl -> (Var n, tl)
  | Symbol '(' :: tl -> (
      let expr, tl' = parse_expr tl in
      match tl' with
      | Symbol ')' :: tl'' -> (expr, tl'')
      | _ -> raise (ParseError "Expected closing )"))
  | tkn :: _ ->
      raise
        (ParseError (Printf.sprintf "Unexpected token %s" (string_of_token tkn)))
  | _ -> failwith "..."

and parse_args : tokens -> exprs * tokens = function
  | Symbol ')' :: tl -> ([], tl)
  | xs ->
      let arg, tl = parse_expr xs in
      let args, tl' =
        match tl with
        | Symbol ',' :: tl'' -> parse_args tl''
        | Symbol ')' :: tl'' -> ([], tl'')
        | _ -> raise (ParseError "Expected , or ) in function arguments")
      in
      (arg :: args, tl')

and parse_math_expr (xs : tokens) : exprs =
  match parse_expr xs with e, [] -> [ e ] | e, tl -> e :: parse_math_expr tl

and parse_loose (xs : tokens) : expr = List.hd (parse_math_expr xs)

and matches (pattern : expr) (target : expr) (env : env_t) : env_t option =
  match (pattern, target) with
  | IntLiteral i, IntLiteral j when i = j -> Some env
  | Var v, _ -> Some ((v, target) :: env)
  | UnaryOp (op, e), UnaryOp (op', e') when op = op' -> matches e e' env
  | BinaryOp (op, l, r), BinaryOp (op', l', r') when op = op' -> (
      match matches l l' env with
      | Some env' -> matches r r' env'
      | None -> None)
  | Call (f, args), Call (f', args')
    when f = f' && List.length args = List.length args' ->
      List.fold_left2
        (fun acc a a' ->
          match acc with Some env' -> matches a a' env' | None -> None)
        (Some env) args args'
  | Group es, Group es' when List.length es = List.length es' ->
      List.fold_left2
        (fun acc e e' ->
          match acc with Some env' -> matches e e' env' | None -> None)
        (Some env) es es'
  | _ -> None

and apply (env : env_t) (e : expr) =
  match e with
  | Var v -> ( try List.assoc v env with Not_found -> e)
  | IntLiteral _ -> e
  | UnaryOp (op, operand) -> UnaryOp (op, apply env operand)
  | BinaryOp (op, lhs, rhs) -> BinaryOp (op, apply env lhs, apply env rhs)
  | Call (f, args) -> Call (f, List.map (apply env) args)
  | Group es -> Group (List.map (apply env) es)

and subst (pattern : expr) (replacement : expr) (target : expr) : expr =
  match matches pattern target [] with
  | Some env -> apply env replacement
  | None -> (
      match target with
      | IntLiteral _ | Var _ -> target
      | UnaryOp (op, operand) -> UnaryOp (op, subst pattern replacement operand)
      | BinaryOp (op, lhs, rhs) ->
          BinaryOp
            (op, subst pattern replacement lhs, subst pattern replacement rhs)
      | Call (f, args) -> Call (f, List.map (subst pattern replacement) args)
      | Group es -> Group (List.map (subst pattern replacement) es))

and run (xs : tokens) : unit =
  let stack = xs |> List.to_seq |> Stack.of_seq in
  let rec parse_until (name : string) : tokens =
    match Stack.pop stack with
    | Name x when x = name -> []
    | x -> x :: parse_until name
  in
  let next_name msg =
    match Stack.pop stack with Name x -> x | _ -> failwith msg
  and exprs : (string, expr) Hashtbl.t = Hashtbl.create 128
  and rules : (string, rule) Hashtbl.t = Hashtbl.create 128 in
  while not (Stack.is_empty stack) do
    match Stack.pop stack with
    | Name "defex" ->
        let name = next_name "Expected name after defex" in
        "endex" |> parse_until |> parse_loose |> Hashtbl.add exprs name
    | Name "induction" ->
        let var = next_name "You have to provide a name" in
        let expr = "end" |> parse_until |> parse_math_expr in
        ()
    | Name "apply" ->
        let rule_name = next_name "Expected rule name after apply"
        and expr_name = next_name "Expected expr name after apply <rule>" in
        let rule = Hashtbl.find rules rule_name in
        let new_expr = subst rule.lhs rule.rhs (Hashtbl.find exprs expr_name) in
        Hashtbl.add exprs expr_name new_expr
    | Name "puts" ->
        next_name "Expected expr name after puts"
        |> Hashtbl.find exprs |> string_of_expr |> print_endline
    | Name "defrule" ->
        let name = next_name "Expected rule name after applyraw" in
        let match_expr = "into" |> parse_until |> parse_loose in
        let replace_expr = "endrule" |> parse_until |> parse_loose in
        Hashtbl.add rules name { lhs = match_expr; rhs = replace_expr }
    | tkn ->
        tkn |> string_of_token
        |> Printf.sprintf "Unexpected token: %s"
        |> failwith
  done;
  ()
;;

let () = if Array.length Sys.argv <> 3 then failwith "uxm <action> <path>" in

let ch = open_in_bin Sys.argv.(2) in
let content = in_channel_length ch |> really_input_string ch in
let () = close_in ch in

let action = Sys.argv.(1) in
match action with "run" -> content |> tokenize |> run | _ -> failwith "TBD"
