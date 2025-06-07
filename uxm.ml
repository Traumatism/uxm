type token = Sym of string | Name of string | Int of int | Expr of token list

and expr =
  | Int of int
  | Var of string
  | UnOp of string * expr
  | BinOp of string * expr * expr
  | Call of string * exprs

and tokens = token list
and exprs = expr list
and env_t = (string * expr) list
and parser_f = tokens -> expr * tokens
and rule_comp = expr * expr
and rule = rule_comp list

let rec string_of_expr : expr -> string = function
  | Int x -> string_of_int x
  | Var x -> x
  | UnOp (o, x) -> Printf.sprintf "(%s%s)" o (string_of_expr x)
  | BinOp (o, x, y) ->
      Printf.sprintf "(%s %s %s)" (string_of_expr x) o (string_of_expr y)
  | Call (f, xs) ->
      Printf.sprintf "%s(%s)" f
        (String.concat ", " (List.map string_of_expr xs))

and string_of_char : char -> string = function c -> Printf.sprintf "%c" c

and gen_parser (ops : string list) (p : parser_f) : parser_f =
  let wrapper (xs : tokens) =
    let x, tl = p xs in
    parse_binop x tl ops p
  in
  wrapper

and parse_binop (x : expr) (xs : tokens) (ops : string list) (p' : parser_f) :
    expr * tokens =
  match xs with
  | (Name op | Sym op) :: tl when List.mem op ops ->
      let y, tl' = p' tl in
      parse_binop (BinOp (op, x, y)) tl' ops p'
  | _ -> (x, xs)

let rec tokenize (str : string) : tokens =
  let chars = ref (str |> String.to_seq |> List.of_seq) in
  let rec lex_chars (acc : tokens) : tokens =
    let rec take_while p acc =
      match !chars with
      | c :: tl when p c ->
          chars := tl;
          take_while p (acc ^ String.make 1 c)
      | _ -> acc
    and is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
    and is_whitespace = function ' ' | '\t' | '\n' -> true | _ -> false
    and is_digit = function '0' .. '9' -> true | _ -> false in

    match !chars with
    | [] -> acc
    | c :: tl -> (
        chars := tl;
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
        else
          match c :: tl with
          | '<' :: (('=' | '-' | '~' | '$') as bar) :: '>' :: tl' ->
              chars := tl';
              lex_chars (Sym ("<" ^ string_of_char bar ^ ">") :: acc)
          | '<' :: '>' :: tl' ->
              chars := tl';
              lex_chars (Sym "<>" :: acc)
          | (('=' | '-' | '~') as bar) :: '>' :: tl' ->
              chars := tl';
              lex_chars (Sym (string_of_char bar ^ ">") :: acc)
          | '/' :: '\\' :: tl' ->
              chars := tl';
              lex_chars (Sym "/\\" :: acc)
          | '\\' :: '/' :: tl' ->
              chars := tl';
              lex_chars (Sym "\\/" :: acc)
          | _ -> lex_chars (Sym (string_of_char c) :: acc))
  in

  lex_chars []

and parse_expr xs =
  (gen_parsers_chain
     [
       [ "<$>" ];
       [ "=>"; "<=>"; "->"; "<->"; "<~>"; "~>" ];
       [ "/\\" ];
       [ "=" ];
       [ "+"; "-" ];
       [ "."; "*"; "/" ];
       [ "^" ];
     ])
    xs

and gen_parsers_chain : string list list -> parser_f = function
  | [] -> gen_parser [] parse_unary
  | ops :: tl -> gen_parser ops (gen_parsers_chain tl)

and parse_unary : parser_f = function
  | ( Sym ("~" as op)
    | Sym ("-" as op)
    | Sym ("+" as op)
    | Sym ("?" as op)
    | Sym ("&" as op) )
    :: tl ->
      let e, tl' = parse_unary tl in
      (UnOp (op, e), tl')
  | xs -> parse_primary xs

and parse_primary : parser_f = function
  | Int n :: tl -> (Int n, tl)
  | Name n :: Sym "(" :: tl ->
      let xs, tl' = parse_args tl in
      (Call (n, xs), tl')
  | Name n :: tl -> (Var n, tl)
  | Sym "(" :: tl -> (
      let expr, tl' = parse_expr tl in
      match tl' with
      | Sym ")" :: tl'' -> (expr, tl'')
      | _ -> failwith "Expected closing )")
  | _ -> failwith "Unexpected token"

and parse_args : tokens -> exprs * tokens = function
  | Sym ")" :: tl -> ([], tl)
  | xs ->
      let arg, tl = parse_expr xs in
      let args, tl' =
        match tl with
        | Sym "," :: tl'' -> parse_args tl''
        | Sym ")" :: tl'' -> ([], tl'')
        | _ -> failwith "Expected , or ) in function arguments"
      in
      (arg :: args, tl')

and parse_math_expr (xs : tokens) : exprs =
  match parse_expr xs with e, [] -> [ e ] | e, tl -> e :: parse_math_expr tl

and parse_loose (xs : tokens) : expr = List.hd (parse_math_expr xs)

and matches (pattern : expr) (on : expr) (env : env_t) : env_t option =
  match (pattern, on) with
  | Int i, Int j when i = j -> Some env
  | Var v, _ -> Some ((v, on) :: env)
  | UnOp (op, e), UnOp (op', e') when op = op' -> matches e e' env
  | BinOp (op, l, r), BinOp (op', l', r') when op = op' -> (
      match matches l l' env with
      | Some env' -> matches r r' env'
      | None -> None)
  | Call (f, xs), Call (f', xs') when List.length xs = List.length xs' ->
      List.fold_left2
        (fun acc a a' ->
          match acc with Some env' -> matches a a' env' | None -> None)
        (Some ((f, Call (f, xs')) :: env))
        xs xs'
  | _ -> None

and apply (env : env_t) (e : expr) =
  match e with
  | Var v -> List.assoc v env
  | Int _ -> e
  | UnOp (op, x) -> UnOp (op, apply env x)
  | BinOp (op, x, y) -> BinOp (op, apply env x, apply env y)
  | Call (f, args) -> Call (f, List.map (apply env) args)

and subst (from : expr) (into : expr) (on : expr) : expr =
  match matches from on [] with
  | Some env -> apply env into
  | None -> (
      match on with
      | Int _ | Var _ -> on
      | UnOp (op, x) -> UnOp (op, subst from into x)
      | BinOp (op, x, y) -> BinOp (op, subst from into x, subst from into y)
      | Call (f, xs) -> Call (f, List.map (subst from into) xs))

and run (xs : tokens) : unit =
  let rec parse_until n =
    match Stack.pop stack with
    | Name x when x = n -> []
    | x -> x :: parse_until n
  and unwrap_find m x = match x with Some y -> y | None -> failwith m
  and next_name m = match Stack.pop stack with Name x -> x | _ -> failwith m
  and exprs = Hashtbl.create 512
  and rules = Hashtbl.create 512
  and stack = List.to_seq xs |> Stack.of_seq in

  while not (Stack.is_empty stack) do
    match Stack.pop stack with
    | Name "defrule" ->
        let name = next_name "Expected a rule name" in
        let rec aux xs acc_m acc_r =
          match Stack.pop stack with
          | Sym "|" ->
              let m' = parse_loose (parse_until "into") in
              let c' =
                if acc_r = [] then xs
                else (acc_m, parse_loose (List.rev acc_r)) :: xs
              in
              aux c' m' []
          | Name "end" -> (acc_m, parse_loose (List.rev acc_r)) :: xs
          | t -> aux xs acc_m (t :: acc_r)
        in

        if Stack.top stack = Sym "|" then
          Hashtbl.add rules name (aux [] (Int 0) [])
        else
          Hashtbl.add rules name
            [
              (parse_loose (parse_until "into"), parse_loose (parse_until "end"));
            ]
    | Name "defex" ->
        let name = next_name "Expected name after defex" in
        parse_until "end" |> parse_loose |> Hashtbl.add exprs name
    | Name "apply" ->
        let rule_name = next_name "Expected rule name after apply"
        and expr_name = next_name "Expected expr name after apply <rule>" in
        let rec aux xs acc =
          match xs with [] -> acc | (x, y) :: tl -> aux tl (subst x y acc)
        in
        Hashtbl.find_opt exprs expr_name
        |> unwrap_find "Expression not found"
        |> aux (Hashtbl.find rules rule_name |> List.rev)
        |> Hashtbl.add exprs expr_name
    | Name "puts" ->
        next_name "Expected expr name after puts"
        |> Hashtbl.find exprs |> string_of_expr |> print_endline
    | tkn -> failwith "Unexpected token while executing"
  done;
  ()
;;

if Array.length Sys.argv <> 3 then failwith "uxm <action> <path>";

let ch = open_in_bin Sys.argv.(2) in
let content = in_channel_length ch |> really_input_string ch in
close_in ch;

let action = Sys.argv.(1) in
match action with "run" -> content |> tokenize |> run | _ -> failwith "TBD"
