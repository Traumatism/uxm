type token = Sym of string | Int of int 
and expr =
  | Int of int
  | Name of string
  | UnOp of string * expr
  | BinOp of string * expr * expr
  | Call of string * exprs

and tokens = token list
and exprs = expr list
and env_t = (string * expr) list
and parser = tokens -> expr * tokens
and rule_comp = expr * expr
and rule = rule_comp list

let rec string_of_token : token -> string = function
  | Int x -> string_of_int x
  | Sym x -> x

let rec string_of_expr : expr -> string = function
  | Int x -> string_of_int x
  | Name x -> x
  | UnOp (o, x) -> Printf.sprintf "(%s%s)" o (string_of_expr x)
  | BinOp (o, x, y) ->
      Printf.sprintf "(%s %s %s)" (string_of_expr x) o (string_of_expr y)
  | Call (f, xs) ->
      Printf.sprintf "%s(%s)" f
        (String.concat ", " (List.map string_of_expr xs))

and string_of_char : char -> string = function c -> Printf.sprintf "%c" c

and gen_parser (ops : string list) (p : parser) : parser =
  let wrapper (xs : tokens) =
    let x, tl = p xs in
    parse_binop x tl ops p
  in
  wrapper

and parse_binop (x : expr) (xs : tokens) (ops : string list) (p : parser) :
    expr * tokens =
  match xs with
  | Sym op :: tl when List.mem op ops ->
      let y, tl' = p tl in
      parse_binop (BinOp (op, x, y)) tl' ops p
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
          lex_chars (Sym content :: acc)
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

and gen_parsers_chain : string list list -> parser = function
  | [] -> gen_parser [] parse_unary
  | ops :: tl -> gen_parser ops (gen_parsers_chain tl)

and parse_unary : parser = function
  | ( Sym ("~" as op)
    | Sym ("-" as op)
    | Sym ("+" as op)
    | Sym ("?" as op)
    | Sym ("&" as op) )
    :: tl ->
      let e, tl' = parse_unary tl in
      (UnOp (op, e), tl')
  | xs -> parse_primary xs

and parse_primary : parser = function
  | Int n :: tl -> (Int n, tl)
  | Sym n :: Sym "(" :: tl ->
      let xs, tl' = parse_args tl in
      (Call (n, xs), tl')
  | Sym "(" :: tl -> (
      let expr, tl' = parse_expr tl in
      match tl' with
      | Sym ")" :: tl'' -> (expr, tl'')
      | _ -> failwith "Expected closing )")
  | Sym n :: tl -> (Name n, tl)
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

and matches (pattern : expr) (on : expr) (env : env_t) : env_t option =
  match (pattern, on) with
  | Int i, Int j when i = j -> Some env
  | Name v, _ -> Some ((v, on) :: env)
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

and apply (env : env_t) (e : expr) : expr =
  match e with
  | Name v -> List.assoc v env
  | Int _ -> e
  | UnOp (op, x) -> UnOp (op, apply env x)
  | BinOp (op, x, y) -> BinOp (op, apply env x, apply env y)
  | Call (f, args) -> Call (f, List.map (apply env) args)

and subst (from : expr) (into : expr) (on : expr) : expr =
  match matches from on [] with
  | Some env -> apply env into
  | None -> (
      match on with
      | Int _ | Name _ -> on
      | UnOp (op, x) -> UnOp (op, subst from into x)
      | BinOp (op, x, y) -> BinOp (op, subst from into x, subst from into y)
      | Call (f, xs) -> Call (f, List.map (subst from into) xs))

and run (xs : tokens) : unit =
  let stack = xs |> List.to_seq |> Stack.of_seq in
  let rec parse_until name =
    match Stack.pop stack with
    | Sym x when x = name -> []
    | x -> x :: parse_until name
  in
  let unwrap = function Some y -> y | None -> failwith "Unwrap None" in
  let next_name () =
    match Stack.pop stack with Sym x -> x | _ -> failwith "Expected a name"
  in
  let parseto x = List.hd (parse_math_expr (parse_until x))
  and exprs = Hashtbl.create 128
  and rules = Hashtbl.create 128 in

  while not (Stack.is_empty stack) do
    match Stack.pop stack with
    | Sym "defrule" ->
        let name = next_name () in
        let u = fun xs -> xs |> List.rev |> parse_math_expr |> List.hd in
        let rec aux cs m r =
          match Stack.pop stack with
          | Sym "|" ->
              let c' = if r = [] then cs else (m, u r) :: cs in
              aux c' (parseto "into") []
          | Sym "end" -> (m, u r) :: cs
          | t -> aux cs m (t :: r)
        in
        if Stack.top stack = Sym "|" then
          Hashtbl.add rules name (aux [] (Int 0) [])
        else
          let m = parseto "into" and r = parseto "end" in
          Hashtbl.add rules name [ (m, r) ]
    | Sym "defex" ->
        let name = next_name () in
        Hashtbl.add exprs name (parseto "end")
    | Sym "apply" ->
        let rec aux xs acc =
          match xs with [] -> acc | (x, y) :: tl -> aux tl (subst x y acc)
        in
        let r = next_name () and e = next_name () in
        Hashtbl.find_opt exprs e |> unwrap
        |> aux (Hashtbl.find rules r |> List.rev)
        |> Hashtbl.add exprs e
    | Sym "puts" ->
        next_name () |> Hashtbl.find exprs |> string_of_expr |> print_endline
    | tkn -> failwith "Unexpected token while executing"
  done;
  ()
;;

if Array.length Sys.argv <> 3 then failwith "uxm <action> <path>";

let ch = open_in_bin Sys.argv.(2) in
let content = in_channel_length ch |> really_input_string ch in
close_in ch;
content |> tokenize |> run
