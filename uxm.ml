type expr =
  | Int of int
  | Sym of string
  | UnOp of string * expr
  | BinOp of string * expr * expr
  | Call of string * exprs

and exprs = expr list
and parser = exprs -> expr * exprs
and env_t = (string * expr) list

let rec str_of_expr : expr -> string = function
  | Int x -> string_of_int x
  | Sym x -> x
  | UnOp (o, x) -> Printf.sprintf "%s%s" o (str_of_expr x)
  | BinOp (o, xs, ys) ->
      Printf.sprintf "(%s %s %s)" (str_of_expr xs) o (str_of_expr ys)
  | Call (f, es) ->
      Printf.sprintf "%s(%s)" f (String.concat ", " (List.map str_of_expr es))

and ( <*> ) : expr -> expr -> string -> expr = fun x y o -> BinOp (o, x, y)
and ( <$> ) : string -> exprs -> expr = fun f xs -> Call (f, xs)

and parse_unary : parser = function
  | Sym f :: tl when List.mem f [ "~"; "-"; "+"; "&" ] ->
      let x, tl' = parse_unary tl in
      (UnOp (f, x), tl')
  | Sym f :: Sym "(" :: tl ->
      let rec aux = function
        | Sym ")" :: tl -> ([], tl)
        | tokens -> (
            match parse tokens with
            | x, Sym "," :: tl ->
                let xs, tl' = aux tl in
                (x :: xs, tl')
            | x, Sym ")" :: tl -> (x :: [], tl)
            | _ -> failwith "")
      in
      let xs, tl = aux tl in
      (f <$> xs, tl)
  | Sym "(" :: tl -> (
      match parse tl with x, Sym ")" :: tl' -> (x, tl') | _ -> failwith "")
  | x :: tl -> (x, tl)
  | [] -> failwith ""

and parse : parser =
 fun xs ->
  let rec aux = function
    | [] -> fun xs -> aux2 [] parse_unary (parse_unary xs)
    | fs :: tl ->
        let p = aux tl in
        fun xs -> aux2 fs p (p xs)
  and aux2 fs p = function
    | x, Sym f :: tl when List.mem f fs ->
        let y, tl' = p tl in
        aux2 fs p ((x <*> y) f, tl')
    | x -> x
  in
  aux
    [
      [ "=>"; "<=>"; "->"; "<->"; "<~>"; "~>" ];
      [ "\\/" ];
      [ "/\\" ];
      [ "<$>" ];
      [ "=" ];
      [ "..." ];
      [ "+"; "-" ];
      [ "."; "*"; "/" ];
      [ "^" ];
    ]
    xs

and tokenize (str : string) : exprs =
  let rec take_while p acc =
    match !chars with
    | c :: tl when p c ->
        chars := tl;
        take_while p (acc ^ String.make 1 c)
    | _ -> acc
  and chars = ref (String.to_seq str |> List.of_seq)
  and is_alph = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
  and is_ws = function ' ' | '\t' | '\n' -> true | _ -> false
  and is_dgt = function '0' .. '9' -> true | _ -> false
  and aux acc =
    match !chars with
    | [] -> acc
    | c :: tl -> (
        chars := tl;
        if is_ws c then aux acc
        else if is_alph c then
          let v =
            take_while
              (fun c -> is_dgt c || is_alph c || c = '_')
              (String.make 1 c)
          in
          aux (Sym v :: acc)
        else if is_dgt c then
          let v = String.make 1 c |> take_while is_dgt |> int_of_string in
          aux (Int v :: acc)
        else
          match c :: tl with
          | '(' :: '*' :: tl' ->
              let rec ignore () =
                match !chars with
                | [] -> failwith "EOF"
                | '*' :: ')' :: tl' -> chars := tl'
                | x :: tl' ->
                    chars := tl';
                    ignore ()
              in
              ignore ();
              aux acc
          | '.' :: '.' :: '.' :: tl' ->
              chars := tl';
              aux (Sym "..." :: acc)
          | '<' :: (('=' | '-' | '~' | '$') as bar) :: '>' :: tl' ->
              chars := tl';
              aux (Sym ("<" ^ String.make 1 bar ^ ">") :: acc)
          | '<' :: '>' :: tl' ->
              chars := tl';
              aux (Sym "<>" :: acc)
          | (('=' | '-' | '~') as bar) :: '>' :: tl' ->
              chars := tl';
              aux (Sym (String.make 1 bar ^ ">") :: acc)
          | ('\\' as l) :: ('/' as r) :: tl' | ('/' as l) :: ('\\' as r) :: tl'
            ->
              chars := tl';
              aux (Sym (Printf.sprintf "%c%c" l r) :: acc)
          | _ -> aux (Sym (String.make 1 c) :: acc))
  in
  aux []

and matches (env : env_t) ((pattern, target) : expr * expr) : env_t =
  match (pattern, target) with
  | Int x, Int y when x = y -> env
  | Sym x, _ -> (x, target) :: env
  | BinOp (f, x, y), BinOp (g, x', y') when f = g ->
      matches (matches env (x, x')) (y, y')
  | UnOp (f, x), UnOp (g, y) when f = g -> matches env (x, y)
  | Call (f, xs), Call (g, ys) when List.length xs = List.length ys -> (
      let rec aux (e : env_t) : exprs * exprs -> env_t = function
        | [], [] -> e
        | x :: t, y :: t' -> (
            match matches e (x, y) with [] -> [] | e' -> aux e' (t, t'))
        | _ -> []
      in
      match aux ((f, f <$> ys) :: env) (xs, ys) with
      | [] -> []
      | x -> (f, Sym g) :: x)
  | _ -> []

and apply (env : env_t) : expr -> expr = function
  | Sym v -> (
      try List.assoc v env
      with Not_found -> failwith (Printf.sprintf "Not found: %s" v))
  | Int _ as e -> e
  | UnOp (o, x) -> UnOp (o, apply env x)
  | BinOp (o, x, y) -> (apply env x <*> apply env y) o
  | Call (f, xs) ->
      let f' = match List.assoc f env with Sym v -> v | _ -> failwith "" in
      f' <$> List.map (apply env) xs

and subst ((x, y) as r : expr * expr) (z : expr) : expr =
  match matches [] (x, z) with
  | [] -> (
      match z with
      | UnOp (o, x) -> UnOp (o, subst r x)
      | BinOp (o, x, y) -> (subst r x <*> subst r y) o
      | Call (f, xs) -> f <$> List.map (subst r) xs
      | z -> z)
  | env -> apply env y

and run (xs : exprs) =
  let stack = List.to_seq xs |> Stack.of_seq in
  let rec pu n = match pop () with Sym n' when n' = n -> [] | n' -> n' :: pu n
  and sym () = match pop () with Sym n -> n | _ -> failwith "..."
  and pop () = Stack.pop stack
  and pto n = pu n |> parse |> fun (x, _) -> x
  and u xs = List.rev xs |> parse |> fun (x, _) -> x
  and es = Hashtbl.create 128
  and ss = Hashtbl.create 128
  and rs = Hashtbl.create 128 in
  while not (Stack.is_empty stack) do
    match pop () with
    | Sym "applyonce" ->
        let r = sym () and e_name = sym () in
        let e = Hashtbl.find es e_name in
        Printf.printf "\n[*] Applying rule: `%s` on expression: %s\n" r
          (str_of_expr e);

        let cs = Hashtbl.find rs r in
        let new_e = List.fold_left (fun acc r -> subst r acc) e cs in
        Printf.printf " => Result: %s\n" (str_of_expr new_e);
        Hashtbl.add es e_name new_e
    | Sym "apply" ->
        let r = sym () and e_name = sym () in
        let e = Hashtbl.find es e_name in
        Printf.printf "\n[*] Applying rule: `%s` on expression: %s\n" r
          (str_of_expr e);
        let rec aux e =
          let e' = List.fold_left (fun acc r -> subst r acc) e cs in
          if e = e' then e else aux e'
        and cs = Hashtbl.find rs r in
        let new_e = aux e in
        Printf.printf " => Result: %s\n" (str_of_expr new_e);
        Hashtbl.replace es e_name new_e
    | Sym "struct" ->
        let n = sym () in
        let rec aux acc =
          match pop () with
          | Sym "|" -> acc :: aux []
          | Sym "end" -> [ acc ]
          | t -> aux (t :: acc)
        in
        let laws =
          aux [] |> List.tl
          |> List.map (fun law ->
                 match List.rev law with
                 | Sym (("intern" | "extern") as t)
                   :: Sym symbol
                   :: identity :: axiom_ids ->
                     let x = (Sym "%" <*> Sym "%%") "<$>"
                     and y = (Sym "%" <*> Sym "%%") symbol in
                     let axioms =
                       List.map str_of_expr axiom_ids
                       |> List.map (Hashtbl.find es)
                       |> List.map (fun z -> subst (x, y) z)
                     in
                     (t, symbol, identity, axioms)
                 | _ -> failwith "Invalid, structure definition")
        in
        Hashtbl.add ss n laws
    | Sym "defrule" ->
        let n = sym () in
        let rec aux cs m r =
          match pop () with
          | Sym "|" ->
              let c' = if r = [] then cs else (m, u r) :: cs in
              aux c' (pto "into") []
          | Sym "end" -> (m, u r) :: cs
          | t -> aux cs m (t :: r)
        in
        if Stack.top stack = Sym "|" then Hashtbl.add rs n (aux [] (Int 0) [])
        else
          let m = pto "into" and r = pto "end" in
          Hashtbl.add rs n [ (m, r) ]
    | Sym "defex" ->
        let n = sym () in
        Hashtbl.add es n (pto "end")
    | t -> failwith (Printf.sprintf "Unexpected token: %s" (str_of_expr t))
  done;
  ()
;;

if Array.length Sys.argv <> 2 then failwith "Usage: uxm <path>";
let ch = open_in_bin Sys.argv.(1) in
in_channel_length ch |> really_input_string ch |> tokenize |> run;
close_in ch
