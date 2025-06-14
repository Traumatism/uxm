(* Une expression est un sous-arbre d'un arbre de syntaxe *)
type expr =
  | Int of int
  | Sym of string
  | UnOp of string * expr
  | BinOp of string * expr * expr
  | Call of string * exprs

and exprs = expr list

(* Un parseur est une fonction qui prend en entrée une
expression et renvoit la première "nouvelle expression"
parsée, ainsi que le reste du flux d'expressions. *)
and parser = exprs -> expr * exprs

(* Environnement de matching, permet par exemple de lier
un nom de variable relier un nom de variable avec un
sous-AST matché. *)
and env_t = (string * expr) list

(* str_of_expr: expr -> string

Prend en entrée une expression et renvoit sa
représentation infix. *)
let rec str_of_expr : expr -> string = function
  | Int x -> string_of_int x
  | Sym x -> x
  | UnOp (o, x) -> Printf.sprintf "%s%s" o (str_of_expr x)
  | BinOp (o, xs, ys) ->
      Printf.sprintf "(%s %s %s)" (str_of_expr xs) o (str_of_expr ys)
  | Call (f, es) ->
      Printf.sprintf "%s(%s)" f (String.concat ", " (List.map str_of_expr es))

(* <*>: expr -> expr -> string -> expr 

Opérateur qui crée un sous-arbre associé à une opération binaire *.
Les deux premières entrées sont les deux fils (LHS, RHS) et la
troisième est le symbole représentant l'opération * en question. *)
and ( <*> ) : expr -> expr -> string -> expr = fun x y o -> BinOp (o, x, y)

(* parse_args : expr -> exprs * exprs

Permet de parser récursivement les arguments d'une fonction.
En gros c'est presque un parser. *)
and parse_args : exprs -> exprs * exprs = function
  | Sym ")" :: tl -> ([], tl)
  | tokens -> (
      match parse tokens with
      | e, Sym "," :: tl ->
          let args, tl' = parse_args tl in
          (e :: args, tl')
      | e, Sym ")" :: tl -> (e :: [], tl)
      | _ -> failwith "")

(* parse_unary : parser

Etape suivant le parsing des opérateurs binaires: on parse
les opérateur d'arité 1: fonctions, opérateurs unaires... *)
and parse_unary : parser = function
  | Sym o :: tl when List.mem o [ "~"; "-"; "+"; "&" ] ->
      let e, tl' = parse_unary tl in
      (UnOp (o, e), tl')
  | Sym n :: Sym "(" :: tl ->
      let es, tl' = parse_args tl in
      (Call (n, es), tl')
  | Sym "(" :: tl -> (
      match parse tl with e, Sym ")" :: tl' -> (e, tl') | _ -> failwith "")
  | x :: tl -> (x, tl)
  | [] -> failwith ""

(* parse : parser

Permet de parser complètement une expression. *)
and parse : parser =
 fun xs ->
  create_parsers_chain
    [
      [ "=>"; "<=>"; "->"; "<->"; "<~>"; "~>" ];
      [ "\\/" ];
      [ "/\\" ];
      [ "=" ];
      [ "<$>" ];
      [ "+"; "-" ];
      [ "."; "*"; "/" ];
      [ "^" ];
    ]
    xs

(* create_parser : string list -> parser -> parser

Crée un parseur pour une liste d'opérateurs "associés",
par exemples les opérateurs en notation multiplicative. *)
and create_parser (os : string list) (p : parser) : parser =
  let rec aux = function
    | x, Sym o :: tl when List.mem o os ->
        let y, tl' = p tl in
        aux ((x <*> y) o, tl')
    | x -> x
  in
  fun xs -> aux (p xs)

(* create_parsers_chain : string list list -> parser

Crée un parseur qui respecte un ordre de priorité sur les
opérateurs. *)
and create_parsers_chain : string list list -> parser = function
  | [] -> create_parser [] parse_unary
  | os :: tl -> create_parser os (create_parsers_chain tl)

(* tokenize : string -> exprs

Permet d'obtenir une liste d'expressions (que des Sym et des Int)
à partir d'une chaine de caractères. *)
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

(* matches : env_t -> expr * expr -> env

Effectue un pattern matching en conservant les matchs
dans une liste associative. La première expression est
le pattern et la seconde est la cible sur laquelle on match. *)
and matches (env : env_t) ((pattern, target) : expr * expr) : env_t =
  match (pattern, target) with
  (* Un litéral du pattern correspond à un litéral
   de l'arbre cible. De plus si c'est un nom, on le
   rajoute à l'environnement pour procéder à une
   identification. *)
  | Int x, Int y when x = y -> env
  | Sym x, _ -> (x, target) :: env
  (* On a trouvé un bon candidat pour une opération binaire,
   on regarde alors le sous-arbre de gauche et le sous-arbre droit. *)
  | BinOp (o, x, y), BinOp (o', x', y') when o = o' ->
      matches (matches env (x, x')) (y, y')
  (* Même chose sur les opérateurs d'arité 1 et géneralisation aux
   fonctions d'arité quelconque. *)
  | UnOp (o, x), UnOp (o', x') when o = o' -> matches env (x, x')
  | Call (f, xs), Call (g, ys) when List.length xs = List.length ys -> (
      let rec aux (e : env_t) : exprs * exprs -> env_t = function
        | [], [] -> e
        | x :: t, y :: t' -> (
            match matches e (x, y) with [] -> [] | e' -> aux e' (t, t'))
        | _ -> []
      in
      match aux ((f, Call (f, ys)) :: env) (xs, ys) with
      | [] -> []
      | x -> (f, Sym g) :: x)
  | _ -> []

(* apply : env_t -> expr -> expr

Applique des modifications sur une expression selon
un environnement donnée. *)
and apply (env : env_t) : expr -> expr = function
  | Sym v -> List.assoc v env
  | Int _ as e -> e
  | UnOp (o, x) -> UnOp (o, apply env x)
  | BinOp (o, x, y) -> (apply env x <*> apply env y) o
  | Call (f, args) ->
      let f' = match List.assoc f env with Sym v -> v | _ -> failwith "" in
      Call (f', List.map (apply env) args)

(* subst : expr (A) * expr (B) -> expr (C) -> expr

Applique la substitution des sous-arbres qui "ressemblent"
à (A) par les sous-arbres (B) sur l'arbre de syntaxe (C). *)
and subst ((x, y) as r : expr * expr) (z : expr) : expr =
  match matches [] (x, z) with
  | [] -> (
      match z with
      | UnOp (o, x) -> UnOp (o, subst r x)
      | BinOp (o, x, y) -> (subst r x <*> subst r y) o
      | Call (f, xs) -> Call (f, List.map (subst r) xs)
      | z -> z)
  | env -> apply env y

(* run : exprs -> unit

Procésseur uXm *)
and run (xs : exprs) =
  let stack = List.to_seq xs |> Stack.of_seq in
  let rec pu n = match pop () with Sym n' when n' = n -> [] | n' -> n' :: pu n
  and sym () = match pop () with Sym n -> n | _ -> failwith ""
  and pop () = Stack.pop stack
  and pto n = pu n |> parse |> fun (x, _) -> x
  and u xs = List.rev xs |> parse |> fun (x, _) -> x
  and es = Hashtbl.create 128
  and ss = Hashtbl.create 128
  and rs = Hashtbl.create 128 in

  while not (Stack.is_empty stack) do
    match pop () with
    (* apply <nom règle> <nom expression> *)
    | Sym "apply" ->
        let r = sym () and e = sym () in
        let rec aux e =
          let e' = List.fold_left (fun acc r -> subst r acc) e cs in
          if e = e' then e else aux e'
        and cs = Hashtbl.find rs r in
        aux (Hashtbl.find es e) |> Hashtbl.replace es e
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
    (*
      defrule <nom>
        <expr> into <expr'>
      end

     (ou)

      defrule <nom>
        | <expr1> into <expr1'>
        ...
        | <exprN> into <exprN'>
      end
     *)
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
    (*
    defex <nom>
        <expr>
    end
    *)
    | Sym "defex" ->
        let n = sym () in
        Hashtbl.add es n (pto "end")
    (* puts <nom> *)
    | Sym "puts" -> sym () |> Hashtbl.find es |> str_of_expr |> print_endline
    | t -> failwith (Printf.sprintf "Unexpected token: %s" (str_of_expr t))
  done;
  ()
;;

if Array.length Sys.argv <> 2 then failwith "Usage: uxm <path>";
let ch = open_in_bin Sys.argv.(1) in
in_channel_length ch |> really_input_string ch |> tokenize |> run;
close_in ch
