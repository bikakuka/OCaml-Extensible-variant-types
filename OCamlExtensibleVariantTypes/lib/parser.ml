(** Copyright 2024-2025, Sheyko Dmitry, Nazdruhin Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast
open Base

let whitespace =
  skip_many
    (satisfy (function
      | ' ' | '\t' | '\n' | '\r' -> true
      | _ -> false))
;;

let is_keyword = function
  | "let"
  | "rec"
  | "fun"
  | "if"
  | "then"
  | "else"
  | "true"
  | "print"
  | "false"
  | "match"
  | "with"
  | "in" -> true
  | _ -> false
;;

let token p = whitespace *> p <* whitespace
let parse_token p = whitespace *> string p
let brackets p = parse_token "(" *> p <* parse_token ")"
let brackets_or_not p = brackets p <|> p
let start_parsing parser string = parse_string ~consume:All parser string

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

(** Identifier parser *)
let ident =
  token
    (take_while1 (function
      | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' -> true
      | _ -> false))
;;

let check_ident cond =
  cond
  >>= fun v ->
  if is_keyword v
  then fail ("You can not use \"" ^ v ^ "\" keywords as vars")
  else if Char.is_digit @@ String.get v 0
  then fail "Identifier first symbol is letter, not digit"
  else return v
;;

(** Literal parser *)
let int_literal =
  token
    (take_while1 (function
      | '0' .. '9' -> true
      | _ -> false))
  >>| int_of_string
  >>| fun i -> Literal (Int i)
;;

let bool_literal =
  token (string "true") *> return (Literal (Bool true))
  <|> token (string "false") *> return (Literal (Bool false))
;;

let char_literal =
  token (char '\'') *> any_char <* token (char '\'') >>| fun c -> Literal (Char c)
;;

let string_literal =
  token (char '"')
  *> take_while (function
    | '"' -> false
    | _ -> true)
  <* token (char '"')
  >>| fun s -> Literal (String s)
;;

let literal = int_literal <|> bool_literal <|> char_literal <|> string_literal

(** Binary operator parser *)
let bin_op =
  token
    (choice
       [ string "+" *> return Add
       ; string "-" *> return Sub
       ; string "*" *> return Mul
       ; string "/" *> return Div
       ; string "=" *> return Eq
       ; string "<>" *> return Neq
       ; string "<" *> return Lt
       ; string ">" *> return Gt
       ; string "<=" *> return Lte
       ; string ">=" *> return Gte
       ; string "&&" *> return And
       ; string "||" *> return Or
       ])
;;

(** Unary operator parser *)
let un_op = token (choice [ string "!" *> return Not; string "-" *> return Neg ])

(** Type annotation parser *)
let type_annot =
  fix (fun type_annot ->
    let t_int = token (string "int") *> return TInt in
    let t_bool = token (string "bool") *> return TBool in
    let t_tuple =
      token (string "tuple") *> token (char '(') *> sep_by (token (char ',')) type_annot
      <* token (char ')')
      >>| fun t -> TTuple t
    in
    let t_list =
      token (string "list") *> token (char '[') *> type_annot
      <* token (char ']')
      >>| fun t -> TList t
    in
    let t_option =
      string "option" *> char '[' *> type_annot <* char ']' >>| fun t -> TOption t
    in
    let t_fun =
      token (string "fun") *> token (char '(') *> type_annot
      <* token (char ')') *> token (char '(') *> type_annot
      <* token (char ')')
      >>= fun t1 -> type_annot >>= fun t2 -> return (TFun (t1, t2))
    in
    choice [ t_int; t_bool; t_tuple; t_list; t_option; t_fun ])
;;

(** Pattern parser *)
let pattern =
  fix (fun (pattern : pattern t) ->
    let pvar : pattern t = token ident >>| fun id -> PVar id in
    let ptuple : pattern t =
      token (char '(') *> sep_by (token (char ',')) pattern
      <* token (char ')')
      >>| fun p -> PTuple p
    in
    let plist : pattern t =
      token (char '[') *> sep_by (token (char ',')) pattern
      <* token (char ']')
      >>| fun p -> PList p
    in
    (*let poption : pattern t =
      char '[' *> option pattern <* char ']'
      >>= function
      | Some p -> return (POption (Some p))
      | None -> return (POption None)
      in*)
    choice [ pvar; ptuple; plist (*poption*) ])
;;

(*let pvar : pattern t = ident >>| fun id -> PVar id
  let ptuple : pattern t =
  char '(' *> sep_by (char ',') pattern <* char ')'
  >>| fun p -> PTuple p
  let plist : pattern t =
  char '[' *> sep_by (char ',') pattern <* char ']'
  >>| fun p -> PList p
  let parse_pattern = choice [pvar; ptuple;plist ]
*)
(* Expression parser *)
let debug_parser parser name show =
  parser
  >>= fun result ->
  Format.printf "Parsed %s: %s\n" name (show result);
  return result
;;

let p_op char_op op = parse_token char_op *> return (fun e1 e2 -> BinOp (op, e1, e2))
let pmulti = p_op "*" Mul <|> p_op "/" Div
let padd = p_op "+" Add <|> p_op "-" Sub
let pcomp = p_op ">=" Gte <|> p_op ">" Gt <|> p_op "<=" Lte <|> p_op "<" Lt
let peq = p_op "=" Eq <|> p_op "<>" Neq
let pconj = p_op "&&" And
let pdisj = p_op "||" Or

let parse_ebinop x =
  let multi = chainl1 x pmulti in
  let add = chainl1 multi padd in
  let comp = chainl1 add pcomp in
  let eq = chainl1 comp peq in
  let conj = chainl1 eq pconj in
  chainl1 conj pdisj
;;

let expr =
  fix (fun expr ->
    let literal_expr = literal in
    let ident_expr = token (check_ident ident) >>| fun id -> Ident id in
    let un_op_expr = un_op >>= fun op -> expr >>= fun e -> return (UnOp (op, e)) in
    (* let bin_op_expr =
       debug_parser expr "e1"
       >>= fun e1 ->
       bin_op
       >>= fun op -> debug_parser expr "e2" >>= fun e2 -> return (BinOp (op, e1, e2))
       in *)
    let if_expr =
      token (string "if") *> expr
      >>= fun cond ->
      token (string "then") *> expr
      >>= fun e1 -> token (string "else") *> expr >>= fun e2 -> return (If (cond, e1, e2))
    in
    let let_expr =
      token (string "let") *> ident
      >>= fun id ->
      token (string "=") *> expr
      >>= fun e1 -> token (string "in") *> expr >>= fun e2 -> return (Let (id, e1, e2))
    in
    let let_rec_expr =
      debug_parser (token (string "let")) "let" show_ident >>= fun _ ->
        debug_parser (token (string "rec")) "rec" show_ident >>= fun _ ->
        debug_parser ident "ident" show_ident >>= fun id ->
        many ident   >>= fun args ->
        debug_parser (token (string "=")) "=" show_ident >>= fun _ ->
        debug_parser expr "expr" show_expr >>= fun e1 ->
        debug_parser (token (string "in")) "in" show_ident >>= fun _ ->
        debug_parser expr "expr" show_expr >>= fun e2 ->
        return (LetRec (id, args, e1, e2))
    in
    let bin_op_expr =
      brackets_or_not
      @@ parse_ebinop (brackets @@ parse_ebinop expr <|> ident_expr <|> literal_expr)
    in
    let fun_expr =
      token (string "fun") *> pattern
      >>= fun pat ->
      token (char ':') *> type_annot
      >>= fun annot ->
      token (string "->") *> expr >>= fun body -> return (Fun (pat, annot, body))
    in
    let app_expr = expr >>= fun e1 -> expr >>= fun e2 -> return (App (e1, e2)) in
    let tuple_expr =
      token (char '(') *> sep_by (token (char ',')) expr
      <* token (char ')')
      >>| fun e -> Tuple e
    in
    let list_expr =
      token (char '[') *> sep_by (token (char ',')) expr
      <* token (char ']')
      >>| fun e -> List e
    in
    let print_expr = token (string "print") *> expr >>= fun e -> return (Print e) in
    choice
      [ let_rec_expr
      ; let_expr
      ; literal_expr
      ; bin_op_expr
      ; if_expr
      ; un_op_expr
      ; fun_expr
      ; print_expr
      ; app_expr
      ; tuple_expr
      ; list_expr
      ; ident_expr
      ])
;;

(** Program parser *)
let program = many expr
