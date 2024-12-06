(** Copyright 2024-2025, Sheyko Dmitry, Nazdruhin Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Identifier *)
type ident = string [@@deriving eq, show { with_path = false }]

type literal =
  | Int of int (** integer [1] *)
  | Bool of bool (** boolean [true, false] *)
  | Char of char (** char ["a"] *)
  | String of string (** string as ["bikakuka"] *)
[@@deriving eq, show { with_path = false }]

type bin_op =
  | Add (* operation of addition *)
  | Sub (* operation of subtraction *)
  | Mul (* operation of multiplication *)
  | Div (* operation of division *)
  | Eq (*  equal *)
  | Neq (* not equal *)
  | Lt (* lesser then*)
  | Gt (* grather then *)
  | Lte (* lesser then or equal *)
  | Gte (* grather then or equal *)
  | And (* conjunction *)
  | Or (* disjuncture *)
[@@deriving eq, show { with_path = false }]

(* inversio *)
type un_op = Not | Neg [@@deriving eq, show { with_path = false }]

type type_annot =
  | TInt (* type integer *)
  | TBool (* type boolean *)
  | TTuple of type_annot list (* type tuple *)
  | TList of type_annot (* type list *)
  | TOption of type_annot (* type optional value *)
  | TFun of type_annot * type_annot (* type function *)
[@@deriving eq, show { with_path = false }]

type pattern =
  | PVar of string (* variable pattern *)
  | PTuple of pattern list (** Patterns [(P1, ..., Pn)]. *)
  | PList of pattern list (** Patterns [P1, ..., Pn]. *)
  | POption of pattern option (* optional patterns *)
[@@deriving eq, show { with_path = false }]

type expr =
  | Literal of literal (* Literal value *)
  | Ident of ident (* Identifier *)
  | BinOp of bin_op * expr * expr (* Binary operation *)
  | UnOp of un_op * expr (* Unary operation *)
  | If of expr * expr * expr (* Conditional operator *)
  | Let of ident * expr * expr (* Binding a value to an identifier *)
  | LetRec of
      ident * ident list * expr * expr (* Recursive binding a value to an identifier *)
  | Fun of pattern * type_annot * expr (* Anonymous function *)
  | App of expr * expr (* Function application *)
  | Tuple of expr list (* Tuple of expressions *)
  | List of expr list (* List of expressions *)
  | Option of expr option (* Optional expression *)
  | Print of expr (* Print expression *)
[@@deriving eq, show { with_path = false }]

type program = expr list [@@deriving eq, show { with_path = false }]