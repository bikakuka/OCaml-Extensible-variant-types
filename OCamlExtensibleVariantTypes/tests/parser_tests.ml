open OCamlExtensibleVariantTypes_lib.Ast
open OCamlExtensibleVariantTypes_lib.Parser

let start_test parser show input =
  let res = start_parsing parser input in
  match res with
  | Ok res -> Format.printf "%s" (show res)
  | Error err -> Format.printf "%s" err
;;

let%expect_test _ =
  let test = "52" in
  start_test literal show_expr test;
  [%expect {| (Literal (Int 52)) |}]
;;

let%expect_test _ =
  let test = "vaka" in
  start_test pattern show_pattern test;
  [%expect {| (PVar "vaka") |}]
;;

let%expect_test _ =
  let test = "(v,c, d)" in
  start_test pattern show_pattern test;
  [%expect {| (PTuple [(PVar "v"); (PVar "c"); (PVar "d")]) |}]
;;

let%expect_test _ =
  let test = "+" in
  start_test bin_op show_bin_op test;
  [%expect {| Add |}]
;;

let%expect_test _ =
  let test = "!" in
  start_test un_op show_un_op test;
  [%expect {| Not |}]
;;

(* Тест для булевого литерала *)
let%expect_test _ =
  let test = "true" in
  start_test literal show_expr test;
  [%expect {| (Literal (Bool true)) |}]
;;

(* Тест для строкового литерала *)
let%expect_test _ =
  let test = "\"bikakuka\"" in
  start_test literal show_expr test;
  [%expect {| (Literal (String "bikakuka")) |}]
;;

(* Тест для целочисленного литерала *)
let%expect_test _ =
  let test = "42" in
  start_test literal show_expr test;
  [%expect {| (Literal (Int 42)) |}]
;;

(* Тест для символьного литерала *)
let%expect_test _ =
  let test = "'a'" in
  start_test literal show_expr test;
  [%expect {| (Literal (Char 'a')) |}]
;;

(* Тест для унарной операции *)
let%expect_test _ =
  let test = "!true" in
  start_test expr show_expr test;
  [%expect {| (UnOp (Not, (Literal (Bool true)))) |}]
;;

(* Тест для выражения if *)
let%expect_test _ =
  let test = "if true then 1 else 2" in
  start_test expr show_expr test;
  [%expect {| (If ((Literal (Bool true)), (Literal (Int 1)), (Literal (Int 2)))) |}]
;;

let%expect_test _ =
  let test = "(1 + 4)" in
  start_test expr show_expr test;
  [%expect {| (BinOp (Add, (Literal (Int 1)), (Literal (Int 4)))) |}]
;;

(* Тест для выражения let *)
let%expect_test _ =
  let test = "let x = 1 in x + 2" in
  start_test expr show_expr test;
  [%expect
    {| (Let ("x", (Literal (Int 1)), (BinOp (Add, (Ident "x"), (Literal (Int 2)))))) |}]
;;

(* Тест для выражения print *)
let%expect_test _ =
  let test = "print 42" in
  start_test expr show_expr test;
  [%expect {| (Print (Literal (Int 42))) |}]
;;

(* Тест для функции *)
let%expect_test _ =
  let test = "fun x:int -> x + 1" in
  start_test expr show_expr test;
  [%expect {| (Fun ((PVar "x"), TInt, (BinOp (Add, (Ident "x"), (Literal (Int 1)))))) |}]
;;

(* Тест для рекурсивного определения функции *)
let%expect_test _ =
  let test = "let rec sum n = if n = 0 then 0 else n + sum (n - 1) in sum" in
  start_test expr show_expr test;
  [%expect {| (Print (Literal (Int 4w2))) |}]
;;

