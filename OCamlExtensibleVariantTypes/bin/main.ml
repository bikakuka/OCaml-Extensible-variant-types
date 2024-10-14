open OCamlExtensibleVariantTypes_lib.Ast

let () =
  let factorial_expr =
    [ LetRec
        ( "factorial"
        , [ "n" ]
        , If
            ( BinOp (Lte, Ident "n", Literal (Int 1))
            , Literal (Int 1)
            , BinOp
                ( Mul
                , Ident "n"
                , App (Ident "factorial", BinOp (Sub, Ident "n", Literal (Int 1))) ) )
        , Print (App (Ident "factorial", Literal (Int 5))) )
    ]
  in
  print_endline (show_program factorial_expr)
;;
