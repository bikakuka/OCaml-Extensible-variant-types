Copyright 2024-2025, Sheyko Dmitry, Matvey Nazdruhin
SPDX-License-Identifier: LGPL-3.0-or-later

  $ ../bin/main.exe
  [(LetRec ("factorial", ["n"],
      (If ((BinOp (Lte, (Ident "n"), (Literal (Int 1)))), (Literal (Int 1)),
         (BinOp (Mul, (Ident "n"),
            (App ((Ident "factorial"),
               (BinOp (Sub, (Ident "n"), (Literal (Int 1))))))
            ))
         )),
      (Print (App ((Ident "factorial"), (Literal (Int 5)))))))
    ]
