module Token exposing (..)

type Token = TInt Int | BinOp BinOpSymbol | LP | RP

type BinOpSymbol = Plus | Minus | Times | Div

input1 = "2 * (3 + 5)"

tokens1 = [TInt 2, BinOp Times, LP, TInt 3,BinOp Plus, TInt 5, RP ]

