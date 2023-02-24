module Token exposing (BinOpSymbol(..), Token(..), UnopSymbol(..), parseToken)

import Parser exposing ((|.), (|=), Parser)


type Token
    = TInt Int
    | TBinOp BinOpSymbol
    | LP
    | RP
    | WS


type BinOpSymbol
    = Plus
    | Minus
    | Times
    | Div


type UnopSymbol
    = UMinus


parseToken str =
    Parser.run
        (Parser.oneOf
            [ parseInt
            , parseLP
            , parseRP
            , parsePlus
            , parseMinus
            , parseTimes
            , parseDiv
            , parseWhiteSpace
            ]
        )
        str


parseWhiteSpace : Parser Token
parseWhiteSpace =
    Parser.spaces |> Parser.map (\_ -> WS)


parseInt : Parser Token
parseInt =
    Parser.int |> Parser.map (\k -> TInt k)


parseLP =
    Parser.symbol "(" |> Parser.map (\_ -> LP)


parseRP =
    Parser.symbol ")" |> Parser.map (\_ -> RP)


parsePlus =
    Parser.symbol "+" |> Parser.map (\_ -> TBinOp Plus)


parseMinus =
    Parser.symbol "-" |> Parser.map (\_ -> TBinOp Minus)


parseTimes =
    Parser.symbol "*" |> Parser.map (\_ -> TBinOp Times)


parseDiv =
    Parser.symbol "/" |> Parser.map (\_ -> TBinOp Div)


input1 =
    "2 * (3 + 5)"


tokens1 =
    [ TInt 2, TBinOp Times, LP, TInt 3, TBinOp Plus, TInt 5, RP ]
