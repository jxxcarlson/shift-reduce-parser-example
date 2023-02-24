module Token exposing (BinOpSymbol(..), Token(..), UnopSymbol(..), parse)

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


parse : String -> Result (List Parser.DeadEnd) (List Token)
parse str = Parser.run parser (String.replace " " "" str)

parser = many tokenParser

tokenParser = Parser.oneOf
  [ intParser
  , leftParenParser
  , rightParenParser
  , plusParser
  , minusParser
  , timesParser
  , divParser
  ]

parseWhiteSpace : Parser Token
parseWhiteSpace =
    Parser.spaces |> Parser.map (\_ -> WS)


intParser : Parser Token
intParser =
    Parser.int |> Parser.map (\k -> TInt k)


leftParenParser =
    Parser.symbol "(" |> Parser.map (\_ -> LP)


rightParenParser =
    Parser.symbol ")" |> Parser.map (\_ -> RP)


plusParser =
    Parser.symbol "+" |> Parser.map (\_ -> TBinOp Plus)


minusParser =
    Parser.symbol "-" |> Parser.map (\_ -> TBinOp Minus)


timesParser =
    Parser.symbol "*" |> Parser.map (\_ -> TBinOp Times)


divParser =
    Parser.symbol "/" |> Parser.map (\_ -> TBinOp Div)

{-| Apply a parser zero or more times and return a list of the results.
-}
many : Parser a -> Parser (List a)
many p =
    Parser.loop [] (manyHelp p)


manyHelp : Parser a -> List a -> Parser (Parser.Step (List a) (List a))
manyHelp p vs =
    Parser.oneOf
        [ Parser.succeed (\v -> Parser.Loop (v :: vs))
            |= p
            |. Parser.spaces
        , Parser.succeed ()
            |> Parser.map (\_ -> Parser.Done (List.reverse vs))
        ]

input1 =
    "2 * (3 + 5)"


tokens1 =
    [ TInt 2, TBinOp Times, LP, TInt 3, TBinOp Plus, TInt 5, RP ]
