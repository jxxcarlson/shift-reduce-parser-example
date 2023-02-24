module Examples exposing (tokensBad, tokensGood)

import Token exposing (Token(..), BinOpSymbol(..), UnopSymbol(..))

{-
    > parseEval tokensGood
    Ok 16 : Result (List (Either.Either Token.Token Expr)) Int

    > parseEval tokensBad
    Err [Right ELP,Right ERP]
-}

inputBad = "()"
inputGood = "2 * (3 + 5)"

tokensBad = [RP, LP]
tokensGood = [TInt 2, TBinOp Times, LP, TInt 3,TBinOp Plus, TInt 5, RP ]
