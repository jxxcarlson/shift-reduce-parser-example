module Expr exposing (parseEval, Token(..), Expr(..))

import Either exposing(Either(..))
import List.Extra

type alias State = { tokens : List Token, currentPosition : Int, stack : List (Either Token Expr)}

type Expr = Integer Int | BinOp BinOpSymbol | Unop UnopSymbol | BinopExpr BinOpSymbol Expr Expr | ELP | ERP

type Token = TInt Int | TBinOp BinOpSymbol | LP | RP

type BinOpSymbol = Plus | Minus | Times | Div

type UnopSymbol = UMinus


init : List Token -> State
init tokens = { tokens = tokens, currentPosition = 0, stack = []}

{-|
    > parseEval tokensGood
    Ok 16 : Result (List (Either.Either Token Expr)) Int

    > parseEval tokensBad
    Err [Right ERP,Right ELP]
-}
parseEval: List Token -> Result (List (Either Token Expr)) Int
parseEval tokens =
    let finalState = reduceState (init tokens) in
    case finalState.stack of
        Right (Integer n) :: [] -> Ok n
        _ -> Err finalState.stack

inputBad = "()"
inputGood = "2 * (3 + 5)"

tokensBad = [RP, LP]
tokensGood = [TInt 2, TBinOp Times, LP, TInt 3,TBinOp Plus, TInt 5, RP ]


{-|

    > reduceState (init tokensGood)
    { currentPosition = 7, stack = [Right (Integer 16)], tokens = [TInt 2,TBinOp Times,LP,TInt 3,TBinOp Plus,TInt 5,RP] }

-}
reduceState : State -> State
reduceState state =
    reduceStateAux (state, state |> shift |> reduce) |> Tuple.second

{-|
    Look for fixed point
-}
reduceStateAux: (State,  State) -> (State, State)
reduceStateAux (state1, state2) =
    if state2.currentPosition > List.length state2.tokens || state1.stack == state2.stack then (state1, state2)
    else
    reduceStateAux (state2, state2 |> shift |> reduce)

shift : State -> State
shift state =
   case List.Extra.getAt state.currentPosition state.tokens of
       Nothing -> state
       Just token ->  { state | currentPosition = state.currentPosition + 1, stack = Left token :: state.stack}


reduce1 : State -> State
reduce1 state =
    case state.stack of
       [] -> state


       (top::rest) ->
            case top of
              Left (TInt k) -> { state | stack = Right (Integer k) :: rest }
              Left (TBinOp s) ->  { state | stack = Right (BinOp s) :: rest }
              Left LP ->  { state | stack = Right ELP :: rest }
              Left RP ->  { state | stack = Right ERP :: rest }
              _ -> state



reduce3 : State -> State
reduce3 state =
    case state.stack of
       [] -> state


       (left::middle::right::rest) ->
                  case (left, middle, right) of
                    (Right (Integer a), Right (BinOp op), Right (Integer b)) ->
                        case op of
                            Plus -> { state | stack = Right (Integer (a + b)) :: rest }
                            Minus -> { state | stack = Right (Integer (a - b)) :: rest }
                            Times -> { state | stack = Right (Integer (a * b)) :: rest }
                            Div -> { state | stack = Right (Integer (a // b)) :: rest }
                    (Right ERP, expr, Right ELP) -> { state | stack = expr :: rest}
                    _ -> state

       _ -> state

reduce : State -> State
reduce state =
    state |> reduce1 |> reduce3