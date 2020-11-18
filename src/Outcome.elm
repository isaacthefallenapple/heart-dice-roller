module Outcome exposing (Outcome(..), all, toString)


type Outcome
    = CritFail
    | Fail
    | SuccAtCost
    | Succ
    | CritSucc


{-| A list of every outcome, sorted from worst to best.
-}
all : List Outcome
all =
    [ CritFail
    , Fail
    , SuccAtCost
    , Succ
    , CritSucc
    ]


toString : Outcome -> String
toString outcome =
    case outcome of
        CritFail ->
            "Critical Failure"

        Fail ->
            "Failure"

        SuccAtCost ->
            "Succes... at a Cost"

        Succ ->
            "Success"

        CritSucc ->
            "Critical Success"
