module Outcome exposing (Outcome(..), all, toString)

{-| An `Outcome` represents one possible outcome of a roll in Heart.
-}


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


{-| Converts an `Outcome` into a string representation.
-}
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
