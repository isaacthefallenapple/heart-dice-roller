module Action exposing (..)

import Die exposing (Die)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Outcome
import Random


type Action n d
    = Normal Difficulty n
    | Difficult d


type alias DicePool =
    Action Int ()


type alias Roll =
    Action (List Int) Int


type Difficulty
    = Standard
    | Risky
    | Dangerous


difficultyToInt : Difficulty -> Int
difficultyToInt d =
    case d of
        Standard ->
            0

        Risky ->
            -1

        Dangerous ->
            -2


rollDice : DicePool -> Random.Generator Roll
rollDice action =
    case action of
        Difficult _ ->
            Random.map Difficult (Random.int 1 10)

        Normal d n ->
            Random.map (Normal d) (Random.list n (Random.int 1 10))


rollToOutcome : Roll -> Outcome.Outcome
rollToOutcome roll =
    case roll of
        Difficult result ->
            if result == 10 then
                Outcome.SuccAtCost

            else if result == 1 then
                Outcome.CritFail

            else
                Outcome.Fail

        Normal d results ->
            let
                max =
                    Maybe.withDefault 1 (List.maximum (List.take (List.length results + difficultyToInt d) (List.sort results)))
            in
            if max == 1 then
                Outcome.CritFail

            else if max <= 5 then
                Outcome.Fail

            else if max <= 7 then
                Outcome.SuccAtCost

            else if max <= 9 then
                Outcome.Succ

            else
                Outcome.CritSucc


diceFromRollResult : Difficulty -> List Int -> List Die
diceFromRollResult difficulty results =
    let
        subThreshold =
            List.length results + difficultyToInt difficulty - 1

        indexed =
            List.indexedMap (\i v -> ( i, v )) results

        sorted =
            List.sortBy Tuple.second indexed
    in
    sorted
        |> List.indexedMap
            (\i ( j, v ) ->
                ( j
                , { ty =
                        if i > subThreshold then
                            Die.Subtracted

                        else if i == subThreshold then
                            Die.Highlit

                        else
                            Die.Default
                  , value = Just v
                  }
                )
            )
        |> List.sortBy Tuple.first
        |> List.map (\( _, d ) -> d)


viewDicePool : DicePool -> Html msg
viewDicePool =
    viewAction
        (\d n ->
            List.map
                (\i ->
                    { ty =
                        if i > n + difficultyToInt d then
                            Die.Subtracted

                        else
                            Die.Default
                    , value = Nothing
                    }
                )
                (List.range 1 n)
        )
        (\_ -> { ty = Die.Single, value = Nothing })


viewRoll : Roll -> Html msg
viewRoll =
    viewAction
        diceFromRollResult
        (\v -> { ty = Die.Single, value = Just v })


viewAction : (Difficulty -> n -> List Die) -> (d -> Die) -> Action n d -> Html msg
viewAction normalToDice difficultToDie action =
    Html.div
        []
        (case action of
            Normal difficulty n ->
                List.map Die.view (normalToDice difficulty n)

            Difficult d ->
                [ Die.view (difficultToDie d) ]
        )


viewDifficulty : (Difficulty -> msg) -> Difficulty -> Difficulty -> Html msg
viewDifficulty toMsg selectedDifficulty difficulty =
    let
        str =
            case difficulty of
                Standard ->
                    "Standard"

                Risky ->
                    "Risky"

                Dangerous ->
                    "Dangerous"

        strLower =
            String.toLower str

        id =
            "button-row__radio--" ++ strLower
    in
    Html.button
        [ Html.Events.onClick (toMsg difficulty)
        , Html.Attributes.class "[ bg-red block-padding-300 ]"
        ]
        [ Html.label
            [ Html.Attributes.for id
            ]
            [ Html.text str
            ]
        ]
