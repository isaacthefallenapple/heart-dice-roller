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
            List.indexedMap Tuple.pair results

        sorted =
            List.sortBy Tuple.second indexed
    in
    sorted
        |> List.indexedMap
            (\sortedIdx ( originalIdx, value ) ->
                ( originalIdx
                , let
                    ( ty, isHighest ) =
                        if sortedIdx > subThreshold then
                            ( Die.GreyedOut, False )

                        else if sortedIdx == subThreshold then
                            ( Die.Default, True )

                        else
                            ( Die.Default, False )
                  in
                  Die.Die
                    { ty = ty
                    , value = value
                    , isHighest = isHighest
                    }
                )
            )
        |> List.sortBy Tuple.first
        |> List.map Tuple.second


viewDicePool : DicePool -> Html msg
viewDicePool =
    viewAction
        (\d n ->
            List.map
                (\i ->
                    Die.Empty
                        (if i > n + difficultyToInt d then
                            Die.GreyedOut

                         else
                            Die.Default
                        )
                )
                (List.range 1 n)
        )
        (\_ -> Die.Empty Die.Difficult)


viewRoll : Roll -> Html msg
viewRoll =
    viewAction
        diceFromRollResult
        (\v -> Die.Die { ty = Die.Difficult, value = v, isHighest = True })


viewRollResult : DicePool -> Maybe Roll -> Html msg
viewRollResult dicePool maybeRoll =
    case maybeRoll of
        Nothing ->
            Html.div
                [ Html.Attributes.class "bg-red centered-text block-padding-400"
                ]
                [ Html.text
                    (case dicePool of
                        Difficult _ ->
                            "Test Your Misfortune"

                        Normal _ _ ->
                            "Test Your Fortune"
                    )
                ]

        Just roll ->
            Html.div
                [ Html.Attributes.class "bg-red-dark centered-text block-padding-400 shadow"
                ]
                [ Html.text (Outcome.toString (rollToOutcome roll)) ]


viewAction : (Difficulty -> n -> List Die) -> (d -> Die) -> Action n d -> Html msg
viewAction normalToDice difficultToDie action =
    Html.div
        [ Html.Attributes.class "flex flex-center-vert flex-center-hor block-padding-200" ]
        (case action of
            Normal difficulty n ->
                List.map Die.view (normalToDice difficulty n)

            Difficult d ->
                [ Die.view (difficultToDie d) ]
        )


viewDifficulty : (Difficulty -> msg) -> Difficulty -> Difficulty -> Html msg
viewDifficulty toMsg selectedDifficulty difficulty =
    let
        isSelected =
            difficulty == selectedDifficulty
    in
    Html.button
        [ Html.Events.onClick (toMsg difficulty)
        , Html.Attributes.class "[ bg-red block-padding-300 ]"
        , Html.Attributes.classList
            [ ( "bg-red", not isSelected )
            , ( "bg-red-dark shadow", isSelected )
            ]
        ]
        [ Html.text
            (case difficulty of
                Standard ->
                    "Standard"

                Risky ->
                    "Risky"

                Dangerous ->
                    "Dangerous"
            )
        ]
