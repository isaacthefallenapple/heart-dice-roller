module Main exposing (main)

import Action
import Browser
import Die
import History exposing (History)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Outcome
import Platform exposing (Program)
import Random
import String
import Svg
import Svg.Attributes


classListCond : List String -> List String -> Bool -> Html.Attribute msg
classListCond ifFalse ifTrue condition =
    Html.Attributes.class
        (String.join " "
            (if condition then
                ifTrue

             else
                ifFalse
            )
        )


type alias Model =
    { skill : Bool
    , domain : Bool
    , mastery : Bool
    , difficulty : Action.Difficulty
    , assistance : Int
    , roll : Maybe Action.Roll
    , history : History.History
    }


dicePool : Model -> Action.DicePool
dicePool model =
    let
        pool =
            1
                + List.sum
                    (List.map
                        (\b ->
                            if b then
                                1

                            else
                                0
                        )
                        [ model.skill, model.domain, model.mastery ]
                    )
                + model.assistance
    in
    if pool + Action.difficultyToInt model.difficulty <= 0 then
        Action.Difficult ()

    else
        Action.Normal model.difficulty pool


initialModel : Model
initialModel =
    { skill = False
    , domain = False
    , mastery = False
    , difficulty = Action.Standard
    , assistance = 0
    , roll = Nothing
    , history = History.empty
    }


view : Model -> Html Msg
view model =
    let
        dicepool =
            dicePool model

        buttonClasses =
            classListCond [ "bg-red" ] [ "bg-red-dark", "shadow" ]
    in
    Html.div
        []
        [ Html.img
            [ Html.Attributes.src "assets/svgs/heart_banner.svg"
            , Html.Attributes.class "[ centered ]"
            ]
            []
        , Html.main_
            [ Html.Attributes.class "[ flow wrapper centered ]"
            ]
            [ Html.section
                [ Html.Attributes.class "[ spread ]" ]
                [ Html.button
                    [ Html.Events.onClick ToggledSkill
                    , Html.Attributes.class "[ block-padding-300 ]"
                    , buttonClasses model.skill
                    ]
                    [ Html.span
                        []
                        [ Html.text "Skill" ]
                    ]
                , Html.button
                    [ Html.Events.onClick ToggledDomain
                    , Html.Attributes.class "[ bg-red block-padding-300 ]"
                    , buttonClasses model.domain
                    ]
                    [ Html.span
                        []
                        [ Html.text "Domain" ]
                    ]
                , Html.button
                    [ Html.Events.onClick ToggledMastery
                    , Html.Attributes.class "[ bg-red block-padding-300 ]"
                    , buttonClasses model.mastery
                    ]
                    [ Html.span
                        []
                        [ Html.text "Mastery" ]
                    ]
                , Html.span
                    [ Html.Attributes.class "[ bg-red block-padding-300 centered-text ] [ help ]"
                    , buttonClasses (model.assistance > 0)
                    ]
                    [ Html.button
                        [ Html.Events.onClick DecreasedAssistance
                        ]
                        [ Html.text "-" ]
                    , Html.span
                        []
                        [ Html.text ("Help+" ++ String.fromInt model.assistance)
                        ]
                    , Html.button
                        [ Html.Events.onClick IncreasedAssistance
                        ]
                        [ Html.text "+" ]
                    ]
                ]
            , let
                viewDifficulty =
                    Action.viewDifficulty SetDifficulty model.difficulty
              in
              Html.section
                [ Html.Attributes.class "[ spread ]" ]
                [ viewDifficulty Action.Standard
                , viewDifficulty Action.Risky
                , viewDifficulty Action.Dangerous
                ]
            , Action.viewRollResult dicepool model.roll
            , model.roll
                |> Maybe.map Action.viewRoll
                |> Maybe.withDefault (Action.viewDicePool dicepool)
            , Html.div
                []
                [ Html.button
                    [ Html.Events.onClick (ClickedRollDice dicepool)
                    , Html.Attributes.class "[ bg-red full-width block-padding-300 ]"
                    ]
                    [ Html.span
                        []
                        [ Html.text "Roll The Bones" ]
                    ]
                ]
            , History.view model.history
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetDifficulty newDifficulty ->
            ( { model | difficulty = newDifficulty, roll = Nothing }, Cmd.none )

        ToggledSkill ->
            ( { model | skill = not model.skill, roll = Nothing }, Cmd.none )

        ToggledDomain ->
            ( { model | domain = not model.domain, roll = Nothing }, Cmd.none )

        ToggledMastery ->
            ( { model | mastery = not model.mastery, roll = Nothing }, Cmd.none )

        DecreasedAssistance ->
            ( { model | assistance = max 0 (model.assistance - 1), roll = Nothing }, Cmd.none )

        IncreasedAssistance ->
            ( { model | assistance = model.assistance + 1, roll = Nothing }, Cmd.none )

        RolledDice roll ->
            ( { model | roll = Just roll, history = History.update (Action.rollToOutcome roll) model.history }, Cmd.none )

        ClickedRollDice pool ->
            ( model, Random.generate RolledDice (Action.rollDice pool) )


type Msg
    = ToggledSkill
    | ToggledDomain
    | ToggledMastery
    | SetDifficulty Action.Difficulty
    | DecreasedAssistance
    | IncreasedAssistance
    | ClickedRollDice Action.DicePool
    | RolledDice Action.Roll


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
