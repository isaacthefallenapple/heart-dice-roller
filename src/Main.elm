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
    in
    Html.main_
        []
        [ Html.img
            [ Html.Attributes.class "banner"
            , Html.Attributes.src "assets/svgs/heart_banner.svg"
            ]
            []
        , Html.section
            [ Html.Attributes.class "modifiers"
            , Html.Attributes.class "button-row"
            , Html.Attributes.class "button-row--modifiers"
            ]
            [ Html.button
                [ Html.Attributes.class "button-row__button"
                , Html.Attributes.class "button-row__button--skill"
                , Html.Attributes.classList [ ( "button-row__button--active", model.skill ) ]
                , Html.Events.onClick ToggledSkill
                ]
                [ Html.span
                    [ Html.Attributes.class "button-row__label"
                    ]
                    [ Html.text "Skill" ]
                ]
            , Html.button
                [ Html.Attributes.class "button-row__button"
                , Html.Attributes.class "button-row__button--domain"
                , Html.Attributes.classList [ ( "button-row__button--active", model.domain ) ]
                , Html.Events.onClick ToggledDomain
                ]
                [ Html.span
                    [ Html.Attributes.class "button-row__label"
                    ]
                    [ Html.text "Domain" ]
                ]
            , Html.button
                [ Html.Attributes.class "button-row__button"
                , Html.Attributes.class "button-row__button--mastery"
                , Html.Attributes.classList [ ( "button-row__button--active", model.mastery ) ]
                , Html.Events.onClick ToggledMastery
                ]
                [ Html.span
                    [ Html.Attributes.class "button-row__label"
                    ]
                    [ Html.text "Mastery" ]
                ]
            , Html.div
                [ Html.Attributes.class "button-row__button"
                , Html.Attributes.class "button-row__button--assistance"
                , Html.Attributes.classList [ ( "button-row__button--active", model.assistance > 0 ) ]
                ]
                [ Html.button
                    [ Html.Attributes.class "button-row__assist-button"
                    , Html.Attributes.class "button-row__assist-button--dec"
                    , Html.Events.onClick DecreasedAssistance
                    ]
                    [ Html.text "-" ]
                , Html.span
                    [ Html.Attributes.class "button-row__label"
                    ]
                    [ Html.text ("Help+" ++ String.fromInt model.assistance)
                    ]
                , Html.button
                    [ Html.Attributes.class "button-row__assist-button"
                    , Html.Attributes.class "button-row__assist-button--inc"
                    , Html.Events.onClick IncreasedAssistance
                    ]
                    [ Html.text "+" ]
                ]
            ]
        , let
            viewDifficulty =
                Action.viewDifficulty SetDifficulty model.difficulty
          in
          Html.section
            [ Html.Attributes.class "difficulty"
            , Html.Attributes.class "button-row"
            , Html.Attributes.class "button-row--difficulty"
            ]
            [ viewDifficulty Action.Standard
            , viewDifficulty Action.Risky
            , viewDifficulty Action.Dangerous
            ]
        , model.roll
            |> Maybe.map
                (\r ->
                    let
                        outcome =
                            Action.rollToOutcome r
                    in
                    Html.section
                        [ Html.Attributes.class "button-row"
                        , Html.Attributes.class "button-row--result"
                        , Html.Attributes.class
                            ("button-row--result--"
                                ++ (case outcome of
                                        Outcome.CritFail ->
                                            "crit-fail"

                                        Outcome.Fail ->
                                            "fail"

                                        Outcome.SuccAtCost ->
                                            "succ-at-cost"

                                        Outcome.Succ ->
                                            "succ"

                                        Outcome.CritSucc ->
                                            "crit-succ"
                                   )
                            )
                        ]
                        [ Html.div
                            [ Html.Attributes.class "button-row__button"
                            , Html.Attributes.class "button-row__button--banner"
                            , Html.Attributes.class "button-row__button--active"
                            ]
                            [ let
                                critSucc =
                                    outcome == Outcome.CritSucc

                                critFail =
                                    outcome == Outcome.CritFail
                              in
                              Html.span
                                [ Html.Attributes.class "button-row__label"
                                , Html.Attributes.classList
                                    [ ( "text-effect__shine", critSucc || critFail )
                                    , ( "text-effect__shine--crit-succ", critSucc )
                                    , ( "text-effect__shine--crit-fail", critFail )
                                    ]
                                ]
                                [ Html.text (outcome |> Outcome.toString) ]
                            ]
                        ]
                )
            |> Maybe.withDefault
                (Html.section
                    [ Html.Attributes.class "button-row"
                    , Html.Attributes.class "button-row--result"
                    , Html.Attributes.class "button-row--result--blurb"
                    ]
                    [ Html.div
                        [ Html.Attributes.class "button-row__button"
                        , Html.Attributes.class "button-row__button--banner"
                        ]
                        [ Html.span
                            [ Html.Attributes.class "button-row__label"
                            ]
                            [ Html.text
                                (case dicepool of
                                    Action.Normal _ _ ->
                                        "Test Your Fortune, Traveller"

                                    Action.Difficult _ ->
                                        "Test Your (Mis)Fortune, Traveller"
                                )
                            ]
                        ]
                    ]
                )
        , model.roll
            |> Maybe.map Action.viewRoll
            |> Maybe.withDefault (Action.viewDicePool dicepool)
        , Html.div
            [ Html.Attributes.class "button-row" ]
            [ Html.button
                [ Html.Events.onClick (ClickedRollDice dicepool)
                , Html.Attributes.class "button-row__button"
                , Html.Attributes.class "button-row__roll-button"
                ]
                [ Html.span
                    [ Html.Attributes.class "button-row__label"
                    ]
                    [ Html.text "Roll The Bones" ]
                ]
            ]
        , History.view model.history
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
