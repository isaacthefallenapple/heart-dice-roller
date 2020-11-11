module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Platform exposing (Program)


type alias Model =
    { skill : Bool
    , domain : Bool
    , mastery : Bool
    , difficulty : Difficulty
    }


dicePool : Model -> Int
dicePool model =
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
        + difficultyToInt model.difficulty


initialModel : Model
initialModel =
    { skill = False
    , domain = False
    , mastery = False
    , difficulty = Standard
    }


view : Model -> Html Msg
view model =
    Html.main_
        []
        [ Html.div
            [ Html.Attributes.class "difficulty" ]
            [ viewDifficulty model.difficulty Standard
            , viewDifficulty model.difficulty Risky
            , viewDifficulty model.difficulty Dangerous
            ]
        , Html.div
            [ Html.Attributes.class "modifiers" ]
            [ Html.div
                [ Html.Attributes.class "modifiers__button"
                , Html.Attributes.class "modifiers__button--skill"
                , Html.Attributes.classList [ ( "modifiers__button--active", model.skill ) ]
                ]
                [ Html.input
                    [ Html.Attributes.type_ "checkbox"
                    , Html.Attributes.class "modifiers__button__checkbox"
                    , Html.Attributes.id "modifiers__button__checkbox--skill"
                    , Html.Events.onInput (\_ -> ToggledSkill)
                    ]
                    []
                , Html.label
                    [ Html.Attributes.class "modifiers__button__label"
                    , Html.Attributes.for "modifiers__button__checkbox--skill"
                    ]
                    [ Html.text "Skill" ]
                ]
            , Html.div
                [ Html.Attributes.class "modifiers__button"
                , Html.Attributes.class "modifiers__button--domain"
                , Html.Attributes.classList [ ( "modifiers__button--active", model.domain ) ]
                ]
                [ Html.input
                    [ Html.Attributes.type_ "checkbox"
                    , Html.Attributes.class "modifiers__button__checkbox"
                    , Html.Attributes.id "modifiers__button__checkbox--domain"
                    , Html.Events.onInput (\_ -> ToggledDomain)
                    ]
                    []
                , Html.label
                    [ Html.Attributes.class "modifiers__button__label"
                    , Html.Attributes.for "modifiers__button__checkbox--domain"
                    ]
                    [ Html.text "Domain" ]
                ]
            , Html.div
                [ Html.Attributes.class "modifiers__button"
                , Html.Attributes.class "modifiers__button--mastery"
                , Html.Attributes.classList [ ( "modifiers__button--active", model.mastery ) ]
                ]
                [ Html.input
                    [ Html.Attributes.type_ "checkbox"
                    , Html.Attributes.class "modifiers__button__checkbox"
                    , Html.Attributes.id "modifiers__button__checkbox--mastery"
                    , Html.Events.onInput (\_ -> ToggledMastery)
                    ]
                    []
                , Html.label
                    [ Html.Attributes.class "modifiers__button__label"
                    , Html.Attributes.for "modifiers__button__checkbox--mastery"
                    ]
                    [ Html.text "Mastery" ]
                ]
            ]
        , Html.div
            [ Html.Attributes.class "dicepool" ]
            [ Html.text (String.fromInt (dicePool model)) ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetDifficulty newDifficulty ->
            ( { model | difficulty = newDifficulty }, Cmd.none )

        ToggledSkill ->
            ( { model | skill = not model.skill }, Cmd.none )

        ToggledDomain ->
            ( { model | domain = not model.domain }, Cmd.none )

        ToggledMastery ->
            ( { model | mastery = not model.mastery }, Cmd.none )


type Msg
    = ToggledSkill
    | ToggledDomain
    | ToggledMastery
    | SetDifficulty Difficulty


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


viewDifficulty : Difficulty -> Difficulty -> Html Msg
viewDifficulty selectedDifficulty difficulty =
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
    in
    Html.div
        [ Html.Attributes.class "difficulty__button"
        , Html.Attributes.class ("difficulty__button--" ++ strLower)
        , Html.Attributes.classList
            [ ( "difficulty__button--selected", difficulty == selectedDifficulty )
            ]
        ]
        [ Html.input
            [ Html.Attributes.type_ "radio"
            , Html.Attributes.name "difficulty__button__radio"
            , Html.Attributes.id ("difficulty__button__radio--" ++ strLower)
            , Html.Attributes.class "difficulty__button__radio"
            , Html.Attributes.class ("difficulty__button__radio--" ++ strLower)
            , Html.Events.onInput (\_ -> SetDifficulty difficulty)
            ]
            []
        , Html.label
            [ Html.Attributes.for ("difficulty__button__radio--" ++ strLower)
            , Html.Attributes.class "difficulty__button__label"
            , Html.Attributes.class ("difficulty__button__radio--" ++ strLower)
            ]
            [ Html.text str
            ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
