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
        [ viewDifficulty model.difficulty Standard
        , viewDifficulty model.difficulty Risky
        , viewDifficulty model.difficulty Dangerous
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


type Msg
    = ToggledSkill
    | ToggledDomain
    | ToggledMastery
    | SetDifficulty Difficulty


type Difficulty
    = Standard
    | Risky
    | Dangerous


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
            , Html.Attributes.class ("difficulty__button__radio--" ++ strLower)
            , Html.Events.onInput (\_ -> SetDifficulty difficulty)
            ]
            []
        , Html.label
            [ Html.Attributes.for ("difficulty__button__radio--" ++ strLower)
            , Html.Attributes.class "difficulty__button__label"
            ]
            [ Html.text str ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
