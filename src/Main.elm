module Main exposing (main)

import Browser
import Html exposing (Html)
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
    Html.div [] [ Html.text "Hello World" ]


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
    | Difficult


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
