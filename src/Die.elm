module Die exposing (Die(..), DieType(..), render, symbol, view)

import Html exposing (Html)
import Html.Attributes
import Svg exposing (Svg)
import Svg.Attributes


{-| A `Die` represents the state of a die. It can either be Empty and not show anything
or have a value to display.
-}
type Die
    = Empty DieType
    | Die
        { ty : DieType
        , value : Int
        , isHighest : Bool
        }


{-| A `Die` can have different types that affect its look.
-}
type DieType
    = Default
    | GreyedOut
    | Difficult


{-| Converts a `DieType` to a set of classes corresponding to the correct styling of the die.
-}
typeToDieStyle : DieType -> Svg.Attribute msg
typeToDieStyle ty =
    let
        standardStyle =
            [ "stroke-red-dark", "fill-none" ]

        join =
            String.join " "
    in
    Svg.Attributes.class
        (case ty of
            Default ->
                join standardStyle

            GreyedOut ->
                join ("opacity-40" :: standardStyle)

            Difficult ->
                join [ "stroke-black fill-red-dark" ]
        )


{-| Converts a `DieType` to a set of classes corresponding to the correct styling of the die's label.
-}
typeToLabelStyle : DieType -> Svg.Attribute msg
typeToLabelStyle ty =
    let
        standardStyle =
            [ "stroke-current", "fill-red-dark" ]

        join =
            String.join " "
    in
    Svg.Attributes.class
        (case ty of
            Difficult ->
                join [ "stroke-red-dark", "fill-black" ]

            _ ->
                join standardStyle
        )


{-| Renders a `Die`.
-}
view : Die -> Svg msg
view die =
    case die of
        Empty ty ->
            Svg.svg
                [ Svg.Attributes.viewBox "0 0 70 70"
                , Svg.Attributes.class "d10"
                ]
                [ Svg.desc
                    []
                    [ Svg.text "A 10-sided die showing no number"
                    ]
                , Svg.g
                    [ Svg.Attributes.class "die"
                    , typeToDieStyle ty
                    ]
                    [ viewOutline ]
                ]

        Die { ty, value, isHighest } ->
            Svg.svg
                [ Svg.Attributes.viewBox "0 0 70 70"
                , Svg.Attributes.class "d10"
                , Html.Attributes.attribute "data-is-highest"
                    (if isHighest then
                        "true"

                     else
                        "false"
                    )
                ]
                [ Svg.desc
                    []
                    [ Svg.text ("A 10-sided die showing a " ++ String.fromInt value)
                    ]
                , Svg.g
                    [ Svg.Attributes.class "die"
                    , typeToDieStyle ty
                    ]
                    [ viewOutline
                    , viewLabel ty value
                    ]
                ]


{-| Renders a die's outline.
-}
viewOutline : Svg msg
viewOutline =
    Svg.use
        [ Svg.Attributes.xlinkHref "#d10-outline"
        , Svg.Attributes.class "outline"
        ]
        []


{-| Renders a die's label.
-}
viewLabel : DieType -> Int -> Svg msg
viewLabel ty n =
    let
        is10 =
            n == 10

        ( labelX, labelY ) =
            if is10 then
                ( 41.064102, 16.438211 )

            else
                ( 39.252209, 17.713945 )
    in
    Svg.text_
        (List.append
            [ Svg.Attributes.x "39.913139"
            , Svg.Attributes.y "15.580445"
            , Svg.Attributes.transform "rotate(35.863796)"
            , Svg.Attributes.class "die-value centered-text"
            , typeToLabelStyle ty
            ]
            (if is10 then
                [ Html.Attributes.attribute "data-is-10" "true" ]

             else
                []
            )
        )
        [ Svg.tspan
            [ Svg.Attributes.x <| String.fromFloat labelX
            , Svg.Attributes.y <| String.fromFloat labelY
            ]
            [ Svg.text <| String.fromInt n
            ]
        ]


{-| Defines the SVG symbol to use for a die's outline.
-}
symbol : Svg msg
symbol =
    Svg.symbol
        [ Svg.Attributes.id "d10-outline"
        ]
        [ Svg.g
            [ Svg.Attributes.class "outline"
            ]
            [ Svg.path
                [ Svg.Attributes.d "M 14.011905,12.178572 49.5,1.1130952 68.684524,47.339286 58.011905,57.422619 20.886905,68.815476 1.3095238,21.869048 Z" ]
                []
            , Svg.path
                [ Svg.Attributes.d "M 6.3511905,22.458334 49.5,1.1130952 41.77381,43.410715 18.922619,43.14881 Z"
                , Svg.Attributes.class "round-linejoin"
                ]
                []
            , Svg.path
                [ Svg.Attributes.d "m 1.3095238,21.869048 5.0416667,0.589286" ]
                []
            , Svg.path
                [ Svg.Attributes.d "m 18.922619,43.14881 1.964286,25.666666"
                ]
                []
            , Svg.path
                [ Svg.Attributes.d "M 41.77381,43.410715 58.011905,57.422619"
                , Svg.Attributes.class "round-linecap"
                , Svg.Attributes.class "bevel-linejoin"
                ]
                []
            ]
        ]


render : Int -> Svg msg
render n =
    let
        is10 =
            n == 10

        ( labelX, labelY ) =
            if is10 then
                ( 41.064037, 13.637207 )

            else
                ( 39.913139, 15.580445 )
    in
    Svg.svg
        (List.append
            [ Svg.Attributes.viewBox "0 0 70 70"
            , Svg.Attributes.class "d10"
            ]
            (if is10 then
                [ Html.Attributes.attribute "data-is-10" "true"
                ]

             else
                []
            )
        )
        [ Svg.g
            [ Svg.Attributes.class "die"
            ]
            [ Svg.g
                (List.append
                    [ Svg.Attributes.class "outline stroke-red-dark"
                    ]
                    (if is10 then
                        [ Html.Attributes.attribute "data-is-10" "true"
                        ]

                     else
                        []
                    )
                )
                [ Svg.path
                    [ Svg.Attributes.d "M 14.011905,12.178572 49.5,1.1130952 68.684524,47.339286 58.011905,57.422619 20.886905,68.815476 1.3095238,21.869048 Z" ]
                    []
                , Svg.path
                    [ Svg.Attributes.d "M 6.3511905,22.458334 49.5,1.1130952 41.77381,43.410715 18.922619,43.14881 Z"
                    , Svg.Attributes.class "round-linejoin"
                    ]
                    []
                , Svg.path
                    [ Svg.Attributes.d "m 1.3095238,21.869048 5.0416667,0.589286" ]
                    []
                , Svg.path
                    [ Svg.Attributes.d "m 18.922619,43.14881 1.964286,25.666666"
                    ]
                    []
                , Svg.path
                    [ Svg.Attributes.d "M 41.77381,43.410715 58.011905,57.422619"
                    , Svg.Attributes.class "round-linecap"
                    , Svg.Attributes.class "bevel-linejoin"
                    ]
                    []
                ]
            , Svg.text_
                [ Svg.Attributes.x "39.913139"
                , Svg.Attributes.y "15.580445"
                , Svg.Attributes.transform "rotate(38.983973)"
                , Svg.Attributes.class "die-value fill-red-dark centered-text"
                ]
                [ Svg.tspan
                    [ Svg.Attributes.x <| String.fromFloat labelX
                    , Svg.Attributes.y <| String.fromFloat labelY
                    ]
                    [ Svg.text <| String.fromInt n
                    ]
                ]
            ]
        ]
