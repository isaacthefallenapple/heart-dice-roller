module Die exposing (Die, DieType(..), render, view)

import Html exposing (Html)
import Html.Attributes
import Svg exposing (Svg)
import Svg.Attributes


type alias Die =
    { ty : DieType
    , value : Maybe Int
    }


type DieType
    = Subtracted
    | Single
    | Highlit
    | Default


view : Die -> Html.Html msg
view { ty, value } =
    Html.div
        []
        [ Html.div
            []
            []
        , Html.div
            []
            []
        ]


render : Svg msg
render =
    Svg.svg
        [ Svg.Attributes.viewBox "0 0 70 70"
        , Svg.Attributes.class "rd10"
        ]
        [ Svg.g
            [ Svg.Attributes.transform "matrix(1.3333333,0,0,-1.3333333,0,1056)"
            ]
            [ Svg.path
                [ Svg.Attributes.style "fill:#000a0a;fill-opacity:1;fill-rule:nonzero;stroke:none;stroke-width:0.994233"
                , Svg.Attributes.d "m 52.428709,756.77305 -14.405537,34.65008 c -0.184918,0.44376 -0.679297,0.67951 -1.142641,0.5339 L 10.126707,783.7514 c -0.107677,-0.0327 -0.2082696,-0.0862 -0.2982836,-0.15453 l -9.3113621,-7.17448 c -0.40935424,-0.20009 -0.60495012,-0.65474 -0.4794095,-1.08464 0.0029938,-0.0149 -0.0039917,-0.0277 0,-0.0426 0.0019958,-0.007 0.0099794,-0.0109 0.01167588,-0.0178 0.0059877,-0.0129 0.0029938,-0.0267 0.0079835,-0.0406 L 14.766621,740.07849 c 0.104384,-0.24862 0.316645,-0.39423 0.551063,-0.4814 0.06607,-0.0297 0.135519,-0.0456 0.207172,-0.0604 0.03832,-0.005 0.06945,-0.0327 0.107775,-0.0327 0.01107,0 0.01996,0.006 0.02944,0.006 0.01826,0 0.03273,-0.01 0.0511,-0.01 0.02554,0 0.0511,0.002 0.07664,0.004 0.02774,0.002 0.0494,0.0159 0.07494,0.0218 0.0461,0.008 0.09171,0.006 0.13552,0.0198 l 27.949853,8.57709 c 0.01778,0.006 0.03104,0.0198 0.0494,0.0258 0.138913,0.0416 0.274434,0.10203 0.3877,0.21 l 7.820941,7.38645 c 0.28042,0.26547 0.369237,0.67357 0.220544,1.02818 z M 4.0864866,774.40863 13.255144,759.21473 14.326131,745.9831 2.3823076,774.53046 Z m 11.0900864,-15.74067 15.687688,-0.13373 c 0.117758,-0.0911 0.251082,-0.16641 0.408757,-0.16641 h 0.003 l 10.556191,-8.91783 -25.277874,-7.77276 z m 15.514347,1.70075 -16.048741,0.16542 -8.5626102,14.19246 29.8350592,14.6273 z m -19.848691,21.6542 15.728204,4.82591 -21.7373889,-10.65821 c -0.04381,0.01 -0.080533,0.0367 -0.1266382,0.0406 l -1.2641882,0.0892 z m 32.862903,-31.74086 -11.223911,9.45272 5.045876,28.00553 12.924696,-31.0871 z"
                ]
                []
            ]
        , viewLabel 0 0 1
        ]


viewLabel : Float -> Float -> Int -> Svg msg
viewLabel x y n =
    Svg.svg
        [ Svg.Attributes.x <| String.fromFloat x
        , Svg.Attributes.y <| String.fromFloat y
        , Svg.Attributes.width "35"
        , Svg.Attributes.height "35"
        ]
        [ Svg.g
            [ Svg.Attributes.transform "rotate(30)"
            ]
            [ Svg.text_
                [ Svg.Attributes.x "50%"
                , Svg.Attributes.y "20%"
                , Svg.Attributes.class "rd10__label"
                ]
                [ Svg.text <| String.fromInt n
                ]
            ]
        ]
