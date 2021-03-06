module History exposing (History, empty, update, view)

import Debug
import Dict exposing (Dict)
import Html
import Html.Attributes
import Outcome exposing (Outcome)
import Svg
import Svg.Attributes


{-| The maximum width a label takes up.
-}
labelWidth : Float
labelWidth =
    75.0


{-| The maximum width of a bar.
-}
barWidth : Float
barWidth =
    180.0


{-| How heigh a bar should bar
-}
barHeight : Float
barHeight =
    9.0


{-| The padding between the labels and bars.
-}
labelBarPadding : Float
labelBarPadding =
    20.0


{-| The height a single row with one label and bar should take up.
-}
rowHeight : Float
rowHeight =
    16.0


{-| The width of the tag at the end of a bar that displays its value.
-}
percentageTagWidth : Float
percentageTagWidth =
    25.0


{-| The top and bottom padding of the bar chart.
-}
topBottomPadding : Float
topBottomPadding =
    5.0


{-| How many rolls should be kept around at any one time.
-}
maxRecordLength : Int
maxRecordLength =
    1000


{-| `History` keeps a record of rolls and can calculate stats based on them.
-}
type History
    = History
        { n : Int
        , stats : List ( Outcome.Outcome, Int )
        , record : List Outcome.Outcome
        }


{-| An empty `History` with no records.
-}
empty : History
empty =
    History
        { n = 0
        , stats = List.map (\o -> ( o, 0 )) Outcome.all
        , record = []
        }


{-| Updates a `History` with a new `Outcome`.
-}
update : Outcome.Outcome -> History -> History
update outcome (History history) =
    History
        { history
            | n = history.n + 1
            , stats =
                List.map
                    (\( o, a ) ->
                        if o == outcome then
                            ( o, a + 1 )

                        else
                            ( o, a )
                    )
                    history.stats
            , record = List.take maxRecordLength (outcome :: history.record)
        }


{-| Returns each `Outcome` and its corresponding percentage of occurring.
-}
percentages : History -> List ( Outcome.Outcome, Float )
percentages (History history) =
    let
        n =
            toFloat history.n
    in
    List.map
        (Tuple.mapSecond
            (\a ->
                if n == 0 then
                    0

                else
                    toFloat a / n
            )
        )
        history.stats


{-| Returns each `Outcome` and its corresponding percentage of occurring
as well as the percentages scaled up to 1.0.
-}
percentagesAndScales : History -> List ( Outcome, Float, Float )
percentagesAndScales history =
    let
        percs =
            percentages history

        scale =
            percs
                |> List.map Tuple.second
                |> List.maximum
                |> Maybe.withDefault 0.0
    in
    List.map
        (\( outcome, p ) ->
            ( outcome
            , p
            , if scale == 0.0 then
                0.0

              else
                p / scale
            )
        )
        percs



-- BAR CHART


{-| Render a `History`.
-}
view : History -> Html.Html msg
view ((History { record }) as history) =
    Html.div
        []
        [ Html.div
            [ Html.Attributes.class "bg-red block-padding-400 inline-padding-800"
            ]
            [ Html.span
                [ Html.Attributes.class "centered-text centered" ]
                [ Html.text "History" ]
            , Html.div
                [ Html.Attributes.class "spread align-items-start gap-top-300" ]
                [ viewRecord record
                , viewChart history
                ]
            ]
        ]


{-| Render the record of `Outcome`s.
-}
viewRecord : List Outcome.Outcome -> Html.Html msg
viewRecord outcomes =
    Html.ul
        [ Html.Attributes.class "record font-size-300 max-height-800" ]
        (List.map
            (\outcome ->
                Html.li
                    []
                    [ Html.text (Outcome.toString outcome) ]
            )
            outcomes
        )


{-| Render a bar chart based on `history`.
-}
viewChart : History -> Svg.Svg msg
viewChart history =
    let
        delimX =
            String.fromFloat (labelWidth + labelBarPadding / 2)

        viewBoxWidth =
            labelWidth + labelBarPadding + barWidth + percentageTagWidth

        viewBoxHeight =
            5 * rowHeight + 2 * topBottomPadding

        viewBox =
            String.join " " (List.map String.fromFloat [ 0, 0, viewBoxWidth, viewBoxHeight ])
    in
    Svg.svg
        [ Svg.Attributes.viewBox viewBox
        , Svg.Attributes.class "history__chart fill-base"
        ]
        (List.append
            [ Svg.line
                [ Svg.Attributes.x1 delimX
                , Svg.Attributes.y1 "0"
                , Svg.Attributes.x2 delimX
                , Svg.Attributes.y1 "100%"
                , Svg.Attributes.class "history__delim stroke-base"
                ]
                []
            ]
            (List.foldl (++)
                []
                (List.indexedMap
                    (\i ( outcome, p, s ) ->
                        let
                            top =
                                topBottomPadding + toFloat i * rowHeight
                        in
                        [ viewLabel 0 top outcome
                        , viewBar 0 (top + (rowHeight - barHeight) / 2) p s
                        ]
                    )
                    (percentagesAndScales history)
                )
            )
        )


{-| Render a single bar of a bar chart.
-}
viewBar : Float -> Float -> Float -> Float -> Svg.Svg msg
viewBar x y percentage scale =
    let
        scaledBarWidth =
            scale * barWidth

        xWithOffset =
            labelWidth + labelBarPadding + x
    in
    Svg.g
        []
        [ Svg.rect
            [ Svg.Attributes.width <| String.fromFloat (max 0.0 (scaledBarWidth - 2))
            , Svg.Attributes.height <| String.fromFloat barHeight
            , Svg.Attributes.x <| String.fromFloat xWithOffset
            , Svg.Attributes.y <| String.fromFloat y
            , Svg.Attributes.class "history__bar"
            ]
            []
        , Svg.text_
            [ Svg.Attributes.x <| String.fromFloat (xWithOffset + scaledBarWidth)
            , Svg.Attributes.y <| String.fromFloat (y + barHeight / 1.7)
            , Svg.Attributes.fontSize (String.fromFloat (barHeight * 1.5))
            , Svg.Attributes.dominantBaseline "middle"
            , Svg.Attributes.textAnchor "start"
            , Svg.Attributes.class "history__percentage-tag"
            ]
            [ Svg.text
                (formatPercent percentage)
            ]
        ]


{-| Format `f` as a percentage.

    formatPercent 0.789 == "78.9%"
    formatPercent 0.1 == "10%"
    formatPercent 0.5643 == "56.43%"

-}
formatPercent : Float -> String
formatPercent f =
    let
        percentage =
            f * 100
    in
    String.left
        (if percentage < 10.0 then
            4

         else
            5
        )
        (String.fromFloat percentage)
        ++ "%"


{-| Render a single label of a bar chart.
-}
viewLabel : Float -> Float -> Outcome.Outcome -> Svg.Svg msg
viewLabel x y outcome =
    Svg.svg
        [ Svg.Attributes.x <| String.fromFloat x
        , Svg.Attributes.y <| String.fromFloat y
        , Svg.Attributes.width <| String.fromFloat labelWidth
        , Svg.Attributes.height <| String.fromFloat rowHeight
        , Svg.Attributes.class "history__label"
        , Svg.Attributes.class "font-size-200"
        ]
        [ Svg.text_
            [ Svg.Attributes.x "100%"
            , Svg.Attributes.y "55%"
            , Svg.Attributes.dominantBaseline "middle"
            , Svg.Attributes.textAnchor "end"
            ]
            [ Svg.text (Outcome.toString outcome)
            ]
        ]
