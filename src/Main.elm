port module Main exposing (Model)

import Browser
import Debug exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Encode as Encode
import List exposing (concat, indexedMap, length, map, range)


type Level
    = Default
    | Known
    | Guessed
    | Unknown


type alias Cell =
    ( Int, Int, Level )


type alias Model =
    List (List Cell)


type Msg
    = Noop
    | ChangeLevel Int Int


init : Maybe Int -> ( Model, Cmd Msg )
init max =
    let
        maxRange =
            case max of
                Just n ->
                    n

                Nothing ->
                    10

        numbers =
            range 0 maxRange

        data =
            map (\x -> map (\y -> ( x, y, Default )) numbers) numbers
    in
    ( data, Cmd.none )


encodeLevel : Level -> Encode.Value
encodeLevel level =
    case level of
        Default ->
            Encode.string "Default"

        Known ->
            Encode.string "Known"

        Guessed ->
            Encode.string "Guessed"

        Unknown ->
            Encode.string "Unknown"


encodeCell : Cell -> Encode.Value
encodeCell cell =
    let
        ( a, b, level ) =
            cell
    in
    Encode.object
        [ ( "x", Encode.int a )
        , ( "y", Encode.int b )
        , ( "level", encodeLevel level )
        ]


encodeCols : List Cell -> Encode.Value
encodeCols =
    Encode.list encodeCell


encodeGrid : Model -> Encode.Value
encodeGrid =
    Encode.list encodeCols


updateLevel : Level -> Level
updateLevel level =
    case level of
        Default ->
            Unknown

        Unknown ->
            Guessed

        Guessed ->
            Known

        Known ->
            Default


updateCell : Int -> Int -> Cell -> Cell
updateCell x y cell =
    let
        ( a, b, level ) =
            cell
    in
    if (a == x && b == y) || (a == y && b == x) then
        ( a, b, updateLevel level )

    else
        cell


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Noop ->
            ( model, Cmd.none )

        ChangeLevel a b ->
            let
                updateRow =
                    map (updateCell a b)

                updateModel =
                    map updateRow
            in
            ( updateModel model
            , cache (encodeGrid model)
            )


cellColor level =
    case level of
        Default ->
            "#ffffff"

        Unknown ->
            "#FEB3F2"

        Guessed ->
            "#cdefff"

        Known ->
            "#B6D19E"


view : Model -> Html Msg
view model =
    let
        cellStyle =
            [ style "padding" "0.5em"
            , style "text-align" "center"
            , style "border-radius" "2em"
            , style "width" "2em"
            , style "height" "2em"
            , style "transition" "background-color 0.2s"
            ]

        displayLabelCell val =
            let
                label =
                    if val == -1 then
                        ""

                    else
                        toString val
            in
            th
                (cellStyle
                    ++ [ style "background-color" "#eee"
                       ]
                )
                [ text label ]

        displayTopLabels m =
            tr [] (indexedMap (\i n -> displayLabelCell n) (range -1 (length m - 1)))

        displayCol ( x, y, level ) =
            td
                (cellStyle
                    ++ [ style "background-color" (cellColor level)
                       , style "cursor" "pointer"
                       , onClick (ChangeLevel x y)
                       ]
                )
                [ text (toString (x * y)) ]

        displayRow i cells =
            tr [] (displayLabelCell i :: map displayCol cells)

        displayLegend =
            let
                legends =
                    [ ( Unknown, "Unknown" )
                    , ( Guessed, "Guessed" )
                    , ( Known, "Known" )
                    ]

                template ( level, legend ) =
                    div
                        [ style "display" "flex"
                        , style "justify-content" "left"
                        , style "padding-bottom" "0.5em"
                        ]
                        [ div
                            (cellStyle
                                ++ [ style "background-color" (cellColor level)
                                   ]
                            )
                            []
                        , div
                            [ style "align-self" "center"
                            , style "padding-left" "0.5em"
                            ]
                            [ text legend ]
                        ]
            in
            map template legends
    in
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "font-family" "Helvetica"
        ]
        [ div
            [ style "display" "flex"
            , style "align-content" "center"
            , style "justify-content" "center"
            ]
            [ table [ style "margin" "1em" ]
                (displayTopLabels model
                    :: indexedMap displayRow model
                )
            , div
                [ style "margin" "1em"
                ]
                displayLegend
            ]
        ]


port cache : Encode.Value -> Cmd msg


main : Program (Maybe Int) Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view =
            \m ->
                { title = "Time table grid"
                , body = [ view m ]
                }
        , subscriptions = \_ -> Sub.none
        }
