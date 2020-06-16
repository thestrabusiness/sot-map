module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Events
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onMouseDown)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode



---- MODEL ----


type alias Position =
    { x : Float
    , y : Float
    }


type Model
    = Static Position
    | Dragging Position


init : ( Model, Cmd Msg )
init =
    ( Static { x = 0, y = 0 }, Cmd.none )


getPosition : Model -> Position
getPosition model =
    case model of
        Static position ->
            position

        Dragging position ->
            position



---- UPDATE ----


type Msg
    = DragEventStart
    | DragEventEnd
    | GotMousePosition Point


type alias Point =
    ( Float, Float )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DragEventStart ->
            case model of
                Static position ->
                    ( Dragging position, Cmd.none )

                Dragging position ->
                    ( Dragging position, Cmd.none )

        DragEventEnd ->
            case model of
                Static position ->
                    ( Static position, Cmd.none )

                Dragging position ->
                    ( Static position, Cmd.none )

        GotMousePosition ( x, y ) ->
            case model of
                Static position ->
                    ( Static position, Cmd.none )

                Dragging _ ->
                    ( Dragging { x = x, y = y }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onMouseUp (Decode.succeed DragEventEnd)
        , Sub.map GotMousePosition (Browser.Events.onMouseMove mousePositionDecoder)
        ]


mousePositionDecoder : Decoder Point
mousePositionDecoder =
    Decode.map2 Tuple.pair
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)



---- VIEW ----


viewTiles : List (Html Msg)
viewTiles =
    buildTiles 1000 []


buildTiles : Int -> List (Html Msg) -> List (Html Msg)
buildTiles numTiles acc =
    case numTiles of
        0 ->
            acc

        _ ->
            buildTiles (numTiles - 1) <| acc ++ [ div [ class "tile" ] [] ]


coordinateToPx : Float -> String
coordinateToPx coordinate =
    String.fromFloat coordinate ++ "px"


view : Model -> Html Msg
view model =
    let
        { x, y } =
            getPosition model

        xCoordinate =
            coordinateToPx x

        yCoordinate =
            coordinateToPx y
    in
    div [ class "main", onMouseDown DragEventStart ]
        [ div
            [ class "tile"
            , style "left" <| xCoordinate
            , style "top" <| yCoordinate
            ]
            [ div [] [ text "X: ", text xCoordinate ]
            , div [] [ text "Y: ", text yCoordinate ]
            ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
