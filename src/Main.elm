module Main exposing (Msg(..), main, update, view)

import Array exposing (Array)
import Board exposing (Board, Coordinates, Space(..))
import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


type alias Model =
    { board : Board
    , gameOver : Bool
    }


init : Model
init =
    let
        board =
            Board.new 10 [ ( 3, 4 ), ( 4, 4 ) ]
    in
    Model board False


main =
    Browser.sandbox { init = init, update = update, view = view }


type Msg
    = ClearSpaces Coordinates


update : Msg -> Model -> Model
update msg model =
    case msg of
        ClearSpaces ( x, y ) ->
            let
                ( updatedBoard, isGameOver ) =
                    Board.clearSpaces ( x, y ) model.board
            in
            { model | board = updatedBoard, gameOver = isGameOver }


view : Model -> Html Msg
view model =
    div []
        [ viewBoard model.board
        ]


viewBoard : Board -> Html Msg
viewBoard board =
    let
        rows =
            board
                |> Array.indexedMap viewRow
                |> Array.toList
    in
    div [] rows


viewRow : Int -> Array Space -> Html Msg
viewRow y row =
    let
        spaces =
            row
                |> Array.indexedMap (\x space -> viewSpace ( x, y ) space)
                |> Array.toList
    in
    div [] spaces


viewSpace : Coordinates -> Space -> Html Msg
viewSpace ( x, y ) space =
    let
        spaceStyles =
            [ style "width" "20px"
            , style "height" "20px"
            , style "display" "inline-block"
            , style "vertical-align" "top"
            , style "text-align" "center"
            ]
    in
    case space of
        Mine True ->
            div
                (List.append [ style "border-style" "inset" ] spaceStyles)
                [ text "X" ]

        Border True n ->
            div
                (List.append [ style "border-style" "inset" ] spaceStyles)
                [ text (String.fromInt n) ]

        Empty True ->
            div
                (List.append [ style "border-style" "inset" ] spaceStyles)
                []

        _ ->
            div
                (List.append
                    [ style "border-style" "outset"
                    , onClick (ClearSpaces ( x, y ))
                    ]
                    spaceStyles
                )
                []
