module Main exposing (Msg(..), main, update, view)

import Array exposing (Array)
import Browser
import Game exposing (Board, Coordinates, Game, Space(..), Status(..))
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


type alias Model =
    Game


init : Model
init =
    Game.new 10 [ ( 3, 4 ), ( 4, 4 ) ]


main =
    Browser.sandbox { init = init, update = update, view = view }


type Msg
    = ClearSpaces Coordinates


update : Msg -> Model -> Model
update msg model =
    case msg of
        ClearSpaces ( x, y ) ->
            model
                |> Game.clearSpaces ( x, y )
                |> Game.checkIfPlayerWon


view : Model -> Html Msg
view model =
    let
        result =
            case model.status of
                InProgress ->
                    "🙂"

                Won ->
                    "😄"

                Lost ->
                    "😵"
    in
    div []
        [ text result
        , viewBoard model.board
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
