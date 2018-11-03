module Main exposing (Msg(..), main, update, view)

import Array exposing (Array)
import Browser exposing (Document)
import Game exposing (Board, Coordinates, Game, Space(..), Status(..))
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Platform.Cmd exposing (Cmd)
import Platform.Sub
import Random
import Random.List


type alias Model =
    Game


init : () -> ( Model, Cmd Msg )
init flags =
    let
        size =
            10

        coordsList =
            Game.getAllCoords size

        randomizeCoordsCmd =
            Random.generate (NewGame size) (Random.List.shuffle coordsList)
    in
    ( Game.new 0 [], randomizeCoordsCmd )


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = \model -> Platform.Sub.none
        }


type Msg
    = ClearSpaces Coordinates
    | NewGame Int (List Coordinates)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClearSpaces ( x, y ) ->
            let
                updatedModel =
                    model
                        |> Game.clearSpaces ( x, y )
                        |> Game.checkIfPlayerWon
            in
            ( updatedModel, Platform.Cmd.none )

        NewGame size randomCoords ->
            let
                mineCoords =
                    List.take 12 randomCoords

                updatedModel =
                    Game.new size mineCoords
            in
            ( updatedModel, Platform.Cmd.none )


view : Model -> Document Msg
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

        body =
            div []
                [ text result
                , viewBoard model.board
                ]
    in
    Document "Elm Minesweeper" [ body ]


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
