module Main exposing (Msg(..), main, update, view)

import Array exposing (Array)
import Browser exposing (Document)
import Game exposing (Board, Coordinates, Game, Space, SpaceKind(..), SpaceStatus(..), Status(..))
import Html exposing (Attribute, Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Json.Decode as Decode
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
    | FlagSpace Coordinates
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

        FlagSpace ( x, y ) ->
            let
                updatedModel =
                    Game.toggleFlagSpace ( x, y ) model
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
            div [ style "text-align" "center" ]
                [ div [ style "font-size" "30px" ] [ text result ]
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
    div [ style "display" "flex", style "justify-content" "center" ] spaces


viewSpace : Coordinates -> Space -> Html Msg
viewSpace ( x, y ) space =
    let
        spaceStyles =
            [ style "width" "30px"
            , style "height" "30px"
            , style "display" "inline-block"
            , style "vertical-align" "top"
            , style "text-align" "center"
            , style "font-size" "24px"
            ]
    in
    case space of
        ( Revealed, Mine ) ->
            div
                (List.append [ style "border-style" "inset" ] spaceStyles)
                [ text "X" ]

        ( Revealed, Border n ) ->
            div
                (List.append [ style "border-style" "inset" ] spaceStyles)
                [ text (String.fromInt n) ]

        ( Revealed, Empty ) ->
            div
                (List.append [ style "border-style" "inset" ] spaceStyles)
                []

        ( Flagged, _ ) ->
            div
                (List.append
                    [ style "border-style" "outset"
                    , onRightClick (FlagSpace ( x, y ))
                    ]
                    spaceStyles
                )
                [ text "F" ]

        _ ->
            div
                (List.append
                    [ style "border-style" "outset"
                    , onClick (ClearSpaces ( x, y ))
                    , onRightClick (FlagSpace ( x, y ))
                    ]
                    spaceStyles
                )
                []


onRightClick : Msg -> Attribute Msg
onRightClick msg =
    Html.Events.preventDefaultOn "contextmenu" (Decode.map alwaysPreventDefault (Decode.succeed msg))


alwaysPreventDefault : Msg -> ( Msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )
