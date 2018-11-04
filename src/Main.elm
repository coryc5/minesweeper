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


defaultSize : Int
defaultSize =
    10


init : () -> ( Model, Cmd Msg )
init flags =
    let
        coordsList =
            Game.getAllCoords defaultSize

        randomizeCoordsCmd =
            Random.generate (NewGame defaultSize) (Random.List.shuffle coordsList)
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
    | ResetGame Int


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

        ResetGame size ->
            let
                coordsList =
                    Game.getAllCoords size

                randomizeCoordsCmd =
                    Random.generate (NewGame size) (Random.List.shuffle coordsList)
            in
            ( model, randomizeCoordsCmd )


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
                , viewGame model
                , viewReset model.status
                ]
    in
    Document "Elm Minesweeper" [ body ]


viewGame : Model -> Html Msg
viewGame model =
    let
        rows =
            model.board
                |> Array.indexedMap (viewRow model.status)
                |> Array.toList
    in
    div [] rows


viewRow : Status -> Int -> Array Space -> Html Msg
viewRow status y row =
    let
        spaces =
            row
                |> Array.indexedMap (\x space -> viewSpace status ( x, y ) space)
                |> Array.toList
    in
    div [ style "display" "flex", style "justify-content" "center" ] spaces


viewSpace : Status -> Coordinates -> Space -> Html Msg
viewSpace status ( x, y ) space =
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
            let
                events =
                    if status == InProgress then
                        [ onRightClick (FlagSpace ( x, y )) ]
                    else
                        []

                attrs =
                    (style "border-style" "outset") :: events
            in
            div (List.append attrs spaceStyles ) [ text "F" ]

        _ ->
            let
                events =
                    if status == InProgress then
                        [ onClick (ClearSpaces ( x, y )) , onRightClick (FlagSpace ( x, y )) ]
                    else
                     []

                attrs =
                    (style "border-style" "outset") :: events
            in
            div (List.append attrs spaceStyles) []


viewReset : Status -> Html Msg
viewReset status =
    let
        buttonAttrs =
            [ style "border" "none"
            , style "color" "white"
            , style "margin-top" "10px"
            , style "padding" "15px 32px"
            , style "text-align" "center"
            , style "text-decoration" "none"
            , style "display" "inline-block"
            , style "font-size" "16px"
            , onClick (ResetGame defaultSize)
            ]

        resetButton : String -> String -> Html Msg
        resetButton colorCode contents =
            Html.button (List.append
                    [ style "background-color" colorCode
                    ]
                    buttonAttrs
                )
            [ text contents ]
    in
    case status of
        InProgress ->
            text ""

        Won ->
            resetButton "#4caf50" "Play Again!"

        Lost ->
            resetButton "#f44336" "Try Again"


onRightClick : Msg -> Attribute Msg
onRightClick msg =
    Html.Events.preventDefaultOn "contextmenu" (Decode.map alwaysPreventDefault (Decode.succeed msg))


alwaysPreventDefault : Msg -> ( Msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )
