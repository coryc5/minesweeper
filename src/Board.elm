module Board exposing (Board, Coordinates, Space(..), clearSpaces, new)

import Array exposing (Array)
import Dict exposing (Dict)
import Set exposing (Set)


type alias Board =
    Array (Array Space)


type Space
    = Empty SpaceRevealed
    | Mine SpaceRevealed
    | Border SpaceRevealed Int


type alias SpaceRevealed =
    Bool


type alias Coordinates =
    ( Int, Int )


new : Int -> List Coordinates -> Board
new size mineCoords =
    let
        borderDict =
            getSurroundingMinesDict mineCoords

        minesSet =
            Set.fromList mineCoords

        emptySpace =
            Empty False

        emptyRow =
            Array.repeat size emptySpace

        board =
            emptyRow
                |> Array.repeat size
                |> Array.indexedMap
                    (\yIdx row ->
                        Array.indexedMap
                            (\xIdx space -> initSpace ( xIdx, yIdx ) minesSet borderDict)
                            row
                    )
    in
    board


getSurroundingMinesDict : List Coordinates -> Dict Coordinates Int
getSurroundingMinesDict coordinatesList =
    let
        accUpdate : Maybe Int -> Maybe Int
        accUpdate currentVal =
            case currentVal of
                Nothing ->
                    Just 1

                Just val ->
                    Just (val + 1)
    in
    coordinatesList
        |> List.concatMap getSurroundingCoordinates
        |> List.foldr (\coords acc -> Dict.update coords accUpdate acc) Dict.empty


getSurroundingCoordinates : Coordinates -> List Coordinates
getSurroundingCoordinates ( x, y ) =
    let
        left =
            x - 1

        right =
            x + 1

        up =
            y + 1

        down =
            y - 1
    in
    [ ( left, up )
    , ( x, up )
    , ( right, up )
    , ( left, y )
    , ( right, y )
    , ( left, down )
    , ( x, down )
    , ( right, down )
    ]


initSpace : Coordinates -> Set Coordinates -> Dict Coordinates Int -> Space
initSpace coords minesSet borderDict =
    case Set.member coords minesSet of
        True ->
            Mine False

        False ->
            case Dict.get coords borderDict of
                Just n ->
                    Border False n

                Nothing ->
                    Empty False


clearSpaces : Coordinates -> Board -> ( Board, Bool )
clearSpaces ( x, y ) board =
    case Array.get y board of
        Nothing ->
            ( board, False )

        Just row ->
            case Array.get x row of
                Nothing ->
                    ( board, False )

                Just space ->
                    clearSpace ( x, y ) space board


clearSpace : Coordinates -> Space -> Board -> ( Board, Bool )
clearSpace ( x, y ) space board =
    case space of
        Mine False ->
            ( setSpace ( x, y ) (Mine True) board, True )

        Empty False ->
            let
                updatedBoard =
                    setSpace ( x, y ) (Empty True) board
            in
            ( clearEmptySpaces ( x, y ) updatedBoard, False )

        Border False n ->
            ( setSpace ( x, y ) (Border True n) board, False )

        _ ->
            ( board, False )


clearEmptySpaces : Coordinates -> Board -> Board
clearEmptySpaces ( x, y ) board =
    let
        surroundingCoords =
            getSurroundingCoordinates ( x, y )

        clearSpacesHelper : Coordinates -> Board -> Board
        clearSpacesHelper coords b =
            let
                ( boardAcc, gameOver ) =
                    clearSpaces coords b
            in
            boardAcc
    in
    List.foldr clearSpacesHelper board surroundingCoords


setSpace : Coordinates -> Space -> Board -> Board
setSpace ( x, y ) space board =
    case Array.get y board of
        Nothing ->
            board

        Just row ->
            let
                updatedRow =
                    Array.set x space row

                updatedBoard =
                    Array.set y updatedRow board
            in
            updatedBoard
