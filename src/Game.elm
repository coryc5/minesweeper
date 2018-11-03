module Game exposing
    ( Board
    , Coordinates
    , Game
    , Space(..)
    , Status(..)
    , checkIfPlayerWon
    , clearSpaces
    , new
    )

import Array exposing (Array)
import Dict exposing (Dict)
import Set exposing (Set)


type alias Game =
    { board : Board
    , status : Status
    , remainingSafeSpaces : Set Coordinates
    }


type alias Board =
    Array (Array Space)


type Status
    = Lost
    | Won
    | InProgress


type Space
    = Empty SpaceRevealed
    | Mine SpaceRevealed
    | Border SpaceRevealed Int


type alias SpaceRevealed =
    Bool


type alias Coordinates =
    ( Int, Int )


new : Int -> List Coordinates -> Game
new size mineCoords =
    let
        borderDict =
            getSurroundingMinesDict mineCoords

        minesSet =
            Set.fromList mineCoords

        safeSpacesSet =
            getSafeSpaces size minesSet

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
    Game board InProgress safeSpacesSet


getSafeSpaces : Int -> Set Coordinates -> Set Coordinates
getSafeSpaces size minesSet =
    let
        emptyList =
            List.range 0 (size - 1)

        safeSet =
            emptyList
                |> List.concatMap (\y -> List.map (\x -> ( x, y )) emptyList)
                |> List.filter (\coords -> not (Set.member coords minesSet))
                |> Set.fromList
    in
    safeSet


updateSafeSpaces : List Coordinates -> Set Coordinates -> Set Coordinates
updateSafeSpaces cleared remaining =
    List.foldr Set.remove remaining cleared


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


clearSpaces : Coordinates -> Game -> Game
clearSpaces ( x, y ) game =
    case Array.get y game.board of
        Nothing ->
            game

        Just row ->
            case Array.get x row of
                Nothing ->
                    game

                Just space ->
                    clearSpace ( x, y ) space game


clearSpace : Coordinates -> Space -> Game -> Game
clearSpace coords space game =
    let
        updatedStatus =
            if space == Mine False then
                Lost

            else
                game.status

        updatedSafeSpaces =
            updateSafeSpaces [ coords ] game.remainingSafeSpaces
    in
    case space of
        Mine False ->
            { game
                | board = setSpace coords (Mine True) game.board
                , remainingSafeSpaces = updatedSafeSpaces
                , status = updatedStatus
            }

        Empty False ->
            let
                tempBoard =
                    setSpace coords (Empty True) game.board

                tempGame =
                    { game | board = tempBoard, remainingSafeSpaces = updatedSafeSpaces }
            in
            clearEmptySpaces coords tempGame

        Border False n ->
            { game
                | board = setSpace coords (Border True n) game.board
                , remainingSafeSpaces = updatedSafeSpaces
                , status = updatedStatus
            }

        _ ->
            game


clearEmptySpaces : Coordinates -> Game -> Game
clearEmptySpaces ( x, y ) game =
    let
        surroundingCoords =
            getSurroundingCoordinates ( x, y )
    in
    List.foldr clearSpaces game surroundingCoords


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


checkIfPlayerWon : Game -> Game
checkIfPlayerWon game =
    case ( game.status, Set.isEmpty game.remainingSafeSpaces ) of
        ( InProgress, True ) ->
            { game | status = Won }

        _ ->
            game
