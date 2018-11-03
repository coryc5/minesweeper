module Game exposing
    ( Board
    , Coordinates
    , Game
    , Space
    , SpaceKind(..)
    , SpaceStatus(..)
    , Status(..)
    , checkIfPlayerWon
    , clearSpaces
    , getAllCoords
    , new
    , toggleFlagSpace
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


type alias Space =
    ( SpaceStatus, SpaceKind )


type SpaceStatus
    = Flagged
    | Revealed
    | Unrevealed


type SpaceKind
    = Empty
    | Mine
    | Border Int


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
            ( Unrevealed, Empty )

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


getAllCoords : Int -> List Coordinates
getAllCoords size =
    let
        emptyList =
            List.range 0 (size - 1)
    in
    List.concatMap (\y -> List.map (\x -> ( x, y )) emptyList) emptyList


getSafeSpaces : Int -> Set Coordinates -> Set Coordinates
getSafeSpaces size minesSet =
    size
        |> getAllCoords
        |> List.filter (\coords -> not (Set.member coords minesSet))
        |> Set.fromList


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
            ( Unrevealed, Mine )

        False ->
            case Dict.get coords borderDict of
                Just n ->
                    ( Unrevealed, Border n )

                Nothing ->
                    ( Unrevealed, Empty )


getSpace : Coordinates -> Game -> Maybe Space
getSpace ( x, y ) game =
    case Array.get y game.board of
        Nothing ->
            Nothing

        Just row ->
            Array.get x row


clearSpaces : Coordinates -> Game -> Game
clearSpaces ( x, y ) game =
    case getSpace ( x, y ) game of
        Nothing ->
            game

        Just space ->
            clearSpace ( x, y ) space game


clearSpace : Coordinates -> Space -> Game -> Game
clearSpace coords space game =
    let
        updatedStatus =
            if space == ( Unrevealed, Mine ) then
                Lost

            else
                game.status

        updatedSafeSpaces =
            Set.remove coords game.remainingSafeSpaces
    in
    case space of
        ( Unrevealed, Mine ) ->
            { game
                | board = setSpace coords ( Revealed, Mine ) game.board
                , remainingSafeSpaces = updatedSafeSpaces
                , status = updatedStatus
            }

        ( Unrevealed, Empty ) ->
            let
                tempBoard =
                    setSpace coords ( Revealed, Empty ) game.board

                tempGame =
                    { game | board = tempBoard, remainingSafeSpaces = updatedSafeSpaces }
            in
            clearEmptySpaces coords tempGame

        ( Unrevealed, Border n ) ->
            { game
                | board = setSpace coords ( Revealed, Border n ) game.board
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


toggleFlagSpace : Coordinates -> Game -> Game
toggleFlagSpace ( x, y ) game =
    case getSpace ( x, y ) game of
        Nothing ->
            game

        Just ( spaceStatus, spaceKind ) ->
            let
                updatedSpaceStatus =
                    case spaceStatus of
                        Flagged ->
                            Unrevealed

                        _ ->
                            Flagged

                updatedBoard =
                    setSpace ( x, y ) ( updatedSpaceStatus, spaceKind ) game.board
            in
            { game | board = updatedBoard }


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
