module Maze.Utils exposing (AlgorithmExtra(..), Direction(..), Height, Index, Maze, MazeCommon, Width, createMaze, deleteEdge, getRandomIndex, getRandomValue, mazeFinished, moveIndexDown, moveIndexLeft, moveIndexRight, moveIndexUp)

import Array exposing (Array)
import Random
import Unwrap



-- Model


type alias Width = Int


type alias Height = Int


type alias Index = Int



-- A cell has information about its borders


type alias Cell =
    { bUp : Bool
    , bDown : Bool
    , bLeft : Bool
    , bRight : Bool
    }


emptyCell : Cell
emptyCell = Cell True True True True


type alias Grid = Array Cell


type AlgorithmExtra
    = PrimExtra { frontier : List Index }


type alias MazeCommon =
    { width : Width
    , height : Height
    , seed : Random.Seed
    , grid : Grid
    , visitedIndexes : List Index
    }


type alias Maze =
    { common : MazeCommon
    , extra : AlgorithmExtra
    }


createMaze : Width -> Height -> Int -> AlgorithmExtra -> Maze
createMaze width height seed extra =
    let
        w = abs width
        h =  abs height
        size = w * h
        newSeed = Random.initialSeed seed
        visitedIndexes = []
        grid =  Array.repeat size emptyCell
        common = MazeCommon w h newSeed grid visitedIndexes
    in
    Maze common extra



-- Helpers


type Direction
    = UP
    | DOWN
    | LEFT
    | RIGHT


deleteEdge : Index -> Index -> Direction -> Grid -> Grid
deleteEdge index1 index2 direction grid =
    let
        cell1 = grid |> Array.get index1 |> Unwrap.maybe
        cell2 = grid |> Array.get index2 |> Unwrap.maybe

        (newCell1, newCell2) =
            case direction of
                UP ->
                    ({ cell1 | bUp = False }, { cell2 | bDown = False })

                DOWN ->
                    ({ cell1 | bDown = False }, { cell2 | bUp = False })

                LEFT ->
                    ({ cell1 | bLeft = False }, { cell2 | bRight = False })

                RIGHT ->
                    ({ cell1 | bRight = False }, { cell2 | bLeft = False })

        newGrid =
            grid
                |> Array.set index1 newCell1
                |> Array.set index2 newCell2
    in
    newGrid


mazeFinished : MazeCommon -> Bool
mazeFinished maze =
    List.length maze.visitedIndexes == (maze.height * maze.width)


moveIndexUp : Index -> Width -> Maybe Index
moveIndexUp index width =
    let
        indexUp = index - width
        condition = indexUp < 0
    in
    if condition then Nothing else Just indexUp


moveIndexDown : Index -> Width -> Height -> Maybe Index
moveIndexDown index width height =
    let
        indexDown = index + width
        condition = indexDown > (width * height - 1)
    in
    if condition then Nothing else Just indexDown


--getIndexRow : Index -> Width -> Int
--getIndexRow index width = index // width


moveIndexRight : Index -> Width -> Height -> Maybe Index
moveIndexRight index width height =
    let
        indexRight = index + 1
        col = getIndexColumn index width
        condition = col == (width - 1) || indexRight > (width * height - 1)
    in
    if condition then Nothing else Just indexRight


getIndexColumn : Index -> Width -> Int
getIndexColumn index width = modBy width index


moveIndexLeft : Index -> Width -> Maybe Index
moveIndexLeft index width =
    let
        indexLeft = index - 1
        col = getIndexColumn index width
        condition = col == 0 || indexLeft < 0
    in
    if condition then Nothing else Just indexLeft


getRandomIndex : Int -> Random.Seed -> ( Index, Random.Seed )
getRandomIndex size seed = Random.step (Random.int 0 (size - 1)) seed


getRandomValue : List a -> Random.Seed -> ( a, Random.Seed )
getRandomValue ls seed =
    let
        size = List.length ls
        ( i, newSeed ) = getRandomIndex size seed
    in
    ( ls |> Array.fromList |> Array.get i |> Unwrap.maybe, newSeed )
