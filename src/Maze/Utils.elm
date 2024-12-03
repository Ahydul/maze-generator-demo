module Maze.Utils exposing (Algorithm(..), AlgorithmExtra(..), Direction(..), Height, Index, Edge, Maze, MazeCommon, Width, createMaze, initExtraData, deleteEdge, getRandomIndex, getRandomValue, mazeFinished, moveIndexDown, moveIndexLeft, moveIndexRight, moveIndexUp, algorithmToString, stringToAlgorithm)

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

type alias Edge = (Index, Index, Direction) 

type AlgorithmExtra
    = PrimExtra { frontier : List Index }
    | KruskalExtra 
        { ids : Array Int
        , edges : List Edge
        }


type Algorithm
    = Prim
    | Kruskal


algorithmToString : Algorithm -> String
algorithmToString algorithm =
    case algorithm of
        Prim ->
            "prim"
        Kruskal ->
            "kruskal"


stringToAlgorithm : String -> Algorithm
stringToAlgorithm str =
    case str of
        "prim" ->
            Prim

        "kruskal" ->
            Kruskal

        _ ->
            Prim



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


initVerticalEdges : Width -> Height -> List Edge
initVerticalEdges width height =
    List.concat (List.range 0 (height - 1)
      |> List.map (\row -> List.range 0 (width - 2)
            |> List.map (\i -> (i+(row*width), i+(row*width)+1, RIGHT))
            ))

initHorizontalEdges : Width -> Height -> List Edge
initHorizontalEdges width height =
    List.range 0 ((width * height) - width - 1)
      |> List.map (\i -> (i, i+width, DOWN))


initExtraData : Algorithm -> Width -> Height -> AlgorithmExtra
initExtraData algorithm width height =
    case algorithm of
        Prim -> PrimExtra { frontier = [] }

        Kruskal -> 
          KruskalExtra 
            { ids = Array.fromList (List.range 1 (width*height))
            , edges = List.append (initVerticalEdges width height) (initHorizontalEdges width height)
            }

createMaze : Width -> Height -> Int -> Algorithm -> Maze
createMaze width height seed algorithm =
    let
        w = abs width
        h =  abs height
        size = w * h
        newSeed = Random.initialSeed seed
        visitedIndexes = []
        grid =  Array.repeat size emptyCell
        common = MazeCommon w h newSeed grid visitedIndexes
        extra = initExtraData algorithm w h
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
