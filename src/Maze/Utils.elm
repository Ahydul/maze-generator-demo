module Maze.Utils exposing (Width, Height, Index, Maze, MazeCommon, AlgorithmExtra(..), Direction(..), createMaze, mazeFinished, deleteEdge, moveIndexUp, moveIndexDown, moveIndexRight, moveIndexLeft, getRandomValue, getRandomIndex)

import Random
import Unwrap
import Array exposing (Array)


-- Model

type alias Width = Int
type alias Height = Int
type alias Index = Int

-- A cell has information about its borders
type alias Cell =
  { bUp: Bool
  , bDown: Bool
  , bLeft:  Bool
  , bRight:  Bool
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
    h = abs height
    size = w*h
    newSeed = Random.initialSeed seed
    --(index, newSeed) = getRandomIndex size tmpSeed
    visitedIndexes = []
    grid = Array.repeat (size) emptyCell
    --frontier = getFrontier visitedIndexes [] index w h
    common = MazeCommon w h newSeed grid visitedIndexes
  in
    Maze common extra--(PrimExtra { frontier = frontier })



-- Helpers

type Direction = UP | DOWN | LEFT | RIGHT

deleteEdge : Index -> Index -> Direction -> Grid -> Grid
deleteEdge index1 index2 direction grid =
  let
    cell1 = grid |> Array.get index1 |> Unwrap.maybe
    cell2 = grid |> Array.get index2 |> Unwrap.maybe
    newGrid = case direction of
      UP -> grid 
        |> Array.set index1 { cell1 | bUp = False } 
        |> Array.set index2 { cell2 | bDown = False }
      DOWN -> grid 
        |> Array.set index1 { cell1 | bDown = False } 
        |> Array.set index2 { cell2 | bUp = False }
      LEFT -> grid 
        |> Array.set index1 { cell1 | bLeft = False } 
        |> Array.set index2 { cell2 | bRight = False }
      RIGHT -> grid 
        |> Array.set index1 { cell1 | bRight = False } 
        |> Array.set index2 { cell2 | bLeft = False }
  in
    newGrid

mazeFinished : MazeCommon -> Bool
mazeFinished maze = List.length maze.visitedIndexes == (maze.height*maze.width)


moveIndexUp : Index -> Width -> Maybe Index
moveIndexUp index width = 
  let
    indexUp = index - width
  in
    if indexUp < 0 then Nothing else Just indexUp

moveIndexDown : Index -> Width -> Height -> Maybe Index
moveIndexDown index width height = 
  let
    indexDown = index + width
  in
    if indexDown > (width*height - 1) then Nothing else Just indexDown

--getIndexRow : Index -> Width -> Int
--getIndexRow index width = index // width

moveIndexRight : Index -> Width -> Height -> Maybe Index
moveIndexRight index width height = 
  let
    indexRight = index + 1
    col = getIndexColumn index width
  in
    if col == (width - 1) || indexRight > (width*height - 1) then Nothing else Just indexRight

getIndexColumn : Index -> Width -> Int
getIndexColumn index width = modBy width index

moveIndexLeft : Index -> Width -> Maybe Index
moveIndexLeft index width = 
  let
    indexLeft = index - 1
    col = getIndexColumn index width
  in
    if col == 0 || indexLeft < 0 then Nothing else Just indexLeft


getRandomIndex : Int -> Random.Seed -> ( Index, Random.Seed )
getRandomIndex size seed = Random.step (Random.int 0 (size - 1)) seed

getRandomValue : List a -> Random.Seed -> ( a, Random.Seed )
getRandomValue ls seed = 
  let
    size = List.length ls
    (i, newSeed) = getRandomIndex size seed
  in
    (ls |> Array.fromList |> Array.get i |> Unwrap.maybe, newSeed)
