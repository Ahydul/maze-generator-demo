module Maze.Prim exposing (buildMazeStep)

import Maze.Utils exposing (Width, Height, Index, Maze, MazeCommon, Direction(..), AlgorithmExtra(..), deleteEdge, moveIndexUp, moveIndexDown, moveIndexRight, moveIndexLeft, getRandomValue, getRandomIndex)

nothingIfListContains : Maybe a -> List a -> Maybe a
nothingIfListContains value list =
  Maybe.andThen (\num ->  if (List.member num list) then Nothing else Just num) value


getFrontier : List Int -> List Int -> Index -> Width -> Height -> List Int
getFrontier prevFrontier visitedIndexes index width height = 
  let
    tmpList = List.append visitedIndexes prevFrontier
    indexUp = moveIndexUp index width
      |> \num -> nothingIfListContains num tmpList
    indexDown = moveIndexDown index width height
      |> \num -> nothingIfListContains num tmpList
    indexRight = moveIndexRight index width height
      |> \num -> nothingIfListContains num tmpList
    indexLeft = moveIndexLeft index width
      |> \num -> nothingIfListContains num tmpList

    frontier = [indexUp, indexDown, indexRight, indexLeft]
      |> List.filterMap identity

  in 
    prevFrontier 
      |> List.filter (\x -> x /= index)
      |> List.append frontier


initMaze : MazeCommon -> Maze
initMaze maze = 
  let
    size = maze.width * maze.height
    (index, newSeed) = getRandomIndex size maze.seed
    visitedIndexes = List.singleton index
    frontier = getFrontier visitedIndexes [] index maze.width maze.height
    common = { maze | seed = newSeed, visitedIndexes = visitedIndexes }
  in
    Maze common (PrimExtra { frontier = frontier })


buildMazeStep : MazeCommon -> { frontier : List Index } -> Maze
buildMazeStep maze extra = 
    if (List.isEmpty extra.frontier) then
        initMaze maze
    else
      
  let
    (index1, newSeed) = getRandomValue extra.frontier maze.seed
    newVisitedIndexes = index1 :: maze.visitedIndexes
    adyacentIndexes : List ( Index, Direction )
    adyacentIndexes = 
      [ moveIndexUp index1 maze.width |> Maybe.andThen (\val -> Just (val, UP))
      , moveIndexDown index1 maze.width maze.height |>  Maybe.andThen (\val -> Just (val, DOWN))
      , moveIndexRight index1 maze.width maze.height |>  Maybe.andThen (\val -> Just (val, RIGHT))
      , moveIndexLeft index1 maze.width |>  Maybe.andThen (\val -> Just (val, LEFT))
      ]
      |> List.filterMap identity
      |> List.filter (\(i,_) -> List.member i newVisitedIndexes)

    ((index2, direction), newSeed2) = getRandomValue adyacentIndexes newSeed

    newGrid = deleteEdge index1 index2 direction maze.grid
    newFrontier = getFrontier extra.frontier maze.visitedIndexes index1 maze.width maze.height

    common = { maze | seed = newSeed2, visitedIndexes = newVisitedIndexes, grid = newGrid }
    newExtra = PrimExtra { frontier = newFrontier }
  in
    Maze common newExtra

