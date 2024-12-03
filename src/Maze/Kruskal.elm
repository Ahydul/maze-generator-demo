module Maze.Kruskal exposing (buildMazeStep)

import Maze.Utils exposing (AlgorithmExtra(..), Direction(..), Edge, Maze, MazeCommon, deleteEdge, getRandomValue)
import Array exposing (Array)
import Unwrap


buildMazeStep : MazeCommon -> { ids : Array Int, edges : List Edge  } -> Maze
buildMazeStep maze extra =
    let
        ( (index1, index2, direction), newSeed ) = getRandomValue extra.edges maze.seed

        newVisitedIndexes = maze.visitedIndexes 
            |> List.append ([index1, index2]
                |> List.filter (\i -> not (List.member i maze.visitedIndexes)))

        (id1, id2) = 
            ( ( Array.get index1 extra.ids |> Unwrap.maybe )
            , ( Array.get index2 extra.ids |> Unwrap.maybe )
            )

        (newGrid, newIds) = if (id1 == id2) 
                then (maze.grid, extra.ids)
            else 
                ( deleteEdge index1 index2 direction maze.grid
                , Array.map (\id -> if id == id2 then id1 else id) extra.ids
                )

        common = { maze | seed = newSeed, visitedIndexes = newVisitedIndexes, grid = newGrid }
        newEdges = List.filter (\x -> x /= (index1, index2, direction)) extra.edges

        newExtra = KruskalExtra { ids = newIds, edges = newEdges }
    in
    Maze common newExtra
