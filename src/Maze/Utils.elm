module Maze.Utils exposing (Width, Height, Index, moveIndexUp, moveIndexDown, moveIndexRight, moveIndexLeft, getRandomValue, getRandomIndex)

import Random
import Unwrap
import Array


type alias Width = Int
type alias Height = Int
type alias Index = Int


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
