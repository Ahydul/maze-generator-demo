module Maze exposing (Maze, defaultModel, Msg, view, Model, update, subscriptions)

import Array exposing (Array)
import Random
import Unwrap
import Maze.Utils exposing (..)
import Html exposing (..)
import Html.Attributes as Attrs exposing (style, value, type_)
import Html.Events exposing (onClick, onInput)
import Array
import Time
import Maybe exposing (withDefault)
import Html.Attributes exposing (width)
import Html.Lazy exposing (lazy)


-- Model

type Algorithm 
  = Prim

algorithmToString : Algorithm -> String
algorithmToString algorithm =
  case algorithm of
    Prim -> "prim"

stringToAlgorithm : String -> Algorithm
stringToAlgorithm str =
  case str of
    "prim" -> Prim
    _ -> Prim --default

type alias Model =
    { maze : Maze
    , runGenerator : Bool
    , tickInterval : Int
    , seed : Int
    , algorithm : Algorithm
    }

newModel : Maybe Width -> Maybe Height -> Maybe Int -> Maybe Algorithm -> Model
newModel width height seed algorithm = 
  let
      w = withDefault widthDefault width 
      h = withDefault heightDefault height 
      s = withDefault 0 seed
      a = withDefault (stringToAlgorithm "") algorithm
  in
  Model (newMaze w h s) False tickIntervalDefault s a

defaultModel : Model
defaultModel = newModel Nothing Nothing Nothing Nothing

type alias Cell =
  { bUp: Bool
  , bDown: Bool
  , bLeft:  Bool
  , bRight:  Bool
  }

emptyCell : Cell
emptyCell = Cell True True True True


type alias Grid = Array Cell

type alias Maze =
  { width : Width
  , height : Height
  , seed : Random.Seed
  , grid : Grid
  , visitedIndexes : List Index
  , frontier : List Index
  }

newMaze : Width -> Height -> Int -> Maze
newMaze width height seed = 
  let
    w = abs width
    h = abs height
    size = w*h
    tmpSeed = Random.initialSeed seed
    (index, newSeed) = getRandomIndex size tmpSeed
    visitedIndexes = List.singleton index
    grid = Array.repeat (size) emptyCell
    frontier = getFrontier visitedIndexes [] index w h
  in
    Maze w h newSeed grid visitedIndexes frontier



-- Logic


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
  

type Direction = UP | DOWN | LEFT | RIGHT



buildMazeStep : Maze -> Maze
buildMazeStep maze = 
  if ( List.isEmpty maze.frontier ) then
    maze
  else 
  let
    (index1, newSeed) = getRandomValue maze.frontier maze.seed
    newVisitedIndexes = index1 :: maze.visitedIndexes 
    
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
    newFrontier = getFrontier maze.frontier maze.visitedIndexes index1 maze.width maze.height    
  in
    { maze | seed = newSeed2
    , visitedIndexes = newVisitedIndexes
    , frontier = newFrontier
    , grid = newGrid
    }


buildMazeStepSkipping : Maze -> Int -> Maze
buildMazeStepSkipping maze skip =
    List.foldl (\_ acc -> buildMazeStep acc) maze (List.repeat (skip + 1) ())

-- Update

widthDefault : Int
widthDefault = 5
heightDefault : Int
heightDefault = 5
tickIntervalDefault : Int
tickIntervalDefault = 10

type Msg 
    = Reset
    | Step
    | Tick Time.Posix
    | Run
    | TickSpeedUpdate String
    | HeightUpdate String
    | WidthUpdate String
    | SeedUpdate String
    | AlgorithmUpdate String

maxTickInterval : Int
maxTickInterval = 100
minTickInterval : Int
minTickInterval = 1
maxSize : Int
maxSize = 50
minSize : Int
minSize = 4

update : Msg -> Model -> Model
update msg model =
    case msg of
        Reset ->
            let
                maze = model.maze
            in
            { model | maze = newMaze maze.width maze.height model.seed, runGenerator = False }
        
        Step -> 
            { model | maze = buildMazeStep model.maze, runGenerator = False } 

        Tick _ ->
          let
            skip = floor (1 / (toFloat model.tickInterval)) * 50
          in
            { model | maze = buildMazeStepSkipping model.maze skip } 

        Run ->
            { model | runGenerator = True }

        TickSpeedUpdate tickUpdate ->
            let 
              newTickInterval 
                = String.toInt tickUpdate 
                |> withDefault tickIntervalDefault 
                |> min maxTickInterval
                |> max minTickInterval
            in
            { model | tickInterval = newTickInterval }

        HeightUpdate heightUpdate ->
            let 
              newHeight 
                = heightUpdate 
                |> String.toInt 
                |> withDefault heightDefault
                |> min maxSize
                |> max minSize
            in
            { model | maze = newMaze model.maze.width newHeight model.seed, runGenerator = False }

        WidthUpdate widthUpdate ->
            let
              newWidth 
                = widthUpdate 
                |> String.toInt 
                |> withDefault widthDefault
                |> min maxSize
                |> max minSize
            in
            { model | maze = newMaze newWidth model.maze.height model.seed, runGenerator = False }
        
        SeedUpdate seedUpdate ->
            let 
              newSeed = seedUpdate |> String.toInt |> withDefault 0
              maze = model.maze
            in
            { model | maze = newMaze maze.width maze.height newSeed, seed = newSeed, runGenerator = False }

        AlgorithmUpdate algorithmUpdate ->
            let 
              newAlgorithm = stringToAlgorithm algorithmUpdate
              maze = model.maze
            in
            { model | maze = newMaze maze.width maze.height model.seed, algorithm = newAlgorithm, runGenerator = False }

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
   if ( model.runGenerator && not (List.isEmpty model.maze.frontier) ) then
      Time.every (toFloat model.tickInterval) Tick
   else
      Sub.none
  



-- View


view : Model -> { title : String, content : Html Msg }
view model = 
  let
    maze = model.maze
  in
  { title = "URL Interceptor"
  , content =
      div 
      [ style "display" "grid"
      , style "grid-template-columns" "1fr 2fr"
      , style "width" "80vw"
      , style "height" "100%"
      ]
      [ viewInputs model.seed model.tickInterval maze.width maze.height
      , viewMaze maze
      ]
  }


viewInputs : Int -> Int -> Width -> Height -> Html Msg
viewInputs seed tickInterval width height = 
  div [ style "display" "flex"
    , style "align-items" "start"
    , style "padding" "10px 0"
    , style "flex-direction" "column"
    ]  
    [ div []
      [ label [ Attrs.for "algorithm-input", style "padding-right" "10px"] [ text "Algorithm" ]
      , select [ Attrs.id "algorithm-input", onInput SeedUpdate] 
          [ option [ Attrs.selected True, Attrs.value (algorithmToString Prim) ] [ text "Prims' Algorithm" ]

          ]
      ]
    , div []
      [ label [ Attrs.for "seed-input", style "padding-right" "10px"] [ text "Seed" ]
      , input [ Attrs.id "seed-input", value (String.fromInt seed), onInput SeedUpdate] []
      ]
    , viewSlider TickSpeedUpdate "Tick speed" minTickInterval maxTickInterval tickInterval
    , viewSlider HeightUpdate "Height" minSize maxSize height
    , viewSlider WidthUpdate "Width" minSize maxSize width
    , div [ style "display" "flex" , style "margin-top" "10px"] 
        [ viewButton "Step" Step
        , viewButton "Run" Run
        , viewButton "Reset" Reset
        ]
    ]


viewSlider : (String -> Msg) -> String -> Int -> Int -> Int -> Html Msg
viewSlider msgUpdate txt min max default = 
  div [ style "display" "flex"
    , style "justify-content" "center"
    , style "align-items" "center"
    ] [
    label 
      [ Attrs.for txt 
      , style "padding-right" "10px"
      ] 
      [ text txt ]
    , input
        [ type_ "range"
        , Attrs.min (String.fromInt min)
        , Attrs.max (String.fromInt max)
        , Attrs.id txt
        , value (String.fromInt default)
        , onInput msgUpdate
        ]
        [ ]   
    , label 
        [ Attrs.for "tickSpeed" 
        , style "padding-left" "10px"
        ] 
        [ text (String.fromInt default) ]
  
  ]   

viewButton : String -> msg -> Html msg
viewButton txt msg =
    button [ onClick msg
        , style "background-color" "#f44336"
        , style "color" "white"
        , style "padding" "14px 25px"
        , style "text-align" "center"
        ] 
        [ text txt ]

emptyList : Int -> List Int
emptyList size = List.range 0 size

viewMaze : Maze -> Html Msg
viewMaze maze =
    let
        divs : List (Html msg)
        divs = emptyList (maze.height - 1) |> List.concatMap (\row -> viewRow row maze ) 
    in
    div 
    [ style "display" "grid"
    , style "grid-template-rows" ("repeat("++ String.fromInt maze.height ++" , 1fr)")
    , style "grid-template-columns" ("repeat("++ String.fromInt maze.width ++" , 1fr)")
    , style "width" "100%"
    , style "height" "100%"
    , style "box-sizing" "border-box"
    ]
     divs 
    

viewRow : Int -> Maze -> List (Html msg)
viewRow row maze =
    let
        divs : List (Html msg)
        divs = List.map (\col -> viewCell (row*maze.width  + col) maze) (emptyList (maze.width - 1))
    in
    divs

backgroundColor : String
backgroundColor = "rgb(251, 243, 231)"

cellColor : Int -> List Int -> List Int -> String
cellColor index cells frontier = 
  if (List.member index cells) 
    then backgroundColor
  else if (List.member index frontier) 
    then "#ffff7e"
  else "gray"


borderStyle : Bool -> String
borderStyle bool = 
  if bool then "1px solid black" else ("1px solid " ++ backgroundColor)
addBorderStyle : Int -> Maze -> List (Attribute msg)
addBorderStyle index maze =
  let
    cell = Array.get index maze.grid |> Unwrap.maybe
  in
    [ style "border-top" (borderStyle cell.bUp)
    , style "border-bottom" (borderStyle cell.bDown)
    , style "border-right" (borderStyle cell.bRight)
    , style "border-left" (borderStyle cell.bLeft)
    ] 

viewCell : Int -> Maze -> Html msg
viewCell index maze = 
  lazy (div (List.append 
    [ Attrs.id ("cell-" ++ String.fromInt index)
    , style "padding" "0"
    , style "background-color" (cellColor index maze.visitedIndexes maze.frontier)
    ]  (addBorderStyle index maze)))

  --[ text (String.fromInt index) ] -- For debug
  [ ]
