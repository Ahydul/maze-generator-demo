module Maze exposing (Model, Msg, defaultModel, subscriptions, update, view)

import Array
import Html exposing (..)
import Html.Attributes as Attrs exposing (style, type_, value, width)
import Html.Events exposing (onClick, onInput)
import Html.Lazy exposing (lazy)
import Maybe exposing (withDefault)
import Maze.Prim as Prim exposing (buildMazeStep)
import Maze.Utils exposing (AlgorithmExtra(..), Height, Maze, MazeCommon, Width, createMaze, mazeFinished)
import Time
import Unwrap



-- Model


type Algorithm
    = Prim


algorithmToString : Algorithm -> String
algorithmToString algorithm =
    case algorithm of
        Prim ->
            "prim"


stringToAlgorithm : String -> Algorithm
stringToAlgorithm str =
    case str of
        "prim" ->
            Prim

        _ ->
            Prim



--default


type alias Model =
    { maze : Maze
    , runGenerator : Bool
    , tickInterval : Int
    , seed : Int
    , algorithm : Algorithm
    }


widthDefault : Int
widthDefault = 5


heightDefault : Int
heightDefault = 5


tickIntervalDefault : Int
tickIntervalDefault = 10


defaultModel : Model
defaultModel =
    let
        seed = 0
        extra = PrimExtra { frontier = [] }
        maze = createMaze widthDefault heightDefault seed extra
    in
    Model maze False tickIntervalDefault seed Prim



-- Logic


buildMazeStep : Maze -> Maze
buildMazeStep maze =
    if mazeFinished maze.common then
        maze

    else
        case maze.extra of
            PrimExtra extra ->
                Prim.buildMazeStep maze.common extra


buildMazeStepSkipping : Maze -> Int -> Maze
buildMazeStepSkipping maze skip =
    List.foldl (\_ acc -> buildMazeStep acc) maze (List.repeat (skip + 1) ())



-- Update


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
                common = maze.common
                newMaze = createMaze common.width common.height model.seed maze.extra
            in
            { model | maze = newMaze, runGenerator = False }

        Step ->
            { model | maze = buildMazeStep model.maze, runGenerator = False }

        Tick _ ->
            let
                skip = floor (1 / toFloat model.tickInterval) * 50
            in
            { model | maze = buildMazeStepSkipping model.maze skip }

        Run ->
            { model | runGenerator = True }

        TickSpeedUpdate tickUpdate ->
            let
                newTickInterval =
                    String.toInt tickUpdate
                        |> withDefault tickIntervalDefault
                        |> min maxTickInterval
                        |> max minTickInterval
            in
            { model | tickInterval = newTickInterval }

        HeightUpdate heightUpdate ->
            let
                newHeight =
                    heightUpdate
                        |> String.toInt
                        |> withDefault heightDefault
                        |> min maxSize
                        |> max minSize
                newMaze = createMaze model.maze.common.width newHeight model.seed model.maze.extra
            in
            { model | maze = newMaze, runGenerator = False }

        WidthUpdate widthUpdate ->
            let
                newWidth =
                    widthUpdate
                        |> String.toInt
                        |> withDefault widthDefault
                        |> min maxSize
                        |> max minSize
                newMaze = createMaze newWidth model.maze.common.height model.seed model.maze.extra
            in
            { model | maze = newMaze, runGenerator = False }

        SeedUpdate seedUpdate ->
            let
                newSeed = seedUpdate |> String.toInt |> withDefault 0
                common = model.maze.common
                newMaze = createMaze common.width common.height newSeed model.maze.extra
            in
            { model | maze = newMaze, seed = newSeed, runGenerator = False }

        AlgorithmUpdate algorithmUpdate ->
            let
                newAlgorithm = stringToAlgorithm algorithmUpdate
                common = model.maze.common
                newMaze = createMaze common.width common.height model.seed model.maze.extra
            in
            { model | maze = newMaze, algorithm = newAlgorithm, runGenerator = False }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.runGenerator && not (mazeFinished model.maze.common) then
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
            [ viewInputs model.seed model.tickInterval maze.common.width maze.common.height
            , viewMaze maze
            ]
    }


viewInputs : Int -> Int -> Width -> Height -> Html Msg
viewInputs seed tickInterval width height =
    div
        [ style "display" "flex"
        , style "align-items" "start"
        , style "padding" "10px 0"
        , style "flex-direction" "column"
        ]
        [ div []
            [ label [ Attrs.for "algorithm-input", style "padding-right" "10px" ] [ text "Algorithm" ]
            , select [ Attrs.id "algorithm-input", onInput SeedUpdate ]
                [ option [ Attrs.selected True, Attrs.value (algorithmToString Prim) ] [ text "Prims' Algorithm" ]
                ]
            ]
        , div []
            [ label [ Attrs.for "seed-input", style "padding-right" "10px" ] [ text "Seed" ]
            , input [ Attrs.id "seed-input", value (String.fromInt seed), onInput SeedUpdate ] []
            ]
        , viewSlider TickSpeedUpdate "Tick speed" minTickInterval maxTickInterval tickInterval
        , viewSlider HeightUpdate "Height" minSize maxSize height
        , viewSlider WidthUpdate "Width" minSize maxSize width
        , div [ style "display" "flex", style "margin-top" "10px" ]
            [ viewButton "Step" Step
            , viewButton "Run" Run
            , viewButton "Reset" Reset
            ]
        ]


viewSlider : (String -> Msg) -> String -> Int -> Int -> Int -> Html Msg
viewSlider msgUpdate txt min max default =
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ label
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
            []
        , label
            [ Attrs.for "tickSpeed"
            , style "padding-left" "10px"
            ]
            [ text (String.fromInt default) ]
        ]


viewButton : String -> msg -> Html msg
viewButton txt msg =
    button
        [ onClick msg
        , style "background-color" "#f44336"
        , style "color" "white"
        , style "padding" "14px 25px"
        , style "text-align" "center"
        ]
        [ text txt ]


emptyList : Int -> List Int
emptyList size =
    List.range 0 size


viewMaze : Maze -> Html Msg
viewMaze maze =
    let
        divs : List (Html msg)
        divs =
            emptyList (maze.common.height - 1) |> List.concatMap (\row -> viewRow row maze)
    in
    div
        [ style "display" "grid"
        , style "grid-template-rows" ("repeat(" ++ String.fromInt maze.common.height ++ " , 1fr)")
        , style "grid-template-columns" ("repeat(" ++ String.fromInt maze.common.width ++ " , 1fr)")
        , style "width" "100%"
        , style "height" "100%"
        , style "box-sizing" "border-box"
        ]
        divs


viewRow : Int -> Maze -> List (Html msg)
viewRow row maze =
    let
        divs : List (Html msg)
        divs =
            emptyList (maze.common.width - 1)
                |> List.map (\col -> viewCell (row * maze.common.width + col) maze)
    in
    divs


backgroundColor : String
backgroundColor = "rgb(251, 243, 231)"


cellColor : Int -> Maze -> String
cellColor index maze =
    if List.member index maze.common.visitedIndexes then
        backgroundColor

    else
        case maze.extra of
            PrimExtra extra ->
                if List.member index extra.frontier then
                    "#ffff7e"

                else
                    "gray"

            --_ -> "gray"


borderStyle : Bool -> String
borderStyle bool =
    if bool then
        "1px solid black"

    else
        "1px solid " ++ backgroundColor


addBorderStyle : Int -> MazeCommon -> List (Attribute msg)
addBorderStyle index maze =
    let
        cell =
            Array.get index maze.grid |> Unwrap.maybe
    in
    [ style "border-top" (borderStyle cell.bUp)
    , style "border-bottom" (borderStyle cell.bDown)
    , style "border-right" (borderStyle cell.bRight)
    , style "border-left" (borderStyle cell.bLeft)
    ]


viewCell : Int -> Maze -> Html msg
viewCell index maze =
    lazy
        (div
            (List.append
                [ Attrs.id ("cell-" ++ String.fromInt index)
                , style "padding" "0"
                , style "background-color" (cellColor index maze)
                ]
                (addBorderStyle index maze.common)
            )
        )
        --[ text (String.fromInt index) ] -- For debug
        []
