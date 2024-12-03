module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Maze
import Page exposing (Page(..))
import Route exposing (..)
import Url



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { key : Nav.Key
    , route : Route
    , commonModel : CommonModel
    }


type CommonModel
    = Redirect
    | UpdateMaze Maze.Model


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model key (Route.parseUrl url) Redirect
    , Cmd.none
    )
        |> initPage



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | MazeMsg Maze.Msg


initPage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
initPage ( model, cmd ) =
    let
        newModel =
            case model.route of
                Route.Home ->
                    { model | commonModel = Redirect }

                Route.Maze ->
                    { model | commonModel = UpdateMaze Maze.defaultModel }

                Route.NotFound ->
                    model
    in
    ( newModel, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | route = Route.parseUrl url }
            , Cmd.none
            )
                |> initPage

        MazeMsg mazeMsg ->
            case model.commonModel of
                UpdateMaze mazeModel ->
                    ( { model | commonModel = UpdateMaze (Maze.update mazeMsg mazeModel) }
                    , Cmd.none
                    )

                -- Ignore
                _ ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.commonModel of
        Redirect ->
            Sub.none

        UpdateMaze mazeModel ->
            Sub.map MazeMsg (Maze.subscriptions mazeModel)



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        viewPage page toMsg pageView =
            let { title, body } =
                    Page.view page pageView
            in
            { title = title
            , body = List.map (Html.map toMsg) body
            }
    in
    case ( model.route, model.commonModel ) of

        ( Route.Home, _ ) -> Page.view Page.Home { title = "Main", content = mainPage }

        ( Route.Maze, UpdateMaze mazeModel ) -> viewPage Page.Maze MazeMsg (Maze.view mazeModel)

        ( _, _ ) -> Page.view Page.Home { title = "Main", content = mainPage }


mainPage : Html msg
mainPage =
    div [ style "display" "flex" ]
        [ Page.viewLink "/maze" "Generate" ]
