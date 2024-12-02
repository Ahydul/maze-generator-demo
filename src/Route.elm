module Route exposing (..)

import Url exposing (Url)
import Url.Parser as Parser exposing (Parser, oneOf, s, parse)

type Route
    = NotFound
    | Home
    | Maze


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Maze (s "maze")
        ]


parseUrl : Url -> Route
parseUrl url =
    case parse parser url of
        Just route ->
            route

        Nothing ->
            NotFound


pathFor : Route -> String
pathFor route =
    case route of
        Home ->
            "/"

        Maze ->
            "/maze"

        NotFound -> 
            "/"

