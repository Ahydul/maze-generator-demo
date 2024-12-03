module Page exposing (..)

import Browser exposing (Document)
import Html exposing (Html, a, button, div, footer, nav, p, span, text, li, h1)
import Html.Attributes exposing (class, href, style)
import Html.Events exposing (onClick)
import Html exposing (h1)


type Page
    = Home
    | Maze


view : Page -> { title : String, content : Html msg } -> Document msg
view _ { title, content } =
    { title = title
    , body = [ 
        div [ style "display" "grid"
            , style "grid-template-rows" "2fr 8fr 1fr"
            , style "height" "100vh"
            , style "background" "antiquewhite"
            ] 
            [ viewHeader
            , div 
                [ style "display" "flex"
                , style "flex-direction" "column" 
                , style "justify-content" "center"
                , style "align-items" "center"
                ] [ content ]
            , viewFooter
            ]
     ]
    }

headerFooterStyle : List (Html.Attribute msg)
headerFooterStyle = 
    [ style "display" "flex"
    , style "justify-content" "center"
    , style "align-items" "center"
    , style "padding" "0 2%"
    ]

viewHeader : Html msg
viewHeader =
    nav headerFooterStyle
        [ a 
            [ style "text-decoration" "none"
            , style "color" "black"
            , style "font-size" "x-large"
            , href "/" 
            ] [ h1 [] [text "Maze generator"] ]

        ]



viewFooter : Html msg
viewFooter =
    footer headerFooterStyle
        [ div [ ]
            [ span [ ]
                [ text "A maze generator demo made by "
                , a [ href "https://github.com/Ahydul/maze-generator-demo" ] [ text "Ahydul" ]
                , text ". Code licensed under Apache License 2.0"
                ]
            ]
        ]



viewLink : String -> String -> Html msg
viewLink path str =
    li [ style "padding" "10px"
        , style "list-style" "none"
        ] 
        [ a [ href path 
            , style "background-color" "#f44336"
            , style "color" "white"
            , style "padding" "14px 25px"
            , style "text-align" "center"
            , style "text-decoration" "none"
            , style "display" "inline-block"
            ] 
            [ text str ] 
        ]


viewErrors : msg -> List String -> Html msg
viewErrors dismissErrors errors =
    if List.isEmpty errors then
        Html.text ""

    else
        div
            [ class "error-messages"
            , style "position" "fixed"
            , style "top" "0"
            , style "background" "rgb(250, 250, 250)"
            , style "padding" "20px"
            , style "border" "1px solid"
            ]
        <|
            List.map (\error -> p [] [ text error ]) errors
                ++ [ button [ onClick dismissErrors ] [ text "Ok" ] ]



