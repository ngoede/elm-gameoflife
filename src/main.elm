module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, div, span, text)
import Html.Events exposing (onClick)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    List (List Cell)


type Cell
    = On
    | Off


init : Model
init =
    [ [ On, Off, On ]
    , [ Off, On, On ]
    , [ Off, Off, Off ]
    ]



-- UPDATE


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    model



-- VIEW


view : Model -> Html Msg
view model =
    div [] (List.map renderRow model)


renderRow : List Cell -> Html Msg
renderRow row =
    div [] (List.map renderCell row)


renderCell : Cell -> Html Msg
renderCell cell =
    case cell of
        On ->
            span [] [ text "■" ]

        Off ->
            span [] [ text "□" ]
