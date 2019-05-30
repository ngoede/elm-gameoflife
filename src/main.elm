module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, div, span, text)
import Html.Events exposing (onClick)
import Time


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    Board


type alias Board =
    List (List Cell)


type Cell
    = On
    | Off


init : () -> ( Model, Cmd Msg )
init _ =
    ( [ [ On, Off, On ]
      , [ Off, On, On ]
      , [ Off, Off, Off ]
      ]
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( nextBoard model, Cmd.none )


-- GAME


nextBoard : Board -> Board
nextBoard =
    List.map (\r -> List.map flipCell r)


flipCell : Cell -> Cell
flipCell cell =
    case cell of
        On ->
            Off

        Off ->
            On


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



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