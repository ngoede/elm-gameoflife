module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, div, span, text)
import Html.Events exposing (onClick)
import Random exposing (Generator)
import Time
import Array exposing (Array)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL
boardSize: Int
boardSize = 50

type alias Model =
    Board


type alias Board =
    Array (List Cell)


type Cell
    = On
    | Off


initialBoardGenerator: Generator (Array (List Cell))
initialBoardGenerator =
    randomArray boardSize (Random.list boardSize (Random.uniform On [ Off ]))

randomArray: Int -> Generator a -> Generator (Array a)
randomArray size generator =
    Random.map Array.fromList (Random.list size generator)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Array.fromList []
    , Random.generate NewBoard initialBoardGenerator
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | NewBoard Model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( nextBoard model, Cmd.none )

        NewBoard board ->
            ( board, Cmd.none )



-- GAME


nextBoard : Board -> Board
nextBoard =
    Array.map (\r -> List.map flipCell r)


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
    div [] (Array.toList (Array.map renderRow model))


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
