module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, div, span, text)
import Html.Events exposing (onClick)
import Random exposing (Generator)
import Time
import Array exposing (Array)
import Maybe


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL
boardSize: Int
boardSize = 200

type alias Model =
    Board


type alias Board =
    Array (Array Cell)


type Cell
    = On
    | Off


initialBoardGenerator: Generator (Array (Array Cell))
initialBoardGenerator =
    randomArray boardSize (randomArray boardSize (Random.uniform On [ Off ]))

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
nextBoard board =
    Array.indexedMap (processRow board) board


processRow : Board -> Int -> Array Cell -> Array Cell
processRow board y =
    Array.indexedMap (processCell board y)


processCell: Board -> Int -> Int -> Cell -> Cell
processCell board y x cell =
    case cell of
        On ->
            case (countNeighbours board y x) of
                2 -> On
                3 -> On
                _ -> Off
        Off ->
            case (countNeighbours board y x) of
                3 -> On
                _ -> Off

countNeighbours: Board -> Int -> Int -> Int
countNeighbours board row column =
    getNeighbours board row column
        |> List.filter (\c -> c == On)
        |> List.length

getNeighbours: Board -> Int -> Int -> List Cell
getNeighbours board row column =
    [ getCell board (row - 1) (column - 1)
    , getCell board (row - 1) column
    , getCell board (row - 1) (column + 1)
    , getCell board row (column - 1)
    , getCell board row (column + 1)
    , getCell board (row + 1) (column - 1)
    , getCell board (row + 1) column
    , getCell board (row + 1) (column + 1)
    ]

getCell: Board -> Int -> Int -> Cell
getCell board row column =
    Array.get row board
        |> Maybe.withDefault Array.empty
        |> Array.get column
        |> Maybe.withDefault Off

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 250 Tick



-- VIEW


view : Model -> Html Msg
view model =
    div [] (Array.toList (Array.map renderRow model))


renderRow : Array Cell -> Html Msg
renderRow row =
    div [] (Array.toList (Array.map renderCell row))


renderCell : Cell -> Html Msg
renderCell cell =
    case cell of
        On ->
            span [] [ text "■" ]

        Off ->
            span [] [ text "□" ]
