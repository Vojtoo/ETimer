module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Time exposing (every)



---- MODEL ----


type alias Model =
    { currentTime : ( Int, Int )
    , baseTime : ( Int, Int )
    , running : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { currentTime = ( 2, 0 )
      , baseTime = ( 2, 0 )
      , running = True
      }
    , Cmd.none
    )


incrementTimeBySecond : ( Int, Int ) -> ( Int, Int )
incrementTimeBySecond ( a, b ) =
    if b + 1 == 60 then
        if a + 1 == 60 then
            ( 0, 0 )

        else
            ( a + 1, 0 )

    else
        ( a, b + 1 )


decrementTimeBySecond : ( Int, Int ) -> ( Int, Int )
decrementTimeBySecond ( a, b ) =
    if b == 0 then
        if a == 0 then
            ( 0, 0 )

        else
            ( a - 1, 59 )

    else
        ( a, b - 1 )



---- UPDATE ----


type Msg
    = Tick
    | ToggleTimer
    | ResetTimer


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.running == True then
        every 1000 (\_ -> Tick)

    else
        Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            ( { model | currentTime = decrementTimeBySecond model.currentTime }, Cmd.none )

        ToggleTimer ->
            ( { model | running = not model.running }, Cmd.none )

        ResetTimer ->
            ( { model | currentTime = model.baseTime, running = False }, Cmd.none )



---- VIEW ----


toggleStartButton : Model -> Html Msg
toggleStartButton model =
    if model.running == True then
        text "Stop"

    else
        text "Start"


paddTime : String -> String
paddTime string =
    if String.length string == 1 then
        "0" ++ string

    else
        string


viewTime : Model -> Html Msg
viewTime model =
    text
        ((model.currentTime |> Tuple.first |> String.fromInt |> paddTime) ++ ":" ++ (model.currentTime |> Tuple.second |> String.fromInt |> paddTime))


view : Model -> Html Msg
view model =
    div []
        [ viewTime model
        , button [ onClick ToggleTimer ] [ toggleStartButton model ]
        , button [ onClick ResetTimer ] [ text "Reset" ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
