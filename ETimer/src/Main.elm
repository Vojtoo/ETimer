port module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (min, type_, value)
import Html.Events exposing (onClick, onInput)
import Time exposing (every)



-- PORTS


port sendRing : () -> Cmd msg



---- MODEL ----


type alias Model =
    { currentTime : ( Int, Int )
    , baseTime : Int
    , running : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { currentTime = ( 1, 0 )
      , baseTime = 1
      , running = False
      }
    , Cmd.none
    )


decrementTimeBySecond : ( Int, Int ) -> ( Int, Int )
decrementTimeBySecond ( a, b ) =
    if b == 0 then
        if a == 0 then
            ( 0, 0 )

        else
            ( a - 1, 59 )

    else
        ( a, b - 1 )


tick : Model -> ( Model, Cmd Msg )
tick model =
    let
        newTime =
            decrementTimeBySecond model.currentTime
    in
    if newTime == ( 0, 0 ) then
        ( { model | currentTime = newTime, running = not model.running }, sendRing () )

    else
        ( { model | currentTime = newTime }, Cmd.none )



---- UPDATE ----


type Msg
    = Tick
    | ToggleTimer
    | ResetTimer
    | ChangeBaseTime String


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
            tick model

        ToggleTimer ->
            ( { model | running = not model.running }, Cmd.none )

        ResetTimer ->
            ( { model | currentTime = ( model.baseTime, 0 ), running = False }, Cmd.none )

        ChangeBaseTime intString ->
            ( { model
                | baseTime =
                    String.toInt intString
                        |> changeBaseTime model.baseTime
              }
            , Cmd.none
            )


changeBaseTime : Int -> Maybe Int -> Int
changeBaseTime oldBaseTime mInt =
    case mInt of
        Nothing ->
            oldBaseTime

        Just a ->
            a



---- VIEW ----


toggleStartButton : Model -> Html Msg
toggleStartButton model =
    if model.running == True then
        text "Stop"

    else
        text "Start"


padTime : String -> String
padTime string =
    if String.length string == 1 then
        "0" ++ string

    else
        string


viewTime : Model -> Html Msg
viewTime model =
    text
        ((model.currentTime |> Tuple.first |> String.fromInt |> padTime) ++ ":" ++ (model.currentTime |> Tuple.second |> String.fromInt |> padTime))


view : Model -> Html Msg
view model =
    div []
        [ viewTime model
        , button [ onClick ToggleTimer ] [ toggleStartButton model ]
        , button [ onClick ResetTimer ] [ text "Reset" ]
        , input
            [ type_ "number"
            , Html.Attributes.min "0"
            , value
                (model.baseTime
                    |> String.fromInt
                )
            , onInput ChangeBaseTime
            ]
            []
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
