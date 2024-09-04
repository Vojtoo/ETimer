port module Main exposing (..)

import Browser
import Html exposing (Html, button, div, i, input, span, text)
import Html.Attributes exposing (class, id, min, type_, value)
import Html.Events exposing (onClick, onInput)
import Time exposing (every)



-- CONSTANTS


minimumTime =
    1



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
            ( intString
                |> String.toInt
                |> changeBaseTime model
            , Cmd.none
            )


changeBaseTime : Model -> Maybe Int -> Model
changeBaseTime model mInt =
    case mInt of
        Nothing ->
            model

        Just a ->
            if a >= minimumTime then
                if model.running == False then
                    { model | baseTime = a, currentTime = ( a, 0 ) }

                else
                    { model | baseTime = a }

            else
                model



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
        ((model.currentTime
            |> Tuple.first
            |> String.fromInt
            |> padTime
         )
            ++ ":"
            ++ (model.currentTime
                    |> Tuple.second
                    |> String.fromInt
                    |> padTime
               )
        )


view : Model -> Html Msg
view model =
    div [ class "container-fluid vh-100 d-flex align-items-center justify-content-center" ]
        [ div [ class "container-fluid p-0" ]
            [ div [ class "row", id "timer-window" ]
                [ div [ class "col-12 text-center" ]
                    [ span [ id "time-span" ] [ viewTime model ]
                    , div [ class "d-flex align-items-center justify-content-center" ]
                        [ button [ class "btn timer-button", onClick ToggleTimer ] [ toggleStartButton model ]
                        , button [ class "btn timer-button", onClick ResetTimer ] [ text "Reset" ]
                        , i [ class "gear-icon bi bi-gear-fill" ] []
                        , input
                            [ type_ "number"
                            , Html.Attributes.min "1"
                            , value
                                (model.baseTime
                                    |> String.fromInt
                                )
                            , onInput ChangeBaseTime
                            ]
                            []
                        ]
                    ]
                ]
            ]
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
