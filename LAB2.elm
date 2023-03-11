module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Random

main = 
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

type alias Model =
    { n : Int
    , k : Int
    , currentPlayer : Int
    , player1Choices : List Int
    , player2Choices : List Int
    , winner : Maybe Int
    }


init : () -> (Model, Cmd Msg)
init _ =
    ( { currentPlayer = 1
      , n = 0
      , k = 2
      , player1Choices = []
      , player2Choices = []
      , winner = Nothing
      }
    , Random.generate Target (Random.int 1000 1000000)
    )


type Msg = 
    ChooseNumber Int
    | Target Int
    | Restart


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Target i ->
            ( { model | n = i }, Cmd.none )
        ChooseNumber num ->
            let
                newK = model.k * num
                currentPlayer = model.currentPlayer
                newPlayer1Choices =
                    if currentPlayer == 1 then
                        model.player1Choices ++ [num]
                    else
                        model.player1Choices
                newPlayer2Choices =
                    if currentPlayer == 2 then
                        model.player2Choices ++ [num]
                    else
                        model.player2Choices
                newWinner =
                    if newK >= model.n then
                        Just currentPlayer
                    else
                        Nothing
            in
                ( { model | currentPlayer =
                        if currentPlayer == 1 then
                            2
                        else
                            1
                  , k = newK
                  , player1Choices = newPlayer1Choices
                  , player2Choices = newPlayer2Choices
                  , winner = newWinner
                  }
                , Cmd.none
                )
        Restart ->
            init ()



view model =
    let
        winnerText =
            case model.winner of
                Just winner ->
                    "Player " ++ String.fromInt winner ++ " wins!"
                Nothing ->
                    "Current Player: " ++ String.fromInt model.currentPlayer
    in
    div []
        [ pre [] [ text winnerText ]
        , span [ id "n" ] [ text <| "n = " ++ String.fromInt model.n ]
        , span [ id "k" ] [ text <| "k = " ++ String.fromInt model.k ]
        , text "\n"
        , span [ id "choice-2" ] [ button [ onClick (ChooseNumber 2) ] [ text "2" ] ]
        , span [ id "choice-3" ] [ button [ onClick (ChooseNumber 3) ] [ text "3" ] ]
        , span [ id "choice-4" ] [ button [ onClick (ChooseNumber 4) ] [ text "4" ] ]
        , span [ id "choice-5" ] [ button [ onClick (ChooseNumber 5) ] [ text "5" ] ]
        , span [ id "choice-6" ] [ button [ onClick (ChooseNumber 6) ] [ text "6" ] ]
        , span [ id "choice-7"  ] [ button [ onClick (ChooseNumber 7) ] [ text "7" ] ]
        , span [ id "choice-8" ] [ button [ onClick (ChooseNumber 8) ] [ text "8" ] ]
        , span [ id "choice-9"] [ button [ onClick (ChooseNumber 9) ] [ text "9" ] ]
        , text "\n"
        , span [ id "moves-player1" ] [ text <| "Player 1 Moves:  " ++ String.join ", " (List.map String.fromInt model.player1Choices) ]
        , span [ id "moves-player2" ] [ text <| "\nPlayer 2 Moves:  " ++ String.join ", " (List.map String.fromInt model.player2Choices) ]
        , text "\n"
        , button [ onClick Restart ] [ text "Restart" ]]