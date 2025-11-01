module Main exposing (..)

import Array 
import Browser
import Dict
import Set
import Html exposing (div, text, button)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick)

import MorphGNT exposing (..)
import SBLGNT exposing (..)
import Model exposing (..)
import Settings exposing (settingsPageView)
import Quiz exposing (quizPageView, pickNewVerb, startQuiz)
import About exposing (aboutPageView)

main =
    Browser.element { init = init,
                      update = update, subscriptions = subscriptions, view = view }

init () = (initialModel, Cmd.none)

subscriptions _ = Sub.none

update msg model = 
  case msg of
    AddSetting category value ->
      case Dict.get category model.parseSettings of
        Nothing -> (model, Cmd.none)
        Just settings -> ({ model | parseSettings = Dict.insert category (Set.insert value settings) model.parseSettings}, Cmd.none)
    RemoveSetting category value ->
      case Dict.get category model.parseSettings of
        Nothing -> (model, Cmd.none)
        Just settings -> ({ model | parseSettings = Dict.insert category (Set.remove value settings) model.parseSettings}, Cmd.none)
    ChangeParsing category value ->
        ({model | currentParsing = Dict.insert category value model.currentParsing}, Cmd.none)
    GotoSettings -> ({ model | currentPage = SettingsPage }, Cmd.none)
    GotoAbout -> ({ model | currentPage = AboutPage }, Cmd.none)
    RunQuiz -> startQuiz { model | currentPage = QuizPage}
    PickNewVerb -> pickNewVerb model
    NewVerb verbNum -> ({model | currentVerb = Array.get verbNum model.allowableVerbs}, Cmd.none)
    DoCheck -> ({model | checkedParsing = Just model.currentParsing}, Cmd.none)

header = div [class "HeaderBase"]
    [
        div [class "Controls"]
          [
              button [class "btn btn-primary", type_ "submit", onClick GotoSettings] [text "Settings"]
            , button [class "btn btn-primary", type_ "submit", onClick RunQuiz] [text "Quiz"]
            , button [class "btn btn-primary", type_ "submit", onClick GotoAbout] [text "About"]
          ]
      , div [class "Logo"] [text "NT Greek Verb Practice"]
    ]

view model =
    div [class "AppBase"] [
        header
      , div [class "Pad1"] []
      , case model.currentPage of
          SettingsPage -> settingsPageView model
          AboutPage -> aboutPageView
          QuizPage -> quizPageView model
      , div [class "Pad2"] []
    ]
