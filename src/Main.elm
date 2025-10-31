module Main exposing (..)

import Array 
import Browser
import Dict
import Set
import Html exposing (Html, div, text, button, label, input)
import Html.Attributes exposing (class, type_, checked)
import Html.Events exposing (onCheck, onClick)
import Random

import MorphGNT exposing (..)
import SBLGNT exposing (..)

type alias ParseSettingsType = Dict.Dict String (Set.Set String)

type alias ParsedVerb = {
        gntWord : GNTWordType
      , checkedType : ParsingType
      }

type PageType = SettingsPage | AboutPage | QuizPage

type alias Model = {
        allowableVerbs : Array.Array GNTWordType
      , parseSettings : ParseSettingsType
      , verbHistory : List ParsedVerb
      , historyPos : Int
      , errorMessage : Maybe String
      , currentVerb : Maybe GNTWordType
      , currentParsing : Dict.Dict String String
      , currentPage : PageType
      }

type ActionType = AddSetting String String | RemoveSetting String String | RunQuiz |
  GotoSettings | PickNewVerb | NewVerb Int

settingTable : Dict.Dict String (List String)
settingTable = Dict.fromList [
    ("Person", [ "First", "Second", "Third"])
  , ("Tense", [ "Present", "Imperfect", "Future", "Aorist", "Perfect", "Pluperfect"])
  , ("Voice", [ "Active", "Middle", "Passive"])
  , ("Mood", [ "Indicative", "Imperative", "Subjunctive" , "Optative", "Infinitive", "Participle"])
  , ("Case", [ "Nominative", "Genitive", "Dative", "Accusative"])
  , ("Number", [ "Singular", "Plural"])
  , ("Gender", [ "Masculine", "Feminine", "Neuter"])
  ]

initialParseSettings : ParseSettingsType
initialParseSettings = Dict.map (\k a -> Set.fromList a) settingTable

initialModel : Model
initialModel = {
        allowableVerbs = Array.empty
      , parseSettings = initialParseSettings
      , verbHistory = []
      , historyPos = -1
      , errorMessage = Nothing
      , currentVerb = Nothing
      , currentParsing = Dict.empty
      , currentPage = SettingsPage
      }

main =
    Browser.element { init = initialModel, update = update, subscriptions = subscriptions, view = view }

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
    GotoSettings -> ({ model | currentPage = SettingsPage }, Cmd.none)
    RunQuiz -> startQuiz { model | currentPage = QuizPage}
    PickNewVerb ->
      if Array.length model.allowableVerbs == 0 then
        ({model | currentVerb=Nothing}, Cmd.none)
      else
        (model, Random.generate NewVerb (Random.int 0 ((Array.length model.allowableVerbs) - 1)))
    NewVerb verbNum -> ({model | currentVerb = Array.get verbNum model.allowableVerbs}, cmd.none)


header = div [class "HeaderBase"]
    [
        div [class "Controls"]
          [
              button [class "btn btn-primary", type_ "submit", onClick GotoSettings] [text "Settings"]
            , button [class "btn btn-primary", type_ "submit", onClick RunQuiz] [text "Quiz"]
            , button [class "btn btn-primary", type_ "submit"] [text "About"]
          ]
      , div [class "Logo"] [text "NT Greek Verb Practice"]
    ]

view model =
    div [class "AppBase"] [
        header
      , div [class "Pad1"] []
      , case model.currentPage of
          SettingsPage -> settingsPageView model
          AboutPage -> div [] []
          QuizPage -> div [] []
      , div [class "Pad2"] []
    ]


settingCheckChange category settingName checked =
  if checked then
    AddSetting category settingName
  else
    RemoveSetting category settingName

isSettingChecked settings category settingName =
  case Dict.get category settings of
      Nothing -> False
      Just settingSet -> Set.member settingName settingSet

settingCheckbox settings category settingName =
  div [] [
    input [class "form-check-input", type_ "checkbox", onCheck (settingCheckChange category settingName)
         , checked (isSettingChecked settings category settingName) ] []
  , label [class "form-check-label"] [text settingName]
  ]
settingGroup settings category =
  case Dict.get category settingTable of
      Nothing -> div [] []
      Just settingNames ->
        div [class "form-check"] 
          (div [class "SettingsLabel"] [text category]
            :: (List.map (settingCheckbox settings category) settingNames))

settingsPageView model =
  div [class "SettingsBase"] [
    div [class "SettingsGridBase"] (List.map (settingGroup model.parseSettings) (Dict.keys settingTable))
  , case model.errorMessage of
    Nothing -> text ""
    Just message -> div [class "ErrorMessage"] [text message]
  , div [class "ControlGrid"] [
      button [class "btn btn-primary", onClick RunQuiz] [text "Run Quiz"]
  ]
  ]

compareParsing parseSettings (category, verbValue) =
  case Dict.get category parseSettings of
    Nothing -> False  -- Verb has a category that isn't in the parse settings
    Just allowableValues -> Set.member verbValue allowableValues

filterVerbs parseSettings verb =
  List.all (compareParsing parseSettings) (Dict.values verb)

startQuiz model =
  let allowableVerbs = Array.fromList (List.filter (filterVerbs model.parseSettings) allVerbs) in
  ({model | allowableVerbs = allowableVerbs}, Cmd PickNewVerb)