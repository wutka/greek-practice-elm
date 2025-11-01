module Main exposing (..)

import Array 
import Browser
import Dict
import Set
import Html exposing (Html, div, text, button, label, input, Attribute)
import Html.Attributes exposing (class, type_, checked, disabled, style)
import Html.Events exposing (onCheck, onClick)
import Random
import Debug

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
      , currentParsing : ParsingType
      , checkedParsing : Maybe (ParsingType)
      , currentPage : PageType
      }

type ActionType = AddSetting String String | RemoveSetting String String | RunQuiz |
  GotoSettings | PickNewVerb | NewVerb Int | ChangeParsing String String Bool

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

books : Array.Array String
books = Array.fromList
  [
    "Matthew", "Mark", "Luke", "John", "Acts", "Romans", "1 Corinthians", "2 Corinthians"
  , "Galatians", "Ephesians", "Philippians", "Colossians", "1 Thessalonians"
  , "2 Thessalonians", "1 Timothy", "2 Timothy", "Titus", "Philemon", "Hebrews", "James"
  , "1 Peter", "2 Peter", "1 John", "2 John", "3 John", "Jude", "Revelation"
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
      , checkedParsing = Nothing
      , currentPage = SettingsPage
      }

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
    ChangeParsing category value checked ->
        ({model | currentParsing = Dict.insert category value model.currentParsing}, Cmd.none)
    GotoSettings -> ({ model | currentPage = SettingsPage }, Cmd.none)
    RunQuiz -> startQuiz { model | currentPage = QuizPage}
    PickNewVerb -> pickNewVerb model
    NewVerb verbNum -> ({model | currentVerb = Array.get verbNum model.allowableVerbs}, Cmd.none)

pickNewVerb model =
      if Array.length model.allowableVerbs == 0 then
        ({model | currentVerb=Nothing}, Cmd.none)
      else
        ({model | currentParsing = Dict.empty, checkedParsing = Nothing},
         Random.generate NewVerb (Random.int 0 ((Array.length model.allowableVerbs) - 1)))

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
          QuizPage -> quizPageView model
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

settingCheckbox : ParseSettingsType -> String -> String -> Html ActionType
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

filterVerbs : ParseSettingsType -> GNTWordType -> Bool
filterVerbs parseSettings verb =
  List.all (compareParsing parseSettings) (Dict.toList verb.verbParsing)

startQuiz model =
  let allowableVerbs = Array.fromList (List.filter (filterVerbs model.parseSettings) allVerbs) in
  pickNewVerb {model | allowableVerbs = allowableVerbs}

getWithDefault index arr default =
  case Array.get index arr of
      Nothing -> default
      Just item -> item

getVerse word =
      case ((Array.get (word.book-1) gntText) |>
            Maybe.andThen (Array.get (word.chapter-1)) |>
            Maybe.andThen (Array.get (word.verse-1))) of
            Nothing -> Array.empty
            Just arr -> arr
                

bookChapterVerse model =
  case model.currentVerb of
      Nothing -> "No verses match the parsing criteria"
      Just verb -> (getWithDefault (verb.book-1) books "Unknown") ++ " " ++ 
        (String.fromInt verb.chapter) ++ ":" ++ (String.fromInt verb.verse)

displayVerse model =
  case model.currentVerb of
    Nothing -> [ text "" ]
    Just word ->
      List.map (\(i,w) ->
      if i == word.versePos then
        div [class "HighlightedVerseWord"] [text w.text]
      else
        div [class "VerseWord"] [text w.text]) (Array.toIndexedList (getVerse word))

parsingCheckChange category settingName checked =
  ChangeParsing category settingName checked

isParsingChecked : ParsingType -> String -> String -> Bool
isParsingChecked currentParsing category settingName =
  case Dict.get category currentParsing of
    Nothing -> False
    Just value -> value == settingName

isParsingDisabled : ParsingType -> String -> Bool
isParsingDisabled currentParsing category = False

getCheckedColor : ParsingType -> Maybe GNTWordType -> String -> Attribute msg
getCheckedColor parsing mverb category =
  case mverb of
    Nothing -> style "color" "#000080"
    Just verb ->
      if Dict.get category parsing == Dict.get category verb.verbParsing then
        style "color" "#008000"
      else
        style "color" "#ff2020"

parsingOption : Model -> String -> String -> Html ActionType
parsingOption model category settingName =
  case model.checkedParsing of
    Nothing ->
      div [] [
        input [class "form-check-input", type_ "checkbox", onCheck (parsingCheckChange category settingName)
              , checked (isParsingChecked model.currentParsing category settingName)
              , disabled (isParsingDisabled model.currentParsing category)] []
      , label [class "form-check-label"] [text settingName]
      ]
    Just parsing ->
      div [] [
        input [class "form-check-input", type_ "checkbox", (getCheckedColor parsing model.currentVerb category)
              , checked (isParsingChecked model.currentParsing category settingName)
              , disabled (isParsingDisabled model.currentParsing category)] []
        , label [class "form-check-label"] [text settingName]
      ]

parsingGroup : Model -> String -> Html ActionType
parsingGroup model category =
  case Dict.get category settingTable of
      Nothing -> div [] []
      Just settingNames ->
        case model.currentVerb of
          Nothing -> div [] []
          Just word ->
            div [class "form-check"] 
              (div [class "ParsingLabel"] [text category]
                :: (List.map (parsingOption model category) settingNames))

quizPageView model =
  div [class "QuizBase"] [
      div [class "BookChapterVerse"] [text (bookChapterVerse model)]
    , div [class "VerseBase"] [div [class "VerseDiv"] (displayVerse model)]
    , div [class "ParsingGridBase"] (List.map (parsingGroup model) (Dict.keys settingTable))
    ]