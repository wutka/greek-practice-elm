module Model exposing (..)

import Array
import Dict
import Set
import MorphGNT exposing (GNTWordType, ParsingType)
import SBLGNT exposing (allVerbs)

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
  GotoSettings | PickNewVerb | NewVerb Int | ChangeParsing String String Bool | DoCheck |
  GotoAbout

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

settingCategories = [ "Voice", "Tense", "Mood", "Person", "Number", "Case", "Gender" ]

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

