module Quiz exposing (..)

import Array
import Dict
import Set
import Random

import Html exposing (Html, div, input, label, button, text)
import Html.Attributes exposing (class, type_, checked, style, disabled)
import Html.Events exposing (onCheck, onClick)

import MorphGNT exposing (GNTWordType, ParsingType)
import SBLGNT exposing (allVerbs, gntText)
import Model exposing (Model, ParseSettingsType, ActionType, books,
    settingTable, settingCategories)

defaultDisabled : Set.Set String
defaultDisabled = Set.fromList ["Gender", "Case"]

disabledCategories : Dict.Dict String (Set.Set String)
disabledCategories = Dict.fromList [
    ("Participle", Set.fromList ["Person"])
  , ("Infinitive", Set.fromList ["Person", "Number", "Gender", "Case"])
  ]

pickNewVerb : Model.Model -> (Model.Model, Cmd ActionType)
pickNewVerb model =
      if Array.length model.allowableVerbs == 0 then
        ({model | currentVerb=Nothing}, Cmd.none)
      else
        ({model | currentParsing = Dict.empty, checkedParsing = Nothing},
         Random.generate Model.NewVerb (Random.int 0 ((Array.length model.allowableVerbs) - 1)))

startQuiz : Model.Model -> (Model, Cmd ActionType)
startQuiz model =
  let allowableVerbs = Array.fromList (List.filter (filterVerbs model.parseSettings) allVerbs) in
  if (Array.length allowableVerbs) == 0 then
    ({model | errorMessage = Just "No verses match the parsing criteria"}, Cmd.none)
  else
    pickNewVerb {model | errorMessage = Nothing, currentPage=Model.QuizPage,
      allowableVerbs = allowableVerbs}

compareParsing : ParseSettingsType -> (String, String) -> Bool
compareParsing parseSettings (category, verbValue) =
  case Dict.get category parseSettings of
    Nothing -> False  -- Verb has a category that isn't in the parse settings
    Just allowableValues -> Set.member verbValue allowableValues

filterVerbs : ParseSettingsType -> GNTWordType -> Bool
filterVerbs parseSettings verb =
  List.all (compareParsing parseSettings) (Dict.toList verb.verbParsing)

getWithDefault : Int -> Array.Array a -> a -> a
getWithDefault index arr default =
  case Array.get index arr of
      Nothing -> default
      Just item -> item

getVerse : GNTWordType -> Array.Array GNTWordType
getVerse word =
      case ((Array.get (word.book-1) gntText) |>
            Maybe.andThen (Array.get (word.chapter-1)) |>
            Maybe.andThen (Array.get (word.verse-1))) of
            Nothing -> Array.empty
            Just arr -> arr
                

bookChapterVerse : Model.Model -> String
bookChapterVerse model =
  case model.currentVerb of
      Nothing -> "No verses match the parsing criteria"
      Just verb -> (getWithDefault (verb.book-1) books "Unknown") ++ " " ++ 
        (String.fromInt verb.chapter) ++ ":" ++ (String.fromInt verb.verse)

displayVerse : Model.Model -> List (Html ActionType)
displayVerse model =
  case model.currentVerb of
    Nothing -> [ text "" ]
    Just word ->
      List.map (\(i,w) ->
      if i == word.versePos then
        div [class "HighlightedVerseWord"] [text w.text]
      else
        div [class "VerseWord"] [text w.text]) (Array.toIndexedList (getVerse word))

parsingCheckChange : String -> String -> Bool -> ActionType
parsingCheckChange category settingName _ =
  Model.ChangeParsing category settingName

isParsingChecked : ParsingType -> String -> String -> Bool
isParsingChecked currentParsing category settingName =
  case Dict.get category currentParsing of
    Nothing -> False
    Just value -> value == settingName

isParsingDisabled : ParsingType -> String -> Bool
isParsingDisabled currentParsing category =
  case Dict.get "Mood" currentParsing of
    Nothing -> False
    Just mood -> Set.member category
        (case Dict.get mood disabledCategories of
          Nothing -> defaultDisabled
          Just disabled -> disabled)
      
getCheckedLabel : ParsingType -> Maybe GNTWordType -> String -> String -> Html ActionType
getCheckedLabel parsing mverb category settingName =
  case mverb of
    Nothing -> label [class "form-check-label"] [text settingName]
    Just verb ->
      if Dict.get category parsing == Just settingName then
        label [class "form-check-label", style "color" "#008000"] [text settingName]
      else if Dict.get category verb.verbParsing == Just settingName then
        label [class "form-check-label", style "color" "#ff2020"] [text settingName]
      else
        label [class "form-check-label"] [text settingName]

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
        input [class "form-check-input", type_ "checkbox"
              , checked (isParsingChecked model.currentParsing category settingName)
              , disabled (isParsingDisabled model.currentParsing category)] []
        , getCheckedLabel parsing model.currentVerb category settingName
      ]

parsingGroup : Model -> String -> Html ActionType
parsingGroup model category =
  case Dict.get category settingTable of
      Nothing -> div [] []
      Just settingNames ->
        case model.currentVerb of
          Nothing -> div [] []
          Just _ ->
            div [class "form-check"] 
              (div [class "ParsingLabel"] [text category]
                :: (List.map (parsingOption model category) settingNames))

displayLemma : Model.Model -> Html ActionType
displayLemma model =
  case model.checkedParsing of
    Nothing -> text ""
    Just _ -> (case model.currentVerb of
                Nothing -> text ""
                Just word -> text ("Verb: " ++ word.lemma))

quizPageView : Model.Model -> Html ActionType
quizPageView model =
  div [class "QuizBase"] [
      div [class "BookChapterVerse"] [text (bookChapterVerse model)]
    , div [class "VerseBase"] [div [class "VerseDiv"] (displayVerse model)]
    , div [class "ParsingGridBase"] (List.map (parsingGroup model) settingCategories)
    , div [class "LemmaDiv"] [div [class "LemmaWord"] [displayLemma model]]
    , div [class "QuizControls"] [
        button [class "btn btn-primary", onClick Model.DoCheck] [text "Check"]
      , button [class "btn btn-primary", onClick Model.PickNewVerb] [text "Next"]
      ]
    ]