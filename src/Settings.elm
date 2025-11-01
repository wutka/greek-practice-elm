module Settings exposing (..)

import Dict
import Set

import Browser
import Html exposing (Html, div, input, label, button, text, Attribute)
import Html.Attributes exposing (class, type_, checked)
import Html.Events exposing (onCheck, onClick)

import Model exposing (ActionType, ParseSettingsType, settingTable, settingCategories)

settingCheckChange category settingName checked =
  if checked then
    Model.AddSetting category settingName
  else
    Model.RemoveSetting category settingName

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
    div [class "SettingsGridBase"] (List.map (settingGroup model.parseSettings) settingCategories)
  , case model.errorMessage of
    Nothing -> text ""
    Just message -> div [class "ErrorMessage"] [text message]
  , div [class "ControlGrid"] [
      button [class "btn btn-primary", onClick Model.RunQuiz] [text "Run Quiz"]
  ]
  ]