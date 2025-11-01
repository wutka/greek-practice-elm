module About exposing (..)

import Html exposing (div, p, text, a)
import Html.Attributes exposing (href)

aboutPageView =
  div [] [
    p [] [text """
     This program allows you to practice parsing Greek New Testament verbs in-context, that
     is, within a verse, rather than just giving you a verb without the verse in which
     it occurs.
    """]
  , p [] [text """
     The Greek NT text used here is the SBL GNT from the Society for Biblical Literature,
     and the parsing data is from the MorphGNT project's parsing of the SBL GNT
     (available here """
     , a [href "https://github.com/morphgnt/sblgnt"] [text "https://github.com/morphgnt/sblgnt"]
     , text "). The Greek font used here is the "
     , a [href "https://www.sbl-site.org/educational/BiblicalFonts_SBLGreek.aspx"] [text "SBL Greek font"]
     , text "."]
  , p [] [text """
      On the settings page, you can select the various options you want to be quizzed on,
      with the default being everything. Use the Check button to see if your guesses
      for the parsing are correct, and the Next button to choose another random
      word.
      """]
  , p [] [ text "The source code for this Elm web app is available at "
           , a [href "https://github.com/wutka/greek-practice-elm"] [text "https://github.com/wutka/greek-practice-elm"]
           , text ". You can e-mail the author at "
           , a [href "mailto:mark@wutka.com"] [text "mark@wutka.com"]
           , text "."
           ]
  ]