module MorphGNT exposing (..)

import Dict
import List

type PartOfSpeechType = Adjective | Conjunction | Adverb |
        Interjection | Noun | Preposition | DefiniteArticle |
        DemonstrativePronoun | IndefinitPronoun |
        PersonalPronoun | RelativePronoun | Verb | Particle

type alias ParsingType = Dict.Dict String String
type alias GNTWordType = {
        book : Int
      , chapter : Int
      , verse : Int
      , versePos : Int
      , partOfSpeech : PartOfSpeechType 
      , verbParsing : ParsingType
      , text : String
      , word : String
      , normalizedWord : String
      , lemma : String
        }

parsingDecoder : Dict.Dict String (Dict.Dict String String)
parsingDecoder = Dict.fromList [
        ("Person", Dict.fromList [("1", "First"), ("2", "Second"), ("3", "Third")])
      , ("Tense", Dict.fromList [("P", "Present"), ("I", "Imperfect")
                              , ("F", "Future"), ("A", "Aorist")
                              , ("X", "Perfect"), ("Y", "Pluperfect")])  
      , ("Voice", Dict.fromList [("A", "Active"), ("M", "Middle"), ("P", "Passive")])  
      , ("Mood", Dict.fromList [("I", "Indicative"), ("D", "Imperative")
                             , ("S", "Subjunctive"), ("O", "Optative")
                             , ("N", "Infinitive"), ("P", "Participle")])
      , ("Case", Dict.fromList [("N", "Nominative"), ("G", "Genitive")
                             , ("D", "Dative"), ("A", "Accusative")])
      , ("Number", Dict.fromList [("S", "Singular"), ("P", "Plural")])
      , ("Gender", Dict.fromList [("M", "Masculine"), ("F", "Feminine"), ("N", "Neuter")])
      ] 

parsePartOfSpeech : String -> PartOfSpeechType
parsePartOfSpeech str =
        if str == "A-" then Adjective
        else if str == "C-" then Conjunction
        else if str == "D-" then Adverb
        else if str == "I-" then Interjection
        else if str == "N-" then Noun
        else if str == "P-" then Preposition
        else if str == "RA" then DefiniteArticle
        else if str == "RD" then DemonstrativePronoun
        else if str == "RI" then IndefinitPronoun
        else if str == "RP" then PersonalPronoun
        else if str == "RR" then RelativePronoun
        else if str == "V-" then Verb
        else if str == "X-" then Particle
        else Particle

verbParsingOffsets : List (String, Int)
verbParsingOffsets = [
        ("Person", 0), ("Tense", 1), ("Voice", 2), ("Mood", 3)
      , ("Case", 4), ("Number", 5), ("Gender", 6) ]

parseParsing : String -> (String, Int) -> ParsingType -> ParsingType
parseParsing verbParsing (category, offset) parsingDict =
        let parsing = String.slice offset (offset+1) verbParsing in
        if (parsing == "-") || (parsing == "") then
                parsingDict
        else
                case Dict.get category parsingDecoder of
                        Nothing -> parsingDict
                        Just decoderDict ->
                                case Dict.get parsing decoderDict of
                                    Nothing -> parsingDict
                                    Just decoded -> Dict.insert category decoded parsingDict

    
parseGNTWord : Int -> Int -> Int -> String -> String -> String -> String -> String -> String -> GNTWordType
parseGNTWord book chapter verse partOfSpeech verbParsing text word normalizedWord lemma =
        { book=book, chapter=chapter, verse=verse, versePos=-1,
          partOfSpeech = parsePartOfSpeech partOfSpeech,
          verbParsing = List.foldl (parseParsing verbParsing) Dict.empty verbParsingOffsets,
          text=text, word=word, normalizedWord=normalizedWord,
          lemma=lemma}
       

