module Verb where

import Hiragana

import Data.Maybe

type KanaStem = String
type KanjiStem = String

-- All verbs have a kana stem, but not all verbs can be written with kanji.
data Stem = Stem KanaStem (Maybe KanjiStem)

instance Show Stem where
    show (Stem kana kanji) = fromMaybe kana kanji

data IrregularType = Kuru | Suru | Iku 

instance Show IrregularType where
    show Kuru = "来る"
    show Suru = "する"
    show Iku = "行く"

data Verb = Godan Stem Ending | Ichidian Stem | Irregular Stem IrregularType

instance Show Verb where
    show = show_verb

-- Show functions.

show_verb (Godan stem ending) = show stem ++ show ending
show_verb (Ichidian stem) = show stem ++ "る"
show_verb (Irregular stem irregular_type) = show stem ++ show irregular_type

-- Example values.

example_godan = Godan (Stem "かえ" (Just "帰" )) Ru

example_ichidian = Ichidian (Stem "たべ" (Just "食べ"))

example_irregular = Irregular (Stem "もって"(Just "持って")) Kuru
example_irregular_nostem = Irregular (Stem "" Nothing) Kuru

