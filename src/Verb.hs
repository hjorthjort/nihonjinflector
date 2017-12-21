module Verb where

type KanaStem = String
type KanjiStem = String

-- All verbs have a kana stem, but not all verbs can be written with kanji.
data Stem = KanaStem (Maybe KanjiStem)
-- Ending of the dictionary form of Godan verbs.
data GodanEnding = U | Tsu | Ru | Bu | Mu | Nu | Ku | Gu | Su
data IrregularType = Kuru | Suru | Iku 
data Verb = Godan Stem GodanEnding | Ichidian Stem | Irregular Stem IrregularType
