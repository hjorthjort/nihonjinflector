module Verb where

type KanaStem = String
type KanjiStem = String

data Stem = KanaStem (Maybe KanjiStem)

data Ending = U | Tsu | Ru | Bu | Mu | Nu | Ku | Gu | Su
data IrregularType = Kuru | Suru | Iku 
data Verb = Godan Stem Ending | Ichidian Stem | Irregular Stem IrregularType

main = undefined
