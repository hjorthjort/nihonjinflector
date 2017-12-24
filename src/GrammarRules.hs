module GrammarRules where

import Basis
import Hiragana

import qualified Data.Map as Map
import Data.Maybe

-- Ending of the jisho-kei for verbs.
data Ending = U | Tsu | Ru | Bu | Mu | Nu | Ku | Gu | Su deriving (Eq, Ord, Enum)

instance Show Ending where
    show ending = fromJust $ Map.lookup ending ending_map

-- | Alternate name for the forms are:
-- | Jisho: Dictionary form
-- | Nenyou: Masu form
-- | Mizen: Nai form
-- | Meirei: Ba form
-- | Katei: ??
-- | Suiryou: U form
data Kei = Jisho | Nenyou | Meirei | Katei | Mizen | Suiryou | Te deriving (Eq, Ord)

type Stem = Writing

data IrregularType = Kuru | Suru deriving (Eq, Ord)

instance Show IrregularType where
    show Kuru = "来る"
    show Suru = "する"

data Verb = Godan Stem Ending | Ichidian Stem | Irregular Stem IrregularType deriving (Eq, Ord)

instance Show Verb where
    show = show_verb

stem :: String -> String -> Stem
stem kana kanji | null kanji = Writing kana Nothing
                | otherwise = Writing kana (Just kanji)

no_stem = stem "" ""

ending_map = Map.fromList 
  [(U, "う"), (Tsu, "つ"), (Ru, "る"), (Bu, "ぶ"), (Mu, "む"), (Nu, "ぬ"),
  (Ku, "く"), (Gu, "ぐ"), (Su, "す")]

godan_ending_get :: Ending -> Kei -> HiraganaCharacter
godan_ending_get ending Jisho   = show ending
godan_ending_get ending Nenyou  = fromJust $ hiragana_get (show ending) "い"
godan_ending_get ending Meirei  = fromJust $ hiragana_get (show ending) "え"
godan_ending_get ending Katei   = godan_ending_get ending Meirei
godan_ending_get ending Mizen   = fromJust $ hiragana_get (show ending) "あ"
godan_ending_get ending Suiryou = fromJust $ hiragana_get (show ending) "お"
godan_ending_get ending Te      = case ending of
                                    U -> tte
                                    Tsu -> tte
                                    Ru -> tte
                                    Nu -> nde
                                    Mu -> nde
                                    Bu -> nde
                                    Ku -> "いて"
                                    Gu -> "いで"
                                    Su -> "して"
   where
       tte = "って"
       nde = "んで"

exception :: Verb -> Kei -> Maybe Writing
exception verb kei = Map.lookup (verb, kei) exceptions_table

exceptions_table = Map.fromList
    [
        ((kuru, Jisho), Writing "くる" (Just "来る")),
        ((kuru, Nenyou), Writing "き" (Just "来")),
        ((kuru, Meirei), Writing "こられ" (Just "来られ")),
        ((kuru, Mizen), Writing "こ" (Just "来")),
        ((kuru, Suiryou), Writing "こよ" (Just "来よ")),
        ((kuru, Te), Writing "き" (Just "来")),
        ((suru, Jisho), Writing "する" Nothing),
        ((suru, Nenyou), Writing "し" Nothing),
        ((suru, Meirei), Writing "すれ" Nothing),
        ((suru, Mizen), Writing "し" Nothing),
        ((suru, Suiryou), Writing "しよ" Nothing),
        ((suru, Te), Writing "し" Nothing),
        ((iku, Te), Writing "いって" (Just "行って"))
    ]

inflect :: Verb -> Kei -> Writing
inflect verb kei = fromMaybe (inflect' verb kei) (exception verb kei)

inflect' (Godan stem end) kei = stem !++ godan_ending_get end kei
inflect' (Ichidian stem) Jisho = stem !++ "る"
inflect' (Ichidian stem) Suiryou = stem !++ "よ"
inflect' (Ichidian stem) _ = stem
inflect' (Irregular stem irr_type) kei =
    stem !+! (fromJust (exception (Irregular no_stem irr_type) kei))

-- Verb constants.
kuru = Irregular no_stem Kuru
suru = Irregular no_stem Suru
iku = Godan (stem "い" "行") Ku

-- Example verbs for testing
show_verb (Godan stem ending) = show stem ++ show ending
show_verb (Ichidian stem) = show stem ++ "る"
show_verb (Irregular stem irregular_type) = show stem ++ show irregular_type

-- Example values.


example_godan = Godan (stem "かえ" "帰") Ru

example_ichidian = Ichidian (stem "たべ" "食べ")

example_irregular = Irregular (stem "もって" "持って") Kuru
example_irregular_nostem = kuru
