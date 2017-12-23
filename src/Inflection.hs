module Inflection where

import Verb
import Hiragana

import Data.Maybe

-- | Alternate name for the forms are:
-- | Jisho: Dictionary form
-- | Nenyou: Masu form
-- | Mizen: Nai form
-- | Meirei: Ba form
-- | Suiryou: U form
data Kei = Jisho | Nenyou | Meirei | Mizen | Suiryou | Te

data Inflected = Inflected Verb Kei

instance Show Inflected where
    show (Inflected verb kei) = inflect verb kei

inflect :: Verb -> Kei -> String
-- Godan.
inflect (Godan stem end) kei = show stem ++ godan_ending_get end kei
-- Ichidian.
inflect (Ichidian stem) Suiryou = show stem ++ "よ" ++ show U
inflect (Ichidian stem) kei = show stem

godan_ending_get ending Jisho   = show ending
godan_ending_get ending Nenyou  = fromJust $ hiragana_get (show ending) "い"
godan_ending_get ending Meirei  = fromJust $ hiragana_get (show ending) "え"
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
