module GrammarRules() where

-- Ending of the jisho-kei for verbs.
data Ending = U | Tsu | Ru | Bu | Mu | Nu | Ku | Gu | Su deriving (Eq, Ord, Enum)

instance Show Ending where
    show ending = fromJust $ Map.lookup ending ending_map

ending_map = Map.fromList 
  [(U, "う"), (Tsu, "つ"), (Ru, "る"), (Bu, "ぶ"), (Mu, "む"), (Nu, "ぬ"),
  (Ku, "く"), (Gu, "ぐ"), (Su, "す")]

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
