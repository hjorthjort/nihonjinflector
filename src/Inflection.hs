module Inflection where

import Verb

data Kei = TeKei | NaiKei | UKei

instance Show Kei where
    show TeKei = "て"
    show NaiKei = "ない"
    show UKei = "う"

data Inflected = Inflected Verb Kei

instance Show Inflected where
    show (Inflected verb kei) = inflect verb kei

inflect :: Verb -> Kei -> String
-- TODO: Make this function exhaustive
-- Godan.
inflect (Godan stem end) UKei = show stem  ++ show UKei
-- Ichidian.
inflect (Ichidian stem) UKei = show stem ++ "よ" ++ show U
inflect (Ichidian stem) kei = show stem  ++ show kei

