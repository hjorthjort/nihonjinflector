module Basis where

-- Basic types and functions, shared by other parts of the logic.

import Data.Maybe

type Kana = String
type Kanji = String

-- All verbs have a kana stem, but not all verbs can be written with kanji.
data Writing = Writing Kana (Maybe Kanji) deriving (Eq, Ord)
-- For debugging: print Kanji in concole. Proper application should use some
-- other mechaningsm than show to display a word.
instance Show Writing where
    show (Writing kana kanji) = fromMaybe kana kanji

-- Append the given string to the end of both writing variants.
(!++) :: Writing -> String -> Writing
(Writing kana kanji) !++ string = Writing (kana ++ string) ((++ string) `fmap` kanji)

-- Combine two Writings into one, where the kana parts are appended, and a kanji
-- part is created if at least one of the Writings have a knji part.
(!+!) :: Writing -> Writing -> Writing
(Writing kana Nothing) !+! (Writing kana' Nothing) =
    Writing (kana ++ kana') Nothing
(Writing kana kanji) !+! (Writing kana' kanji') =
    Writing (kana ++ kana') $ Just $ fromMaybe kana kanji ++ fromMaybe kana' kanji'
