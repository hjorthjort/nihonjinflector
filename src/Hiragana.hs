module Hiragana (hiragana_get) where

import Data.List
import Data.Maybe
import Test.QuickCheck
import Test.QuickCheck.Gen

a_line = ["あ", "う", "お", "い", "え"]
k_line = ["か", "く", "こ", "き", "け"]
g_line = ["が", "ぐ", "ご", "ぎ", "げ"]
s_line = ["さ", "す", "そ", "し", "せ"]
z_line = ["ざ", "ず", "ぞ", "じ", "ぜ"]
t_line = ["た", "つ", "と", "ち", "て"]
d_line = ["だ", "づ", "ど", "ぢ", "で"]
n_line = ["な", "ぬ", "の", "に", "ね"]
h_line = ["は", "ふ", "ほ", "ひ", "へ"]
b_line = ["ば", "ぶ", "ぼ", "び", "べ"]
m_line = ["ま", "む", "も", "み", "め"]
r_line = ["ら", "る", "ろ", "り", "れ"]
y_line = ["や", "ゆ", "よ"]
w_line = ["わ", "ん", "を"]

hiragana_matrix = [
                    a_line,
                    k_line,
                    g_line,
                    s_line,
                    z_line,
                    t_line,
                    d_line,
                    n_line,
                    h_line,
                    b_line,
                    m_line,
                    r_line,
                    y_line,
                    w_line
                  ]

-- | Find the hiragana at an intersection of a line (representing a single
-- | consonant) and a column (representing a single vowel).
hiragana_get :: String -> String -> Maybe String
hiragana_get consonant vowel = do
    line_list <- find (elem consonant) hiragana_matrix
    col_list <- find (elem vowel) (transpose hiragana_matrix)
    let hiragana = intersect line_list col_list
    if null hiragana then Nothing else return (hiragana !! 0)

-- Tests.

-- The square part of the hiragana matrix should always give a value.
hiragana = elements $ concat $ take 12 hiragana_matrix
hiragana_tup = do
    h1 <- hiragana
    h2 <- hiragana
    return (h1, h2)

prop_always_finds = forAll hiragana_tup $ (\(h1, h2) -> isJust (hiragana_get h1 h2))
