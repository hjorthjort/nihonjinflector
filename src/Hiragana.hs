module Hiragana (hiragana_get) where

import qualified Data.Map as Map
import Data.List (transpose, intersect)

a_line = ["あ", "い", "う", "え", "お"]
k_line = ["か", "き", "く", "け", "こ"]
g_line = ["が", "ぎ", "ぐ", "げ", "ご"]
s_line = ["さ", "し", "す", "せ", "そ"]
z_line = ["ざ", "じ", "ず", "ぜ", "ぞ"]
t_line = ["た", "ち", "つ", "て", "と"]
d_line = ["だ", "ぢ", "づ", "で", "ど"]
n_line = ["な", "に", "ぬ", "ね", "の"]
h_line = ["は", "ひ", "ふ", "へ", "ほ"]
b_line = ["ば", "び", "ぶ", "べ", "ぼ"]
m_line = ["ま", "み", "む", "め", "も"]
r_line = ["ら", "り", "る", "れ", "ろ"]
y_line = ["や", "ゆ", "よ"]
w_line = ["わ", "を", "ん"]

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


hiragana_matrix_lines = zip (transpose hiragana_matrix !! 0) hiragana_matrix
hiragana_matrix_cols = zip a_line (transpose hiragana_matrix)

hiragana_get :: String -> String -> Maybe String
hiragana_get line col = do
    line_list <- lookup line hiragana_matrix_lines
    col_list <- lookup col hiragana_matrix_cols
    let hiragana = intersect line_list col_list
    if null hiragana then Nothing else return (hiragana !! 0)
