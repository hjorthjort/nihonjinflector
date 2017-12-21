module Inflection where

import Verb

data Inflection = Te | Nai

data Inflected = Inflected Verb Inflection
