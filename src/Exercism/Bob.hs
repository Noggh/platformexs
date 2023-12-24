{-# LANGUAGE OverloadedStrings #-}
module Exercism.Bob (responseFor) where

import           Control.Applicative (asum)
import           Control.Monad       (guard, liftM2)
import qualified Data.Char           as C
import           Data.Maybe          (fromMaybe)
import qualified Data.Text           as T

{-
Instructions
Your task is to determine what Bob will reply to someone when they say something to him or ask him a question.

Bob only ever answers one of five things:

"Sure.":
    This is his response if you ask him a question, such as "How are you?" The convention used for questions is that it ends with a question mark.
"Whoa, chill out!":
    This is his answer if you YELL AT HIM. The convention used for yelling is ALL CAPITAL LETTERS.
"Calm down, I know what I'm doing!":
    This is what he says if you yell a question at him.
"Fine. Be that way!":
    This is how he responds to silence. The convention used for silence is nothing, or various combinations of whitespace characters.
"Whatever.":
    This is what he answers to anything else.

Implement the responseFor function that returns Bob's response for a given input. You can use the provided signature if you are unsure about the types, but don't let it restrict your creativity:
responseFor :: String -> String
-}

responseFor :: Prompt -> Reply
responseFor p = fromMaybe "Whatever." $ asum $ flip unconv p <$> conventions

type Prompt = T.Text
type Reply  = T.Text

newtype Convention = Convention { unconv :: Prompt -> Maybe Reply }

conventions = [calmDownConvention, chillOutConvention, sureConvention, silenceConvention]

sureConvention = Convention $ \p ->T.stripSuffix "?" (T.stripEnd p) >> Just "Sure."

chillOutConvention = Convention $ \p -> guard (liftM2 (&&) (T.all C.isUpper) (not . T.null) $ T.filter C.isAlpha p) >> Just "Whoa, chill out!"

calmDownConvention = Convention $ \p -> (,) <$> unconv chillOutConvention p <*> unconv sureConvention p >> Just "Calm down, I know what I'm doing!"

silenceConvention = Convention $ \p -> guard (T.all C.isSpace p) >> Just "Fine. Be that way!"
