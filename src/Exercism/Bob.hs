{-# LANGUAGE OverloadedStrings #-}
module Exercism.Bob (responseFor) where

import           Control.Applicative (asum)
import           Control.Monad       (guard, liftM2)
import qualified Data.Char           as C
import           Data.Maybe          (fromMaybe)
import qualified Data.Text           as T

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
