{-# LANGUAGE OverloadedStrings #-}
module Exercism.WordCount (wordCount) where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Text as T

space = " "
comma = ","

notAlphaNum = not . C.isAlphaNum

cleanWord = L.dropWhile notAlphaNum . L.dropWhileEnd notAlphaNum

countEquals = L.map (\ws -> (head ws, L.length ws)) . L.group

removeNulls = L.filter $ not . T.null . T.pack . fst

uncomma = T.unpack . T.replace comma space . T.pack

wordCount s = removeNulls $ countEquals $ L.sort $ L.map cleanWord $ L.map C.toLower <$> L.words (uncomma s)
