{-# LANGUAGE OverloadedStrings #-}
module Exercism.RunLength (encode, decode) where

import           Control.Monad (join)
import qualified Data.Char     as C
import qualified Data.Text     as T

encode = join . map (T.unpack . extract) . T.group . T.pack
    where extract t | (== 1) $ T.length t = t
                    | otherwise           = T.pack (show $ T.length t) <> headT t

decode [] = ""
decode s = portion <> decode nextPortion
    where text    = T.pack s
          portion = case T.unpack $ T.takeWhile C.isDigit text of
              "" -> T.unpack $ headT text
              s' -> T.unpack $ T.replicate (read s') (headT $ dropDigits text)
          nextPortion = T.unpack . T.tail . dropDigits $ text

dropDigits = T.dropWhile C.isDigit
headT      = T.singleton . T.head
