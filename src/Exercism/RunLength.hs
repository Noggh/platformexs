{-# LANGUAGE OverloadedStrings #-}
module Exercism.RunLength (encode, decode) where

import           Control.Monad (join)
import qualified Data.Char     as C
import qualified Data.Text     as T

{-
Instructions

Implement run-length encoding and decoding.
Run-length encoding (RLE) is a simple form of data compression, where runs (consecutive data elements) are replaced by just one data value and count.
For example we can represent the original 53 characters with only 13.

    "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB"  ->  "12WB12W3B24WB"

RLE allows the original data to be perfectly reconstructed from the compressed data, which makes it a lossless data compression.

    "AABCCCDEEEE"  ->  "2AB3CD4E"  ->  "AABCCCDEEEE"

For simplicity, you can assume that the unencoded string will only contain the letters A through Z (either lower or upper case) and whitespace. 
This way data to be encoded will never contain any numbers and numbers inside data to be decoded always represent the count for the following character.
-}

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
