{-# LANGUAGE OverloadedStrings #-}
module Exercism.Luhn (isValid) where

import           Control.Monad (guard, liftM2)
import           Data.Char     (digitToInt, isNumber)
import qualified  Data.List     as L
import           Data.Text     (all, length, pack, reverse, splitOn, unpack)
import           Prelude       hiding (all, and, length, null, reverse)

type Score = Int

parseCreditCard cc = do
    let stripped = mconcat $ splitOn " " cc
    guard $ liftM2 (&&) ((> 1) . length) (all isNumber) stripped
    Just stripped

score = sum . L.reverse . zipWith (curry parseDigit) [1..] . unpack . reverse
    where parseDigit (i, c) =
            let d = digitToInt c
                t = if even i then (* 2) d else d
            in if (> 9) t then t - 9 else t

validScore = (== 0) . (`rem` 10)

isValid s = maybe False (validScore . score) (parseCreditCard $ pack s)
