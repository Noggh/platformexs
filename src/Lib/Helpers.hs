module Lib.Helpers (anyPred) where

anyPred :: a -> [a -> Bool] -> Bool
anyPred x = any ($ x)
