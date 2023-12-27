{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Exercism.Wordy (answer) where

answer :: String -> Maybe Integer
answer = undefined

data Comp a where
    Lift     :: {seed :: a, next :: Comp a} -> Comp a
    Plus     :: {seed :: a, next :: Comp a} -> Comp a
    Multiply :: {seed :: a, next :: Comp a} -> Comp a
    Subtract :: {seed :: a, next :: Comp a} -> Comp a
    Divide   :: {seed :: a, next :: Comp a} -> Comp a
    Return   :: Comp a

fn (Plus s _)     = (+ s)
fn (Multiply s _) = (* s)
fn (Subtract s _) = flip (-) s
fn (Divide s _)   = (`div` s)

eval (Lift s Return) = Just s
eval (Lift s op)     = let f = fn op in eval $ Lift (f s) (next op)

parse :: String -> Comp a
parse s = undefined

