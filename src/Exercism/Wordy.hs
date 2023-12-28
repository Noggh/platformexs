{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Exercism.Wordy (answer) where

import           Control.Monad              (void)
import           Data.Text                  (Text)
import           Data.Void                  (Void)
import           Text.Megaparsec            (MonadParsec (try), Parsec, choice, parseMaybe)
import           Text.Megaparsec.Char       (space1, string')
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

answer t = parseMaybe parseInput t >>= \case Return s -> Just s

lexeme  = L.lexeme parseSpace
integer = lexeme L.decimal

parseInput :: Parser (Comp Int)
parseInput = try $ do
    void $ lexeme $ string' "what is"
    foldr1 (<**>) <$> parseOpsUntilEnd

parseOpsUntilEnd = go []
    where go xs = do
            comp <- lexeme parsePattern
            case comp of Return _ -> pure $ reverse $ comp : xs
                         _        -> go $ comp : xs

parsePattern = do
    s <- integer
    parseOneOp >>= \case Nothing -> pure . Return $ s
                         Just o  -> pure . Perf s $ o

parseOneOp = choice
    [ Just Plus     <$ string' "plus"
    , Just Subtract <$ string' "subtract"
    , Just Multiply <$ string' "multiply"
    , Just Divide   <$ string' "divide"
    , Nothing       <$ string' "?"
    ]

parseSpace = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

data Comp a where
    Return :: a -> Comp a
    Perf   :: a -> Operation -> Comp a
    deriving (Show)

(<**>) :: Comp Int -> Comp Int -> Comp Int
(Perf s op) <**> (Return s')   = Return $ run op s s'
(Perf s op) <**> (Perf s' op') = flip Perf op' $ run op s s'

run :: (Integral a) => Operation -> (a -> a -> a)
run Plus     = (+)
run Subtract = (-)
run Multiply = (*)
run Divide   = div

data Operation = Plus | Multiply | Subtract | Divide | End
    deriving (Show)

