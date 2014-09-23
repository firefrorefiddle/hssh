{-# LANGUAGE NoMonomorphismRestriction #-}

module ShellParser where

import Text.Parsec hiding (parse)
import qualified Data.Text as T
import Data.Text (Text)
import Control.Applicative ((<$>))
import Prelude hiding (exp)
import Data.Char (isSpace)

import Command
import Types

type Parser s m = ParsecT s (ParseState s m) m Exp

data ParseState s m = ParseState {
  psSpecialChars :: [Char],
  psIgnoreSpaces :: [Bool],
  psSpecialExps  :: [Parser s m]
  }
          
excludeChar c = modifyState $ \s -> s {psSpecialChars = c:psSpecialChars s}
includeChar c = modifyState $ \s -> s {psSpecialChars = remove c $ psSpecialChars s}
  where remove c = filter (not . (== c))

ignoreSpaces b = modifyState $ \s -> s {psIgnoreSpaces = b:psIgnoreSpaces s}
unIgnoreSpaces = modifyState $ \s -> s {psIgnoreSpaces = ignore' s}
  where ignore' s = case psIgnoreSpaces s of
          [] -> error "commandline parser: unIgnoreSpaces without ignoreSpaces"
          (_:xs) -> xs

ignoreSpacesIn b p = do
  ignoreSpaces b
  res <- p
  unIgnoreSpaces
  return res

spacesIgnored s = case psIgnoreSpaces s of
  []    -> False
  (b:_) -> b
  
enclosed start end p = do
  char start
  excludeChar end
  r <- p
  char end
  includeChar end
  return r

specialChars = "$\""
insideChar = getState >>= \s ->
  let specialCharCheck = not . flip elem (psSpecialChars s)
      spaceCheck = if spacesIgnored s
                   then const True
                   else not . isSpace
  in satisfy (\c -> spaceCheck c && specialCharCheck c) 

enclosedExp start end = enclosed start end exp
quoteExp   = ignoreSpacesIn True $ QuoteExp   <$> enclosedExp '\"' '\"'
bracketExp = BracketExp <$> enclosedExp '[' ']'
parenExp   = ParenExp   <$> enclosedExp '(' ')'
braceExp   = BraceExp   <$> enclosedExp '{' '}'
dollarExp  = DollarExp  <$> (char '$' >> exp)
specialExp = do
    s <- getState
    choice (psSpecialExps s)

word = T.pack <$> many1 insideChar
exp = simplify . ConcatExp <$> many1 (specialExp <|> StrExp <$> word)

arg = exp
program = exp

command = do
  prog <- program
  spaces
  args <- sepBy arg spaces
  return $ Command prog args

simplify (ConcatExp [x]) = x
simplify e = e

parse p = runParser p initState
  where initState = ParseState {psSpecialChars = specialChars,
                                psIgnoreSpaces = [],
                                psSpecialExps  = [quoteExp, parenExp, dollarExp]}
