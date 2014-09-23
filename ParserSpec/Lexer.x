{
module Lexer (lexBS, Token(..)) where

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as B
}

%wrapper "basic-bytestring"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters
$anyWord = [^\#$white\`\n]
$any     = [^\#\`\n]
$commentStart = [\#]
$quote     = [\"]
$squote    = [\"]
$whiteNoNL = [\ \t\f\v\r]

tokens :-

  $commentStart$any*\n                  ;
  $whiteNoNL+                           ;
  \n                                    { \_ -> Newline }
  [\<]                                  { \_ -> LessThan }
  [>]                                   { \_ -> GreaterThan }
  [\|]                                  { \_ -> Pipe }
  [=]                                   { \_ -> Equal }
  [\`]                                  { \_ -> Backtick }
  $quote$any*$quote                     { \s -> Word . L.toStrict . L.init . L.tail $ s }
  $squote$any*$squote                   { \s -> SQWord . L.toStrict . L.init . L.tail $ s }
  [\$]$anyWord+                         { \s -> Var  . L.toStrict . L.tail $ s }
  $anyWord+                             { \s -> Word . L.toStrict $ s }

{
-- Each action has type :: String -> Token

-- The token type:
data Token = Word B.ByteString
           | SQWord B.ByteString
           | Var  B.ByteString
           | Equal
           | Pipe
           | LessThan
           | GreaterThan
           | Backtick
           | Newline
	deriving (Eq,Show)

lexBS :: B.ByteString -> [Token]
lexBS = alexScanTokens . L.fromStrict

}
