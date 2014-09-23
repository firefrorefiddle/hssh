{
module Parser (parseTokens, parseBS) where

import qualified Lexer as Lex
import Types
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
}

%name parseTokens
%tokentype { Lex.Token }
%error { (error . show) }

%token
   '<'    { Lex.LessThan }
   '>'    { Lex.GreaterThan }
   '|'    { Lex.Pipe }
   '='    { Lex.Equal }
   bt     { Lex.Backtick }
   nl     { Lex.Newline }
   word   { Lex.Word $$ }
   sqword { Lex.SQWord $$ }
   var    { Lex.Var $$ }

%%

CommandLine   : SimpleCommand nl { $1 }

SimpleCommand : Exp Args { Command $1 $2 }
              | Exp      { Command $1 [] }

Args          : Exp Args { $1 : $2 }
              | Exp      { [$1] }

Exp           : word     { StrExp . T.decodeUtf8 $ $1 }
              | sqword   { StrExp . T.decodeUtf8 $ $1 }
              | var      { DollarExp . StrExp . T.decodeUtf8 $ $1 }
              | bt Exp bt { BackTickExp $2 }    

{
parseBS = parseTokens . Lex.lexBS
}
