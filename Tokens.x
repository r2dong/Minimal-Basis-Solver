{
module Tokens where
import qualified Data.ByteString.Char8 as C
}

%wrapper "basic"

tokens :-

  $white+                       ;
  [a-zA-Z]+    { \s -> TokenAttribute (C.pack s) }
  "Relation:"     { \s -> TokenRelation}
  "->"         { \s -> TokenArrow}
  "FDs:"     { \s -> TokenFD}
  ","            { \s -> TokenComma}
  ";"          { \s -> TokenSemi}
{

-- The token type:
data Token = TokenAttribute C.ByteString |
             TokenRelation |
             TokenFD |
             TokenArrow |
             TokenComma |
             TokenSemi
           deriving (Eq,Show)

scanTokens = alexScanTokens

}
