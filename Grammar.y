{
module Grammar where
import Tokens
import MinimalBasis
}

%name parseFDs
%tokentype { Token }
%error { parseError }

%token
    Attr { TokenAttribute $$ }
    RELATION { TokenRelation }
    FDS { TokenFD }
    ' , ' { TokenComma }
    ARROW { TokenArrow }
    SEMI { TokenSemi }
%%

FunctionalDs :
RELATION attribs FDS singleFDs        { FDs $2 $4 }

attribs :
Attr ' , ' attribs             { $1 : $3 }
| Attr                        { [$1] }

singleFDs :
attribs ARROW attribs SEMI singleFDs      { ($1, $3) : $5 }
| attribs ARROW attribs SEMI       { [($1, $3)] }

{
  
parseError :: [Token] -> a
parseError ts = error ("Parse error: " ++ show ts)

}
