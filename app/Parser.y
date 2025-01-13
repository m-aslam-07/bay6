{
module Parser (parse, parseStmt, parsePrint, parseAssn) where

import Types
}

%name parse Sequence
%name parseStmt Statement
%name parsePrint PrintStatement
%name parseAssn Assignment
%tokentype { Token }
%error { parseError }

%nonassoc '<' '>' '=' Le Ge
-- %left AND OR
%left '+' '-'
%left '*' '/'
%token
    '+'         { TAdd }
    '-'         { TSub }
    '*'         { TMul }
    '/'         { TDiv }
    intLit      { TIntLit $$ }
    strLit      { TStringLit $$ }
    symbol      { TSymbol $$ }
    PRINT       { TPrint }
    ','         { TCommSep }
    ';'         { TSemiSep }
    '='         { TEq }
    '('         { TOpenParen }
    ')'         { TClosedParen }
    Le          { TLe }
    Ge          { TGe }
    '<'         { TLt }
    '>'         { TGt }
    eol         { TNewline }
    if          { TIf }
    then        { TThen }
    endif       { TEndIf }
    else        { TElse }
    elseif      { TElseIf }
    do          { TDo }
    while       { TWhile }
    endwhile    { TEndWhile }

%%

Sequence :: { [Stmt] }
Sequence : Statement eol { [$1] }
        | Statement eol Sequence { $1 : $3 }

Statement :: { Stmt }
Statement: PrintStatement { $1 }
        | Assignment { $1 }
        | IfStatement { $1 }
        | WhileStatement { $1 }

PrintStatement :: { Stmt }
PrintStatement: PRINT PrintSeq { Print $2 } 

PrintSeq :: { [Expr] }
PrintSeq: PrintElem { [$1] }
        | PrintElem PrintSeq { $1 : $2 }

PrintElem :: { Expr }
PrintElem: Expression { $1 }
         | ',' { StringLit " " }
         | ';' { StringLit "\t" }

Assignment :: { Stmt }
Assignment: symbol '=' Expression { Assign (Symbol $1) $3 }

Expression :: { Expr }
Expression: '(' Expression ')' { $2 }
       | intLit { IntLit $1 }
       | strLit { StringLit $1 }
       | symbol { Symbol $1 }
       | Expression '*' Expression { Mul $1 $3 }
       | Expression '/' Expression { Div $1 $3 }
       | Expression '+' Expression { Add $1 $3 }
       | Expression '-' Expression { Sub $1 $3 }
       | Expression Le Expression { Le $1 $3 }
       | Expression Ge Expression { Ge $1 $3 }
       | Expression '<' Expression { Lt $1 $3 }
       | Expression '>' Expression { Gt $1 $3 }

IfStatement :: { Stmt }
IfStatement: if Expression then eol Sequence endif { IfStmt $2 $5 [] }
        | if Expression then eol Sequence else eol Sequence endif { IfStmt $2 $5 $8 }
        | if Expression then eol Sequence ElseIfSequence { IfStmt $2 $5 [$6] }

ElseIfSequence :: { Stmt }
ElseIfSequence: elseif Expression then eol Sequence else eol Sequence endif { IfStmt $2 $5 $8 }
        | elseif Expression then eol Sequence endif { IfStmt $2 $5 [] }
        | elseif Expression then eol Sequence ElseIfSequence { IfStmt $2 $5 [$6] }

WhileStatement :: { Stmt }
WhileStatement: while Expression do eol Sequence endwhile { WhileStmt $2 $5 }
