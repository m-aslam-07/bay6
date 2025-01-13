module Types (
    Token(..),
    Value(..),
    Lexer,
    parseError,
    valAdd, valSub, valMul, valDiv,
    Env(..),
    Expr(..),
    Stmt(..)
) where

-- import Text.Megaparsec
-- import Data.Void

import qualified Data.Map as Map

data Token = TAdd | TSub | TMul | TDiv | TIntLit Integer | TStringLit String
    | TSymbol String | TPrint | TEq | TLt | TGt | TGe | TLe | TCommSep | TSemiSep
    | TOpenParen | TClosedParen | TLet | TNewline | TIf | TThen | TElseIf | TEndIf
    | TElse | TWhile | TEndWhile | TDo
    deriving (Show, Eq)

data Expr = Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Eq Expr Expr
    | Lt Expr Expr
    | Gt Expr Expr
    | Le Expr Expr
    | Ge Expr Expr
    | IntLit Integer
    | StringLit String
    | Symbol String
    deriving (Show, Eq)

type Sequence = [Stmt]

data Value = ValInt Integer | ValString String deriving (Eq, Show)

valAdd :: Value -> Value -> Maybe Value
valAdd (ValInt v1) (ValInt v2) = Just (ValInt (v1 + v2))
valAdd (ValString v1) (ValString v2) = Just (ValString (v1 ++ v2))
valAdd _ _ = Nothing

valSub :: Value -> Value -> Maybe Value
valSub (ValInt v1) (ValInt v2) = Just (ValInt (v1 - v2))
valSub _ _ = Nothing

valMul :: Value -> Value -> Maybe Value
valMul (ValInt v1) (ValInt v2) = Just (ValInt (v1 * v2))
valMul _ _ = Nothing

valDiv :: Value -> Value -> Maybe Value
valDiv _ (ValInt 0) = Nothing
valDiv (ValInt v1) (ValInt v2) = Just (ValInt (v1 `div` v2))
valDiv _ _ = Nothing

data Stmt = Print [Expr]
    | Assign Expr Expr
    | IfStmt Expr Sequence Sequence
    | WhileStmt Expr Sequence
    deriving (Show, Eq)

data Env = Env {
    cur :: [Stmt],
    ast :: Sequence,
    symtab :: Map.Map String Value,
    jumptab :: Map.Map String [Stmt],
    returnStack :: [[Stmt]]
    }

-- type Lexer = Parsec Void String Types.Token
type Lexer = String -> Maybe (Token, String)
-- type Parser = Parsec Void String
--
--
parseError :: [Token] -> a
parseError xs = error $ "Could not parse tokens " ++ show xs
