module Main where

import Lexer
import Parser
import Types
import Runtime
import qualified Data.Map

main :: IO ()
main = do
    putStrLn "Enter the name of the prgm"
    fname <- getLine :: IO String
    prgm <- readFile fname
    let tokens = lexify prgm
    putStrLn $ "Tokens: " ++ show tokens
    let curAst = parse tokens
    putStrLn $ "AST: " ++ show curAst
    putStrLn "---OUTPUT---"
    let env = Env { cur = curAst, ast = curAst, 
        symtab = Data.Map.empty, jumptab = Data.Map.empty,
        returnStack = [] }
    _ <- exec env
    return ()
