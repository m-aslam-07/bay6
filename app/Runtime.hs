module Runtime (
    evalExpr,
    exec
) where

import Types
import Data.Map (lookup, insert)
import Data.Maybe

evalExpr :: Env -> Expr -> Maybe Value
evalExpr _ (IntLit val) = Just $ ValInt val
evalExpr _ (StringLit val) = Just $ ValString val
evalExpr env (Symbol sym) = Data.Map.lookup sym (symtab env)
evalExpr env (Add e1 e2) = do
    x1 <- evalExpr env e1
    x2 <- evalExpr env e2
    valAdd x1 x2
evalExpr env (Sub e1 e2) = do
    x1 <- evalExpr env e1
    x2 <- evalExpr env e2
    valSub x1 x2
evalExpr env (Mul e1 e2) = do
    x1 <- evalExpr env e1
    x2 <- evalExpr env e2
    valMul x1 x2
evalExpr env (Div e1 e2) = do
    x1 <- evalExpr env e1
    x2 <- evalExpr env e2
    valDiv x1 x2
evalExpr env (Gt e1 e2) = do
    x1 <- evalExpr env e1
    x2 <- evalExpr env e2
    case (x1, x2) of 
        (ValInt a, ValInt b) -> 
                if a > b then
                    return $ ValInt 1
                else
                    return $ ValInt 0
        _ -> Nothing
evalExpr env (Eq e1 e2) = do
    x1 <- evalExpr env e1
    x2 <- evalExpr env e2
    case (x1, x2) of 
        (ValInt a, ValInt b) -> 
                if a == b then
                    return $ ValInt 1
                else
                    return $ ValInt 0
        _ -> Nothing
evalExpr env (Lt e1 e2) = do
    x1 <- evalExpr env e1
    x2 <- evalExpr env e2
    case (x1, x2) of 
        (ValInt a, ValInt b) -> 
                if a < b then
                    return $ ValInt 1
                else
                    return $ ValInt 0
        _ -> Nothing
evalExpr env (Ge e1 e2) = do
    x1 <- evalExpr env e1
    x2 <- evalExpr env e2
    case (x1, x2) of 
        (ValInt a, ValInt b) -> 
                if a >= b then
                    return $ ValInt 1
                else
                    return $ ValInt 0
        _ -> Nothing
evalExpr env (Le e1 e2) = do
    x1 <- evalExpr env e1
    x2 <- evalExpr env e2
    case (x1, x2) of 
        (ValInt a, ValInt b) -> 
                if a <= b then
                    return $ ValInt 1
                else
                    return $ ValInt 0
        _ -> Nothing
-- evalExpr _ _ = Nothing -- TODO: must implement error handling

valToString :: Value -> String
valToString (ValString str) = str
valToString (ValInt x) = show x

execPrint :: Env -> [Expr] -> IO Env
execPrint env [] = putStr "\n" >> return env
execPrint env (x:xs) = do
    let v1 = evalExpr env x
    if isNothing v1 then
        error "Expr evaluated to Nothing, have to build error system"
    else do
        putStr $ valToString (fromJust v1)
        execPrint env xs

execAssign :: Env -> Expr -> Expr -> Env
execAssign env (Symbol sym) expr = do
    let mval = evalExpr env expr
    case mval of 
        Nothing -> error "Expr evaluated to nothing"
        (Just val) -> env {symtab = insert sym val (symtab env)}
execAssign _ _ _ = error "Invalid assignment"

exec :: Env -> IO Env
exec env = do
    case cur env of
        (Print exprs): res -> do
            env1 <- execPrint env exprs
            exec (env1 { cur = res })
        (Assign e1 e2): res -> do
            let env1 = execAssign env e1 e2
            exec (env1 { cur = res })
        (IfStmt cond stmts alt): res -> do
            let condResult =  evalExpr env cond
            case condResult of 
                Nothing -> error "Condition evaluated to nothing"
                (Just val) -> do
                    env1 <- exec $ env { cur = if val /= ValInt 0 then stmts else alt }
                    exec $ env { cur = res, symtab = symtab env1 }
        (WhileStmt cond stmts): res -> do
            let condResult = evalExpr env cond
            case condResult of 
                Nothing -> error "Condition evaluated to nothing"
                (Just val) -> do
                    if val /= ValInt 0 then do
                        env1 <- exec $ env { cur = stmts } -- Execute loop once
                        exec $ env { symtab = symtab env1 }
                    else
                        exec $ env { cur = res }
        [] -> return env
