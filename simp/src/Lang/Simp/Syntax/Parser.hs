{-# LANGUAGE MultiParamTypeClasses #-}
{- HLINT ignore "Use <$>" -}
module Lang.Simp.Syntax.Parser where


import Lang.Simp.Syntax.Parsec
import Lang.Simp.Syntax.SrcLoc
import Lang.Simp.Syntax.AST
import Lang.Simp.Syntax.Lexer
import Control.Monad.Error.Class (MonadError(catchError))


-- | The `PEnv` datatype defines a parser environment
newtype PEnv = PEnv {
    toks :: [LToken]
}

-- | check whether the parsing is done based on the list of tokens left. 
done :: PEnv -> Bool
done penv = null $ toks penv

instance ParserEnv PEnv LToken where
    getCol penv = case toks penv of
        { [] -> -1
        ; (tok:_) -> case srcLoc tok of
            { SrcLoc ln col -> col }
        }
    getLine penv = case toks penv of
        { [] -> -1
        ; (tok:_) -> case srcLoc tok of
            { SrcLoc ln col -> ln }
        }
    setTokens ts penv = penv{toks = ts}
    setLine ln penv = penv --  "setLine for PEnv has no write permission."
    setCol col penv = penv --  "setCol for PEnv ihas no write permission."
    isNextTokNewLine penv = case getTokens penv of
           { ((WhiteSpace _ c):_) ->  c == '\n'
           ; _ -> False
           }
    getTokens = toks


-- | The function `parser` is the top level parsing function; 
parser :: Parser PEnv [Stmt]
parser = pStmts 

-- | The function `pStmts` parses zero or more statements separated by some spaces.
pStmts :: Parser PEnv [Stmt]
pStmts = many pOne 
    where 
        pOne = do 
        { pSpaces
        ; s <- pStmt
        ; pSpaces
        ; return s
        }

-- | The function `pStmt` parses one statement 
pStmt :: Parser PEnv Stmt 
pStmt = choice pAssign (choice pRet (choice pNop (choice pIfElse pWhile)))

-- | The function `pNop` parses a nop statement 
pNop :: Parser PEnv Stmt 
pNop = do 
    _ <- sat (\tok -> case tok of { NopKW src -> True ; _ -> False}) "expecting a NOP keyword but none is found."
    pSpaces 
    pSemiColon
    return Nop

-- | The function `pAssign` parses an assignment statement 
pAssign :: Parser PEnv Stmt 
pAssign = do 
    x <- pVar 
    pSpaces 
    pEqual
    pSpaces 
    e <- pExp 
    pSpaces 
    pSemiColon
    return (Assign x e)

-- | The function `pRet` parses a return statement
pRet :: Parser PEnv Stmt 
pRet = do 
    pReturnKW
    pSpaces 
    x <- pVar
    pSpaces
    pSemiColon
    return (Ret x)  


-- | The function`pIfElse` parses an if-else statement
pIfElse :: Parser PEnv Stmt
pIfElse = do 
    pIfKW
    pSpaces 
    e <- pExp 
    pSpaces 
    pLBrace
    s1 <- pStmts 
    pRBrace 
    pSpaces 
    pElseKW 
    pSpaces 
    pLBrace 
    s2 <- pStmts 
    pRBrace
    return (If e s1 s2)

pWhile :: Parser PEnv Stmt 
pWhile = do 
    pWhileKW
    pSpaces 
    e <- pExp 
    pLBrace 
    s <- pStmts 
    pRBrace
    return (While e s) 


-- Lab 1 Task 1.1 

-- | the `pSpace` function parses / skips a space token
pSpace :: Parser PEnv LToken 
pSpace = sat (\tok -> case tok of 
    { WhiteSpace _ _ -> True 
    ; _ -> False 
    }) "expecting a + token but none is found."


-- | the `pSpaces` function parses / skips zero or more space tokens
pSpaces :: Parser PEnv [LToken]
pSpaces = many pSpace

-- Lab 1 Task 1.1 end 


{-  Lab 1 Task 1.2 
    Parsing an expression
    Note that 
    E ::= E Op E | X | C | (E) contains left recursion

    Eliminated:
    T  ::= X | C | (E)
    E  ::= T E'
    E' ::= OP T E' | ε
-}

-- | the `pExp` function parses an expression
pExp :: Parser PEnv Exp 
pExp = do
    first <- pTerm          -- 1. read the first term
    rest  <- many pOpTerm   -- 2. read zero or more pairs
    _     <- pSpaces         -- skip trailing spaces (pWhile needs this!)
    return (foldl combine first rest)   -- 3. combine
    where
        combine acc (op, t) = op acc t


pTerm :: Parser PEnv Exp
pTerm = choice pVarTerm (choice pConstTerm pParenTerm)


pVarTerm :: Parser PEnv Exp
pVarTerm = do
    x <- pVar           
    return (VarExp x)   


pConstTerm :: Parser PEnv Exp
pConstTerm = do
    c <- pConst
    return (ConstExp c)


pParenTerm :: Parser PEnv Exp
pParenTerm = do
    _ <- pLParen
    e <- pExp
    _ <- pRParen
    return (ParenExp e)


pOpTerm :: Parser PEnv (Exp -> Exp -> Exp, Exp)
pOpTerm = attempt (do
    _  <- pSpaces
    op <- pOp      -- read an operator
    _  <- pSpaces
    t  <- pTerm      -- read a term
    return (op, t))


pOp :: Parser PEnv (Exp -> Exp -> Exp)
pOp = choice pPlusOp (choice pMinusOp (choice pMultOp (choice pLThanOp pDEqualOp)))


pPlusOp :: Parser PEnv (Exp -> Exp -> Exp)
pPlusOp = do 
    _ <- pPlus
    return Plus


pMinusOp :: Parser PEnv (Exp -> Exp -> Exp)
pMinusOp = do
    _ <- pMinus
    return Minus


pMultOp :: Parser PEnv (Exp -> Exp -> Exp)
pMultOp = do
    _ <- pMult
    return Mult


pLThanOp :: Parser PEnv (Exp -> Exp -> Exp)
pLThanOp = do
    _ <- pLThan
    return LThan


pDEqualOp :: Parser PEnv (Exp -> Exp -> Exp)
pDEqualOp = do
    _ <- pDEqual
    return DEqual


-- Lab 1 Task 1.2 end 


pPlus :: Parser PEnv LToken
pPlus = sat (\tok -> case tok of 
    { PlusSign _ -> True 
    ; _ -> False 
    }) "expecting a + token but none is found."


pMinus :: Parser PEnv LToken 
pMinus = sat (\tok -> case tok of 
    { MinusSign _ -> True 
    ; _ -> False 
    }) "expecting a - token but none is found."


pMult :: Parser PEnv LToken 
pMult = sat (\tok -> case tok of 
    { AsterixSign _ -> True 
    ; _ -> False 
    }) "expecting a * token but none is found."


pLThan :: Parser PEnv LToken 
pLThan = sat (\tok -> case tok of 
    { LThanSign _ -> True 
    ; _ -> False 
    }) "expecting a < token but none is found."


pDEqual :: Parser PEnv LToken 
pDEqual = sat (\tok -> case tok of 
    { DEqSign _ -> True 
    ; _ -> False 
    }) "expecting a == token but none is found."


pEqual :: Parser PEnv LToken 
pEqual = sat (\tok -> case tok of 
    { EqSign _ -> True 
    ; _ -> False 
    }) "expecting a = token but none is found."



pVar :: Parser PEnv Var
pVar = do 
    tok <- sat (\tok -> case tok of 
        { IdTok src v -> True
        ; _ -> False 
        }) "expecting an identifier but none is found."
    name <- justOrFail tok (\t-> case t of 
        { IdTok src v -> Just v 
        ; _ -> Nothing 
        }) "expecting an identifier but none is found."
    return (Var name)

pConst :: Parser PEnv Const 
pConst = choice pTrue (choice pFalse pInt) 


pTrue :: Parser PEnv Const 
pTrue = do 
    tok <- sat (\tok -> case tok of 
        { TrueKW src -> True 
        ; _ -> False 
        }) "expecting a true keyword but none is found."
    return (BoolConst True)



pFalse :: Parser PEnv Const 
pFalse = do 
    tok <- sat (\tok -> case tok of 
        { FalseKW src -> True 
        ; _ -> False 
        }) "expecting a false keyword but none is found."
    return (BoolConst False)


pInt :: Parser PEnv Const 
pInt = do 
    tok <- sat (\tok -> case tok of 
        { IntTok src v -> True 
        ; _ -> False 
        }) "expecting an integer but none is found."
    i <- justOrFail tok (\t -> case t of 
        { IntTok srv v -> Just v
        ; _ -> Nothing 
        }) "expecting an integer but none is found." 
    return (IntConst i)


-- parsing keywords 

pReturnKW :: Parser PEnv LToken
pReturnKW = sat (\tok -> case tok of 
    { RetKW src -> True 
    ; _ -> False 
    }) "expecting a return keyword but none is found."

pIfKW :: Parser PEnv LToken 
pIfKW = sat (\tok -> case tok of 
    { IfKW src -> True 
    ; _ -> False 
    }) "expecting an if keyword but none is found."

pElseKW :: Parser PEnv LToken 
pElseKW = sat (\tok -> case tok of 
    { ElseKW src -> True 
    ; _ -> False 
    }) "expecting an else keyword but none is found."


pWhileKW :: Parser PEnv LToken 
pWhileKW = sat (\tok -> case tok of 
    { WhileKW src -> True 
    ; _ -> False 
    }) "expecting a while keyword but none is found."


-- parsing symbols

pLBrace :: Parser PEnv LToken 
pLBrace = sat (\tok -> case tok of 
    { LBrace src -> True 
    ; _ -> False 
    }) "expecting a { but none is found."



pRBrace :: Parser PEnv LToken 
pRBrace = sat (\tok -> case tok of 
    { RBrace src -> True 
    ; _ -> False 
    }) "expecting a } but none is found."


pLParen :: Parser PEnv LToken 
pLParen = sat (\tok -> case tok of 
    { LParen src -> True 
    ; _ -> False 
    }) "expecting a ( but none is found."



pRParen :: Parser PEnv LToken 
pRParen = sat (\tok -> case tok of 
    { RParen src -> True 
    ; _ -> False 
    }) "expecting a ) but none is found."




pSemiColon = sat (\tok -> case tok of 
    { SemiColon src -> True 
    ; _ -> False 
    }) "expecting a ; but none is found."
