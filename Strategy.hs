module Strategy where

{-
suitable :: Expr -> [Expr]
suitable e = candidates
    where tops       = map toCtx $ concat $ take 2 $ subExprS [assocR, assocbR, evalR] Set.empty [e]
          subs       = tops ++ concatMap subExpr tops
          candidates = map fst $ filter (\(e,ze) -> simple e) subs

simple :: Expr -> Bool
simple (Add x y)    = constant x && constant y
simple (Sub x y)    = constant x && constant y
simple (Mul x y)    = constant x && constant y
simple (Div x y)    = constant x && constant y
simple _            = False

simpleScore :: Expr -> Expr -> Integer
simpleScore (Con _) (Con _) = 100
simpleScore _       (Con _) = 10
simpleScore (Con _) _       = 10
simpleScore _       _       = 1

evalScore :: Double -> Integer
evalScore x | x == fromIntegral (round x) = 10
        | otherwise                   = 1

niceness :: Expr -> IntegernonEmptySubsequences         :: [a] -> [[a]]
nonEmptySubsequences []      =  []
nonEmptySubsequences (x:xs)  =  [x] : foldr f [] (nonEmptySubsequences xs)
  where f ys r = ys : (x : ys) : r


subs xs = partition g . map f . nonEmptySubsequences $ xs
  where f ys = (ys, xs\\ys)
        g ([_],_) = True
        g _ = False


niceness (Add x y) = (evalScore (eval x + eval y)) * (simpleScore x y) * 2 
niceness (Sub x y) = (evalScore (eval x - eval y)) * (simpleScore x y)
niceness (Mul x y) = (evalScore (eval x * eval y)) * (simpleScore x y)
niceness (Div x y) = (evalScore (eval x / eval y)) * (simpleScore x y)
niceness _         = 0

niceSort :: Expr -> Expr -> Ordering
niceSort a b = compare (niceness b) (niceness a)

lineRule :: I.Rule Expr
lineRule = I.describe "Execute a line from a program" $ I.makeRule "program.line" f
    where
        f :: Expr -> Maybe Expr
        f (Con _)    = Nothing
        f (Double _) = Nothing
        f e          = let sorted = sortBy niceSort $ suitable e
                           lhs = head $ sorted
                           rhs = fromJust $ depends $ eval lhs
                       in if [] == sorted then Nothing else performStep e (lhs, rhs)

------- Traversal

addSymbol = I.newSymbol "add"
subSymbol = I.newSymbol "subtract"
mulSymbol = I.newSymbol "multiply"
divSymbol = I.newSymbol "divide"
dubSymbol = I.newSymbol "double"

instance I.IsTerm Expr where
    toTerm (Con x)    = I.TNum (toInteger x)
    toTerm (Add x y)  = I.binary addSymbol (I.toTerm x) (I.toTerm y)
    toTerm (Sub x y)  = I.binary subSymbol (I.toTerm x) (I.toTerm y)
    toTerm (Mul x y)  = I.binary mulSymbol (I.toTerm x) (I.toTerm y)
    toTerm (Div x y)  = I.binary divSymbol (I.toTerm x) (I.toTerm y)
    --toTerm (Double x) = I.TNum (toInteger x)
    
    fromTerm (I.TNum x) = return (Con (fromInteger x))
    fromTerm term       = I.fromTermWith f term
        where
            f s [x, y] | s == addSymbol    = return (Add x y)
            f s [x, y] | s == subSymbol    = return (Sub x y)
            f s [x, y] | s == mulSymbol    = return (Mul x y)
            f s [x, y] | s == divSymbol    = return (Div x y)
            f _ _ = fail "invalid expression"

-------
programStrat :: I.LabeledStrategy (I.Context Expr)
programStrat = I.label "do-it" $
    I.repeatS (I.liftToContext lineRule)

minimalExercise :: I.Exercise Expr
minimalExercise = I.emptyExercise
   { I.exerciseId    = I.describe "Evaluate an expression (minimal)" $
                        I.newId "eval.minimal"
   , I.strategy      = programStrat
   , I.prettyPrinter = show
   }

   -}
