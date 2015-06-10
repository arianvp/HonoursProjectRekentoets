module Main where
import Data.Maybe
import qualified Data.Set as Set

----------------------------------
--          DATASTRUCTS         --
----------------------------------
-- Expression data type
data Expr = Con Int
	  | Add Expr Expr
	  | Sub Expr Expr 
	  | Mul Expr Expr
	  | Div Expr Expr
	  | Double Double
     deriving (Eq, Read, Ord)
-- Zipper data type
data ZExpr = AddL ZExpr Expr
	   | AddR ZExpr Expr
	   | SubL ZExpr Expr
	   | SubR ZExpr Expr
	   | MulL ZExpr Expr
	   | MulR ZExpr Expr
	   | DivL ZExpr Expr
	   | DivR ZExpr Expr
	   | Top
      deriving (Eq, Show, Read)
-- Location consisting out of an (sub)expression and ZExpr.
type Ctx = (Expr, ZExpr)


instance Show Expr where
	show (Con x)    = show x
	show (Add l r)  = "(" ++ show l ++ " + " ++ show r ++ ")"
	show (Sub l r)  = "(" ++ show l ++ " - " ++ show r ++ ")" 
	show (Mul l r)  = "(" ++ show l ++ " * " ++ show r ++ ")"
	show (Div l r)  = "(" ++ show l ++ " / " ++ show r ++ ")"
	show (Double x) = show x

----------------------------------
--      DATASTRUCTS FUNCS       --
----------------------------------
-- Evaluate the expression.
eval :: Expr -> Double
eval (Con x)    = fromIntegral x
eval (Add x y)  = eval x + eval y
eval (Sub x y)  = eval x - eval y
eval (Mul x y)  = eval x * eval y
eval (Div x y)  = eval x / eval y
eval (Double x) = x

-- Lift an expression to a zipper
toCtx :: Expr -> Ctx
toCtx e = (e, Top)

goLeft :: Ctx -> Maybe Ctx
goLeft (e, ze) = res e
	where res (Add l r) = Just (l, AddL ze r)
	      res (Sub l r) = Just (l, SubL ze r)
	      res (Mul l r) = Just (l, MulL ze r)
	      res (Div l r) = Just (l, DivL ze r)
	      res _         = Nothing

goRight :: Ctx -> Maybe Ctx
goRight (e, ze) = res e
	where res (Add l r) = Just (r, AddR ze l)
	      res (Sub l r) = Just (r, SubR ze l)
	      res (Mul l r) = Just (r, MulR ze l)
	      res (Div l r) = Just (r, DivR ze l)
	      res _         = Nothing

goUp :: Ctx -> Maybe Ctx
goUp (e, ze) = res ze
	where res (AddL ze' r) = Just ((Add e r), ze')
	      res (AddR ze' l) = Just ((Add l e), ze')
	      res (SubL ze' r) = Just ((Sub e r), ze')
	      res (SubR ze' l) = Just ((Sub l e), ze')
	      res (MulL ze' r) = Just ((Mul e r), ze')
	      res (MulR ze' l) = Just ((Mul l e), ze')
	      res (DivL ze' r) = Just ((Div e r), ze')
	      res (DivR ze' l) = Just ((Div l e), ze')
	      res (Top)        = Nothing


-- Unsafe shorthands for goLeft/goRight/goUp.
goLeftU :: Ctx -> Ctx
goLeftU = fromJust . goLeft
goRightU :: Ctx -> Ctx
goRightU = fromJust . goRight
goUpU :: Ctx -> Ctx
goUpU = fromJust . goUp

-- Note: this does not include the ctx itself
trailUp :: Ctx -> [Ctx]
trailUp ctx | goUp ctx == Nothing = []
	    | otherwise           = up : trailUp up
	where up = fromJust $ goUp ctx

getFullExpr :: Ctx -> Expr
getFullExpr (e, Top) = e
getFullExpr ctx      = getFullExpr $ goUpU ctx -- Unsafely go up, should work since the top should be indicated by ZExpr Top

----------------------------------
--          EXAMPLE EXPR        --
----------------------------------
expr1 :: Expr
expr1 = Mul (Con 5) (Add (Con 2) (Add (Con 3) (Con 2)))

expr2 :: Expr
expr2 = Mul (Con 32) (Mul expr2_sub expr2_sub)
	where expr2_sub = Add (Con 1) (Double (-0.25))

expr2' :: Expr -- Matroesjka
expr2' = Mul (Con 32) (Mul expr2_sub expr2_sub)
	where expr2_sub = Sub (Con 1) (Double 0.25)
	
expr3 :: Expr
expr3 = Mul (Add (Con 2) (Con 3)) (Add (Con 5) (Con 7))

expr4 :: Expr
expr4 = Add (Con 1) (Add (Con 2) (Con 3))

expr5 :: Expr
expr5 = Mul (Add (Con 32) (Sub (Con 5) (Add (Con 2) (Con 3)))) (Con 2)

----------------------------------
--          RULES               --
----------------------------------
type Rule = Expr -> Maybe Expr
distR :: Rule
distR (Mul x y) = split y x
	where split (Add x y) e = Just (Add (Mul e x) (Mul e y))
	      split (Sub x y) e = Just (Sub (Mul e x) (Mul e y))
	      split (Div x y) e = Just (Div (Mul e x) y)
	      split _ _         = Nothing
distR _         = Nothing
      
evalR :: Rule
evalR (Add (Con x) (Con y))       = Just $ Con (x + y)
evalR (Add (Con x) (Double y))    = Just $ Double (fromIntegral x + y)
evalR (Add (Double x) (Con y))    = Just $ Double (x + fromIntegral y)
evalR (Add (Double x) (Double y)) = depends (x + y)
evalR (Mul (Con x) (Con y))       = Just $ Con (x * y)
evalR (Mul (Con x) (Double y))    = depends (fromIntegral x * y)
evalR (Mul (Double x) (Con y))    = depends (x * fromIntegral y)
evalR (Mul (Double x) (Double y)) = depends (x * y)
evalR (Sub (Con x) (Con y))       = Just $ Con (x - y)
evalR (Sub (Con x) (Double y))    = Just $ Double (fromIntegral x - y)
evalR (Sub (Double x) (Con y))    = Just $ Double (x - fromIntegral y)
evalR (Sub (Double x) (Double y)) = depends (x - y)
evalR (Div (Con x) (Con y))       = depends (fromIntegral x / fromIntegral y)
evalR (Div (Con x) (Double y))    = depends (fromIntegral x / y)
evalR (Div (Double x) (Con y))    = depends (x / fromIntegral y)
evalR (Div (Double x) (Double y)) = depends (x / y)
evalR _          = Nothing

depends x = Just $ (if integral then Con (round x) else Double x)
	where integral = x == fromIntegral (round x)

commR :: Rule
commR (Add x y)         = Just $ Add y x
commR (Mul x y)         = Just $ Mul y x
commR _                 = Nothing

assocR :: Rule
assocR (Add x (Add y z)) = Just $ Add (Add x y) z
assocR (Mul x (Mul y z)) = Just $ Mul (Mul x y) z
assocR _                 = Nothing

assocbR :: Rule
assocbR (Add (Add x y) z) = Just $ Add x (Add y z)
assocbR (Mul (Mul x y) z) = Just $ Mul x (Mul y z)
assocbR _                 = Nothing

fracR :: Rule
fracR (Double x)        | nice      = Just $ Div (Con (n `div` diver)) (Con (10000 `div` diver))
			| otherwise = Nothing
	where n     = round (x * 10000)
	      nice  = fromIntegral n / 10000 == x
	      diver = gcd 10000 n
fracR _                  = Nothing

apply :: Rule -> Ctx -> [Ctx]
apply r (e,ze) = case r e of
		 Nothing -> []
		 _       -> [(fromJust $ r e, ze)]





----------------------------------
--          SUB-EXPR            --
----------------------------------
subExpr :: Ctx -> [Ctx]
subExpr ctx = case left of
         Nothing -> []
         _       -> [leftU, rightU] ++ subExpr leftU ++ subExpr rightU 
   where left   = goLeft  ctx
         right  = goRight ctx
         leftU  = fromJust left
         rightU = fromJust right

-- Computes the top-level expressions possible per step without duplicates.
subExprS :: [Rule] -> Set.Set Expr -> [Expr] -> [[Expr]]
subExprS rs set []     = []
subExprS rs set xs     = let newSet = Set.union nextLayer set
                         in  xs : subExprS rs newSet nextLayerL
    where subs            = ctxXS ++ concatMap subExpr ctxXS
          transformedExpr = concatMap (\e -> concatMap (\r -> r e) (map apply rs)) subs
          nextLayer       = Set.difference (Set.fromList (map getFullExpr transformedExpr)) set
          nextLayerL      = Set.toList nextLayer
          ctxXS           = map toCtx xs   

----------------------------------
--          STEPS               --
----------------------------------

-- Replace the expression the ctx is focussing on with the given expr.
step :: Ctx -> Expr -> Ctx
step (e, ze) e' = (e', ze)

-- Finds a context that matches the lhs of an equal
findCtx :: Expr -> Expr -> Maybe Ctx
findCtx expr lhs | null candidates  = Nothing
                 | otherwise        = Just $ head candidates
   where tops       = map toCtx $ concat $ subExprS [distR, evalR, assocR, assocbR, fracR] Set.empty [expr]
         subs       = tops ++ concatMap subExpr tops
         candidates = filter (\(e,ze) -> e == lhs) subs


type Equal = (Expr, Expr)
performStep :: Expr -> Equal -> Maybe Expr
performStep e (lhs, rhs) | checkEqual = case ctx of
					     Nothing -> Nothing
					     _       -> Just $ getFullExpr $ step (fromJust ctx) rhs
			 | otherwise  = Nothing
	where checkEqual = eval lhs == eval rhs
	      ctx        = findCtx e lhs

-- Given only an expression, find the steps.
performHalfStep :: Expr -> Expr -> Maybe Expr
performHalfStep e lhs = case ctx of
				Nothing -> Nothing
				_       -> Just $ getFullExpr $ fromJust ctx
	where ctx        = findCtx e lhs



----------------------------------
--          TEST               --
----------------------------------

data Line = Lhs Expr 
	  | Both Expr Expr
type Program = [Line]


n_m :: Program
n_m = [Both (Div (Con 32) (Con 4)) (Con 8),
       Both (Sub (Con 32) (Con 8)) (Con 24),
       Both (Div (Con 24) (Con 4)) (Con 6),
       Both (Sub (Con 24) (Con 6)) (Con 18)]
n_p :: Program
n_p = [Lhs (Con 32),
       Lhs (Con 24),
       Lhs (Con 18)]
n_t :: Program
n_t = [Lhs  (Mul (Double 0.75) (Double 0.75)),
       Both (Mul (Div (Con 3) (Con 4)) (Div (Con 3) (Con 4))) (Div (Con 9) (Con 16)),
       Both (Mul (Div (Con 9) (Con 16)) (Con 32)) (Con 18)]
n_t' = [Lhs  (Mul (Double 0.75) (Double 0.75)),
        Both (Mul (Div (Con 3) (Con 4)) (Div (Con 3) (Con 4))) (Div (Con 9) (Con 16)),
        Both (Mul (Con 32) (Div (Con 9) (Con 16))) (Con 18)]
    
        --Matroesjka exercise
opdr1 = Mul (Con 32) (Mul sub sub)
	where sub = Sub (Con 1) (Double 0.25)        

ex1cor = (opdr1, [uitw1_1, uitw1_2, uitw1_3, uitw1_4, uitw1_5])
    
uitw1_1 :: Program
uitw1_1 = [Both (Div (Con 32) (Con 4)) (Con 8),
	 Both (Sub (Con 32) (Con 8)) (Con 24),
	 Both (Div (Con 24) (Con 4)) (Con 6),
	 Both (Sub (Con 24) (Con 6)) (Con 18)]

uitw1_2 :: Program
uitw1_2 = [Both (Div (Con 32) (Con 4)) (Con 8),
	 Both (Sub (Con 32) (Con 8)) (Con 24),
	 Lhs  (Con 18)]

uitw1_3 :: Program
uitw1_3 = [Both (Sub (Con 32) (Con 8)) (Con 24),
	 Both (Sub (Con 24) (Con 6)) (Con 18)]

uitw1_4 :: Program
uitw1_4 = [Both (Div (Con 32) (Con 4)) (Con 8),
         Both (Div (Con 24) (Con 4)) (Con 6),
         Both (Sub (Con 32) (Con 8)) (Con 24),
         Both (Sub (Con 24) (Con 6)) (Con 18)]
       
uitw1_5 :: Program
uitw1_5 = [Both (Mul (Con 32) (Mul (Double 0.75) (Double 0.75))) (Mul (Con 24) (Double 0.75)),
         Both (Mul (Con 24) (Double 0.75)) (Con 18)]
         


         --Chocolate exercise
opdr2 = Mul (Div (Add (Sub (Con 11) (Con 2)) (Sub (Con 7) (Con 4))) (Add (Con 18) (Con 12))) (Con 100)    

ex2cor = (opdr2, [uitw2_1,uitw2_2])
        
uitw2_1 :: Program
uitw2_1 = [Lhs (Con 9),
           Lhs (Con 3),
           Lhs (Con 12),
           Lhs (Con 30),
           Both (Mul (Div (Con 12) (Con 30)) (Con 100)) (Con 40)]

uitw2_2 :: Program
uitw2_2 = [Both (Add (Con 18) (Con 12)) (Con 30),
           Both (Sub (Con 11) (Con 2)) (Con 9),
           Lhs (Con 3),
           Both (Mul (Div (Con 12) (Con 30)) (Con 100)) (Con 40)]
 
        --Wine exercise
opdr3 = Div (Con 225) (Div (Con 3) (Con 4)) 

ex3cor = (opdr3, [uitw3_1])

uitw3_1 :: Program 
uitw3_1 = [Both (Div (Con 225) (Div (Con 3) (Con 4))) (Mul (Con 225) (Div (Con 4) (Con 3))),
           Both (Mul (Con 225) (Div (Con 4) (Con 3))) (Div (Con 900) (Con 3)),
           Both (Div (Con 900) (Con 3)) (Con 300)]       
           
        --Stamp exercise
opdr4 = Sub (Div (Double 4.74) (Con 6)) (Double 0.25)

ex4cor = (opdr4, [uitw4_1,uitw4_2])

uitw4_1 :: Program 
uitw4_1 = [Both (Div (Double 4.74) (Con 6)) (Double 0.79),
           Both (Sub (Double 0.79) (Double 0.25)) (Double 0.54)]       

uitw4_2 :: Program 
uitw4_2 = [Both (Mul (Double 0.25) (Con 6)) (Double 1.50),
           Both (Sub (Double 4.74) (Double 1.50)) (Double 3.24),
           Both (Div (Double 3.24) (Con 6)) (Double 0.54)]       
           
        --Work pay exercise
opdr5 = Mul (Add mado vr) (Double 4.80)
    where mado = Mul (Sub (Sub (Double 16.5) (Con 8)) (Sub (Double 12.75) (Con 12))) (Con 4)
          vr   = Sub (Sub (Con 14) (Con 7)) (Sub (Con 12) (Double 11.5))

ex5cor = (opdr5, [uitw5_1,uitw5_2,uitw5_3])
          
uitw5_1 :: Program 
uitw5_1 = [Both (Sub (Sub (Add (Mul (Double 8.5) (Con 4)) (Con 7)) (Mul (Con 4) (Double 0.75))) (Double 0.5)) (Sub (Sub (Add (Double 34.0) (Con 7)) (Double 3.0)) (Double 0.5)),
           Both (Sub (Sub (Add (Double 34.0) (Con 7)) (Double 3.0)) (Double 0.5)) (Double 37.5),
           Both (Mul (Double 37.5) (Double 4.80)) (Double 180.0)]       
           
           
uitw5_2 :: Program 
uitw5_2 = [Lhs (Double 8.5),
           Lhs (Double 0.75),
           Both (Mul (Con 4) (Double 8.5)) (Double 34.0),
           Both (Sub (Double 34.0) (Con 3)) (Double 31.0),
           Both (Add (Double 31.0) (Con 7)) (Double 38.0),
           Both (Sub (Double 38.0) (Double 0.5)) (Double 37.5),
           Both (Add (Double 32.0) (Con 2)) (Double 34.0),
           Both (Mul (Double 37.5) (Double 4.80)) (Double 180.0)]       

uitw5_3 :: Program 
uitw5_3 = [Lhs (Double 8.5),
           Lhs (Double 0.75),
           Both (Mul (Con 4) (Double 8.5)) (Add (Double 32.0) (Con 2)),
           Both (Add (Double 32.0) (Con 2)) (Double 34.0),
           Both (Sub (Double 34.0) (Con 3)) (Double 31.0),
           Both (Add (Double 31.0) (Con 7)) (Double 38.0),
           Both (Sub (Double 38.0) (Double 0.5)) (Double 37.5),
           Both (Add (Double 32.0) (Con 2)) (Double 34.0),
           Both (Mul (Double 37.5) (Double 4.80)) (Double 180.0)]   
           
        --Recipe exercise
opdr6 = Mul (Div (Con 600) (Con 800)) (Con 300)   

ex6cor = (opdr6, [uitw6_1])

uitw6_1 :: Program
uitw6_1 = [Both (Div (Con 600) (Con 800)) (Div (Con 3) (Con 4)),
           Both (Mul (Con 300) (Div (Con 3) (Con 4))) (Con 225)]


-- Process function
process :: Expr -> Program -> IO ()
process e []             = putStrLn ("Done: " ++ show e)
process e ((Lhs lhs):xs) = 
             do putStrLn ("Step: " ++ show lhs)
                let e' = performHalfStep e lhs
                case e' of
                     Nothing -> putStrLn ("\tFail")
                     _       -> do putStrLn ("\t" ++ show e')
                                   process (fromJust e') xs
process e ((Both lhs rhs):xs)  =
             do putStrLn ("Step: " ++ show lhs ++ " = " ++ show rhs)
                let e' = performStep e (lhs, rhs)
                case e' of
                     Nothing -> putStrLn ("\tFail")
                     _       -> do putStrLn ("\t" ++ show e')
                                   process (fromJust $ e') xs

process' opdr uitws = do putStrLn (show opdr)
                         putStrLn (replicate 80 '-')
                         mapM_ douitw uitws
    where douitw e = do process opdr e
                        putStrLn ("\n - \n")
                    
processmany :: [(Expr, [Program])] -> IO()
processmany exs = mapM_ doex exs
    where doex (opdr, uitw) = do putStrLn (replicate 80 '-')
                                 process' opdr uitw
                                 putStrLn ("\n")
                        
correct = processmany [ex1cor, ex2cor, ex3cor, ex4cor, ex5cor, ex6cor]    