module Main where
import Data.Maybe

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
     deriving (Eq, Read)
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

getFocus :: Ctx -> Expr
getFocus (e, _) = e

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

expr2' :: Expr
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
evalR (Con _)    = Nothing
evalR (Double _) = Nothing
evalR x          = Just $ (if integral then Con (round value) else Double value)
	where value    = eval x
	      integral = value == fromIntegral (round value)

commR :: Rule
commR (Add x y)         = Just $ Add y x
commR (Mul x y)         = Just $ Mul y x
commR _                 = Nothing

commbR :: Rule
commbR (Add x (Add y z)) = Just $ Add (Add x y) z
commbR (Mul x (Mul y z)) = Just $ Mul (Mul x y) z
commbR _                 = Nothing

commcR :: Rule
commcR (Add (Add x y) z) = Just $ Add x (Add y z)
commcR (Mul (Mul x y) z) = Just $ Mul x (Mul y z)
commcR _                 = Nothing

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
		   _       -> [fromJust left, fromJust right]
	where left  = goLeft  ctx
	      right = goRight ctx
subExpr' ctx = case left of
         Nothing -> []
         _       -> [leftU, rightU] ++ subExpr' leftU ++ subExpr' rightU 
   where left   = goLeft  ctx
         right  = goRight ctx
         leftU  = fromJust left
         rightU = fromJust right


subExprR :: [Rule] -> Ctx -> [Ctx]
subExprR rs e = layerExprs ++ upExprs ++ concatMap (subExprR rs) (subExprs ++ rightUpExpr)
	where -- Apply the rules and get the transformed expressions
	      transformedExpr  = concatMap (\r -> r e) (map apply rs)
	      -- The expressions for this layer, is e + all transformed expressions.
	      layerExprs       = e : transformedExpr
	      -- Go back upwards to add the injected transformed expressions.
	      upExprs          = concatMap trailUp transformedExpr
	      leftSidedUpExpr  = filter leftSided transformedExpr
	      rightUpExpr      = map rightExpr leftSidedUpExpr
	      -- Compute the sub-expressions of the expression and transformed expression (next-layer)
	      subExprs         = concatMap subExpr layerExprs


	      
subExprS :: [Rule] -> Ctx -> [[Ctx]]
subExprS rs e = transformedExpr : concatMap (subExprS rs) transformedExpr
    where transformedExpr = concatMap (\e -> concatMap (\r -> r e) (map apply rs)) (e : subExpr' e)
	      
-- Utility funcs
leftSided (_, (AddL _ _)) = True
leftSided (_, (SubL _ _)) = True
leftSided (_, (MulL _ _)) = True
leftSided _               = False

rightExpr (e, (AddL ze e')) = (e', (AddR ze e))
rightExpr (e, (SubL ze e')) = (e', (SubR ze e))
rightExpr (e, (MulL ze e')) = (e', (MulR ze e))
rightExpr _                 = error "Has no right expression!"
	      

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
	where subs       = subExprR [distR, evalR, commR, commbR, commcR, fracR] $ toCtx expr
	      candidates = filter x(\(e,ze) -> e == lhs) subs


type Equal = (Expr, Expr)
performStep :: Expr -> Equal -> Expr
performStep e (lhs, rhs) | checkEqual = case ctx of
					     Nothing -> error "No matching sub-expression"
					     _       -> getFullExpr $ step (fromJust ctx) rhs
			 | otherwise  = error "Invalid equal"
	where checkEqual = eval lhs == eval rhs
	      ctx        = findCtx e lhs

-- Given only an expression, find the steps.
performHalfStep :: Expr -> Expr -> Expr
performHalfStep e lhs = case ctx of
				Nothing -> error "No matching sub-expression"
				_       -> getFullExpr $ fromJust ctx
	where ctx        = findCtx e lhs

-------------------------------------------------------------------	
-----------VOORBEELD UITWERKINGEN VANAF HIER-----------------------
-------------------------------------------------------------------
	
uitw1 :: [Step]
uitw1 = [ Both (Div (Con 32) (Con 4), Con 8), Both (Sub (Con 32) (Con 8) , Con 24), Both (Div (Con 24) (Con 4), Con 6), Both (Sub (Con 24) (Con 6), Con 18)]

uitw2 :: [Step]
uitw2 = [ Both (Div (Con 32) (Con 4), Con 8), Both (Sub (Con 32) (Con 8), Con 24), Lhs (Con 18)]

uitw3 :: [Step]
uitw3 = [Both(Sub (Con 32) (Con 8), Con 24), Both (Sub (Con 24) (Con 6), Con 18)]


-----VISSENKOM-------

uitw4 :: [Step]
uitw4 = [Both (Mul (Con 8) (Mul (Con 4) (Con 5)), Con 160),Both (Div (Con 160) (Con 5), Con 32)]

-----FLESSEN---------

uitw5 :: [Step]
uitw5 = [Both (Div (Con 225) (Div (Con 3) (Con 4)),Mul (Con 225) (Div (Con 4) (Con 3))),
		Both (Mul (Con 225) (Div (Con 4) (Con 3)),Div (Con 900) (Con 3)),
		Both (Div (Con 900) (Con 3)),Con 300)]
		
uitw6 :: [Step --Fout--
uitw6 = [Both (Div (Con 225) (Div (Con 3) (Con 4)),Mul (Con 225) (Div (Con 4) (Con 3))),
		Both (Mul (Con 225) (Div (Con 4) (Con 3)),Div (Con 800) (Con 3)),
		Both (Div (Con 800) (Con 3)),Con 266)]
		
-----OLIEBOLLEN--------
uitw7 :: [Step]
uitw7 = [Both (Mul (Con 300) (Div (Con 3) (Con 4))),Div (Con 900) (Con 4)) ,Both (Div (Con 900) (Con 4),Con 225)]	