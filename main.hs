module Main where
import Data.Maybe

----------------------------------
--          DATASTRUCTS         --
----------------------------------
-- Expression data type
data Expr = Con Int
	  | Add Expr Expr
	  | Mul Expr Expr
     deriving (Eq, Read)
-- Zipper data type
data ZExpr = AddL ZExpr Expr
	   | AddR ZExpr Expr
	   | MulL ZExpr Expr
	   | MulR ZExpr Expr
	   | Top
      deriving (Eq, Show, Read)
-- Location consisting out of an (sub)expression and ZExpr.
type Ctx = (Expr, ZExpr)


instance Show Expr where
	show (Con x)   = show x
	show (Add l r) = "(" ++ show l ++ " + " ++ show r ++ ")" 
	show (Mul l r) = "(" ++ show l ++ " * " ++ show r ++ ")" 

----------------------------------
--      DATASTRUCTS FUNCS       --
----------------------------------
-- Evaluate the expression.
eval :: Expr -> Int
eval (Con x)   = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

-- Lift an expression to a zipper
toCtx :: Expr -> Ctx
toCtx e = (e, Top)

getFocus :: Ctx -> Expr
getFocus (e, _) = e

goLeft :: Ctx -> Maybe Ctx
goLeft (e, ze) = res e
	where res (Add l r) = Just (l, AddL ze r)
	      res (Mul l r) = Just (l, MulL ze r)
	      res _         = Nothing

goRight :: Ctx -> Maybe Ctx
goRight (e, ze) = res e
	where res (Add l r) = Just (r, AddR ze l)
	      res (Mul l r) = Just (r, MulR ze l)
	      res _         = Nothing

goUp :: Ctx -> Maybe Ctx
goUp (e, ze) = res ze
	where res (AddL ze' r) = Just ((Add e r), ze')
	      res (AddR ze' l) = Just ((Add l e), ze')
	      res (MulL ze' r) = Just ((Mul e r), ze')
	      res (MulR ze' l) = Just ((Mul l e), ze')
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
getFullExpr ctx      = fst $ head $ trailUp ctx

----------------------------------
--          EXAMPLE EXPR        --
----------------------------------
expr1 :: Expr
expr1 = Mul (Con 5) (Add (Con 2) (Add (Con 3) (Con 2)))





----------------------------------
--          RULES               --
----------------------------------
type Rule = Expr -> Maybe Expr
distR :: Rule
distR (Mul x y) = split y x
	where split (Add x y) e = Just (Add (Mul e x) (Mul e y))
	      split (Mul x y) e = Just (Mul (Mul e x) (Mul e y))
	      split _ _         = Nothing
distR _         = Nothing

evalR :: Rule
evalR (Con _) = Nothing
evalR x       = Just $ Con (eval x)


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


subExprR :: [Rule] -> Ctx -> [Ctx]
subExprR rs e = layerExprs ++ upExprs ++ concatMap (subExprR rs) subExprs
	where -- Apply the rules and get the transformed expressions
	      transformedExpr = concatMap (\r -> r e) (map apply rs)
	      -- The expressions for this layer, is e + all transformed expressions.
	      layerExprs      = e : transformedExpr
	      -- Compute the sub-expressions of the expression and transformed expression (next-layer)
	      subExprs        = concatMap subExpr layerExprs
	      -- Go back upwards to add the injected transformed expressions.
	      upExprs         = concatMap trailUp transformedExpr
	      

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
	where subs       = subExprR [distR, evalR] $ toCtx expr
	      candidates = filter (\(e,ze) -> e == lhs) subs


type Equal = (Expr, Expr)
performStep :: Expr -> Equal -> Expr
performStep e (lhs, rhs) | checkEqual = case ctx of
					     Nothing -> error "No matching sub-expression"
					     _       -> getFullExpr $ step (fromJust ctx) rhs
			 | otherwise  = error "Invalid equal"
	where checkEqual = eval lhs == eval rhs
	      ctx        = findCtx e lhs