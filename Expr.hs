{-# LANGUAGE DeriveDataTypeable #-}

module Expr where

import Bag
import Data.Maybe
import Data.Typeable
import Data.List (subsequences, nub, (\\), partition, intersperse, sortBy)

-- Expression data type
data Expr = 
        Con    Int         -- Positive only
      | Add    (Bag Expr)  -- Addition of the elements
      | Negate Expr        -- Unary negation
      | Mul    (Bag Expr)  -- Multiplication of the elements
      | Div    Expr        -- Unary division
      | Double Double      -- Positive only
     deriving (Eq, Read, Ord, Typeable)
-- Zipper data type
data ZExpr = 
        AddI   ZExpr [Expr]
       | NegI   ZExpr
       | MulI   ZExpr [Expr]
       | DivI   ZExpr
       | Top
      deriving (Eq, Show, Read)
-- Location consisting out of an (sub)expression and ZExpr.
type Ctx = (Expr, ZExpr)

-- Check the type of expression
isAdd, isMul :: Expr -> Bool
isAdd (Add _) = True
isAdd _ = False
isMul (Mul _) = True
isMul _ = False

instance Show Expr where
    show (Con x)    = show x
    show (Add xs)   = "(" ++ (concat $ intersperse " + " (map show (toList xs))) ++ ")"
    show (Negate x) = "-[" ++ show x ++ "]"
    show (Mul xs)   = "(" ++ (concat $ intersperse " * " (map show (toList xs))) ++ ")"
    show (Div x)    = "1/" ++ show x
    show (Double x) = show x


----------------------------------
--      DATASTRUCTS FUNCS       --
----------------------------------
-- Evaluate the expression.
eval :: Expr -> Double
eval (Con x)    = fromIntegral x
eval (Add xs)   = sum (map eval $ toList xs)
eval (Negate x) = negate $ eval x
eval (Mul xs)   = product (map eval $ toList xs)
eval (Div x)    = 1.0 / eval x
eval (Double x) = x

isConst :: Expr -> Bool
isConst (Con _)    = True
isConst (Double _) = True
isConst (Negate x) = isConst x
isConst _          = False

isNeg :: Expr -> Bool
isNeg (Negate x) = True
isNeg _          = False
      
-- Lift an expression to a zipper
toCtx :: Expr -> Ctx
toCtx e = (e, Top)

goDown :: Ctx -> [Ctx]
goDown (e, ze) = res e
    where 
            res :: Expr -> [Ctx]
            res (Add xs)   | isAddI ze = []
                           | otherwise =
                             let (single, combinations) = subs $ toList xs
                                 single' = map (\([e], es) -> (e,                AddI ze es)) single
                                 comb'   = map (\(e,   es) -> (Add $ fromList e, AddI ze es)) combinations
                             in comb' ++ single'
            res (Negate (Mul xs)) = (map (\(e,ze) -> (Negate e,ze)) $ res (Mul xs)) ++ [((Mul xs), NegI ze)]
            res (Negate x) = [(x, NegI ze)]
            res (Mul xs)   | isMulI ze = []
                           | otherwise =
                             let (single, combinations) = subs $ toList xs
                                 single' = map (\([e], es) -> (e,                MulI ze es)) single
                                 comb'   = map (\(e,   es) -> (Mul $ fromList e, MulI ze es)) combinations
                             in comb' ++ single' 
            res (Div x)    = [(x, DivI ze)]
            res _          = []
            isAddI (AddI _ _) = True
            isAddI _          = False
            isMulI (MulI _ _) = True
            isMulI _          = False


nonEmptySubsequences         :: [a] -> [[a]]
nonEmptySubsequences []      =  []
nonEmptySubsequences (x:xs)  =  [x] : foldr f [] (nonEmptySubsequences xs)
  where f ys r = ys : (x : ys) : r

subs xs = partition g . map f . nonEmptySubsequences $ xs
  where f ys = (ys, xs\\ys)
        g ([_],_) = True
        g _       = False

goUp :: Ctx -> Maybe Ctx
goUp (e, ze) = res ze
    where res (AddI ze' xs) | isAdd e   = Just (Add (insertList xs (comps e)), ze')
                            | otherwise = Just (Add (fromList (e : xs)), ze')
          res (NegI ze')    = Just (Negate e, ze')
          res (MulI ze' xs) | isMul e   = Just (Mul (insertList xs (comps e)), ze')
                            | otherwise = Just (Mul (fromList (e : xs)), ze')
          res (DivI ze')    = Just (Div e, ze')
          res (Top)         = Nothing
          isAdd (Add _)     = True
          isAdd _           = False
          isMul (Mul _)     = True
          isMul _           = False
          comps (Add xs)    = xs
          comps (Mul xs)    = xs


-- Unsafe shorthands for goUp.
goUpU :: Ctx -> Ctx
goUpU = fromJust . goUp

getFullExpr :: Ctx -> Expr
getFullExpr (e, Top) = e
getFullExpr ctx      = getFullExpr $ goUpU ctx -- Unsafely go up, should work since the top should be indicated by ZExpr Top


{-  Forces an expression into normal form
    In particular, the following things are fixed:
        - Remove double negations
        - Additions contained in additions should be flattened
        - Multiplications in multiplications should be flattened
        - Singleton multiplications/additions should be flattened
 -}
normalise :: Expr -> Expr
normalise = fixPointNormalise

-- normalization pass flattening nested adds and muls
normalise1 e@(Add _) = normaliseAssocRule isAdd Add (\ (Add b) -> b) e
normalise1 e@(Mul _) = normaliseAssocRule isMul Mul (\ (Mul b) -> b) e
normalise1 (Con x)    | x < 0     = Negate (Con (negate x))
                      | otherwise = Con x
normalise1 (Double x) | x < 0     = Negate (Double (negate x))
                      | otherwise = Double x
normalise1 (Negate e) = (Negate $ normalise1 e)
normalise1 (Div e)    = (Div $ normalise1 e)
--normalise1 e = e
--
normalise2 (Add xs) = Add $ fromList $ filter (filterExpr 0) $ map normalise2 (toList xs)
normalise2 (Mul xs) =      normalMul $ filter (filterExpr 1) $ map normalise2 (toList xs)
normalise2 (Negate (Negate e)) = normalise2 e
normalise2 (Negate (Mul xs))   = let mul' = normalise2 (Mul xs)
				 in if isNeg mul'
                                      then (\(Negate mul) -> mul) mul'
                                      else Negate mul'
normalise2 (Negate e)          = (Negate $ normalise2 e)
--normalise2 (Div (Con x))       = Double (1.0 / fromIntegral x)
--normalise2 (Div (Double x))    = depends $ 1.0 / x
normalise2 (Div e)             = (Div $ normalise2 e)
normalise2 e = e

ntest = Add $ fromList [Con 0, Mul $ fromList [Con (-1), Con 6]]

fixPointNormalise :: Expr -> Expr
fixPointNormalise = flip fixPointNormalise' Nothing

fixPointNormalise' :: Expr -> Maybe Expr -> Expr
fixPointNormalise' e Nothing = let k = normalise2 . normalise1 $ e
                              in fixPointNormalise' k (Just e)
fixPointNormalise' e (Just e')
  | e == e'   = e
  | otherwise = let k = normalise2 . normalise1 $ e
                in fixPointNormalise' k (Just e)

-- Put the multiplication into normal form
normalMul :: [Expr] -> Expr
normalMul xs     = f xs [] False
	where f [] res c | c == True  = Negate (Mul $ fromList res)
	                 | otherwise  =         Mul $ fromList res
	      f ((Negate x):xs) res c = f xs (x:res) (not c)
	      f (x:xs)          res c = f xs (x:res) c

-- Filter the epxressions from a 
filterExpr :: Integer -> Expr -> Bool
filterExpr ido expr = not (((isMul expr || isAdd expr) && isEmpty expr) || (isConst expr && not (check expr)) || (ido == 1 && useless expr))
    where check (Con x)    = (toInteger x) /= ido
          check (Double x) = x /= fromInteger ido
          check (Negate e)    | ido == 0  = check e -- Addition
                              | otherwise = True    -- Multiplication
          isEmpty (Add xs) = null $ toList xs
          isEmpty (Mul xs) = null $ toList xs
          isEmpty _        = False
          useless (Div (Con 1)) = True
          useless _        = False

-- Given an associative rule (determined by a rule matcher, a constructor
-- and an extractor (for the contained bag)) and an expression, normalises  
-- all sub expressions, then flattens all occurences of the rule.
-- (because who wants to write duplicate functions for Add and Mul?!?)
normaliseAssocRule :: (Expr -> Bool) -> ((Bag Expr) -> Expr) -> (Expr -> (Bag Expr)) -> Expr -> Expr
normaliseAssocRule match construct extract e
        | (length asList == 1) = asList !! 0   -- normalisation has already happened
        | otherwise            = construct $ fromList allOthers
    where asList = map normalise1 $ toList $ extract e
          others = filter match asList
          getList = (\ b -> toList (extract b) )
          allOthers = (asList \\ others) ++ (concatMap getList others)

depends :: Double -> Expr
depends x = if integral then Con (round x) else Double x
    where integral = x == fromIntegral (round x)
