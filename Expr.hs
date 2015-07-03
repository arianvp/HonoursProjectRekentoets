{-# LANGUAGE DeriveDataTypeable #-}

module Expr where

import Bag (Bag)
import qualified Bag as B
import Data.Maybe
import Data.Typeable
import Data.List (subsequences, nub, (\\), partition, intercalate, sortBy)
import Control.Arrow as A (first, (***))

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
         AddI   ZExpr (Bag Expr)
       | NegI   ZExpr
       | MulI   ZExpr (Bag Expr)
       | DivI   ZExpr
       | Top
      deriving (Eq, Show, Read)
-- Location consisting out of an (sub)expression and ZExpr.
type Ctx = (Expr, ZExpr)

-- Check the type of expression
isAdd, isMul, isConst, isNeg :: Expr -> Bool
isAdd (Add _) = True
isAdd _       = False

isMul (Mul _) = True
isMul _       = False

isNeg (Negate x) = True
isNeg _          = False

isConst (Con _)    = True
isConst (Double _) = True
isConst (Negate x) = isConst x
isConst _          = False

instance Show Expr where
    show (Con x)    = show x
    show (Add xs)   = "(" ++ intercalate " + " (map show (B.toList xs)) ++ ")"
    show (Negate x) = "-[" ++ show x ++ "]"
    show (Mul xs)   = "(" ++ intercalate " * " (map show (B.toList xs)) ++ ")"
    show (Div x)    = "1/" ++ show x
    show (Double x) = show x

----------------------------------
--      DATASTRUCTS FUNCS       --
----------------------------------
-- Evaluate the expression.
eval :: Expr -> Double
eval (Con x)    = fromIntegral x
eval (Add xs)   = B.fold (\ a b -> eval a + b) 0 xs
eval (Negate x) = negate $ eval x
eval (Mul xs)   = B.fold (\ a b -> eval a * b) 1 xs
eval (Div x)    = 1.0 / eval x
eval (Double x) = x
      
-- Lift an expression to a zipper
toCtx :: Expr -> Ctx
toCtx e = (e, Top)

depends :: Double -> Expr
depends x = if integral then Con (round x) else Double x
    where integral = x == fromIntegral (round x)

goDown :: Ctx -> [Ctx]
goDown (e, ze) = res e
    where 
            res :: Expr -> [Ctx]
            res (Add xs)   | isAddI ze = []
                           | otherwise =
                             let (single, combinations) = subs' xs
                                 single' = map (B.headL e A.*** AddI ze) single
                                 comb'   = map (Add A.*** AddI ze) combinations
                             in comb' ++ single'
            res (Negate (Mul xs)) = map (A.first Negate) (res (Mul xs)) ++ [(Mul xs, NegI ze)]
            res (Negate x) = [(x, NegI ze)]
            res (Mul xs)   | isMulI ze = []
                           | otherwise =
                             let (single, combinations) = subs' xs
                                 single' = map (B.headL A.*** MulI ze) single
                                 comb'   = map (Mul     A.*** MulI ze) combinations
                             in comb' ++ single' 
            res (Div x)    = [(x, DivI ze)]
            res _          = []
            isAddI (AddI _ _) = True
            isAddI _          = False
            isMulI (MulI _ _) = True
            isMulI _          = False

goUp :: Ctx -> Maybe Ctx
goUp (e, ze) = res ze
    where res (AddI ze' xs) | isAdd e   = Just (Add (B.union xs (comps e)), ze')
                            | otherwise = Just (Add (B.insert e xs), ze')
          res (NegI ze')    = Just (Negate e, ze')
          res (MulI ze' xs) | isMul e   = Just (Mul (B.union xs (comps e)), ze')
                            | otherwise = Just (Mul (B.insert e xs), ze')
          res (DivI ze')    = Just (Div e, ze')
          res (Top)         = Nothing
          comps (Add xs)    = xs
          comps (Mul xs)    = xs


-- Unsafe shorthands for goUp.
goUpU :: Ctx -> Ctx
goUpU = fromJust . goUp

getFullExpr :: Ctx -> Expr
getFullExpr (e, Top) = e
getFullExpr ctx      = getFullExpr $ goUpU ctx -- Unsafely go up, should work since the top should be indicated by ZExpr Top

nonEmptySubsequences         :: [a] -> [[a]]
nonEmptySubsequences []      =  []
nonEmptySubsequences (x:xs)  =  [x] : foldr f [] (nonEmptySubsequences xs)
  where f ys r = ys : (x : ys) : r

subs xs = partition g . map f . nonEmptySubsequences $ xs
  where f ys = (ys, xs\\ys)
        g ([_],_) = True
        g _       = False

subs' :: Ord a => Bag a -> ([(Bag a, Bag a)], [(Bag a, Bag a)])      
subs' bag | B.null bag = ([],[])
          | otherwise  = partition (\(a,b) -> B.isSingleton a) $ f xs bag
    where xs           = B.toList bag
          f [x] bag    = [(B.singleton x, B.delete x bag)]
          f (x:xs) bag = let bag' = B.delete x bag
                             subs = f xs bag'
                             in (B.singleton x, bag') : concatMap (\(a,b) -> [(B.insert x a, b), (a, B.insert x b)]) subs
                             
----------------------------------
--      Normalisation           --
----------------------------------
        
{-  Forces an expression into normal form
    In particular, the following things are fixed:
        - Remove double negations
        - Additions contained in additions should be flattened
        - Multiplications in multiplications should be flattened
        - Singleton multiplications/additions should be flattened
 -}
normalise :: Expr -> Expr
normalise = flip fixPointNormalise Nothing

-- normalization pass flattening nested adds and muls
normalise1 e@(Add _) = normaliseAssocRule isAdd Add (\(Add b) -> b) e
normalise1 e@(Mul _) = normaliseAssocRule isMul Mul (\(Mul b) -> b) e
normalise1 (Con x)    | x < 0     = Negate (Con (negate x))
                      | otherwise = Con x
normalise1 (Double x) | x < 0     = Negate (Double (negate x))
                      | otherwise = Double x
normalise1 (Negate e) = Negate $ normalise1 e
normalise1 (Div e)    = Div $ normalise1 e

--
normalise2 (Add xs) = Add       . B.filter (filterExpr 0) $ B.map normalise2 xs
normalise2 (Mul xs) = normalMul . B.filter (filterExpr 1) $ B.map normalise2 xs
normalise2 (Negate (Negate e)) = normalise2 e
normalise2 (Negate (Mul xs))   | isNeg mul' = (\(Negate mul) -> mul) mul'
                               | otherwise  = Negate mul'
    where mul' = normalise2 (Mul xs)
normalise2 (Negate e)          = Negate $ normalise2 e
--normalise2 (Div (Con x))       = Double (1.0 / fromIntegral x)
--normalise2 (Div (Double x))    = depends $ 1.0 / x
normalise2 (Div e)             = Div $ normalise2 e
normalise2 e = e

fixPointNormalise :: Expr -> Maybe Expr -> Expr
fixPointNormalise e Nothing = let k = normalise2 . normalise1 $ e
                              in fixPointNormalise k (Just e)
fixPointNormalise e (Just e')
  | e == e'   = e
  | otherwise = let k = normalise2 . normalise1 $ e
                in fixPointNormalise k (Just e)

-- Put the multiplication into normal form (negative or positive multiplication)
normalMul :: Bag Expr -> Expr
normalMul xs | even $ count xs = Mul xs'
             | otherwise       = Negate $ Mul xs'
    where g (Negate x) = x
          g x          = x
          xs'          = B.map g xs
          count        = B.size . B.filter isNeg

-- Filter the epxressions from a 
filterExpr :: Integer -> Expr -> Bool
filterExpr ido expr = not (((isMul expr || isAdd expr) && isEmpty expr) || (isConst expr && not (check expr)) || (ido == 1 && useless expr))
    where check (Con x)    = toInteger x /= ido
          check (Double x) = x /= fromInteger ido
          check (Negate e)    | ido == 0  = check e -- Addition
                              | otherwise = True    -- Multiplication
          isEmpty (Add xs) = B.null xs
          isEmpty (Mul xs) = B.null xs
          isEmpty _        = False
          useless (Div (Con 1)) = True
          useless _        = False

-- Given an associative rule (determined by a rule matcher, a constructor
-- and an extractor (for the contained bag)) and an expression, normalises  
-- all sub expressions, then flattens all occurences of the rule.
-- (because who wants to write duplicate functions for Add and Mul?!?)
normaliseAssocRule :: (Expr -> Bool) -> (Bag Expr -> Expr) -> (Expr -> Bag Expr) -> Expr -> Expr
normaliseAssocRule match construct extract e
        | B.size asList == 1 = head $ B.toList asList   -- normalisation has already happened
        | otherwise        = construct allOthers
    where asList            = B.map normalise1 $ extract e
          (matches, others) = B.partition match asList
          allOthers         = others `B.union` B.unionsMap extract matches


