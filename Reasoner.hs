{-# LANGUAGE DeriveDataTypeable #-}

module Expr where

import Bag
import Data.Maybe
import Data.Typeable
import Data.List (subsequences, nub, (\\), partition, intersperse, sortBy)
import Ideas.Common.Library
import Ideas.Main.Default
import qualified Data.Set as Set
-------------------------------------------------------------------------------
------- DataType
-------------------------------------------------------------------------------
-- Expression data type
data Expr = 
        Con    Int         -- Positive only
      | Add    (Bag Expr)  -- Addition of the elements
      | Negate Expr        -- Unary negation
      | Mul    (Bag Expr)  -- Multiplication of the elements
      | Div    Expr        -- Unary division
      | Double Double      -- Positive only
     deriving (Eq, Read, Ord, Typeable)

instance Show Expr where
    show (Con x)    = show x
    show (Add xs)   = "(" ++ (concat $ intersperse " + " (map show (toList xs))) ++ ")"
    show (Negate x) = "-[" ++ show x ++ "]"
    show (Mul xs)   = "(" ++ (concat $ intersperse " * " (map show (toList xs))) ++ ")"
    show (Div x)    = "1/" ++ show x
    show (Double x) = show x

isConst :: Expr -> Bool
isConst (Con _)    = True
isConst (Double _) = True
isConst (Negate x) = isConst x
isConst _          = False

isNeg :: Expr -> Bool
isNeg (Negate x) = True
isNeg _          = False


isAdd, isMul :: Expr -> Bool
isAdd (Add _) = True
isAdd _ = False

isMul (Mul _) = True
isMul _ = False

eqExpr :: Expr -> Expr -> Bool
eqExpr x y = eval x == eval y

-------------------------------------------------------------------------------
-------Traversal
-------------------------------------------------------------------------------
addSymbol = newSymbol "add"
negSymbol = newSymbol "negate"
mulSymbol = newSymbol "multiply"
divSymbol = newSymbol "divide"

instance IsTerm Expr where
    toTerm (Con x)    = TNum (toInteger x)
    toTerm (Add xs)   = TCon addSymbol $ map toTerm $ toList xs
    toTerm (Negate x) = unary negSymbol $ toTerm x
    toTerm (Mul xs)   = TCon mulSymbol $ map toTerm $ toList xs
    toTerm (Div x)    = unary divSymbol $ toTerm x
    toTerm (Double x) = TFloat x
    
    fromTerm (TNum x)   = return (Con (fromInteger x))
    fromTerm (TFloat x) = return (Double x)
    fromTerm term       = fromTermWith f term
        where
            f s [x] | s == negSymbol    = return (Negate x)
            f s [x] | s == divSymbol    = return (Div x)
            f s xs  | s == addSymbol    = return (Add $ fromList xs)
            f s xs  | s == mulSymbol    = return (Mul $ fromList xs)
            f _ _ = fail "invalid expression"
-------------------------------------------------------------------------------
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
   
depends :: Double -> Expr
depends x = if integral then Con (round x) else Double x
    where integral = x == fromIntegral (round x)

distR :: Rule Expr
distR = describe "Distribute factor over an addition" $ ruleList "distr" distR'
distR':: Expr -> [Expr]
distR' (Mul multies)   = map convert $ concatMap dist_optos (addies ++ negaddies)
    where addies           = filter isAdd    $ toListU multies
          negaddies        = filter isNegAdd $ toListU multies  
          othas e          = toList (remove' e multies)
          dist_optos e     = map (\opt -> (opt, e)) $ othas e
          convert (mul, add@(Add xs)) = 
                    let distedAdd = Add (fromList $ map (\x -> Mul $ fromList [mul, x]) $ toList xs)
                    in Mul $ fromList (distedAdd : (toList multies \\ [mul, add]))
          convert (mul, e@(Negate add@(Add xs))) = 
                    let distedAdd = Add (fromList $ map (\x -> Mul $ fromList [mul, x]) $ toList xs)
		    in Negate $ Mul $ fromList (distedAdd : (toList multies \\ [mul, e]))
          isNegAdd (Negate (Add _)) = True
          isNegAdd _                = False
distR' _         = []

combinations :: Int -> [a] -> [[a]]
combinations k xs = combinations' (length xs) k xs
	where combinations' n k' [] = []
	      combinations' n k' l@(y:ys)
		| k' == 0   = [[]]
		| k' >= n   = [l]
		| null l    = []
		| otherwise = map (y :) (combinations' (n - 1) (k' - 1) ys) ++ combinations' (n - 1) k' ys 

evalR :: Rule Expr
evalR = describe "Evaluate expression" $ ruleList "eval" evalR'
evalR' :: Expr -> [Expr]
evalR' x       = f x
    where f (Add xs)   | length (toList xs) == 2 = compute sum     $ candids xs
                       | otherwise               =
                          map (\x -> Add  $ fromList ((depends $ sum     $ map eval x) : (toList xs \\ x))) $ candids xs
          f (Mul xs)   | length (toList xs) == 2 =
                          compute product $ candids xs
                       | otherwise               =
                          map (\x -> Mul  $ fromList ((depends $ product $ map eval x) : (toList xs \\ x))) $ candids xs
          f _          = []
          compute :: ([Double] -> Double) -> [[Expr]] -> [Expr]
          compute f xs = map (depends . f) $ map (map eval) xs
          candids xs   = filter (\es -> length es == 2) $ combinations 2 $ filter isConst $ toList xs

neg2subR :: Rule Expr
neg2subR = describe "Flip signs of multiplication or additions" $ ruleList "neg" neg2subR'
neg2subR' :: Expr -> [Expr]
neg2subR' (Add xs)          = if negatives then [Negate (Add flipped)] else []
    where negatives        = any (\x -> isNeg x) $ toList xs
          flipped          = fromList $ map flipE $ toList xs
          flipE (Negate x) = x
          flipE x          = Negate x
neg2subR' (Negate (Add xs)) = [Add flipped]
    where flipped          = fromList $ map flipE $ toList xs
          flipE (Negate x) = x
          flipE x          = Negate x
neg2subR' _              = []

fracR :: Rule Expr
fracR = describe "Frac" $ ruleList "frac" fracR'
fracR' :: Expr -> [Expr]
fracR' (Div (Div x)) = [x]
fracR' (Div x) | isConst x                  = if isNeg x then [Negate $ Double $ (1 / eval x)] else [Double $ (1 / eval x)]
	      | otherwise                  = []
fracR' (Double x)        
            | nice && abs (n `div` diver) /= 1 = [Mul $ fromList [Con (n `div` diver), Double (fromIntegral diver / 10000)], Div $ Mul $ fromList [Con (10000 `div` diver), Div $ Con (n `div` diver)]]
	    | nice                             = [Div $ Con (10000 `div` diver)]
            | otherwise                        = []
    where n       = round (x * 10000)
          nice    = fromIntegral n / 10000 == x
          diver   = gcd 10000 (abs n)
          flipped = x < 0
fracR' (Negate (Double x))
            | nice && abs (n `div` diver) /= 1 = [Mul $ fromList [Negate $ Con (abs n `div` diver), Double (fromIntegral diver / 10000)]]
            | otherwise                    = []
    where n       = round (x * 10000)
          nice    = fromIntegral n / 10000 == x
          diver   = gcd 10000 (abs n)
          flipped = x < 0
fracR' _                  = []

--Inverse of the fracR rule, Div -> Double
ifracR :: Rule Expr
ifracR = describe "Inverse Frac" $ ruleList "frac.inverse" ifracR'
ifracR' :: Expr -> [Expr]
ifracR' (Div (Double x))         = [Double (1/x)]
ifracR' (Div (Negate(Double x))) = [Double (1/x)]
ifracR' (Div (Con x))            = [Double (1/(fromIntegral x))]
ifracR' (Div (Negate(Con x)))    = [Double (1/(fromIntegral x))]
ifracR' _                        = []

-------------------------------------------------------------------------------
-------------Examples
-------------------------------------------------------------------------------
mopdr :: Expr
mopdr = Add $ fromList [Con 3, Con 5, Mul $ fromList [Con 6, Con 2]]

-------------------------------------------------------------------------------
-------------Strategy
-------------------------------------------------------------------------------

rules = [distR, evalR, neg2subR, fracR]

rulesStrat :: LabeledStrategy Expr
rulesStrat = label "all-rules" $ alternatives rules 

evalStrategy :: LabeledStrategy (Context Expr)
evalStrategy = label "eval" $
   repeatS (somewhere (liftToContext rulesStrat))
  
fEx :: Exercise Expr
fEx = emptyExercise
   { exerciseId    = describe "Evaluate an expression (full)" $
                        newId "eval.full"
   , status        = Experimental
   , strategy      = evalStrategy
   , prettyPrinter = show
   , navigation    = termNavigator
   , parser        = readM
   , equivalence   = withoutContext eqExpr
   , ready         = predicate isConst
   }
 
-------------------------------------------------------------------------------
-------------Strategy
-------------------------------------------------------------------------------

{-stepStrat :: Strategy Expr
stepStrat = alternatives [stepR rules 8]

stepR :: [Rule Expr] -> Int -> Rule Expr
stepR rs depth = (makeRule "step" f)
    where f (Con _)    = []
          f (Double _) = []
          f exp        = sortedoptions rs depth exp
                       
niceness :: Expr -> Expr -> Int
niceness expr org = length $ rawTermsOrg \\ rawTermsExpr
	where rawTermsOrg  = filter (\e -> not (isConst e && eval e == 1)) $ rawTerms org
	      rawTermsExpr = filter (\e -> not (isConst e && eval e == 1)) $ rawTerms expr    
          sortedoptions :: [Rule Expr] -> Int -> Expr -> [Expr]
          
sortedoptions rules d expr = map getFullExpr $ sortBy (\a b -> compare (nicenessCtx a expr) (nicenessCtx b expr)) subs
   where 
         tops       = map toCtx $ concat $ take d $ subExprS' rules Set.empty [expr]
         subs       = tops ++ concatMap subExpr tops
         
subExprS' :: [Rule Expr] -> Set.Set Expr -> [Expr] -> [[Expr]]
subExprS' rs set []     = []
subExprS' rs set xs     = let newSet = Set.union nextLayer set
                         in  xs : subExprS' rs newSet nextLayerL
    where subs            = ctxXS ++ concatMap subExpr ctxXS
          transformedExpr = concatMap (\e -> concatMap (\r -> r e) (map applyAll rs)) $ map getFullExpr subs
          nextLayer       = Set.difference (Set.fromList (map (normalise . getFullExpr) $ map toCtx transformedExpr)) set
          nextLayerL      = Set.toList nextLayer
          ctxXS           = map toCtx xs    
-} 