module SubExpr where

import Bag
import Expr
import Rule
import Data.Maybe
import Data.List (subsequences, nub, (\\), partition, intersperse, sortBy)
import qualified Data.Set as Set

subExpr :: Ctx -> [Ctx]
subExpr ctx = downs ++ concatMap subExpr downs
    where downs     = goDown ctx


-- Computes the top-level expressions possible per step without duplicates.
subExprS :: [Rule] -> Set.Set Expr -> [Expr] -> [[Expr]]
subExprS rs set []     = []
subExprS rs set xs     = let newSet = Set.union nextLayer set
                         in  xs : subExprS rs newSet nextLayerL
    where subs            = ctxXS ++ concatMap subExpr ctxXS
          transformedExpr = concatMap (\e -> concatMap (\r -> r e) (map apply rs)) subs
          nextLayer       = Set.difference (Set.fromList (map (normalise . getFullExpr) transformedExpr)) set
          nextLayerL      = Set.toList nextLayer
          ctxXS           = map toCtx xs   

subExprQ :: [Rule] -> [Expr] -> [[Expr]]
subExprQ rs []     = []
subExprQ rs xs     = xs : subExprQ rs nextLayer
    where subs            = ctxXS ++ concatMap subExpr ctxXS
          transformedExpr = concatMap (\e -> concatMap (\r -> r e) (map apply rs)) subs
          nextLayer       = map (normalise . getFullExpr) transformedExpr
          ctxXS           = map toCtx xs   
----------------------------------
--          STEPS               --
----------------------------------

-- Replace the expression the ctx is focussing on with the given expr.
step :: Ctx -> Expr -> Ctx
step (e, ze) e' = (e', ze)

-- Finds a context that matches the lhs of an equal
findCtx :: Expr -> Expr -> [Ctx]
findCtx expr lhs = sortBy (\a b -> compare (nicenessCtx a expr) (nicenessCtx b expr)) candidates
   where 
         tops       = map toCtx $ concat $ take 8 $ subExprS [distR, evalR, neg2subR, fracR] Set.empty [expr]
         subs       = tops ++ concatMap subExpr tops
         candidates = filter (\(e,ze) -> e == lhs) subs

nicenessCtx :: Ctx -> Expr -> Int
nicenessCtx expr org = length $ rawTermsOrg \\ rawTermsExpr
    where rawTermsOrg  = filter (\e -> not (isConst e && eval e == 1)) $ rawTerms org
          rawTermsExpr = filter (\e -> not (isConst e && eval e == 1)) $ rawTerms $ getFullExpr expr

rawTerms :: Expr -> [Expr]
rawTerms expr = map fst $ filter (\(e,ze) -> isConst e && not (isNeg e)) $ subExpr (toCtx expr)

type Equal = (Expr, Expr)
performStep :: Expr -> Equal -> Maybe Expr
performStep e (lhs, rhs) | checkEqual = case ctx of
                         Nothing -> Just e
                         _       -> Just $ getFullExpr $ step (fromJust ctx) rhs
             | otherwise  = Nothing
    where checkEqual = eval lhs == eval rhs
          ctx        = let ctxs = findCtx e lhs in if null ctxs then Nothing else Just $ head ctxs

-- Given only an expression, find the steps.
performHalfStep :: Expr -> Expr -> Maybe Expr
performHalfStep e lhs = case ctx of
                Nothing -> Nothing
                _       -> Just $ getFullExpr $ fromJust ctx
    where ctx        = let ctxs = findCtx e lhs in if null ctxs then Nothing else Just $ head ctxs

