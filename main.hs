module Main where

import Bag
import Expr
import Rule
import SubExpr
import Uitw
import Strategy
import Data.Maybe
import Data.List (subsequences, nub, (\\), partition, intersperse, sortBy)
import qualified Data.Set as Set
import qualified Data.Map as M

--import qualified Ideas.Common.Library as I
--import qualified Ideas.Main.Default as I

small :: Expr
small = Mul $ fromList [Con 2, Negate $ Con 3, Con 4, Add $ fromList [Con 5, Con 9]]

small_uit :: Program
small_uit = [Both (Mul $ fromList [Con 2, Negate $ Con 3, Con 4]) (Negate $ Con 24),
	     Both (Mul $ fromList [Con 5, Negate $ Con 24]) (Negate $ Con 120),
	     Both (Mul $ fromList [Con 9, Negate $ Con 24]) (Negate $ Con 216),
	     Both (Add $ fromList [Negate $ Con 120, Negate $ Con 216]) (Negate $ Con 336)]

-- Process function
process :: Expr -> Program -> IO ()
process e []             = putStrLn ("Done: " ++ show e)
process e ((Lhs lhs):xs) = 
             do putStrLn ("Step: " ++ show lhs)
                let lhs' = normalise lhs
                putStrLn ("Normalised: " ++ show lhs')
                let e' = performHalfStep e lhs'
                case e' of
                     Nothing -> putStrLn ("\tFail")
                     _       -> do putStrLn ("\t" ++ show e')
                                   process (normalise $ fromJust e') xs
process e ((Both lhs rhs):xs)  =
             do putStrLn ("Step: " ++ show lhs ++ " = " ++ show rhs)
                let lhs' = normalise lhs
                putStrLn ("Normalised: " ++ show lhs')
                let e' = performStep e (lhs', rhs)
                case e' of
                     Nothing -> putStrLn ("\tFail")
                     _       -> if (fromJust e') == e then
                                   do putStrLn ("\tIgnored:" ++ show e)
                                      process e xs
                                else
                                   do putStrLn ("\t" ++ show e' ++ "   \t\t" ++ (show $ eval $ fromJust e'))
                                      process (normalise $ fromJust $ e') xs

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

