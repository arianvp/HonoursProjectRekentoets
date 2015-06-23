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
import qualified Ideas.Common.Library as I
import qualified Ideas.Main.Default as I

small :: Expr
small = Mul $ fromList [Con 2, Negate $ Con 3, Con 4, Add $ fromList [Con 5, Con 9]]

small_uit :: Program
small_uit = [Both (Mul $ fromList [Con 2, Negate $ Con 3, Con 4]) (Negate $ Con 24),
	     Both (Mul $ fromList [Con 5, Negate $ Con 24]) (Negate $ Con 120),
	     Both (Mul $ fromList [Con 9, Negate $ Con 24]) (Negate $ Con 216),
	     Both (Add $ fromList [Negate $ Con 120, Negate $ Con 216]) (Negate $ Con 336)]

small_uit2 :: Program
small_uit2 = [Both (Mul $ fromList [Con 2, Con 3, Con 4]) (Con 24),
	     Both (Mul $ fromList [Con 5, Con 24]) (Con 120),
	     Both (Mul $ fromList [Con 9, Con 24]) (Con 216),
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
                     _       -> do putStrLn ("\t" ++ show (normalise $ fromJust $ e') ++ "   \t\t" ++ (show $ eval $ fromJust e'))
                                   process (normalise $ fromJust e') xs
process e ((Both lhs rhs):xs)  =
             do putStrLn ("Step: " ++ show lhs ++ " = " ++ show rhs)
                let lhs' = normalise $ normalise lhs
                putStrLn ("Normalised: " ++ show lhs')
                let e' = performStep e (lhs', rhs)
                case e' of
                     Nothing -> putStrLn ("\tFail")
                     _       -> if (fromJust e') == e then
                                   do putStrLn ("\tIgnored:" ++ show e)
                                      process e xs
                                else
                                   do putStrLn ("\t" ++ show (normalise $ fromJust $ e') ++ "   \t\t" ++ (show $ eval $ fromJust e'))
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

                                 
process'' :: Expr -> [Program] -> IO ()
process'' e ps = mapM_ f ps
	where f p = do let (c,s) = execute e p (0,0)
		       putStrLn (show c ++ "/" ++ show s)

execute :: Expr -> Program -> (Int, Int) -> (Int, Int)
execute e [] res               = res
execute e ((Lhs lhs):xs) (c,s) = 
             do let lhs' = normalise lhs
                let e' = performHalfStep e lhs'
                case e' of
                     Nothing -> (c,s+1)
                     _       -> execute (normalise $ fromJust e') xs (c + 1, s + 1)
execute e ((Both lhs rhs):xs)  (c,s) =
             do let lhs' = normalise $ normalise lhs
                let e' = performStep e (lhs', rhs)
                case e' of
                     Nothing -> (c,s+1)
                     _       -> if (fromJust e') == e then
                                   execute e xs (c, s+1)
                                else
                                   execute (normalise $ fromJust $ e') xs (c+1,s+1)
correct = processmany [ex1cor, ex2cor, ex3cor, ex4cor, ex5cor, ex6cor]
corList = [ex1cor, ex3cor, ex4cor, ex6cor]    

main :: IO ()
main = mapM_ exe corList -- [(opdr1, [uitw1_7])]
	where exe (opdr,uitws) = do putStrLn (replicate 80 '-')
				    process'' (normalise opdr) uitws
	  