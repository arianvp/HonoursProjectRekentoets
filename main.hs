module Main where

import Data.Maybe
import Data.List (subsequences, nub, (\\), partition)
import qualified Data.Set as Set
import qualified Data.Map as M
import Data.Map (Map)
import qualified Ideas.Common.Library as I
import qualified Ideas.Main.Default as I

----------------------------------
--          BAG                 --
----------------------------------
newtype Bag a = Bag (Map a Int)
    deriving (Show,Read,Ord,Eq)

empty :: Bag a
empty = Bag $ M.empty

singleton :: a -> Bag a
singleton a = Bag $ M.singleton a 1

insert :: (Ord a) => a -> Bag a -> Bag a
insert x (Bag map) = Bag $ M.insertWith (+) x 1 map

remove :: (Ord a) => a -> Bag a -> Bag a
remove a b@(Bag map) | isNothing value     = b
                     | fromJust value <= 1 = Bag $ M.delete a map
                     | otherwise           = Bag $ M.insertWith (-) a 1 map
    where value = M.lookup a map

fromList :: (Ord a) => [a] -> Bag a
fromList = foldl f empty
    where
        f (Bag map) x = Bag $ M.insertWith (+) x 1 map

toList :: Bag a -> [a]
toList (Bag m) = concatMap f $ M.toList m
    where f (a,b) = replicate b a

toListU :: Bag a -> [a]
toListU (Bag m) = map f $ M.toList m
    where f (a,b) = a


----------------------------------
--          DATASTRUCTS         --
----------------------------------
-- Expression data type
data Expr = Con Int
	  | Add (Bag Expr)
	  | Negate Expr
	  | Mul (Bag Expr)
	  | Div Expr
	  | Double Double
     deriving (Eq, Read, Ord)
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


instance Show Expr where
	show (Con x)    = show x
	show (Add xs)   = "( +: " ++ show (toList xs) ++ ")"
	show (Negate x) = "-" ++ show x
	show (Mul xs)   = "( *:" ++ show (toList xs) ++ ")"
	show (Div x)    = "1/" ++ show x
	show (Double x) = show x

----------------------------------
--      DATASTRUCTS FUNCS       --
----------------------------------
-- Evaluate the expression.
eval :: Expr -> Double
eval (Con x)    = fromIntegral x
eval (Add xs)   = sum (map eval $ toList xs)
eval (Negate x) = 0.0 - eval x
eval (Mul xs)   = product (map eval $ toList xs)
eval (Div x)    = 1.0 / eval x
eval (Double x) = x

-- Lift an expression to a zipper
toCtx :: Expr -> Ctx
toCtx e = (e, Top)

goDown :: Ctx -> [Ctx]
goDown (e, ze) = res e
    where 
            res :: Expr -> [Ctx]
            res (Add xs)   = let (single, combinations) = f $ toList xs
                                 single' = map (\([e], es) -> (e, AddI ze $ fromList es)) single
                                 comb'   = map (\(e, es) -> (Add $ fromList e, AddI ze $ fromList es)) combinations
                             in single' 
                                 --map (\es -> (Add $ fromList es, AddI (Add (fromList [Add $ fromList es] ++ ((toList xs) \\ es)), ze))) (snd $ f xs)
                           -- ++ map (\e' -> (e', AddI ze (remove e' xs))) (toList xs) -- Hammer time makes its easier
            res (Negate x) = [(x, NegI ze)]
            res (Mul xs)   = let (single, combinations) = f $ toList xs
                                 single' = map (\([e], es) -> (e, MulI ze $ fromList es)) single
                                 comb'   = map (\(e, es) -> (Mul $ fromList e, MulI ze $ fromList es)) combinations
                             in comb'  ++ concatMap goDown single' 
                                 --map (\e' -> (e', MulI ze (remove e' xs))) $ concat $ nub $ filter (not . null) $ subsequences $ toList xs
            res (Div x)    = [(x, DivI ze)]
            res _          = []
            -- unnsubs xs     = nub $ filter (not . null) $ subsequences $ toList xs
            --f xs           = partition (\(l, r) -> length l == 1) $ nonEmptySubExpr xs
            f = subs
            
nonEmptySubExpr         :: [Expr] -> [([Expr],[Expr])]
nonEmptySubExpr []          =  []
nonEmptySubExpr (x: xs) =  ([x], xs) : foldr f [] (nonEmptySubExpr xs)
    where f (b, bs) r = (b, x : bs) : (x : b, bs) : r

nonEmptySubsequences         :: [a] -> [[a]]
nonEmptySubsequences []      =  []
nonEmptySubsequences (x:xs)  =  [x] : foldr f [] (nonEmptySubsequences xs)
  where f ys r = ys : (x : ys) : r
          


subs xs = partition g . map f . nonEmptySubsequences $ xs
  where f ys = (ys, xs\\ys)
        g ([_],_) = True
        g _ = False

goUp :: Ctx -> Maybe Ctx
goUp (e, ze) = res ze
	where res (AddI ze' xs) = Just (Add (insert e xs), ze')
	      res (NegI ze')    = Just (Negate e, ze')
	      res (MulI ze' xs) = Just (Mul (insert e xs), ze')
	      res (DivI ze')    = Just (Div e, ze')
	      res (Top)         = Nothing


-- Unsafe shorthands for goLeft/goRight/goUp.
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
--          RULES               --
----------------------------------
type Rule = Expr -> [Expr]
distR :: Rule
distR (Mul multies)   = map convert $ concatMap dist_optos addies 
    where addies           = filter is_addie $ toListU multies
          is_addie (Add _) = True
          is_addie _       = False
          othas e          = toList (remove e multies)
          dist_optos e     = map (\opt -> (opt, e)) $ othas e
          convert (mul, (Add xs)) = Add (fromList $ map (\x -> Mul $ fromList [mul, x]) $ toList xs) -- TODO: Subsequences
distR _         = []
      
evalR :: Rule
evalR (Add xs)       = map (depends . sum)     $ map (map eval) $ filter (\seq -> length seq == 2) $ subsequences $ toList xs 
evalR (Mul xs)       = map (depends . product) $ map (map eval) $ filter (\seq -> length seq == 2) $ subsequences $ toList xs 
evalR _              = []

depends x = if integral then Con (round x) else Double x
	where integral = x == fromIntegral (round x)

{-
fracR :: Rule
fracR (Double x)        | nice      = Just $ Mul $Div (Con (n `div` diver)) (Con (10000 `div` diver))
			| otherwise = Nothing
	where n     = round (x * 10000)
	      nice  = fromIntegral n / 10000 == x
	      diver = gcd 10000 n
fracR _                  = Nothing
-}

apply :: Rule -> Ctx -> [Ctx]
apply r (e, ze) = map (\x -> (x, ze)) $ r e


----------------------------------
--          SUB-EXPR            --
----------------------------------
subExpr :: Ctx -> [Ctx]
subExpr ctx = downs ++ concatMap goDown downs
   where downs     = goDown ctx
         

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
   where tops       = map toCtx $ concat $ subExprS [distR, evalR] Set.empty [expr]
         subs       = tops ++ concatMap subExpr tops
         candidates = filter (\(e,ze) -> e == lhs) subs


type Equal = (Expr, Expr)
performStep :: Expr -> Equal -> Maybe Expr
performStep e (lhs, rhs) | checkEqual = case ctx of
					     Nothing -> Just e
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

opdr1 = Mul $ fromList [Con 32, sub, sub]
    where sub = Add $ fromList [Con 1, Double (-0.25)]

{-
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
        --
        --
opdr5 = Mul $ fromList [Add $ fromList [mado, vr], Double 4.80]
  where mado = Mul $ fromList [Add $ fromList [(Add $ fromList [Double 16.5, Con (-8)]), Negate (Add $ fromList [Double 12.75, Con (-12)])], Con 4]
        vr   = Add $ fromList [Add $ fromList [Con 14, Con (-7)], Negate (Add $ fromList [Double (-11.5), Con 12])]
{-opdr5 = Mul (Add mado vr) (Double 4.80)

    -- mado =  ((16.5 - 8) - (12.75 - 12)) * 4
    where mado = Mul (Sub (Sub (Double 16.5) (Con 8)) (Sub (Double 12.75) (Con 12))) (Con 4)
          vr   = Sub (Sub (Con 14) (Con 7)) (Sub (Con 12) (Double 11.5))
-}

ex5cor = (opdr5, [uitw5_1,uitw5_2,uitw5_3])
          
uitw5_1 :: Program 
uitw5_1 = [ Both
              (Add $ fromList [ Double (-0.5)
                              , Add $ fromList [ Add $ fromList [ Mul $ fromList [ Double 8.5
                                                                                 , Con 4]
                                                                , Con 7]
                                               , Mul $ fromList [ Con -4
                                                                , Double 0.75]
                                               ]
                              ]
              )
              (Add $ fromList [ Add $ fromList [ Add $ fromList [Con 34, Con 7],  Con (-3) ], Double (-0.5)])
          , Both
              (Add $ fromList [ Add $ fromList [ Add $ fromList [Con 34, Con 7]  Con (-3)],  Double (-0.5)])
              (Double 37.5)
          , Both
              (Mul $ fromList [Double 37.5, Double 4.80])
              (Con 180)
          ]
{-uitw5_1 = [Both (Sub (Sub (Add (Mul (Double 8.5) (Con 4)) (Con 7)) (Mul (Con 4) (Double 0.75))) (Double 0.5))
                (Sub (Sub (Add (Con 34) (Con 7)) (Con 3)) (Double 0.5)),
           Both (Sub (Sub (Add (Con 34) (Con 7)) (Con 3)) (Double 0.5))
                (Double 37.5),
           Both (Mul (Double 37.5) (Double 4.80))
                (Con 180)]       
-}
           
           
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
        --
--opdr6 = Mul (Div (Con 600) (Con 800)) (Con 300)   
opdr6 =  Mul $ fromList [ Div (Con 800), Con 600, Con 300]
ex6cor = (opdr6, [uitw6_1])

uitw6_1 :: Program
uitw6_1 = [ Both
              (Mul $ fromList [Div (Con 800), Con 600])
              (Mul $ fromList [Div (Con 4), Con 3])
          , Both
              (Mul $ fromList [Con 300, Con 4, Div (Con 3)])
              (Con 225)
          ]
{-uitw6_1 = [Both (Div (Con 600) (Con 800)) (Div (Con 3) (Con 4)),
           Both (Mul (Con 300) (Div (Con 3) (Con 4))) (Con 225)]-}

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
                     _       -> if (fromJust e') == e then
                                   do putStrLn ("\tIgnored")
                                      process e xs
                                else
                                   do putStrLn ("\t" ++ show e')
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
                        
--correct = processmany [ex1cor, ex2cor, ex3cor, ex4cor, ex5cor, ex6cor]    

----------------------------------
--          STRATEGY            --
----------------------------------
{-
suitable :: Expr -> [Expr]
suitable e = candidates
	where tops       = map toCtx $ concat $ take 2 $ subExprS [assocR, assocbR, evalR] Set.empty [e]
	      subs       = tops ++ concatMap subExpr tops
	      candidates = map fst $ filter (\(e,ze) -> simple e) subs

constant :: Expr -> Bool
constant (Con _)    = True
constant (Double _) = True
constant _          = False

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
