module Rule where

import Bag
import Expr
import Data.Maybe
import Data.List (subsequences, nub, (\\), partition, intersperse, sortBy, intersect)

type Rule = Expr -> [Expr]
distR :: Rule
distR (Mul multies)   = map convert $ concatMap dist_optos (addies ++ negaddies)
    where addies           = filter isAdd    $ toListU multies
          negaddies        = filter isNegAdd $ toListU multies  
          othas e          = toList (remove e multies)
          dist_optos e     = map (\opt -> (opt, e)) $ othas e
          convert (mul, add@(Add xs)) = 
                    let distedAdd = Add (fromList $ map (\x -> Mul $ fromList [mul, x]) $ toList xs)
                    in Mul $ fromList (distedAdd : (toList multies \\ [mul, add]))
          convert (mul, e@(Negate add@(Add xs))) = 
                    let distedAdd = Add (fromList $ map (\x -> Mul $ fromList [mul, x]) $ toList xs)
		    in Negate $ Mul $ fromList (distedAdd : (toList multies \\ [mul, e]))
          isNegAdd (Negate (Add _)) = True
          isNegAdd _                = False
distR _         = []



iDistR :: Rule

-- vind alle willekeurige paren. 
iDistR (Add addies) = do
  as <- listAddies
  bs <- listAddies
  map (\x ->
          case get x bs of
            Just x -> _ -- Ja we hebben een common element! EXTRAPOLEREN
            Nothing -> _ -- nee we hebben geen common element, niks doen
      ) as

  -- als a en b eeen common subsequence hebben.
  -- voeg a en b toe aan de resultaten
  -- we kijken alleen naar common subsequences van lengte
  -- kijk in elk element in a
  -- als het element ook in b voorkomt. Ga verder met inverse distributie
  -- voeg toe. Flatten
  -- return alles
  -- ???
  -- profit!
  where listAddies = toList addies
      
combinations :: Int -> [a] -> [[a]]
combinations k xs = combinations' (length xs) k xs
	where combinations' n k' [] = []
	      combinations' n k' l@(y:ys)
		| k' == 0   = [[]]
		| k' >= n   = [l]
		| null l    = []
		| otherwise = map (y :) (combinations' (n - 1) (k' - 1) ys) ++ combinations' (n - 1) k' ys 

evalR :: Rule
evalR x       = f x
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

neg2subR :: Rule
neg2subR (Add xs)          = if negatives then [Negate (Add flipped)] else []
    where negatives        = any (\x -> isNeg x) $ toList xs
          flipped          = fromList $ map flipE $ toList xs
          flipE (Negate x) = x
          flipE x          = Negate x
neg2subR (Negate (Add xs)) = [Add flipped]
    where flipped          = fromList $ map flipE $ toList xs
          flipE (Negate x) = x
          flipE x          = Negate x
neg2subR _              = []

fracR :: Rule
fracR (Div (Div x)) = [x]
fracR (Div x) | isConst x                  = if isNeg x then [Negate $ Double $ (1 / eval x)] else [Double $ (1 / eval x)]
	      | otherwise                  = []
fracR (Double x)        
            | nice && abs (n `div` diver) /= 1 = [Mul $ fromList [Con (n `div` diver), Double (fromIntegral diver / 10000)], Div $ Mul $ fromList [Con (10000 `div` diver), Div $ Con (n `div` diver)]]
	    | nice                             = [Div $ Con (10000 `div` diver)]
            | otherwise                        = []
    where n       = round (x * 10000)
          nice    = fromIntegral n / 10000 == x
          diver   = gcd 10000 (abs n)
          flipped = x < 0
fracR (Negate (Double x))
            | nice && abs (n `div` diver) /= 1 = [Mul $ fromList [Negate $ Con (abs n `div` diver), Double (fromIntegral diver / 10000)]]
            | otherwise                    = []
    where n       = round (x * 10000)
          nice    = fromIntegral n / 10000 == x
          diver   = gcd 10000 (abs n)
          flipped = x < 0
fracR _                  = []

--Inverse of the fracR rule, Div -> Double
ifracR :: Rule
ifracR (Div (Double x))         = [Double (1/x)]
ifracR (Div (Negate(Double x))) = [Double (1/x)]
ifracR (Div (Con x))            = [Double (1/(fromIntegral x))]
ifracR (Div (Negate(Con x)))    = [Double (1/(fromIntegral x))]
ifracR _                        = []

apply :: Rule -> Ctx -> [Ctx]
apply r (e, ze) = map (\x -> (x, ze)) $ r e

rules = [distR, evalR, neg2subR, fracR]
