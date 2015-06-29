module Rule where

import Bag
import Expr
import Data.Maybe
import Data.List (subsequences, nub, (\\), partition, intersperse, sortBy, intersect)

type Rule = Expr -> [Expr]
distR :: Rule
distR (Mul multies)   = map convert $ concatMap dist_optos addies
    where addies           = filter (\x -> isAdd x|| isNegAdd x) $ toListU multies
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


{-
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
 -}     
combinations :: Int -> [a] -> [[a]]
combinations k xs = combinations' (length xs) k xs
	where combinations' n k' [] = []
	      combinations' n k' l@(y:ys)
		| k' == 0   = [[]]
		| k' >= n   = [l]
		| null l    = []
		| otherwise = map (y :) (combinations' (n - 1) (k' - 1) ys) ++ combinations' (n - 1) k' ys 

evalR :: Rule
evalR       = f
    where f (Add xs) | length xs' == 2 = compute sum $ candids xs'
                     | otherwise       = map (\x -> Add  $ fromList 
                            ((depends . sum     $ map eval x) : (xs' \\ x))) $ candids xs'
            where xs'= toList xs
          f (Mul xs) | length xs' == 2 = compute product $ candids xs'
                     | otherwise       = map (\x -> Mul  $ fromList 
                            ((depends . product $ map eval x) : (xs' \\ x))) $ candids xs'
            where xs'= toList xs
          f _                                    = []
          compute :: ([Double] -> Double) -> [[Expr]] -> [Expr]
          compute g = map (depends . g . map eval)
          candids   = filter ((==2) . length) . combinations 2 . filter isConst
          

neg2subR :: Rule
neg2subR (Add xs)          = [Negate (Add flipped) | any isNeg xs']
    where flipped          = fromList $ map flipE xs'
          flipE (Negate x) = x
          flipE x          = Negate x
          xs'              = toList xs
neg2subR (Negate (Add xs)) = [Add flipped]
    where flipped          = fromList $ map flipE $ toList xs
          flipE (Negate x) = x
          flipE x          = Negate x
neg2subR _                 = []

fracR :: Rule
fracR (Div (Div x)) = [x]
fracR (Div x) | isConst x                  = if isNeg x then [Negate $ Double (1 / eval x)] else [Double (1 / eval x)]
	          | otherwise                  = []
fracR (Double x)        
            | nice && abs (n `div` diver) /= 1 = [Mul $ fromList [Con (n `div` diver), Double (fromIntegral diver / 10000)], Div $ Mul $ fromList [Con (10000 `div` diver), Div $ Con (n `div` diver)]]
	        | nice                             = [Div $ Con (10000 `div` diver)]
            | otherwise                        = []
    where n       = round (x * 10000)
          nice    = fromIntegral n / 10000 == x
          diver   = gcd 10000 (abs n)
          flipped = x < 0
fracR (Negate (Double x)) | null xs = []
                          | otherwise = [Negate (head xs)]            
    where xs = fracR (Double x)
fracR _                  = []

--Inverse of the fracR rule, Div -> Double
ifracR :: Rule
ifracR (Div (Double x))         = [Double (1 / x)]
ifracR (Div (Negate(Double x))) = [Double (1 / x)]
ifracR (Div (Con x))            = [Double (1 / fromIntegral x)]
ifracR (Div (Negate(Con x)))    = [Double (1 / fromIntegral x)]
ifracR _                        = []

apply :: Rule -> Ctx -> [Ctx]
apply r (e, ze) = map (\x -> (x, ze)) $ r e

rules = [distR, evalR, neg2subR, fracR]
