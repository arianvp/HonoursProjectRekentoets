module Rule where

import Bag (Bag)
import qualified Bag as B
import Expr
import Data.Maybe
import Data.List (subsequences, nub, (\\), partition, intersperse, sortBy)

type Rule = Expr -> [Expr]
distR :: Rule
distR (Mul multies) = concatMap (uncurry convertm) . addies $ B.toList multies
    where addies (x:xs) | isAddie x = (x, xs) : xs'
                        | otherwise = xs'
            where xs' = map (Control.Arrow.second (:x)) $ addies xs
          convertm a = map (convert a)
          convert e@(Add xs) mul = 
            Mul . B.insert (distedAdd mul xs) $ B.deleteList [mul, e] multies
          convert e@(Negate add@(Add xs)) mul = let Mul xs = convert add mul
            in Negate . Mul $ B.delete e xs
          distedAdd mul = Add . B.map (\x -> Mul $ B.fromList [mul, x])
          isAddie (Negate x) = isAdd x
          isAddie x          = isAdd x
distR _             = []


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
-- ?? waar komt die get vandaan?
 
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
                     | otherwise       = map (\x -> Add  $ B.fromList 
                            ((depends . sum     $ map eval x) : (xs' \\ x))) $ candids xs'
            where xs'= B.toList xs
          f (Mul xs) | length xs' == 2 = compute product $ candids xs'
                     | otherwise       = map (\x -> Mul  $ B.fromList 
                            ((depends . product $ map eval x) : (xs' \\ x))) $ candids xs'
            where xs'= B.toList xs
          f _                                    = []
          compute :: ([Double] -> Double) -> [[Expr]] -> [Expr]
          compute g = map (depends . g . map eval)
          candids   = filter ((==2) . length) . combinations 2 . filter isConst
          

neg2subR :: Rule
neg2subR (Add xs)          = [Negate (Add flipped) | any isNeg xs']
    where flipped          = B.fromList $ map flipE xs'
          flipE (Negate x) = x
          flipE x          = Negate x
          xs'              = B.toList xs
neg2subR (Negate (Add xs)) = [Add flipped]
    where flipped          = B.fromList . map flipE $ B.toList xs
          flipE (Negate x) = x
          flipE x          = Negate x
neg2subR _                 = []

fracR :: Rule
fracR (Div (Div x)) = [x]
fracR (Div x) | isConst x                  = if isNeg x then [Negate $ Double (1 / eval x)] else [Double (1 / eval x)]
	          | otherwise                  = []
fracR (Double x)        
            | nice && abs (n `div` diver) /= 1 = [Mul $ B.fromList [Con (n `div` diver), Double (fromIntegral diver / 10000)], Div $ Mul $ B.fromList [Con (10000 `div` diver), Div $ Con (n `div` diver)]]
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
