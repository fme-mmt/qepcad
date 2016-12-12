module UPolynomial where
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (intercalate)
import Data.Ord (comparing)
import Data.Matrix (fromLists, Matrix, nrows, submatrix, detLaplace)
import Prelude hiding (filter, lookup, const, tail)

newtype UPolynomial r = UP { coeffs :: M.Map Int r } deriving Show

const :: Eqnum r => r -> UPolynomial r
deg :: Eqnum r => UPolynomial r -> Int
deg 0 = minBound
deg p = maximum . M.keys $ coeffs p
const 0 = UP M.empty
const c = UP $ M.singleton 0 c

toList :: Eqnum r => UPolynomial r -> [r]
fromList :: [r] -> UPolynomial r
iszero :: UPolynomial r -> Bool
toList p = [maybe 0 id (M.lookup i mp) | i <- [0..deg p]] where
   mp = coeffs p
fromList = UP . M.fromList . zip [0..]
iszero = null . coeffs


class (Eq r, Num r) => Eqnum r
type Z = Integer
instance Eqnum Integer
instance Eqnum r => Eqnum (UPolynomial r)

simplify :: Eqnum r => UPolynomial r -> UPolynomial r
simplify = UP . M.filter (/=0) . coeffs

instance Eqnum r => Ord (UPolynomial r) where
  compare p q =
    case comparing deg p q of
      EQ -> if p == 0
            then EQ
            else case comparing lt p q of
                  EQ -> comparing tail p q
                  o  -> o
      o -> o



instance Eqnum r => Eq (UPolynomial r) where
  p == q = coeffs p' == coeffs q' where
    p' = simplify p
    q' = simplify q

instance Eqnum r => Num (UPolynomial r) where
    negate = UP . fmap negate . coeffs
    (UP p) + (UP q) = UP $ M.unionWith (+) p q
    (UP p) * (UP q) = UP . M.filter (/=0) $ mp where
        products = [(i + j, c*d) | (i,c) <- M.toList p, (j,d) <- M.toList q]
        mp = foldr (\(i,c) m -> M.insertWith (+) i c m) M.empty products
    abs = undefined
    signum = undefined
    fromInteger 0 = UP M.empty
    fromInteger n = const $ fromInteger n

ltc :: Eqnum r => UPolynomial r -> (r, UPolynomial r)
lt :: Eqnum r => UPolynomial r -> UPolynomial r
lc :: Eqnum r => UPolynomial r ->  r
tail :: Eqnum r => UPolynomial r -> UPolynomial r
diff :: Eqnum r => UPolynomial r -> UPolynomial r

ltc p = if deg p < 0
    then (0, 0)
    else (c, UP $ M.fromList [(n,c)]) where
      n = deg p
      Just c = M.lookup n (coeffs p)

lt = snd . ltc
lc = fst . ltc

tail p = simplify $ p - lt p
diff (UP m) = UP $ flist [(i-1, fromIntegral i * c) | (i,c) <- tlist m, i/=0]
  where flist = M.fromList
        tlist = M.toList



isConstant :: Eqnum r => UPolynomial r -> Bool
isConstant = (<=0) . deg


red :: Eqnum r => UPolynomial r -> [UPolynomial r]
red = takeWhile (/=0) . iterate tail

{-cp1, cp2 :: (Ord r, Eqnum r) => [UPolynomial r] -> S.Set (UPolynomial r)
cp1 fs = S.unions [S.insert (lt g) (psc g (diff g)) | f <- fs, g <- red f]
cp2 fs = S.unions [psc g g' | i <- [0..length fs], j <- [i+1..length fs], g <- red (fs !! i), g' <- red (fs !! j)]
-}

sylvester :: Eqnum r => UPolynomial r -> UPolynomial r -> Matrix r
sylvester p q = fromLists . map reverse $ cps ++ cqs  where
  extend n  l = take n . iterate rotl $ replicate (n-1) 0 ++ l
  rotl [] = []
  rotl (b:bs) = bs ++ [b]
  cps = extend (deg q) . toList $ p
  cqs = reverse . extend (deg p) . toList $ q

determinant :: Num r => Matrix r -> r
determinant = detLaplace

psrc :: Eqnum r => Int -> UPolynomial r -> UPolynomial r -> r
psrc j p q = determinant $ submatrix k l k l sm where
  sm = sylvester p q
  k = j + 1
  n = nrows sm
  l = n - j

psc :: (Ord r, Eqnum r) => UPolynomial r -> UPolynomial r -> S.Set r
psc p q = S.fromList $  sub $ sylvester p q where
  strip m = submatrix 2 n' 2 n' m where
    n' = nrows m - 1
  sub m =
    if nrows m <= 2
      then [determinant m]
      else (determinant m):(sub $ strip m)




class Sympy a where
  sympy :: a -> String

instance Sympy Integer where
  sympy n = show n

instance (Eqnum r, Sympy r) => Sympy (UPolynomial r) where
  sympy p =  intercalate " + " $ zipWith (\c e -> "(" ++ sympy c ++ ")*X^" ++ show e) (toList p) [0..]

x :: UPolynomial Z
x = fromList [0, 1]
