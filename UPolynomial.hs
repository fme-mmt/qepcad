{-# LANGUAGE TypeSynonymInstances,
             FlexibleInstances,
             FlexibleContexts #-}
module UPolynomial where
import Data.Map (lookup, insertWith, empty, keys, Map)
import Data.List (intercalate)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe, mapMaybe)
import Prelude hiding (filter, lookup, const)
import Data.Ratio (numerator, denominator)


newtype UPolynomial r = Upol { coeffs :: [r] }  -- Leading coeff first

instance Functor UPolynomial where
  fmap f = Upol . fmap f . coeffs

instance (Num r, Eq r) => Eq (UPolynomial r) where
  (Upol ps) == (Upol qs) = (normal ps) == (normal qs)


fromMap :: (Num r, Eq r) => Map Int r -> UPolynomial r
fromMap mp =  Upol . normal . reverse $ cs where
  cs = [fromMaybe 0 (lookup e mp) | e <- [0..d]]
  d = maximum $ keys mp


normal :: (Num r, Eq r) => [r] -> [r]
unormal :: Num r => Int -> [r] -> [r]
enumerate :: UPolynomial r -> [(r, Int)]
simplify :: (Num r, Eq r) => UPolynomial r -> UPolynomial r
normal = dropWhile (==0)
unormal n l = replicate (n - length l) 0 ++ l
enumerate (Upol cs)= zip (reverse cs) [0..]
simplify =  Upol . normal . coeffs

uzero :: UPolynomial r
uvar ::Num r =>  UPolynomial r
monomial :: Num r => Int -> r -> UPolynomial r
uzero = Upol []
monomial k c = Upol . reverse $ unormal (k+1) [c]
uvar  = monomial 1 1

deg  :: UPolynomial r -> Int
deg (Upol []) = minBound
deg (Upol cs) = length cs - 1


instance (Num r, Ord r) => Ord (UPolynomial r) where
  compare (Upol cp) (Upol cq) =
      case comparing length cp cq of
        EQ -> lexcmp cp cq
        o  -> o
    where
      lexcmp [] [] = EQ
      lexcmp _ []  = GT
      lexcmp [] _  = LT
      lexcmp (c:cs) (d:ds) =
        case compare c d of
          EQ -> lexcmp cs ds
          o  -> o

instance (Eq r, Num r) => Num (UPolynomial r) where
    negate = fmap negate

    (Upol p) + (Upol q) = Upol . normal  $ zipWith (+) cs ds where
      cs = unormal n p
      ds = unormal n q
      n = max (length p) (length q)

    p * q = Upol . normal $ reverse pqs where
      cp = enumerate p
      cq = enumerate q
      products = [(i + j, c * d) | (c,i) <- cp, (d,j) <- cq]
      mp = foldr (\(i,c) m -> insertWith (+) i c m) empty products
      d = maximum $ map fst products
      pqs = map (\i -> fromMaybe 0 $ lookup i mp) [0..d]

    abs = undefined

    signum = undefined

    fromInteger 0 = uzero
    fromInteger n = Upol [fromInteger n]

lt :: Num r => UPolynomial r -> UPolynomial r
lc :: Num r => UPolynomial r ->  r
trunc :: UPolynomial r -> UPolynomial r
sep :: (Num r, Eq r) => UPolynomial r -> UPolynomial r

lt (Upol []) = uzero
lt p         = monomial (deg p) (lc p)

lc (Upol [])    = 0
lc (Upol (c:_)) = c

trunc (Upol [])     = uzero
trunc (Upol (_:cs)) = Upol cs

sep (Upol cs) = Upol . normal $ reverse cps where
  cps = [fromIntegral k * c | (c,k) <- zip (reverse cs) [0..], k > 0]


eval :: Num r => r -> UPolynomial r -> r
eval x = foldr (\c y -> c + x * y) 0  . reverse . coeffs

division :: (Eq r, Fractional r) => UPolynomial r -> UPolynomial r -> (UPolynomial r,  UPolynomial r)
division p q | deg p < deg q = (0, p)
division p q                 = (c + c', r) where
  (c', r) = division (simplify $ p - c * q) q
  c = monomial (deg p - deg q) (lc p/lc q)

remainder :: (Eq r, Fractional r) => UPolynomial r -> UPolynomial r -> UPolynomial r
remainder p q = snd $ division p q

pgcd :: (Ord r, Fractional r) => [UPolynomial r] -> UPolynomial r
pgcd = foldl1 gcd' where
  gcd' 0 q = q
  gcd' p q | deg p < deg q = gcd' q p
  gcd' p q = gcd' (remainder p q) q



instance (Eq r, Num r, Show r) => Show (UPolynomial r) where
  show (Upol []) = "0"
  show p = foldl1 splus  . mapMaybe showmon . reverse $ ics where
    var = "t"
    splus s (c:t) | c `elem` "+-" = s ++ ' ':c:' ':t
    splus s t                     = s ++ " + " ++ t
    ics = enumerate p
    sexp 1 = var
    sexp 0 = ""
    sexp k = var ++ "^" ++ show k
    showmon (0,k) = Nothing
    showmon (c,0) = Just $ show c
    showmon (1,k) = Just $ sexp k
    showmon (c,k) = Just $ show c ++ ' ':sexp k
