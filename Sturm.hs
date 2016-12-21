module Sturm where
import UPolynomial
import Data.Ord
import Numeric (fromRat)


type Sturm r = [UPolynomial r]
type Interval r = (r, r)


midpoint :: Fractional r => Interval r -> r
bisect :: Fractional r => Interval r -> (Interval r, Interval r)
midpoint (a, b) = (a + b)/2
bisect i@(a, b) = ((a, c), (c, b)) where c = midpoint i




sturm :: (Eq r, Fractional r) => UPolynomial r -> Sturm r
sturm p = stseq p (sep p) where
  stseq p 0 = [p]
  stseq p q = p:stseq q (-r) where
    r = remainder p q

vp :: (Ord r, Num r) => Sturm r -> r -> Int
vp ss x = length $ filter id $ zipWith (/=) (tail sgs) (init sgs) where
      sgs = filter (/=0) . map (signum . eval x) $ ss

nroots :: (Ord r, Num r) => Sturm r -> Interval r -> Int
nroots ss (a, b) = vp ss a - vp ss b

-- | [SAG, Prop. 1.3]
bound :: UPolynomial Rational -> Int
bound p = ceiling $ 0.1 + maximum [(fromIntegral d * abs (c'/a0))**(1.0/fromIntegral i) |
  (c,e) <- enumerate p,
  let i = d - e,  0 < i, let c' = fromRat c] where
 d = deg p
 a0 = fromRat $ lc p


split :: UPolynomial Rational -> [Interval Rational]
split p = wrap (-b, b) where
  b = fromIntegral $ bound p
  nr a b = nroots (sturm p) (a,b)
  wrap (a, b) = case nr a b of
    0 -> []
    1 -> [(a,b)]
    _ -> let (i,j) = bisect (a,b)
         in wrap i ++ wrap j

-- newtype Cell r = Cell (Either r (Int, UPolynomial r)) deriving Show
data Cell r = Cell r | Root Int (UPolynomial r)
  deriving Show
newtype CAD r = CAD [Cell r] deriving Show




cells :: UPolynomial Rational -> CAD Rational
cells p = CAD $ f 0 Nothing (split p) where
    f :: Int -> Maybe Rational -> [Interval Rational] -> [Cell Rational]
    f k Nothing ((a,b):ivs)   = left a:root k:f (k+1) (Just b) ivs
    f k (Just x) ((a,b):ivs)  = sample (x,a):root k:f (k+1) (Just b) ivs
    f k (Just x) []         = [right x]
    f k Nothing []          = [sample (0,0)]
    sample = Cell . midpoint
    left x = Cell (x-1)
    right x = Cell (x+1)
    root k = Root k p


p, x :: UPolynomial Rational
x = uvar
p = x^3 - 3*x +1
