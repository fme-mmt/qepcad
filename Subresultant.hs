{-# LANGUAGE  TypeSynonymInstances,
              FlexibleInstances #-}
module Subresultant where
import UPolynomial
import Data.Matrix
import Data.Set (Set, fromList)

class HasConstants r where
    isConstant :: r -> Bool

instance HasConstants Integer where
    isConstant _ = True

instance HasConstants Rational where
  isConstant _ = True

instance HasConstants (UPolynomial r) where
  isConstant = (<=0) . deg


sylvester :: Num r => UPolynomial r -> UPolynomial r -> Matrix r
sylvester p q = fromLists . map reverse $ cps ++ cqs  where
  extend n  l = take n . iterate rotl $ replicate (n-1) 0 ++ l
  rotl [] = []
  rotl (b:bs) = bs ++ [b]  -- redo
  cps = extend (deg q) $ coeffs p
  cqs = reverse . extend (deg p) $ coeffs q

-- Very inefficient, but does not require Fractional
determinant :: Num r => Matrix r -> r
determinant = detLaplace

psrc :: (Num r) => Int -> UPolynomial r -> UPolynomial r -> r
psrc j p q = determinant $ submatrix k l k l sm where
  sm = sylvester p q
  k = j + 1
  n = nrows sm
  l = n - j

psc :: (Num r, Ord r) => UPolynomial r -> UPolynomial r -> Set r
psc p q = Data.Set.fromList $  sub $ sylvester p q where
  strip m = submatrix 2 n' 2 n' m where
    n' = nrows m - 1
  sub m =
    if nrows m <= 2
      then [determinant m]
      else determinant m : sub (strip m)


-- trivial :: HasConstants r => UPolynomial r -> Bool
-- trivial = all isConstant
