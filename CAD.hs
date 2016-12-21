{-# LANGUAGE TypeSynonymInstances,
             FlexibleInstances     #-}
module CAD where
import Polynomial
import Data.Set (Set)
import qualified Data.Set as S
import Data.Ratio (denominator, numerator)
import UPolynomial (UPolynomial)
import Sturm (split, Interval, midpoint)
import Control.Monad (zipWithM_)

instance HasConstants Z where
  isConstant _ = True

instance HasConstants Q where
    isConstant _ = True


projection :: (Show r, Fractional r, Ord r, HasConstants r) => [Variable] -> Set (Polynomial r) -> [Set (Polynomial r)]
projection [] ps  = []
projection (v:vs) ps = ps':projection vs ps' where
  ps' =  S.map id . proj . S.map (toUnivariate v) $ ps



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


type Z = Integer
type Q = Rational


var3 :: [Polynomial Z]
var3 = map single  "xyz"


examples :: [Set (Polynomial Z)]
toQ :: Polynomial Z -> Polynomial Q
fromQ :: Polynomial Q -> Polynomial Z
fromQ = fmap (\q -> if denominator q == 1 then numerator q else error "Cannot convert from Q")
toQ = fmap fromIntegral

examples =
  let [x,y,z] = map single "xyz"
      p1 = [x^2 + y^2 + z^2 -2]
      p2 = [x * y * z - x^2 - y^2]
      p3 = [p, q, r] where
        p = z
        q = x^2 + y^2 - z
        r = x^2 + y^2 - 2 * x
  in map S.fromList [p1, p2, p3]


main = zipWithM_ (\ps msg -> do
        putStrLn ""
        putStrLn msg
        let qs = S.map toQ ps
        mapM_ (print . S.map (fromQ . toMonic))$ projection "zyx"  qs) examples msgs
  where
    msgs = lines "For S^2:\nFor the example in [SAG, pg 41]:\nFor the paraboloid:"
