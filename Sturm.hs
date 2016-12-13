module Sturm where
import UPolynomial hiding (tail)
import Data.Ord


type Sturm r = [UPolynomial r]
type Interval r = (r, r)

sturm :: (Eqnum r, Fractional r) => UPolynomial r -> Sturm r
sturm p = stseq p (diff p) where
  stseq p 0 = [p]
  stseq p q = p:stseq q (-r) where
    r = remainder p q

vp :: (Ord r, Eqnum r) => Sturm r -> r -> Int
vp ss x = length $ filter id $ zipWith (/=) (tail sgs) (init sgs) where
      sgs = filter (/=0) . map (signum . eval x) $ ss

nroots :: (Ord r, Eqnum r) => Sturm r -> Interval r -> Int
nroots ss (a, b) = vp ss a - vp ss b

bound :: UPolynomial Q -> Float
bound p = maximum [(d * c)**(1.0/ fromIntegral i) | (i,c) <- tail ec] where
 a = toList p
 d = fromIntegral $ deg p
 Just a0 = lcof p
 a' :: [Float]
 a' = map (abs . (/fromRational a0) . fromRational) a
 ec = zip [0..] (reverse a')



p, x :: UPolynomial Q
x = fromList [0, 1]
p = x^3 - 3*x +1
