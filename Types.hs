import Data.Set (Set)
import Data.Polynomial (Polynomial)
import Data.Ratio (Rational)

data Cell r = Between (Fn r) (Fn r)
	| Lt (Fn r)
	| Gt (Fn r)
	| Section (Fn r)

type Poly = Polynomial Rational
type Stack r = [Fn r]
type Pols v = Set (Poly v) 

isSector :: Cell r -> Bool
isSector (Section _) = False
_                    = True


endian = BE

proj :: Pols r -> Pols r

ldcf :: Poly r -> r
ldcf  = head . polyCoeffs endian 

ldt :: Poly r -> Poly r
ldt f = poly (ldcf f):zeros where
	zeros = take (poyDegree f) $ repeat zero

red :: Poly r -> Poly r
red f = f - ldt f

