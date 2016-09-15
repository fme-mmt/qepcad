import Data.Set (Set)
import Data.Ratio (Rational)
import Polynomial (Polynomial)

data Cell r = Between (Fn r) (Fn r)
	| Lt (Fn r)
	| Gt (Fn r)
	| Section (Fn r)

type Stack r = [Fn r]
type Pols v = Set (Polynomial v) 

isSector :: Cell r -> Bool
isSector (Section _) = False
_                    = True



proj :: Pols r -> Pols r
proj = undefined

ldcf :: Polynomial r -> r
ldcf  = head . polyCoeffs endian 

ldt :: Polynomial r -> Polynomial r
ldt f = poly (ldcf f):zeros where
	zeros = take (poyDegree f) $ repeat zero

red :: Polynomial r -> Polynomial r
red f = f - ldt f

