module Polynomial where
import qualified Data.Map as M
import Data.Char (Char)


type Variable = Char

newtype Monomial  = Monomial { ixmap :: M.Map Variable Int }
	deriving (Eq, Ord)


instance Show Monomial where
	show m = show (ixmap m)



type Term k = (k, Monomial)


tvar :: Num k => Variable -> Term k  
tdeg :: Term k -> Int
tmult :: Num k => Term k -> Term k -> Term k

tvar x = (1, Monomial $ M.singleton x 1)
tdeg = sum . M.elems . ixmap . snd
tmult (k1,m1) (k2,m2) = (km, mm) where
	km = k1 * k2
	mm = Monomial $ M.unionWith (+) (ixmap m1) (ixmap m2)

newtype Polynomial k = Poly { coeffs :: M.Map Monomial k }
	deriving (Eq, Ord)

instance (Eq k, Num k) => Num (Polynomial k) where
	(+) = plus
	(*) = times
	negate = neg
	fromInteger n = constant (fromInteger n)

mone :: Monomial
mone = Monomial M.empty

constant :: k -> Polynomial k
constant k = Poly $ M.singleton mone k


fromTerms :: (Eq k, Num k) => [Term k] -> Polynomial k
fromTerms = Poly . purge . M.fromListWith (+) . map (\(k,m) -> (m,k))   where
	purge = M.filter (/=0)  

neg p = Poly (M.map negate $ coeffs p)
plus (Poly p) (Poly q) = Poly $ M.unionWith (+) p q 
times p q = fromTerms pqs where
	lcoeffs = map (\(m,k) -> (k,m)) . M.toList . coeffs
	pqs = [tmult tp tq  | tp <- lcoeffs p, tq <- lcoeffs q]
