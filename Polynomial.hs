module Polynomial where
import qualified Data.Map as M
import Data.Char (Char)
import qualified Data.Set as S
import Data.Ord (comparing)
import Data.List (inits, sortBy, maximumBy)

type Variable = Char

newtype Monomial  = Monomial { ixmap :: M.Map Variable Int }
    deriving (Eq, Ord)


instance Show Monomial where
    show m = let lm = M.toList $ ixmap m
         in if null lm 
            then "1"
            else concat $ map shvar lm  where
                shvar (_,0) = ""
                shvar (v,1) = [v]
                shvar (v,d) = v:("^" ++ show d)



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
    abs = undefined
    signum = undefined 
    negate = neg
    fromInteger n = constant (fromInteger n)


univariate :: (Eq k, Num k) => Variable -> Polynomial k -> Polynomial (Polynomial k)
univariate  x = fromTerms . map (tunivariate x) . toTerms 


tvars :: Term k  ->  S.Set Variable
vars :: Polynomial k -> S.Set Variable
isUnivariate :: Polynomial k -> Bool

tvars = S.fromList . M.keys . ixmap . snd
vars = S.unions . map tvars . toTerms  
isUnivariate p = S.size (vars p) == 1


tunivariate :: Num k => Variable -> Term k -> Term (Polynomial k)
tunivariate z (k, Monomial m) = (p, Monomial mz) where
    (mz,mnz) = M.partitionWithKey (\k _ -> k==z) m
    p = Poly $ M.fromList [(Monomial mnz, 1)]


mone :: Monomial
mone = Monomial M.empty

constant :: k -> Polynomial k
constant k = Poly $ M.singleton mone k


treverse (k,m) = (m,k)

fromTerms :: (Eq k, Num k) => [Term k] -> Polynomial k
toTerms :: Polynomial k -> [Term k]
fromTerms = Poly . purge . M.fromListWith (+) . map treverse  where
    purge = M.filter (/=0)  
toTerms (Poly ps) = map treverse $ M.toList ps

neg p = Poly (M.map negate $ coeffs p)
plus (Poly p) (Poly q) = Poly $ M.unionWith (+) p q 
times p q = fromTerms pqs where
    lcoeffs = map treverse . M.toList . coeffs
    pqs = [tmult tp tq  | tp <- lcoeffs p, tq <- lcoeffs q]

deg :: Polynomial k -> Int
ltf :: Polynomial k -> Maybe (Term k)
lcof :: Polynomial k -> Maybe k

deg = maximum . map tdeg . toTerms
ltf p = case toTerms p of
    [] -> Nothing 
    ts -> Just $ maximumBy (comparing tdeg) ts 

lcof = fmap fst . ltf


truncs :: (Num k,Eq k) => Polynomial k -> [Polynomial k]
truncs = tail . map fromTerms . inits . sortBy (comparing tdeg) . toTerms


p :: Polynomial Integer
p = (x + y) * (x - y) + x ^ 3 where
    x = fromTerms [tvar 'x']
    y = fromTerms [tvar 'y']

    
