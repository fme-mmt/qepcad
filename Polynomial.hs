module Polynomial where
import qualified Data.Map as M
import Data.Char (Char)
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Ord (comparing)
import qualified Data.List as L
import Prelude hiding (exp)
import UPolynomial
import Subresultant (psrc)



class HasConstants r where
  isConstant :: r -> Bool

minf :: Int
minf = minBound

type Variable = Char

newtype Monomial  = Monomial { exp :: M.Map Variable Int } deriving (Eq, Ord)

instance Show Monomial where
    show m = let lm = M.toList $ exp m
         in if null lm
            then ""
            else concatMap shvar lm  where
                shvar (_,0) = ""
                shvar (v,1) = [v]
                shvar (v,d) = v:("^" ++ show d)

instance (Num k, Eq k, Show k, HasConstants k) => Show (Polynomial k) where
    show p = case toTerms p of
        [] -> "0"
        ts -> L.intercalate " + " . map show $ ts



mdiff :: Num k => Variable -> Monomial -> (Int, Monomial)
mmvar :: Monomial -> Maybe Variable
mdeg :: Variable -> Monomial -> Int
mdiff v  (Monomial m) = (i, Monomial m') where
  m' = M.filter (/=0) $  M.adjust (\x -> x-1) v m
  i = maybe 0 id $ M.lookup v m

mmvar m = case M.keys (exp m) of
            []  -> Nothing
            vs -> Just . last . L.sort $ vs

mdeg v (Monomial m) = fromMaybe 0 $ M.lookup v m

newtype Term k = T (k, Monomial)

instance (Num k, Eq k, Show k, HasConstants k) => Show (Term k) where
    show (T (0, _)) = "0"
    show (T (1, m)) = show m
    show (T (c, m)) | isConstant c = show c ++ show m
    show (T (c, m))                = "(" ++ show c ++ ")" ++ show m


isUnivariate :: Polynomial r -> Bool
reduce :: (Num r, Eq r) => Polynomial r -> Maybe (UPolynomial r)
isUnivariate = (<=1) . S.size . vars . (:[])
reduce p = if isUnivariate p
  then Just $ fromMap mp
  else Nothing
  where
    mp = foldr (\(e, c) m -> M.insertWith (+) e c m) M.empty [(exp m, c) | T (c, m) <- toTerms p]
    exp (Monomial m) = case M.elems m of
            [] -> 0
            [e]  -> e

tconst :: k -> Term k
tvar :: Num k => Variable -> Term k
tdeg :: Variable -> Term k -> Int
tmult :: Num k => Term k -> Term k -> Term k
tdiff :: Num k => Variable -> Term k -> Term k
tmvar :: Term k -> Maybe Variable
tdiff v (T (c, m)) = T (c * fromIntegral i, m') where
  (i, m') = mdiff v m
tconst c = T (c, mone)
tvar x = T (1, Monomial $ M.singleton x 1)
tdeg v (T (_, m)) = mdeg v m
tmult (T (c1,m1)) (T (c2,m2)) = T (cm, mm) where
    cm = c1 * c2
    mm = Monomial $ M.unionWith (+) (exp m1) (exp m2)
tmvar (T (_,m)) = mmvar m

newtype Polynomial k = Poly (M.Map Monomial k)
    deriving (Eq, Ord)

instance (Eq k, Num k) => Num (Polynomial k) where
  negate (Poly cs) = Poly (M.map negate cs)
  (Poly p) + (Poly q) = Poly $ M.unionWith (+) p q
  p * q = fromTerms pqs where
    pqs = [tmult tp tq  | tp <- toTerms p, tq <- toTerms q]
  abs = undefined
  signum = undefined
  fromInteger n = constant (fromInteger n)

instance Functor Polynomial where
    fmap f (Poly cs) = Poly $ fmap f cs


tvars :: Term k  ->  S.Set Variable
vars :: [Polynomial k] -> S.Set Variable
areUnivariate :: [Polynomial k] -> Bool

tvars (T (_,m)) = S.fromList . M.keys $ exp m
vars = S.unions . map (S.unions . map tvars . toTerms)
areUnivariate = (<=1) . S.size . vars


tunivariate :: (Eq k, Num k) => Variable -> Term k -> Term (Polynomial k)
tunivariate z (T (k, Monomial m)) = T (p, Monomial mz) where
    (mz,mnz) = M.partitionWithKey (\v _ -> v==z) m
    p = fromTerms [T (k, Monomial mnz)]


toUnivariate   :: (Num r, Eq r) => Variable -> Polynomial r -> UPolynomial (Polynomial r)
fromUnivariate :: (Num r, Eq r) => Variable -> UPolynomial (Polynomial r) -> Polynomial r
toUnivariate v p = Upol . normal . reverse $ cs
  where ins m (e,c) = M.insertWith (+) e c m
        exp (T (c, Monomial m)) =
            case M.toList m of
              []                -> (0, c)
              [(w, e)] | w == v -> (e, c)
        mp = foldl ins M.empty . map (exp . tunivariate v) $ toTerms p
        d = maximum $ M.keys mp
        cs = [fromMaybe 0 (M.lookup k mp) | k <- [0..d]]

freeOf :: Variable -> Polynomial r -> Bool
freeOf v p =  not $ or [v `elem` M.keys m | T (_, Monomial m) <- toTerms p]
fromUnivariate v p =
  if all (freeOf v) (coeffs p)
  then fromTerms [T (c, expand i m) | (u, i) <- enumerate p, T (c, m) <- toTerms u]
  else error $ "Not free from " ++ show v
  where expand :: Int -> Monomial -> Monomial
        expand j  (Monomial m) = Monomial $ M.insert v j m




mone :: Monomial
single :: Num r => Variable -> Polynomial r
mone = Monomial M.empty
single v = Poly $ M.singleton m 1 where
  m = Monomial $ M.singleton v 1

constant :: (Num k, Eq k) => k -> Polynomial k
constant k = fromTerms [tconst k]

instance HasConstants (Polynomial r) where
  isConstant p = case toTerms  p of
    []                  -> True
    [T (_, Monomial m)] -> M.null m
    _                   -> False


fromTerms :: (Eq k, Num k) => [Term k] -> Polynomial k
toTerms :: Polynomial k -> [Term k]
fromTerms = Poly . purge . M.fromListWith (+) . map (\(T (c,m)) -> (m,c))  where
    purge = M.filter (/=0)
toTerms (Poly ps) = map (\(m,c) -> T (c,m)) $ M.toList ps


simplify :: (Num k, Eq k) => Polynomial k -> Polynomial k
simplify = fromTerms . toTerms

fullsimplify :: (Eq r, Num r) => UPoly r -> UPoly r
fullsimplify = Upol . filter (/=0) . map Polynomial.simplify . coeffs


degree :: Variable -> Polynomial k -> Int
ltf :: Polynomial k -> Maybe (Term k)
lcof :: (Eq k,Num k) => Polynomial k ->  k
mvar :: Polynomial r -> Maybe Variable
level :: [Variable] -> Polynomial r -> Maybe Int
maindeg :: Polynomial r -> Int
mvar p = case L.sortBy (flip compare) . S.toList $ vars [p] of
    []  -> Nothing
    v:_ -> Just v
level vs p = case mvar p of
            Just v -> case L.elemIndex v vs of
                         Just k -> Just k
                         _ -> Nothing
            _      -> Nothing
maindeg p = maybe minf (`degree` p) (mvar p)

degree v = maximum . map (tdeg v) . toTerms

ltf p = case toTerms p of
    [] -> Nothing
    ts -> case mvar p of
          Nothing -> Nothing
          Just v -> Just $ L.maximumBy (comparing (tdeg v)) ts

lcof = maybe 0 (\(T (c,_)) -> c) . ltf



toMonic :: (Eq r, Fractional r) => Polynomial r -> Polynomial r
toMonic p = fmap (/c0) p where c0 = lcof p


type UPoly r = UPolynomial (Polynomial r)


proj :: (Show r, Ord r, Num r, HasConstants r) => S.Set (UPoly r) -> S.Set (Polynomial r)
proj sps = S.unions $ map (\f -> f ps) [proj1, proj2, proj3, proj4] where
  ps = S.toList sps
  purge = S.fromList . filter (not . isConstant) . map Polynomial.simplify
  proj1 ps = purge [psrc j p (sep p) | p <- ps, let d = deg p,  d >= 2, j <- [0..d - 1]]
  proj2 ps = purge [psrc j p q | (k,p) <- ps', (l,q) <- ps', k < l, let d = min (deg p) (deg q), d >= 1, j <- [0..d-1]] -- must be [0..d]
    where ps' = zip [0..] ps
  proj3 ps = pj1 `S.union`  pj2
    where pj1 = S.fromList $ map (lc . snd) ps'
          pj2 = S.unions $ map (ipj . fst) ps'
          ipj j = let (p1, p:p2) = splitAt j ps
                  in proj $ S.fromList $ trunc p:(p1++p2)
          ps' = filter (cond . snd) $ zip [0..] ps
          cond p = deg p >= 1 && (not . isConstant . lc) p
  proj4 ps = S.fromList [c | p <- ps, deg p == 0, let c = lc p, not . isConstant $ c]
