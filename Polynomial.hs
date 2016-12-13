module Polynomial where
import qualified Data.Map as M
import Data.Char (Char)
import qualified Data.Set as S
import Data.Ord (comparing)
import qualified Data.List as L
import Prelude hiding (exp)
import UPolynomial (UPolynomial(UP), Eqnum, Sympy(..))
import qualified UPolynomial as P

minf :: Int
minf = minBound

type Variable = Char

newtype Monomial  = Monomial { exp :: M.Map Variable Int } deriving (Eq, Ord)

instance Show Monomial where
    show m = let lm = M.toList $ exp m
         in if null lm
            then ""
            else concat $ map shvar lm  where
                shvar (_,0) = ""
                shvar (v,1) = [v]
                shvar (v,d) = v:("^" ++ show d)

instance (Num k, Eq k, Show k) => Show (Polynomial k) where
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
            vs -> Just . head . L.reverse . L.sort $ vs

mdeg v (Monomial m) = maybe 0 id $ M.lookup v m

newtype Term k = T (k, Monomial)

instance (Num k, Eq k, Show k) => Show (Term k) where
    show (T (0, _)) = "0"
    -- show (T (1, m)) = show m
    show (T (c, m)) = "(" ++ show c ++ ") " ++ show m


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

newtype Polynomial k = Poly { coeffs :: M.Map Monomial k }
    deriving (Eq, Ord)

instance (Eq k, Num k) => Num (Polynomial k) where
    (+) = plus
    (*) = times
    abs = undefined
    signum = undefined
    negate = neg
    fromInteger n = constant (fromInteger n)

instance Functor Polynomial where
    fmap f = Poly . fmap f . coeffs


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


toUnivariate :: (Num k, Eq k) => Variable -> Polynomial k -> UPolynomial (Polynomial k)
fromUnivariate :: Eqnum r => Variable -> UPolynomial (Polynomial r) -> Polynomial r
toUnivariate v p = UP $ foldl ins M.empty $
    map (exp . tunivariate v) $  toTerms p
  where ins m (e,c) = M.insertWith (+) e c m
        exp (T (c, Monomial m)) =
            case M.toList m of
              []                -> (0, c)
              [(w, e)] | w == v -> (e, c)
freeOf :: Variable -> Polynomial r -> Bool
freeOf v p =  not $ or [v `elem` (M.keys m) | T (_, Monomial m) <- toTerms p]
fromUnivariate v p =
  if all (freeOf v) (P.toList p)
  then fromTerms $ [T (c, expand i m) | (i, u) <- M.toList (P.coeffs p), T (c, m) <- toTerms u]
  else error $ "Not free from " ++ show v
  where expand :: Int -> Monomial -> Monomial
        expand j  (Monomial m) = Monomial $ M.insert v j m




mone :: Monomial
mone = Monomial M.empty

constant :: (Num k, Eq k) => k -> Polynomial k
isConstant :: Polynomial k -> Bool
constant k = fromTerms [tconst k]
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

neg p = Poly (M.map negate $ coeffs p)
plus (Poly p) (Poly q) = Poly $ M.unionWith (+) p q
times p q = fromTerms pqs where
    pqs = [tmult tp tq  | tp <- toTerms p, tq <- toTerms q]

degree :: Variable -> Polynomial k -> Int
ltf :: Polynomial k -> Maybe (Term k)
lcof :: (Eq k,Num k) => Polynomial k ->  k
mvar :: Polynomial r -> Maybe Variable
level :: [Variable] -> Polynomial r -> Maybe Int
maindeg :: Polynomial r -> Int
mvar p = case L.reverse . L.sort . S.toList $ vars [p] of
    []  -> Nothing
    v:_ -> Just v
level vs p = case (mvar p) of
            Just v -> case L.elemIndex v vs of
                         Just k -> Just k
                         _ -> Nothing
            _      -> Nothing
maindeg p = maybe minf (flip degree p) (mvar p)

toArray :: Num k => Int -> Polynomial k -> [k]
toArray n p =
   case S.toList $ vars [p] of
        [v] -> reverse . map snd . M.toList $ mp `M.union` mz v
        []  -> case toTerms p of
                 []  -> take n $ repeat 0
                 [T (c,_)]  -> reverse $ c:(take (n-1) $ repeat 0)
        _   -> error "not univariate"
     where
       m 0 _ = Monomial M.empty
       m i v = Monomial $ M.singleton v i
       mp = coeffs p
       mz v = M.fromList [(m k v, 0) | k <- [0..n-1]]

degree v = maximum . map (tdeg v) . toTerms

ltf p = case toTerms p of
    [] -> Nothing
    ts -> case mvar p of
          Nothing -> Nothing
          Just v -> Just $ L.maximumBy (comparing (tdeg v)) ts

lcof = maybe 0 (\(T (c,_)) -> c) . ltf



instance Eqnum r => Eqnum (Polynomial r)

instance Sympy Monomial where
  sympy m = if null s then "1" else s where
    s = concat [(v:'^':show e) | (v,e) <- M.toList . exp $ m,  e/=0]

instance (Eqnum r, Sympy r) => Sympy (Term r) where
  sympy (T (0,m)) = "0"
  sympy (T (1,m)) = sympy m
  sympy (T (c,m)) = sympy c ++ " " ++
      (case sympy m of
          "1" -> ""
          sm  -> sm)

instance (Eqnum r, Sympy r) => Sympy (Polynomial r) where
  sympy = L.intercalate " + " . map sympy .toTerms


type UPoly r = UPolynomial (Polynomial r)

proj1, proj2, proj3 :: (Ord r, Eqnum r) => [UPoly r] -> [S.Set (UPoly r)]
proj1 ps = [psc' p (P.diff p) | p <- ps, P.deg p >= 2]
  where psc' p q = S.map P.const $ P.psc p q
proj2 ps = [psc' p q | (i,p) <- eps, (j,q) <- eps, i/=j, min (P.deg p) (P.deg q) >= 1]
  where eps = zip [0..] ps
        psc' p q = S.map P.const $ P.psc p q
proj3 ps = [S.fromList [P.const (P.lc p), P.tail p] | p <- ps, P.deg p >= 1 , (not . isConstant . P.lc) p]

type SetPoly r = S.Set (UPoly r)

trivial :: Eqnum r => UPoly r -> Bool
trivial = all isConstant . P.toList


proj :: (Ord r, Eqnum r) => Variable -> S.Set (Polynomial r) -> SetPoly r
proj v ps = ft trivial $ ss
  where ps' = map (toUnivariate v) . S.toList $ ps
        ft f = S.filter (not . f)
        ss = S.unions . concat $ map (\f -> f ps') [proj1, proj2, proj3]
