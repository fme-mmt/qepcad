module CAD where
import Polynomial
import UPolynomial (Eqnum)
import Data.Set (Set, map)
import Data.Set (fromList)

-- type SetPoly r = Set (Polynomial r)

iproj :: (Ord r, Eqnum r) => [Variable] -> Set  (Polynomial r) -> [Set (Polynomial r)]
iproj [] ps  = []
iproj (v:vs) ps = ps':(iproj vs ps') where
  ps' = Data.Set.map (fromUnivariate v) $ proj v ps

type Z = Integer

main :: IO ()
example =
  let p :: Polynomial Z
      p = x^2 + y^2 + z^2 -1
      q = x * y * z - x^2 - y^2
      [x,y,z] = Prelude.map (fromTerms . (:[]) . tvar)  "xyz"
      ps, qs :: Set (Polynomial Z)
      ps = fromList [p]
      qs = fromList [q]
  in [ps, qs]

main = do
  let [ps,qs] = example
  putStrLn "For P:"
  mapM_ print $ iproj "zyx" ps
  putStrLn "For Q:"
  mapM_ print $ iproj "zyx" qs
