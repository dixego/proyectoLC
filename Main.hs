import Data.BDD
import Data.Prop
import Data.BDD.Visualization
import Data.Text.Lazy.IO as TIO


test1 = (PVar 1 `PImpl` (PVar 2 `PImpl` PVar 3)) `PImpl` (PVar 2 `PImpl` (PVar 1 `PImpl` PVar 3))
test2 = ((PVar 1 `PAnd` PVar 2) `POr` (PVar 1 `PAnd` PVar 3)) `PImpl` (PVar 1 `PAnd` (PVar 2 `POr` PVar 3))
test3 = (PVar 1 `PImpl` PVar 2)
test4 = (PVar 1 `PAnd` PVar 2) `POr` (PVar 3 `PAnd` (PVar 1 `PImpl` PVar 2))
test5 = (PVar 1 `POr` PVar 2) `PAnd` (PVar 3 `POr` PVar 4) `PAnd` (PVar 5 `POr` PVar 6)
test5' = (PVar 1 `POr` PVar 4) `PAnd` (PVar 2 `POr` PVar 5) `PAnd` (PVar 3 `POr` PVar 6)

main = do
  let tests = [test1, test2, test3, test4, test5, test5']
  let results = map testSat tests
  let names = map (<> ".dot") ["test1", "test2", "test3", "test4", "test5", "test5_1"]
  print results

  mapM_ (uncurry TIO.writeFile) (zip names $ map (toDot.build) tests)
