module TestFunctionalMap
where

import           Data.FunctionalMap
import           Prelude            hiding (lookup)
import qualified Prelude            as P
import           Test.QuickCheck

-- test insertion.
prop_1 :: String -> Bool
prop_1 xs = 
	case (lookup 5 (insert 5 xs empty)) of
		Nothing -> False
		Just ys -> ys == xs

-- test deletion.
prop_2 :: String -> Bool
prop_2 xs = 
	case (lookup 5 (delete 5 (insert 5 xs empty))) of
		Nothing -> True
		Just _ -> False

-- test union (??)
prop_3 :: String -> Bool
prop_3 xs = undefined

-- ----------------------------------------

quickCheck' :: Testable prop => prop -> IO ()
quickCheck' = quickCheckWith stdArgs{maxSuccess=200}

main
  = do quickCheck'  prop_1
       quickCheck'  prop_2
       quickCheck'  prop_3
       
-- ----------------------------------------

