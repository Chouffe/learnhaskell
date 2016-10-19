import qualified Monoidspec as MS (main)
import qualified Semigroupspec as SS (main)

main :: IO ()
main = do
        putStrLn "\nSemigroup Tests"
        SS.main

        putStrLn "\nMonoid Tests"
        MS.main
