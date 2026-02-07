module Main where

import qualified Continuation
import qualified Defunctionalization
import qualified Free
import qualified Freer
import qualified Codensity

main :: IO ()
main = do
  Continuation.example
  putStrLn ""
  Defunctionalization.example
  putStrLn ""
  Free.example
  putStrLn ""
  Freer.example
  putStrLn ""
  Codensity.example
