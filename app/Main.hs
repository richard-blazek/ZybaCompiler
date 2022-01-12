module Main where

import qualified System.IO as IO
import qualified System.Environment as Env

import Loader (load)
import Codegen (gen)
import Functions ((!?), (??=))
import Fallible (Fallible (..), FallibleT (..))

compile :: [String] -> IO ()
compile (input : output : args) = do
  result <- runFallibleT $ fmap (uncurry gen) $ load input
  case result of
    Ok code -> IO.writeFile output code
    Error err -> IO.hPutStrLn IO.stderr err
compile _ = IO.hPutStrLn IO.stderr "Input or output file missing"

main :: IO ()
main = Env.getArgs >>= compile
