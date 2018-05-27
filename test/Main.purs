module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Default (class PartialDefault, withRequired)
import Data.Maybe (Maybe(..))

data Test = Test {a :: Maybe String, b :: String}

instance test ∷ PartialDefault Test {b :: String} where
  withRequired {b} = Test {a : Nothing, b}

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  let (Test test) = (withRequired {b: "as"}) :: Test
  log $ "You should add some tests." <> test.b
