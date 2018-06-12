module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Default (class PartialDefault, withRequired)
import Data.Maybe (Maybe(..))

data Test = Test {a :: Maybe String, b :: String}

instance test ∷ PartialDefault Test {b :: String} where
  withRequired {b} = Test {a : Nothing, b}

data T2 f a b
  = C1 f a
  | C2 f b

type T2Required = T2 {a :: String, b∷ String} Int

type T2Options = T2 {a :: String, b:: String, c∷ Boolean} {e∷Int, d::Boolean}

instance test2 :: PartialDefault T2Options T2Required where
  withRequired (C1 {a}) = C1 {a, b: "", c: false}
  withRequired (C2 int) = C2 {e:int, d: false}

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  let (Test test) = (withRequired {b: "as"}) :: Test
  log $ "You should add some tests." <> test.b
