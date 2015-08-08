module Test.Main where

import Prelude

import Control.Monad.Eff.Console
import Data.Tuple (Tuple(..))

import Data.Function.Multivariate

main = do
  log "You should add some tests."

-- Testing that type classes provide the right conversions

multiTest1 :: forall a b c d. (Tuple a (Tuple b (Tuple c Unit)) -> d) -> Fn (Tuple a (Tuple b (Tuple c Unit))) d
multiTest1 = toMulti

multiTest2 :: forall a b c d. Fn (Tuple a (Tuple b (Tuple c Unit))) d -> (Tuple a (Tuple b (Tuple c Unit)) -> d)
multiTest2 = fromMulti

isoTest1 :: forall a b c d. (a -> b -> c -> d) -> Fn (Tuple a (Tuple b (Tuple c Unit))) d
isoTest1 = to

isoTest2 :: forall a b c d. Fn (Tuple a (Tuple b (Tuple c Unit))) d -> a -> b -> c -> d
isoTest2 = from

isoTest3 :: forall a b c d. (Tuple a (Tuple b c) -> d) -> Fn (Tuple a (Tuple b (Tuple c Unit))) d
isoTest3 = to

isoTest4 :: forall a b c d. Fn (Tuple a (Tuple b (Tuple c Unit))) d -> (Tuple a (Tuple b c) -> d)
isoTest4 = from
