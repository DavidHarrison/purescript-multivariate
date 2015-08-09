module Test.Main where

import Prelude

import Control.Monad.Eff.Console (log)
import Data.Iso                  (Iso())
import Data.Tuple                (Tuple(..))

import Data.Function.Multivariate

main = do
  log "You should add some tests."

-- Testing that type classes provide the right conversions

multiTest1 :: forall a b c d. (Tuple a (Tuple b (Tuple c Unit)) -> d) -> Fn (Cons a (Cons b (Cons c Nil))) d
multiTest1 = toMulti

multiTest2 :: forall a b c d. Fn (Cons a (Cons b (Cons c Nil))) d -> (Tuple a (Tuple b (Tuple c Unit)) -> d)
multiTest2 = fromMulti

isoTest1 :: forall a b c d. Iso (a -> b -> c -> d) (Fn (Cons a (Cons b (Cons c Nil))) d)
isoTest1 = isoArrFnS $ isoArrFnS $ isoArrFnS isoConstFnZ

isoTest2 :: forall a b c d. Iso (Tuple a (Tuple b c) -> d) (Fn (Cons a (Cons b (Cons c Nil))) d)
isoTest2 = isoUncurriedFnS $ isoUncurriedFnS $ isoArrFnS isoConstFnZ
