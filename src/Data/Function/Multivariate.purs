-- | `Fn` provides the type of native JavaScript functions, which can include
-- | multiple arguments. In a dependently typed setting, the kind would be
-- | `List * -> * -> *`, which we represent in PureScript using `Tuple` to
-- | represent `Cons` and `Unit` to represent `Nil`. Argument list is
-- | therefore not a real PureScript value, and should not be treated as such.

module Data.Function.Multivariate
  ( Multivariate, toMulti, fromMulti
  , Isomorphic, to, from
  , Fn()
  , fnCurry
  , fnUncurry
  ) where

import Prelude

import Data.Tuple (Tuple(..))
--import Data.Iso (Iso(..))

foreign import data Fn :: * -> * -> *

-- | `O(1)`.
foreign import getConst :: forall a. Fn Unit a -> a

-- | `O(n)` where `n` is the number of arguments.
foreign import fnCurry :: forall args ret a. Fn (Tuple a args) ret -> a -> Fn args ret

-- | `O(n)` where `n` is the number of arguments.
foreign import fnUncurry :: forall args ret a. (a -> Fn args ret) -> Fn (Tuple a args) ret

-- | Currently one of two options for converting to and from multivariate
-- | functions. This is probably more reliable in terms of avoiding type
-- | ambiguity, but may not be as flexible as `Isomorphic` for direct
-- | conversion of PureScript functions.
class Multivariate a where
  toMulti :: forall r. (a -> r) -> Fn a r
  fromMulti :: forall r. Fn a r -> a -> r

instance multiUnit :: Multivariate Unit where
  toMulti = pure <<< ($ unit)
  fromMulti fn _ = getConst fn

instance multiCurry :: (Multivariate args) => Multivariate (Tuple a args) where
  toMulti f = fnUncurry \a -> toMulti \args -> f (Tuple a args)
  fromMulti fn (Tuple a args) = fromMulti (fnCurry fn a) args

-- | Another option for converting to and from multivariate functions. It can
-- | be more flexible than `Multivariate` in its ability to directly convert
-- | to and from PureScript functions, but may suffer from type ambiguity.
class Isomorphic a b where
  to   :: a -> b
  from :: b -> a

-- reify :: Isomorphic a b => Iso a b
-- reify = Iso to from

instance isoRefl :: Isomorphic a a where
  to   = id
  from = id

instance isoConstFnZ :: Isomorphic ret (Fn Unit ret) where
  to   = pure
  from = getConst

instance isoArrFnZ :: Isomorphic (Unit -> ret) (Fn Unit ret) where
  to   = pure <<< ($ unit)
  from = const <<< getConst

instance isoCurriedFnS :: (Isomorphic (b -> c) (Fn args ret)) => Isomorphic (Tuple a b -> c) (Fn (Tuple a args) ret) where
  to f                   = fnUncurry \a -> to \args -> f (Tuple a args)
  from fn (Tuple a args) = from (fnCurry fn a) args

instance isoArrFnS :: (Isomorphic f (Fn args ret)) => Isomorphic (a -> f) (Fn (Tuple a args) ret) where
  to   = fnUncurry <<< map to
  from = map from  <<< fnCurry

foreign import fnMap :: forall args a b. (a -> b) -> Fn args a -> Fn args b

foreign import fnApply :: forall args a b. Fn args (a -> b) -> Fn args a -> Fn args b

foreign import fnPure :: forall args a. a -> Fn args a

foreign import fnBind :: forall args a b. Fn args a -> (a -> Fn args b) -> Fn args b

instance functorFn :: Functor (Fn args) where
  map = fnMap 

instance applyFn :: Apply (Fn args) where
  apply = fnApply

instance applicativeFn :: Applicative (Fn args) where
  pure = fnPure

instance bindFn :: Bind (Fn args) where
  bind = fnBind

instance monadFn :: Monad (Fn args)

-- NOTE: we cannot define `Semigroupoid` or `Category` instances for `Fn`
--       because its two arguments represent (though aren't actually)
--       different kinds.
