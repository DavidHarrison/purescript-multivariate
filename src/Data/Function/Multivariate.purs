module Data.Function.Multivariate (Fn()) where

import Prelude

import Data.Tuple (Tuple())
--import Data.Iso (Iso(..))

-- | The type of native JavaScript functions, which can include multiple
-- | arguments. In a dependently typed setting, the kind would be
-- | `List * -> * -> *`, which we represent in PureScript using `Tuple` to
-- | represent `Cons` and `Unit` to represent `Nil`. Argument list is
-- | therefore not a real PureScript value, and should not be treated as such.
foreign import data Fn :: * -> * -> *

foreign import getConst :: forall a. Fn Unit a -> a

foreign import fnCurry :: forall args ret a. Fn (Tuple a args) ret -> a -> Fn args ret

foreign import fnUncurry :: forall args ret a. (a -> Fn args ret) -> Fn (Tuple a args) ret

{-
isoFnZConst :: Iso (Fn Unit a) a
isoFnZConst = Iso getConst pure

isoFnZArr :: Iso (Fn Unit a) (Unit -> a)
isoFnZArr = Iso (const <<< getConst) (pure <<< ($ unit))

isoFnSArr :: Iso (Fn args ret) f -> Iso (Fn (Tuple a args) ret) (a -> f)
isoFnSArr (Iso f g) = Iso (fnCurry <<< f) (fnUncurry <<< g)
-}

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
