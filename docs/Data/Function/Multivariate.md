## Module Data.Function.Multivariate

`Fn` provides the type of native JavaScript functions, which can include
multiple arguments. In a dependently typed setting, the kind would be
`List * -> * -> *`, which we represent in PureScript using `Tuple` to
represent `Cons` and `Unit` to represent `Nil`. Argument list is
therefore not a real PureScript value, and should not be treated as such.

#### `Fn`

``` purescript
data Fn :: * -> * -> *
```

##### Instances
``` purescript
instance isoConstFnZ :: Isomorphic ret (Fn Unit ret)
instance isoArrFnZ :: Isomorphic (Unit -> ret) (Fn Unit ret)
instance isoCurriedFnS :: (Isomorphic (b -> c) (Fn args ret)) => Isomorphic (Tuple a b -> c) (Fn (Tuple a args) ret)
instance isoArrFnS :: (Isomorphic f (Fn args ret)) => Isomorphic (a -> f) (Fn (Tuple a args) ret)
instance functorFn :: Functor (Fn args)
instance applyFn :: Apply (Fn args)
instance applicativeFn :: Applicative (Fn args)
instance bindFn :: Bind (Fn args)
instance monadFn :: Monad (Fn args)
```

#### `fnCurry`

``` purescript
fnCurry :: forall args ret a. Fn (Tuple a args) ret -> a -> Fn args ret
```

`O(n)` where `n` is the number of arguments.

#### `fnUncurry`

``` purescript
fnUncurry :: forall args ret a. (a -> Fn args ret) -> Fn (Tuple a args) ret
```

`O(n)` where `n` is the number of arguments.

#### `Multivariate`

``` purescript
class Multivariate a where
  toMulti :: forall r. (a -> r) -> Fn a r
  fromMulti :: forall r. Fn a r -> a -> r
```

Currently one of two options for converting to and from multivariate
functions. This is probably more reliable in terms of avoiding type
ambiguity, but may not be as flexible as `Isomorphic` for direct
conversion of PureScript functions.

##### Instances
``` purescript
instance multiUnit :: Multivariate Unit
instance multiCurry :: (Multivariate args) => Multivariate (Tuple a args)
```

#### `Isomorphic`

``` purescript
class Isomorphic a b where
  to :: a -> b
  from :: b -> a
```

Another option for converting to and from multivariate functions. It can
be more flexible than `Multivariate` in its ability to directly convert
to and from PureScript functions, but may suffer from type ambiguity.

##### Instances
``` purescript
instance isoRefl :: Isomorphic a a
instance isoConstFnZ :: Isomorphic ret (Fn Unit ret)
instance isoArrFnZ :: Isomorphic (Unit -> ret) (Fn Unit ret)
instance isoCurriedFnS :: (Isomorphic (b -> c) (Fn args ret)) => Isomorphic (Tuple a b -> c) (Fn (Tuple a args) ret)
instance isoArrFnS :: (Isomorphic f (Fn args ret)) => Isomorphic (a -> f) (Fn (Tuple a args) ret)
```


