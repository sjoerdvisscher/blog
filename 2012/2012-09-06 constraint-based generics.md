Constraint-based Generics
=========================

Inspired by Multiplate and lenses I have been searching for more ways to do generics in the style of applicative traversals. Last weekend I found an interesting method using [the ConstraintKinds extension](http://blog.omega-prime.co.uk/?p=127).

I hope you like reading code, because I think the easiest way to explain this is to show the code.

The single constructor case
---------------------------

It started with some code from Martijn van Steenbergen. He had a `Monoid` instance for a record type with 8 fields, with each field itself a `Monoid`. It was code that looks like you'd want to able to derive with generics. But the available generics packages seemed to be too heavy weight for such a small task. So I wondered what a simple solution could look like and this is what I came up with:

```Haskell
class MonoidRecord t where
  build :: (forall s. Monoid s => (t -> s) -> s) -> t

memptyRecord :: MonoidRecord t => t
memptyRecord = build (\_ -> mempty)

mappendRecord :: MonoidRecord t => t -> t -> t
mappendRecord t1 t2 = build (\proj -> proj t1 <> proj t2)

mconcatRecord :: MonoidRecord t => [t] -> t
mconcatRecord ts = build (\proj -> mconcat (map proj ts))
```

Here's how it works: `build` expects a function that can generate a value for each field of `t`, given a projector function `t -> s` and the fact that the field type is s `Monoid` instance. And since this function has to work for all types `s`, the only thing you can do is to use the methods from the `Monoid` class, combined with using the projector function on any values of type `t` that you might already have.

Here's a `MonoidRecord` instance for an example type `P`:

```Haskell
data P a = P { p1 :: a, p2 :: String }

instance Monoid a => MonoidRecord (P a) where
  build f = P (f p1) (f p2)

instance Monoid a => Monoid (P a) where
  mempty = memptyRecord
  mappend = mappendRecord
  mconcat = mconcatRecord
```

Neat, but still quite a bit of work for just some `Monoid` instances. It would be nice if we could use this method for writing instances of other classes too. So let's turn `Monoid` into a constraint type variable:

```Haskell
build :: (forall s. c s => (t -> s) -> s) -> t
```

But the type variable `c` only appears in the constraint, and it is not possible to do unification in constraints. For example: how should the constraint `(c a, Show a)` be unified with `(Show a, b a)`? So we will have to add a proxy value that mentions `c` in its type:

```Haskell
data For (c :: * -> Constraint) = For

build :: For c -> (forall s. c s => (t -> s) -> s) -> t
```

There's one more problem. With `build` this way, the instance for `P` would become:

```Haskell
instance (c a, c String) => Record (P a) where
  build For f = P (f p1) (f p2)
```

But `c` is not visible outside `build`, so we'll have to move the constraints to `build`, with a type family:

```Haskell
class Record t where
  type Constraints t c :: Constraint
  build :: Constraints t c => For c -> (forall s. c s => (t -> s) -> s) -> t
```

And now the instance for `P` becomes:

```Haskell
instance Record (P a) where
  type Constraints (P a) c = (c a, c String)
  build For f = P (f p1) (f p2)
```

The `Constraints` type instance says: if you want an instance of `c` for `P a`, you'll need an instance `c a` and an instance `c String`. And now we can write generic functions for f.e. `Num`:

```Haskell
plus :: (Record t, Constraints t Num) => t -> t -> t
plus s t = build (For :: For Num) (\proj -> proj s + proj t)
```

Note that we now pass the value `For` of type `For Num` to `build`, so build knows which type class to use.

For some generic methods, for example for the `Enum` methods, you want to communicate information between fields. Applicative functors help out here, for example you could use the applicative functor (and monad) `State`. The applicative version of `build` looks like this: 

```Haskell
class Record t where
  type Constraints t c :: Constraint
  buildA :: (Constraints t c, Applicative f) => For c -> (forall s. c s => (t -> s) -> f s) -> f t

build :: (Record t, Constraints t c) => (forall s. c s => (t -> s) -> s) -> t
build for f = runIdentity $ buildA for (Identity . f)
```

`build` is then `buildA` specialized to the identity functor. Writing instances now starts to look like writing an instance for `Traversable`:

```Haskell
instance Record (P a) where
  type Constraints (P a) c = (c a, c String)
  buildA For f = P <$> f p1 <*> f p2
```

Multiple constructors
---------------------

To extend this method to data types with multiple constructors, `buildA` will have to return a list, with one value
for each constructor. And we need a function that gives the index in the list for a given constructor.

```Haskell
class ADT t where

  ctorIndex :: t -> Int

  type Constraints t c :: Constraint
  buildsA :: (Constraints t c, Applicative f) => For c -> (forall s. c s => (t -> s) -> f s) -> [f t]
```

Now we have a generic framework for algebraic data types. I'll show how to write generic versions of the [binary  package](http://hackage.haskell.org/package/binary). First I need a version of `buildsA` that collects values using the `Monoid` class. It uses the `Constant` applicative functor, similar to how `foldMap` can be derived from `traverse`.

```Haskell
mbuilds :: forall t c m. (ADT t, Constraints t c, Monoid m) => For c -> (forall s. c s => (t -> s) -> m) -> [m]
mbuilds for f = fmap getConstant ms
  where
    ms :: [Constant m t]
    ms = buildsA for (Constant . f)
```

Then I need to make the `Put` type from `binary` a `Monoid`, which it trivally is, because it is just a synonym for `PutM ()` and `PutM` is a monad.

```Haskell
instance Monoid B.Put where
  mempty = return ()
  mappend = (>>)
```

Next the actual generic `put`. As is customary in `Binary` instances, the first byte is the constructor index, and the following bytes are the serialized components. We use `mbuilds` to generate a `Put` value for each constructor and then use `ctorIndex` to pick out the right one for `t`.

```Haskell
putADT :: (ADT t, Constraints t B.Binary) => t -> B.Put
putADT t = B.putWord8 (toEnum (ctorIndex t)) >> 
  mbuilds (For :: For B.Binary) (\proj -> B.put (proj t)) !! ctorIndex t
```

The generic `get` is even simpler, as `Get` is an applicative functor, so it fits `buildsA` very well. First it reads the constructor index, which is then used to pick out the right build from the list that `buildsA` returns.

```Haskell
getADT :: (ADT t, Constraints t B.Binary) => B.Get t
getADT = do
    ix <- fromEnum <$> B.getWord8
    buildsA (For :: For B.Binary) (const B.get) !! ix
```

And that's it!

More example code is [here](https://gist.github.com/3591546), including a version of `ADT` for kind `* -> *`. Next week I'm at ICFP, and I hope I'll have some time to release this code to Hackage. And of course I'd love to hear your feedback if you're at ICFP too!