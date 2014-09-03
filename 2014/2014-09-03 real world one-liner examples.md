Real world `one-liner` examples
===============================

Recently I uploaded [a new version of the `one-liner` package][0]. The goal
with the new version was to make writing generic functions as simple and
unscary as I could. To do this I had to sacrifice some power, so I wanted to
check it would still be useful for a lot of real world cases.
To do this, I searched Google for [uses of GHC.Generics on Hackage][1].

`Data.Binary`
------------------
The first example I already used in the [previous blogpost][4]:
`put` and `get` from the [binary][3] package.
With the new version of `one-liner`, the code looks like this:

```haskell
gput :: (ADT t, Constraints t Binary) => t -> Put
gput t = putWord8 (toEnum (ctorIndex t)) <> gfoldMap (For :: For Binary) put t

gget :: (ADT t, Constraints t Binary) => Get t
gget = getWord8 >>= \ix -> createA (For :: For Binary) get !! fromEnum ix
```
`Put` is a `Monoid` (with `mappend = return ()` and `(<>) = (>>)`), so we can
use `gfoldMap`.

```haskell
gfoldMap :: (ADT t, Constraints t c, Monoid m)
         => for c -> (forall s. c s => s -> m) -> t -> m
```

We pass the proxy `For Binary` to tell `gfoldMap` what class we want to use,
and then we need a function of type `forall s. Binary s => s -> Put`,
which is simply `put`! But before that we need to store a byte containing
the index of the constructor, so we'll know what to do when we `get`.

`Get` is `Applicative`, so we can use `createA`.

```haskell
createA :: (ADT t, Constraints t c, Applicative f)
        => for c -> (forall s. c s => f s) -> [f t]
```

`createA (For :: For Binary) get` returns a list of ways to read binary data,
one for each constructor, and we pick the right one using the index
that we stored first.

`Data.Hashable`
---------------

The next example is `hashWithSalt` from the [hashable][5]. It's type is
`Int -> a -> Int`, which at first sight doesn't look like something `one-liner`
can handle. But if we flip it we get `a -> Int -> Int`, and `Int -> Int` is
a monoid if we wrap it in `Endo`, so we can use `gfoldMap` again.

```haskell
ghashWithSalt :: (ADT t, Constraints t Hashable) => Int -> t -> Int
ghashWithSalt = flip $ \t -> flip hashWithSalt (ctorIndex t) .
  appEndo (gfoldMap (For :: For Hashable) (Endo . flip hashWithSalt) t)
```

[As the documentation explains][6] we should first hash the constructor index
and and then hash all the subcomponents, while effectively composing all the
`Int -> Int` functions along the way.

`Control.DeepSeq.Generics`
--------------------------

The next example is going to be a walk in the park, it is `rnf` from the
[deepseq-generics][7] package. `()` is a monoid, so `gfoldMap` again is what we use:

```haskell
grnf :: (ADT t, Constraints t NFData) => t -> ()
grnf = gfoldMap (For :: For NFData) rnf
```

One minor detail, we should check that the `Monoid` instance of `()` is strict.
[Let's see][8]:

```haskell
instance Monoid () where
        -- Should it be strict?
        mempty        = ()
        _ `mappend` _ = ()
        mconcat _     = ()
```

D'oh, look at that comment! So we must create our own strict unit type,
and convert between that and `()`. I'll skip that here.
The real world can be annoying sometimes!

`GHC.Generics.Lens`
-------------------

The first result on Google was the [tinplate][2] function from the `lens` package.
Straight away this was a non-standard use of `GHC.Generics`, because it does
a deep traversal, i.e. all the immediate subcomponents also need to be an instance
of `Generic`.

It was possible to do this without chaning the library, but it was quite tricky
and it required [a hack to prevent GHC to detect a class declaration cycle][9].
Also there's the problem that the GHC Generics representation of atomic types
like `Char` and `Int` contain themselves.
So I decided to add a utility constraint `Deep` to the library that calculates
the constraints needed for all the deep components of a datatype to be an
instance of the given class, and a utility function `isAtom` to test if the
given type is atomic or not.

With these utility functions `tinplate` can be relatively cleanly implemented
using `gtraverse`:

```Haskell
whenCastableOrElse :: forall a b f. (Typeable a, Typeable b)
                   => (b -> f b) -> (a -> f a) -> a -> f a
f `whenCastableOrElse` g = maybe g (\Refl -> f) (eqT :: Maybe (a :~: b))

tinplate :: forall t b. (Typeable b, Deep Typeable t) => Traversal' t b
tinplate f
  | isAtom (Proxy :: Proxy t) = f `whenCastableOrElse` pure
  | otherwise = gtraverse (For :: For (Deep Typeable)) $
                   f `whenCastableOrElse` tinplate f
```

`Test.SmallCheck.Series`
------------------------

The last examples are `series` and `coseries` from the [smallcheck][10] package.

`Series m` is `Applicative` so it may seem we could use `createA` directly. But
the documentation of SmallCheck makes it clear we're not supposed to use `<*>`,
but instead use `<~>` to get fair, breadth-first generation of values. So we'll
create a newtype wrapper to get the right `Applicative`. Then we can call
`createA`, and fold the ways to produce series with `(\/)`.

```haskell
newtype Fair m a = Fair { runFair :: Series m a } deriving Functor
instance MonadLogic m => Applicative (Fair m) where
  pure a = Fair $ pure a
  Fair fs <*> Fair as = Fair $ fs <~> as

gseries :: forall t m. (ADT t, Constraints t (Serial m), MonadLogic m) => Series m t
gseries = foldr ((\/) . decDepth . runFair) mzero $ createA (For :: For (Serial m)) (Fair series)
```

`coseries` was the most interesting of all the generic functions I found on
Hackage. It's type is:

```haskell
coseries :: Series m b -> Series m (a -> b)
```

So to generate coseries we need a newtype wrapper:

```haskell
newtype CoSeries m a = CoSeries { runCoSeries :: forall r. Series m r -> Series m (a -> r) }
```

Is `Coseries m` `Applicative`? No, it can't be because it is contravariant!

```haskell
instance Contravariant (CoSeries m) where
  contramap f (CoSeries g) = CoSeries $ fmap (. f) . g
```

This was a use case I hadn't thought of yet. So let's see if we can find a
function that we can add to `one-liner`.
Purely by accident just a few weeks earlier Edward Kmett figured out a way
to do [`Applicative` contravariantly][11], using Day convolution! And
`CoSeries m` turns out to be an instance:

```haskell
instance MonadLogic m => Divisible (CoSeries m) where
  divide f (CoSeries g) (CoSeries h) = CoSeries $ \rs -> do
    rs' <- fixDepth rs
    f2 <- decDepthChecked (constM $ constM rs') (g $ h rs')
    return $ uncurry f2 . f
  conquer = CoSeries constM
```

My first attempt was then to go on to mirror `createA`, and make a function
that returns a list of ways to consume a type, one for each constructor.
But that doesn't work because when you consume a value, you don't know which
constructor you're going to get, so we need a special way to combine these ways
to consume a value such that the right way is called depending on the
constructor of the value that is being consumed.

For that we only need to look a bit further down the documentation of `Divisible`
and there we find `Decidable` which is a contravariant version of `Alternative`,
and this precisely fits our needs! So the generic function that consumes
values becomes:

```haskell
consume :: (ADT t, Constraints t c, Decidable f)
        => for c -> (forall s. c s => f s) -> f t
```

So, is `CoSeries m` `Decidable`? Yes it is!

```haskell
instance MonadLogic m => Decidable (CoSeries m) where
  choose f (CoSeries g) (CoSeries h) = CoSeries $ \rs ->
    (\br cr -> either br cr . f) <$> g rs <~> h rs
  lose f = CoSeries $ \_ ->
    return $ absurd . f
```

After all this, implementing `coseries` becomes straightforward:

```haskell
gcoseries :: forall t m r. (ADT t, Constraints t (CoSerial m), MonadLogic m)
          => Series m r -> Series m (t -> r)
gcoseries = runCoSeries $ consume (For :: For (CoSerial m)) (CoSeries coseries)
```

By the way, working with contravariant functors _really_ made my head hurt.
I would not have been able to implement this if I wouldn't have been able to
mostly just follow the types everywhere.

What doesn't work?
-----------------
There are 3 uses of generics that I can think of which don't work with `one-liner`:

- Code that uses meta data. For example Aeson needs record field names to generate
  JavaScript property names.
- Code that does calculations on types. For example deriving zippers needs to
  calculate the derivative of a type.
- Code that needs non-trivial return types. An example here is deriving lenses,
  which if you do it generically returns a list of HLists, with a lens to each
  field of each constructor.

Conclusion
----------
Looking for practical uses of generics turned out to be very useful. As I had
hoped, most generic functions that did not fit in one of the 3 categories above,
were implementable with `one-liner`, and the few ones that didn't provided useful
new additions to the library.


[0]: http://hackage.haskell.org/package/one-liner-0.5
[1]: https://www.google.se/search?q=%22import+GHC.Generics%22+site:hackage.haskell.org
[2]: http://hackage.haskell.org/package/lens-4.3.3/docs/Generics-Deriving-Lens.html#v:tinplate
[3]: http://hackage.haskell.org/package/binary-0.7.2.1/docs/Data-Binary.html
[4]: https://github.com/sjoerdvisscher/blog/blob/master/2012/2012-09-06%20constraint-based%20generics.md
[5]: http://hackage.haskell.org/package/hashable-1.2.2.0/docs/Data-Hashable.html
[6]: http://hackage.haskell.org/package/hashable-1.2.2.0/docs/Data-Hashable.html#g:7
[7]: http://hackage.haskell.org/package/deepseq-generics-0.1.1.1
[8]: http://hackage.haskell.org/package/base-4.7.0.1/docs/src/Data-Monoid.html#Monoid
[9]: http://stackoverflow.com/a/14133573/5852
[10]: http://hackage.haskell.org/package/smallcheck-1.1.1/docs/Test-SmallCheck-Series.html
[11]: https://hackage.haskell.org/package/contravariant-1.2/docs/Data-Functor-Contravariant-Divisible.html
