# Zippers with (a variation on) Multiplate.

For a while I've been thinking about how to implement zippers with Multiplate. [Oleg showed][1] how to implement a zipper for traversables, and Multiplate is a generalization of traversable to a family of types, so this seemed a good place to start.

## A zipper for traversables

I noticed that if you take Oleg's `Zipper` type

```haskell
data Zipper t a = ZDone (t a) 
                | Z a (Maybe a -> Zipper t a)
```

and ignore the `Maybe` (it's not a required part), that you can write it as `Free (Store a) (t a)`. The helper functions for [`Free`][2] and [`Store`][3] make the rest of the code a lot shorter, for example, `zip_up = iter extract`. The zipper itself becomes

```haskell
zipper :: Traversable t => t a -> Free (Store a) (t a)
zipper = flip runCont return . traverse (\a -> cont (\k -> wrap (store k a)))
```

This uses the applicative instance of the continuation monad for the traversal. But `Free` is applicative too, so what happens if we use that? What we'll need is a function of type `a -> Free (Store a) a`. We could use `pure`, but that obviously has "no effect", so we'll use `wrap`, and then we'll need a `Store a (Free (Store a) a)`, which means an `a` and a function of type `a -> Free (Store a) a` again. We have an `a`, and this time we will use `pure`. So by just following the types we have:

```haskell
zipper :: Traversable t => t a -> Free (Store a) (t a)
zipper = traverse (wrap . store pure)
```

And it works! I find this amazing; we have 2 types, `Free` and `Store`, which together happen to fit the zipper type, and it turns out their semantics as implemented in their `Applicative` and `Functor` instances also match the semantics of zippers!

## Zippers and lenses

So, a zipper of a datastructure of type `a` with parts of type `b` has type `Free (Store b) a`. You can think of it as a number of chained stores, something like `(b, b -> (b, b -> (b, b -> (... -> a))))`. It is interesting to compare this type to the types mentioned by Russel O'Connor in his [Multiplate paper][4]. A lens is `a -> Store b a`, or a zipper with exactly one subpart. And his `CartesianStore b a` is actually isomorphic to `Free (Store b) a`, but it is a nested type. It is roughly `(b, (b, ... (b -> b -> ... a)))`.

## Zipping through multiple types

If a type `A` contains parts of both type `B` and type `C`, then the zipper needs to have both stores for `B` and for `C`, something like `Free (Store B :+: Store C) A`. But we need an easier way to have a store for a certain type in a family of types. What we need is a kind of _dependent sum_. To implement this, we first need to have type witnesses, values that prove that a type belongs to the family. This can be done with a GADT:

```haskell
data Fam :: * -> * where
  B :: Fam B
  C :: Fam C
```

It is convenient to reuse the name of the type as the name of the constructor, but that's not required. Next we need a datatype that stores a type witness together with a store of that type:

```haskell
data FamStore fam a where
  FamStore :: fam b -> Store b a -> FamStore fam a

instance Functor (FamStore fam) where
  fmap f (FamStore mem s) = FamStore mem (fmap f s)
```

When we put this together we have the type of a zipper through a family of types:

```haskell
type Zipper fam a = Free (FamStore fam) a
```

As `FamStore` is a functor, this zipper is applicative, so we should be able to use this with Multiplate.

## Getting the store back out

To get the store out of a `FamStore` value, we'd need a function with a type like `FamStore fam a -> Store c a`. But if you do that the compiler will complain that it needs proof that this `c` is the same as the `b` in the `FamStore` value. We can fix this by passing the type witness of the type we expect and check if it is the same witness as the one stored in the `FamStore`. But this equality test is not just `fam b -> fam c -> Bool`, if `b` and `c` are equal we need proof, so the type becomes `fam b -> fam c -> Maybe (b :=: c)`. [The `EqT` type class from the type-equality package][5] is exactly what we need. Now we can write a function that gets the store from a `FamStore` value.

```haskell
getStore :: EqT fam => fam c -> FamStore fam a -> Maybe (Store c a)
getStore wC (FamStore wB st) = (\Refl -> st) <$> wC `eqT` wB
```

`Refl` is the proof that types `b` and `c` are equal. If we pass in the wrong type witness we'll get `Nothing`.

## A Multiplate variation

Multiplate works with the concept of 'plates', a record parametrized by a functor f with one field of type A -> f A for each type in a family. Here's an example:

```haskell
data ABCPlate f = ABCPlate
                { fieldA :: A -> f A
                , fieldB :: B -> f B
                , fieldC :: C -> f C
                }
```

But, records are not always easy to work with, and there's a pattern in the type of the fields that is not enforced, which is why Multiplate needs a `mkPlate` method. But there's another way, because what we have here is a _dependent product_, which means that a plate is just a function with a type witness as argument:

```haskell
type Plate fam f = forall x. fam x -> x -> f x
```

The Multiplate type class now no longer needs `mkBuild`, for example `purePlate` can now simply be implemented like this:

```haskell
purePlate :: Applicative f => Plate fam f
purePlate = const pure
```

We will need to have type equality for the family of types, so `EqT` becomes a superclass of `Multiplate`:

```haskell
class EqT fam => Multiplate fam where
  multiplate :: Applicative f => Plate fam f -> Plate fam f
```

[1]: http://www.haskell.org/pipermail/haskell-cafe/2009-April/059069.html
[2]: http://hackage.haskell.org/packages/archive/free/2.0.2/doc/html/Control-Monad-Free.html
[3]: http://hackage.haskell.org/packages/archive/comonad-transformers/2.0.2/doc/html/Control-Comonad-Trans-Store-Lazy.html
[4]: http://arxiv.org/abs/1103.2841
[5]: http://hackage.haskell.org/packages/archive/type-equality/0.1.0.2/doc/html/Data-Type-Equality.html#t:EqT