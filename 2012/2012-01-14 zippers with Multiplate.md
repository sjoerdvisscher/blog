# Zippers with (a variation on) Multiplate.

_The complete code of this blogpost can be found in [this gist][0]._

For a while I've been thinking about how to implement zippers with [Multiplate][6]. [Oleg showed][1] how to implement a zipper for traversables, and Multiplate is a generalization of traversable to a family of types, so this seemed a good place to start.

## A zipper for traversables

I noticed that if you take Oleg's `Zipper` type

```haskell
data Zipper t a = ZDone (t a) 
                | Z a (Maybe a -> Zipper t a)
```

and ignore the `Maybe` (it's not a required part), that you can write it as `Free (Store a) (t a)`, where `Free` is from the [free package][2]:

```haskell
data Free f a = Pure a | Free (f (Free f a))
```

and `Store` is from the [comonad-transformers package][3], and could be declared as (it is actually based on `StoreT`):

```haskell
data Store b a = Store (b -> a) b
```

The helper functions for `Free` and `Store` make the rest of the code a lot shorter, for example, `zip_up = iter extract`. The zipper itself becomes (after quite a bit of inlining and rewriting)

```haskell
zipper :: Traversable t => t a -> Free (Store a) (t a)
zipper = flip runCont Pure . traverse (\a -> cont (\k -> Free (store k a)))
```

This uses the applicative instance of the continuation monad for the traversal. But `Free` is applicative too, so what happens if we use that? What we'll need is a function of type `a -> Free (Store a) a`. We could use `Pure`, but that obviously has "no effect", so we'll use `Free`, and then we'll need a `Store a (Free (Store a) a)`, which means an `a` and a function of type `a -> Free (Store a) a` again. We have an `a`, and this time we will use `Pure`. So by just following the types we have:

```haskell
zipper :: Traversable t => t a -> Free (Store a) (t a)
zipper = traverse (Pure . store Free)
```

And it works! I find this amazing; we have 2 types, `Free` and `Store`, which together happen to fit the zipper type, and it turns out their semantics as implemented in their `Applicative` and `Functor` instances also match the semantics of zippers!

## Zippers and lenses

So, a zipper of a datastructure of type `a` with parts of type `b` has type `Free (Store b) a`. You can think of it as a number of chained stores, something like `(b, b -> (b, b -> (b, b -> (... -> a))))`. It is interesting to compare this type to the types mentioned by Russel O'Connor in his [Multiplate paper][4]. A lens is `a -> Store b a`, or a zipper with exactly one subpart. And Russel's `CartesianStore b a` is actually isomorphic to `Free (Store b) a`, but it is a nested type. It is roughly `(b, (b, ... (b -> b -> ... a)))`.

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

We can now use `FamStore fam` instead of `Store b` to get the type of a zipper through a family of types:

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

Multiplate works with the concept of 'plates', a record parametrized by a functor `f` with one field of type `A -> f A` for each type in a family. Here's an example:

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

The Multiplate type class now no longer needs `mkBuild`, for example `purePlate` can simply ignore the type witness:

```haskell
purePlate :: Applicative f => Plate fam f
purePlate = const pure
```

We will need to have type equality for the family of types, so `EqT` becomes a superclass of `Multiplate`:

```haskell
class EqT fam => Multiplate fam where
  multiplate :: Applicative f => Plate fam f -> Plate fam f
```

## Putting it all together

We now have everything ready to build a zipper plate. It is almost the same code as the traversable zipper, but we now use `multiplate` instead of `traverse`, and we also need to wrap the store with a `FamStore` containing the type witness, which is provided by `multiplate`.

```haskell
zipperPlate :: Multiplate fam => Plate fam (Zipper fam)
zipperPlate = multiplate (\w -> Free . FamStore w . store Pure)
```

How do we use this plate? First we need a function that converts a value into a zipper. As always with Multiplate we have to say for which type we want to run the plate, but instead of using a projector now we use the type witness. The function is just the zipper plate with the `Plate` type synonym expanded. 

```haskell
enter :: Multiplate fam => fam a -> a -> Zipper fam a
enter = zipperPlate
```

We'll also need a function that moves to the next step (if possible), and one that converts the zipper back to a value again. These functions don't need a type witness, because we use the function `extract :: Store b a -> a`, which doesn't care what `b` is.

```haskell
next :: Zipper fam a -> Zipper fam a
next (Pure t) = Pure t
next (Free (FamStore _ s)) = extract s

leave :: Zipper fam a -> a
leave = iter (\(FamStore _ s) -> extract s)
```

And finally we need a function to modify the value of the current position. We need a type witness again, and if it doesn't match the one in the `FamStore`, we'll keep the old value.

```haskell
modify :: Multiplate fam => fam b -> (b -> b) -> Zipper fam a -> Zipper fam a
modify _ _ (Pure t) = Pure t
modify w f (Free fs) = Free (maybe fs (FamStore w . seeks f) (getStore w fs))
```

These are the most important operations on zippers. There are some more in [the complete code listing][0], which also contains a complete example that uses the zipper to change an expression.

## Conclusion

As usual with my Haskell code, this was just a fun and interesting programming exercise. So the question is, what to do with this? Is this useful enough to turn into a package, and if so, should I include the modified `Multiplate` type class, or should the Multiplate package get an update?

I also wonder about the performance. There's not much going on, but that's certainly not always a guarantee for good performance. Does anybody have a performance benchmark for zippers?

And a final question: Are there datatypes where you could write a zipper for manually, but not a `Multiplate` instance?

_You can add comments to this article [on reddit][7]._

[0]: https://gist.github.com/1611472
[1]: http://www.haskell.org/pipermail/haskell-cafe/2009-April/059069.html
[2]: http://hackage.haskell.org/packages/archive/free/2.0.2/doc/html/Control-Monad-Free.html
[3]: http://hackage.haskell.org/packages/archive/comonad-transformers/2.0.2/doc/html/Control-Comonad-Trans-Store-Lazy.html
[4]: http://arxiv.org/abs/1103.2841
[5]: http://hackage.haskell.org/packages/archive/type-equality/0.1.0.2/doc/html/Data-Type-Equality.html#t:EqT
[6]: http://www.haskell.org/haskellwiki/Multiplate
[7]: http://www.reddit.com/r/haskell/comments/omzwj/zippers_with_a_variation_on_multiplate/