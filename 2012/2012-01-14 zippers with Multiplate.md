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

If a type `A` contains parts of both type `B` and type `C`, then the zipper needs to have both stores for `B` and for `C`, something like `Free (Store B :+: Store C) A`.

[1]: http://www.haskell.org/pipermail/haskell-cafe/2009-April/059069.html
[2]: http://hackage.haskell.org/packages/archive/free/2.0.2/doc/html/Control-Monad-Free.html
[3]: http://hackage.haskell.org/packages/archive/comonad-transformers/2.0.2/doc/html/Control-Comonad-Trans-Store-Lazy.html
[4]: http://arxiv.org/abs/1103.2841