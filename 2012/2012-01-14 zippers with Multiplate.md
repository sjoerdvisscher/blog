# Zippers with (a variation on) Multiplate.

For a while I've been thinking about how to implement zippers with Multiplate. [Oleg showed][1] how to implement a zipper for traversables, and Multiplate is a generalization of traversable to a family of types, so this seemed a good place to start.

I noticed that if you take Oleg's `Zipper` type
```haskell
data Zipper t a = ZDone (t a) 
                | Z a (Maybe a -> Zipper t a)
```
and ignore the `Maybe` (it's not a required part), that you can write it as `Free (Store a) (t a)`. The helper functions for [Free][2] and [Store][3] make the rest of the code a lot shorter, for example, `zip_up = iter extract`. The zipper itself becomes
```haskell
zipper = flip runCont return . traverse (\a -> cont (\k -> wrap (store k a)))
```

[1]: http://www.haskell.org/pipermail/haskell-cafe/2009-April/059069.html
[2]: http://hackage.haskell.org/packages/archive/free/2.0.2/doc/html/Control-Monad-Free.html
[3]: http://hackage.haskell.org/packages/archive/comonad-transformers/2.0.2/doc/html/Control-Comonad-Trans-Store-Lazy.html