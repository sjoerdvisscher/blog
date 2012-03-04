How to work generically with mutually recursive datatypes, easier.
==================================================================

Ok, let's do [this][0] again.

Combining mutually recursive functions into one function
--------------------------------------------------------

There's a much easier way to combine functions that I missed previously. We can simply create a datatype that can hold both `Expr` and `Decl` values.

```haskell
data AST = Expr Expr
         | Decl Decl
         deriving (Eq, Show)

freeVars :: AST -> [Var] -> ([Var], [Var])
freeVars (Decl (v := e)   ) = const ([], [v]) `mappend` freeVars (Expr e)
freeVars (Decl (Seq d1 d2)) = freeVars (Decl d1) `mappend` freeVars (Decl d2)
freeVars (Expr (Con _)    ) = mempty
freeVars (Expr (Add e1 e2)) = freeVars (Expr e1) `mappend` freeVars (Expr e2)
freeVars (Expr (Mul e1 e2)) = freeVars (Expr e1) `mappend` freeVars (Expr e2)
freeVars (Expr (EVar v)   ) = \bound -> (if (v `elem` bound) then [] else [v], [])
freeVars (Expr (Let d e)  ) = \bound -> 
  let
    (freeD, declD) = freeVars (Decl d) bound
    (freeE, _)     = freeVars (Expr e) (declD ++ bound)
  in
    (freeD ++ freeE, [])
```

Foldable families
-----------------

Again we see the folding pattern: `fold` is a function that folds a family of datatypes given a way to fold the children. But the type is much simpler this time:

```haskell
class FoldableFamily fam where
  fold :: Monoid m => (fam -> m) -> fam -> m

instance FoldableFamily AST where
  fold foldChild (Decl (_ := e)   ) = foldChild (Expr e)
  fold foldChild (Decl (Seq d1 d2)) = foldChild (Decl d1) `mappend` foldChild (Decl d2)
  fold foldChild (Expr (Add e1 e2)) = foldChild (Expr e1) `mappend` foldChild (Expr e2)
  fold foldChild (Expr (Mul e1 e2)) = foldChild (Expr e1) `mappend` foldChild (Expr e2)
  fold foldChild (Expr (Let d  e )) = foldChild (Decl d)  `mappend` foldChild (Expr e)
  fold _         _                  = mempty

freeVarsFold :: AST -> [Var] -> ([Var], [Var])
freeVarsFold (Decl (v := e))  = const ([], [v]) `mappend` freeVarsFold (Expr e)
freeVarsFold (Expr (Let d e)) = \bound -> 
  let
    (freeD, declD) = freeVarsFold (Decl d) bound
    (freeE, _)     = freeVarsFold (Expr e) (declD ++ bound)
  in
    (freeD ++ freeE, [])
freeVarsFold (Expr (EVar v)) = \bound -> (if (v `elem` bound) then [] else [v], [])
freeVarsFold v               = fold freeVarsFold v
```

Traversable families
--------------------

We can again generalize to traversable families:

```haskell
class TraversableFamily fam where
  traverse :: Applicative f => (fam -> f fam) -> fam -> f fam
```

But there's a catch. With the GADT the type was: `(forall b. fam b -> b -> f b) -> fam a -> a -> f a`, so the compiler knows that when we put an `Expr` in, we'll get `f Expr` back. But this is now hidden for the compiler.

```haskell
instance TraversableFamily AST where
  traverse travChild (Decl (v := e)   ) = (\(Expr e) -> Decl (v := e)) <$> travChild (Expr e)
  traverse travChild (Decl (Seq d1 d2)) = (\(Decl d1) (Decl d2) -> Decl (Seq d1 d2)) <$> travChild (Decl d1) <*> travChild (Decl d2)
  traverse travChild (Expr (Add e1 e2)) = (\(Expr d1) (Expr d2) -> Expr (Add d1 d2)) <$> travChild (Expr e1) <*> travChild (Expr e2)
  traverse travChild (Expr (Mul e1 e2)) = (\(Expr d1) (Expr d2) -> Expr (Mul d1 d2)) <$> travChild (Expr e1) <*> travChild (Expr e2)
  traverse travChild (Expr (Let d e)  ) = (\(Decl d) (Expr e) -> Expr (Let d e)) <$> travChild (Decl d) <*> travChild (Expr e)
  traverse _         v                  = pure v
```

So we can now only hope that `travChild` returns an `Expr` value if we put an `Expr` value in.

Comparison with Uniplate
------------------------

If you look at the `TraversableFamily` type class, there's nothing in there that is specific to families of datatypes. It is just a definition of the traversal of a single datatype. And indeed, it is equivalent to [Uniplate][1]. 

```haskell
uniplate :: TraversableFamily fam => fam -> ([fam], [fam] -> fam)
uniplate = unPair . traverse (\v -> Pair (Constant [v]) (do (w:ws) <- get; put ws; return w))
  where
    unPair (Pair a b) = (getConstant a, evalState b)
```

And to go from Uniplate to TraversableFamily, we make an applicative version of `descendM`.

```haskell
traverse :: (Applicative f, Uniplate on) => (on -> f on) -> on -> f on
traverse f x = generate <$> T.traverse f current
    where (current, generate) = uniplate x
```

So it is also possible to work with mutually recursive datatypes with Uniplate, as I've tried to do [here][2]. (There might be an easier way to create that instance.)

Conclusion
----------

So, at the cost of a bit of type safety, we can do generics with mutually recursive datatypes without any extensions to Haskell'98. Which you prefer probably depends a lot on how much you like or hate GADTs.

[0]: https://github.com/sjoerdvisscher/blog/blob/master/2012/2012-03-03%20how%20to%20work%20generically%20with%20mutually%20recursive%20datatypes.md
[1]: http://community.haskell.org/~ndm/uniplate/
[2]: https://gist.github.com/1974310