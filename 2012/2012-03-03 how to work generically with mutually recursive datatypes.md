How to work generically with mutually recursive datatypes.
==========================================================

On Haskell-Cafe, Matt Brown [recently asked][0] how to get free variables from a syntax tree with Multiplate. My knee-jerk reaction was to dive in [the API documentation of Multiplate][1] to see which method would help me, but I did not find anything. Then I tried thinking about it the other way around: how would I implement it in the normal way and where could Multiplate help make the code shorter. That was really insightful and made me understand Multiplate better. In this blogpost I want to take yet another approach: Let's assume Multiplate does not yet exist, what kind of generics library can we come up with that helps make the code shorter?

The task
--------

So we want to get free variables from a syntax tree with these datatypes:

```haskell
data Decl = Var := Expr
          | Seq Decl Decl
          deriving (Eq, Show)

data Expr = Con Int
          | Add Expr Expr
          | Mul Expr Expr
          | EVar Var
          | Let Decl Expr
          deriving (Eq, Show)

type Var = String
```

The normal way to implement this is like this: Given a list of bounded variables, we return a tuple of free variables and "declared" variables. The `Let` constructor turns these declared variables into bound variables. So we have the type `[Var] -> ([Var], [Var])`. This turns out to be a Monoid (using the instances for `(->)`, `(,)` and `[]`) which helps to shorten the code.

```haskell
freeVarsDecl :: Decl -> [Var] -> ([Var], [Var])
freeVarsDecl (v := e) = const ([], [v]) `mappend` freeVarsExpr e
freeVarsDecl (Seq d1 d2) = freeVarsDecl d1 `mappend` freeVarsDecl d2

freeVarsExpr :: Expr -> [Var] -> ([Var], [Var])
freeVarsExpr (Con _)     = mempty
freeVarsExpr (Add e1 e2) = freeVarsExpr e1 `mappend` freeVarsExpr e2
freeVarsExpr (Mul e1 e2) = freeVarsExpr e1 `mappend` freeVarsExpr e2
freeVarsExpr (EVar v)    = \bounded -> (if (v `elem` bounded) then [] else [v], [])
freeVarsExpr (Let d e)   = \bounded -> 
  let
    (freeD, declD) = freeVarsDecl d bounded
    (freeE, _)     = freeVarsExpr e (declD ++ bounded)
  in
    (freeD ++ freeE, [])
```

Combining mutually recursive functions into one function
--------------------------------------------------------

The first and most important step towards generic programming with mutually recursive datatypes is to be able to traverse these datatypes with one single function. So how can we combine the two mutually recursive functions above to one function? Multiplate does this with records, using one field for each function. The advantage of that solution is that it is conceptually very simple. But records aren't very flexible, you can't pattern match on the projectors for example, and the Haskell syntax for functions in records is clumsy and not very pretty. (You can try to rewrite the code below with records and you'll see what I mean.)

I prefer to use a family GADT, with one constructor for each datatype in the family. By pattern matching on the GADT constructors, we make clear with which datatype we want to work.

```haskell
data AST a where
  Decl :: AST Decl
  Expr :: AST Expr

freeVars :: AST a -> a -> [Var] -> ([Var], [Var])
freeVars Decl (v := e)    = const ([], [v]) `mappend` freeVars Expr e
freeVars Decl (Seq d1 d2) = freeVars Decl d1 `mappend` freeVars Decl d2
freeVars Expr (Con _)     = mempty
freeVars Expr (Add e1 e2) = freeVars Expr e1 `mappend` freeVars Expr e2
freeVars Expr (Mul e1 e2) = freeVars Expr e1 `mappend` freeVars Expr e2
freeVars Expr (EVar v)    = \bounded -> (if (v `elem` bounded) then [] else [v], [])
freeVars Expr (Let d e)   = \bounded -> 
  let
    (freeD, declD) = freeVars Decl d bounded
    (freeE, _)     = freeVars Expr e (declD ++ bounded)
  in
    (freeD ++ freeE, [])
```

The difference is very minimal: `freeVarsDecl` becomes `freeVars Decl` and `freeVarsExpr` becomes `freeVars Expr`.

Foldable families
-----------------

Except for `(:=)`, `Evar` and `Let`, the `freeVars` function is the standard way to fold a recursive data structure: Fold the children and mappend the results.
We can capture this pattern in a type class: `fold` is a function that folds a family of datatypes given a way to fold the children.

The way to fold the children is a function of type `AST a -> a -> m`, for all `a`. (Well, not actually *all* `a`, the `AST` GADT makes sure it can only be `Expr` or `Decl`.) The resulting fold has the same type, so let's give it a name:

```haskell
type Fold fam m = forall a. fam a -> a -> m
```

Then the class definition looks like this:

```haskell
class FoldableFamily fam where
  fold :: Monoid m => Fold fam m -> Fold fam m
```

And we can make our `AST` GADT an instance:

```haskell
instance FoldableFamily AST where
  fold foldChild Decl (_ := e)    = foldChild Expr e
  fold foldChild Decl (Seq d1 d2) = foldChild Decl d1 `mappend` foldChild Decl d2
  fold foldChild Expr (Add e1 e2) = foldChild Expr e1 `mappend` foldChild Expr e2
  fold foldChild Expr (Mul e1 e2) = foldChild Expr e1 `mappend` foldChild Expr e2
  fold foldChild Expr (Let d  e ) = foldChild Decl d  `mappend` foldChild Expr e
  fold _         _    _           = mempty
```

We can now take out the boring parts of the `freeVars` function, and let them be handled generically though the `fold` function, telling it that it should fold the child expressions with `freeVarsFold`.

```haskell
freeVarsFold :: Fold AST ([Var] -> ([Var], [Var]))
freeVarsFold Decl (v := e)    = const ([], [v]) `mappend` freeVarsFold Expr e
freeVarsFold Expr (Let d e)   = \bounded -> 
  let
    (freeD, declD) = freeVarsFold Decl d bounded
    (freeE, _)     = freeVarsFold Expr e (declD ++ bounded)
  in
    (freeD ++ freeE, [])
freeVarsFold Expr (EVar v)    = \bounded -> (if (v `elem` bounded) then [] else [v], [])
freeVarsFold w    a           = fold freeVarsFold w a
```

Traversable families
--------------------

Just like Foldable can be generalized to Traversable, FoldableFamily can be generalized to TraversableFamily. `traverse` is a function that traverses a family of datatypes given a way to traverse the children. Instead of a Monoid `m` we now return an Applicative `f a`. To derive the fold from the traversal we can use the Applicative functor `Constant m`.

```haskell
type Traversal fam f = forall a. fam a -> a -> f a

class TraversableFamily fam where
  traverse :: Applicative f => Traversal fam f -> Traversal fam f

fold :: (TraversableFamily fam, Monoid m) => Fold fam m -> Fold fam m
fold child w a = getConstant $ traverse (\v b -> Constant $ child v b) w a

instance TraversableFamily AST where
  traverse travChild Decl (v := e)    = (:=) <$> pure v <*> travChild Expr e
  traverse travChild Decl (Seq d1 d2) = Seq <$> travChild Decl d1 <*> travChild Decl d2
  traverse travChild Expr (Add e1 e2) = Add <$> travChild Expr e1 <*> travChild Expr e2
  traverse travChild Expr (Mul e1 e2) = Mul <$> travChild Expr e1 <*> travChild Expr e2
  traverse travChild Expr (Let d e)   = Let <$> travChild Decl d <*> travChild Expr e
  traverse _         _    a           = pure a
```

We don't need to change `freeVarsFold`, it can still use the `fold` function, which is now derived from `traverse`. So, let's try it.

```haskell
expr1 :: Expr
expr1 = Let ("x" := Con 42) (Add (EVar "x") (EVar "y"))

test :: [Var]
test = fst . ($ []) . freeVarsFold Expr $ expr1
```

```
>>> test
["y"]
```

Comparison with Multiplate
--------------------------

The `TraversableFamily` type class is about the same as the `Multiplate` type class. We can even implement `multiplate` with `traverse` like this:

```haskell
data ASTPlate f = ASTPlate
                { expr :: Expr -> f Expr
                , decl :: Decl -> f Decl
                }

plate2trav :: ASTPlate f -> Traversal AST f
plate2trav plate Expr = expr plate
plate2trav plate Decl = decl plate

trav2plate :: Traversal AST f -> ASTPlate f
trav2plate trav = ASTPlate (trav Expr) (trav Decl)

instance Multiplate ASTPlate where
  multiplate child = trav2plate (traverse (plate2trav child))
  mkPlate build = ASTPlate (build expr) (build decl)
```

Conclusion
----------

Generic programming libraries like Multiplate that work with the original datatypes, instead of using their own generic representations, have the advantage that they don't have to do everything for you. You can simply write your own functions with a specific implementation for some of the cases and use generic programming for the boring boilerplate cases.

_You can add comments to this article [on reddit][2]._

[0]: http://comments.gmane.org/gmane.comp.lang.haskell.cafe/96737
[1]: http://hackage.haskell.org/packages/archive/multiplate/0.0.1.1/doc/html/Data-Generics-Multiplate.html
[2]: http://www.reddit.com/r/haskell/comments/qfyyn/how_to_work_generically_with_mutually_recursive/