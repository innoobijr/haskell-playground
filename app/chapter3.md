# Applicative Style

```haskell
-- Each time we want to use a pure function in a monadic or applicative context, we use:

(<$>) = fmap
(<*>) = ap

```