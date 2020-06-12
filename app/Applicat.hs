module Applicat where


{-|

fmap g   :: m a -> m(b -> c)
       x :: m a
fmap g x :: m(b -> c)

The scenario is as follows: we have something of type m(b -> c) and we would liek to "apply" it to a value of type m b
to get our final m c. In other words, we want the following operation to be available for our monad:

ap :: Monad m => m(b -> c) -> m b -> m b

Now, we comer to one of those moments in which all the small elements in funcitonal programming fit together in an
almost magical way: we need northing more than fmap and ap to be able ot lift any pure function regardless of the number
of arguments.


The first step is fmap f x, which has type m(b -> c -> d). As discovered above, to apply yet another argument, we need ap.
Th result of fmap f x `ap` y is:

ap       :: m (u -> v)            -> m u -> m v
fmap f x :: m (b -> c -> d)
       y ::                          m b
----------------------------------------------------------
fmap f x `ap` y ::                                  m(c -> d)

In the last step, we need ot rmember that b -> c -> d stands for b -> (c -> d). Thus, our use of ap chooses u to
become b and v to become c -> d. As a result, we obtain m (c -> d) as our final value. Here come the trick: this type is
exactly of the form we need to apply ap once again:

fmap f x `ap` y `ap` z :: m d

if you strive for a bit more generality, you can even rewrite each of the liftN function using a similar number of ap
combinators:


APPLICATIVES

Applicative functors - or simply applicatives - are those contexts, containers, or boxes for which you cannot only lift
unary functions - which you get from Functor - but functions with any number of arguments, for which you use ap. Note
that in both Haskell and Scala, the abstraction is defined as extending Function, from which we inherit the mapping
operation.

class Functor f => Applicat f where
    pure  :: a -> f a
    (<**>) :: f(a -> b) -> fa -> f b
    fmap :: (a -> b) -> f a -> f b
    fmap f = (<**>) (Applicat.pure f)


In Scala:
trait Applicative[F[_]] extends Functor[F]{
    def point[A](x: A): F[A]
    def ap[A, B](x: F[A])(f: F[A => B]): F[B]
}

Imagine thast in validatePerson, you want to check the requirements of hte provided named and age. However,
 the result value should not be a new Person value, but the full name in capital letters - to format it for a boarding
 pass, for example. You cannot write:

 map toUpper <$> validateName name <*> validateAge age

The problem is that `map toUpper` takes only one argument. One solution is to resort to a monadic block,
either fully or using some applicative help.

-- Option 1
do validateAge age
    map toUpper <$> validateName name
    
-- Option 2
do validateAge age
    m <- validateName name
    return (map toUpper m)
    
Another possibility is to discard the result of validating the age and only use its contextual behavior - if validation
fails, the full computation ought to fail. this is made explicity by teh combinator (<*), which discards the value 
following it.

map toUpper <$> validateName name <* validateAge age


Another is to validate teh age before the name. In that case, we need o use (<$) instead of (<$>)

map toUpper <$ validateAge age <*> validateName name




class Functor f => Monoidial f where
    unit :: f()
    (**) :: f a -> f b -> f(a, b)


class Monoidial f => ApplicatF f where
    pure    :: a -> f a
    (<**>)  :: f(a -> b) -> fa -> f b
    pure    =  fmap (\_ -> x) unit
    f <**> x = fmap(\(g, y) -> g y) (f ** x) 
    
    -}