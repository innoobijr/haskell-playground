module Chap4.Utilities where



{-|Monadic action
    We will use printing a string as an example = that oyu want to execute over all the elements of a list. Is is possible
    to use map in the usual way?
    
    map (\name -> print ("Hello, " ++ name))["Innocent", "Obi"]
    
    No!! There problem is that the type of print is String -> IO (), and thus the result of that mapping is of type [IO ()].
    In other worlds, it is a list of monadic actions, not a monadic action itsefl. In the latter case, the reuslt type
    woudl be IO T for some T - note the absense of the top-levl list constructor.
    
    There are two approaches to solving this problem. The first one is to write a new version of map that takes a 
    monadic action as it first argument. In this way, we arrrvie at mapM:
    
-}
mapMp           :: Monad m => (a -> m b) -> [a] -> m [b]
--mapMp _ []      = return []
--mapMp f (x: xs) = do r  <- f x
--                     rs <- mapMp f xs
--                     return (r:rs)

-- Alternatively in the applicative style
--mapM f (x:xs) = (:) <$> f x <*> mapM f xs

-- With Haskell's base library, you would you forM
--forM ["Innocent", "Frank"] $ \name -> print("Hello, " ++ name)

{-|
    The other option is to keep map as  is but introduce a new function to moe the monadic context from inside the list 
    constrcutor, [m a], to the outside m [a]:
    
-}

sequen :: Monad m => [m a] -> m[a]
sequen []       = return []
sequen (x: xs)  = do r <- x -- extracting or unwrapping the value from the monadic context
                     rs <- sequen xs -- recursively extracting the values
                     return (r:rs) -- lifting back into the monadic context

-- Once again, we may use the applicative style
--sequen (x:xs) =  (:) <$> x <*> sequen xs

zipWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m[ c]
zipWithM f x y = sequen $ zipWith f x y


{-|
    !!!!!!!!!!!!!!!!
    
    This is actually a super interesting fact about haskell. That the definition with one param will not compile
    Need to explicitly passing the arguments, otherwise haskell will resovle incorrectly. There might be a way to do 
    this using a more applicative style. Will need to check out later
-}
mapMp f = sequen . map f


adder :: (a -> b -> c) -> [a] -> [b] -> [c]
adder f =  zipWith(\a b -> f a b)


{-|
    Action Without Value
    
    A pure expression has no side effects. It jsut computes it output value. In constract. monadic actions are executed
     for both their side effects and their result values. In some extreme - nut not unusual - case a monadic action is
     apparent only from its side effects. This is the case for example, when you change the state value contained by the
     State monad:
     
     put :: s -> State s ()
     
     Or when you print to the console:
     putStrLn :: String -> IO()
     
     The following return type IO [()]:
     
     forM ["Innocent", "Frank"] $ \name -> print("Hello, " ++ name)
     Compiler is okay with you forgeting to pattern match on the unit, but for example, the above function will cause
     the compiler to scream at you. In Haskell, this is mitigated by using the void functor
     
-}
void :: Functor m => m a -> m ()
void =  fmap (\_ -> ())

{-|
    This functon substitutes any meaningful value contained in the monad with teh dummy (). 
    void $ forM ["Innocent", "Frank"] $ \name -> print("Hello, " ++ name)
    
    This idiom is common in monadic code. For that reason, Haskell libraries follow the convention of defining two
     variants of their combinators, the common one - usually ending in M - and the one thaet ignores the return value -
     ending in _ symbol. 
-}


{-|
    Traversables
    Similar to how the Functor is the type class that gives the list constructor the map method for hte pure context. 
    Traversable is what gives the monadic mapping functionality `mapM`
-}

class Functor f => Traversable f where
  mapM :: Monad m => (a -> mb) -> f a -> m (f b)
  traverse :: Applicative m => (a -> mb) -> f a -> m (f b)
  
{-|
We first introdcue `Functor` as the class of types that allow mapping pure functions and then extended it to Traversable
for types that also allows mapping monadic functions. We have been taling about monads as a means ot add a certain 
context to a computation - the abiliuty to fail, threading state, and so on. 

--- Beginning of pseudo-haskell
newtype Idd a = I a

instance Monad Idd where
  return :: a -> Idd a
  return x = I x
  (>>=) :: Idd a ->(a -> Idd b) -> Idd b
  (I x) >>= f = f x

 
instance Functor Idd where
  fmap :: (a -> b) -> Idd a -> Idd b
  fmap f (I x) = I (f x)
  
  -}