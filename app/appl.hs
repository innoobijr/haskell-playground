import Text.Read

interactiveDoubling = do
    putStrLn "Choose a number:"
    s <- getLine
    let mx = readMaybe s :: Maybe Double
    case mx of
        Just x -> putStrLn ("The double of your number is " ++ show (2*x))
        Nothing -> do
            putStrLn "This is not a valid number. Retrying..."
            interactiveDoubling

{-|
	Application in functors
	
	Now, let's do something slightly more sophisticated: reading two number with readMaybe and printing their sum
-}

interactiveSumming = do
    putStrLn "Choose two numbers:"
    sx <- getLine
    sy <- getLine
    let mx = readMaybe sx :: Maybe Double
        my = readMaybe sy
    case mx of
        Just x -> case my of
            Just y -> putStrLn("The sume of your numbers is " ++ show(x+y))
            Nothing -> retry
        Nothing -> retry
    where
    retry = do
        putStrLn "Invalid number. Retrying..."
        interactiveSumming

{-|
	interactiveSumming works, but it is somewhat annoying to write. In particular, the nested case statements are not pretty, and make reading the code a little difficult. If only there was a way of summing the number beofre unwrapping them, analogously to whast we did with fmap in the second version of interactiveDoubling, we would be able ot get aways with just one case:

Applicative Functor:
 a functor is a functor which supports applying functions within the functor, thus allowing for smooth usage of partial application (and therefore functions of multiple arguments). All instacne of Applicative are Functors, and besides Maybe, there are many other common Functors whoch are also Applicative. 

-}
