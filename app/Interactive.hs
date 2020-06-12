module Interactive where
{-# LANGUAGE BlockArguments #-}

import System.IO

{-|
    Modeling interactiv programs as pure functions may seem infeasuble, because
    such programs by their very nature require the side effects of taking additional input and
    and producing output while the program is running. 
    
    In Haskell, an interactive program is viewed as a pure function that takes the current
    state of the world as its argument, and produces a modified world as it result, in which the modified
    world reflects any side effects performed by the program. 
-}
--type IO = World -> World
-- However an interactive program may return a result value in addition to
-- performing side effects. For example, a program for reading a character from 
-- the keyboard may return the character that was read. For this reason, we generalise
-- out type for interactive programs to also reurn a result value, with the type of such 
-- values being a parameter of the IO type:
--type IO a = World -> (a, World)

-- Expressions of type IO a are called actions. Actions that return and empty tuple (IO ()) can be thought
-- of as purely side-effecting actions that return no result value.
-- An interactive program that take a character and returns an integer would have type
-- Char -> IO Int, which abbreviates the curried function type Char -> World -> (Int, World)

--type IO a = String -> (a, String)

--(>>=)   ::  Prelude.IO a -> (a ->  Prelude.IO b) ->  Prelude.IO b
--f >>= g =   \world -> case f world of
--                            (v, world') -> g v world'

strlen :: IO ()
strlen =  do putStr "Enter a string: "
             xs <- getLine
             putStr "The string has "
             putStr (show (length xs))
             putStrLn " characters"


beep :: IO()
beep =  putStr "\BEL"
cls  :: IO()
cls  =  putStr "\ESC[2J"

type Pos = (Int, Int)

goto :: Pos -> IO()
goto(x, y) =  putStr( "\ESC["  ++ show y ++";" ++ show x ++ "H" )

writeat :: Pos -> String -> IO()
writeat p xs = do goto p
                  putStr xs

seqn :: [IO a] -> IO()
seqn[] = return ()
seqn(a:as) = do a
                seqn as



                     