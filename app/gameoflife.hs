

width :: Int
width =  5

height :: Int
height = 5

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

{-|
We represent a board as alist of (x, y) positions at which there is a living cell,using th same coordinate convention
as the screen-}
type Board = [Pos]

glider :: Board
glider = [(4, 2), (2, 3), (4, 3), (3, 4), (4, 4)]

showcells :: Board -> IO()
showcells b = seqn[writeat p "O" | p <- b]

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

wrap :: Pos -> Pos
wrap(x, y) = (((x-1)`mod`width) + 1,
              ((y-1)`mod`height) +1)


neighbs :: Pos -> [Pos]
neighbs (x, y) = map wrap[(x-1, y-1), (x, y-1),
                          (x+1, y-1), (x-1, y),
                          (x+1, y), (x-1, y-1),
                          (x, y+1), (x+1, y+1)]

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: Board -> [Pos]
survivors b = [p | p <- b, elem (liveneighbs b p)[2, 3]]

births :: Board -> [Pos]
births b = [(x, y) | x <- [1..width],
                     y <- [1..height],
                     isEmpty b(x, y),
                     liveneighbs b(x, y) == 3]

rmdups  :: Eq a => [a] -> [a]
rmdups[] = []
rmdups(x:xs) = x:rmdups(filter(/= x)xs)

nextgen :: Board -> Board
nextgen b = survivors b ++ births b


wait :: Int -> IO()
wait n  = seqn[return() | _ <- [1..n]]

{-|life :: Board -> IO()
life b = do cls
            showcells b
            wait 500
            life (nextgen b)-}