import Data.Char

doubleMe x = x + x
doubleUs x y = x * 2 + y * 2

{-| Concatenation 
    [1, 2, 3, 4,] ++ 5  -- Wrong!
    [1, 2, 3, 4] ++ [5] -- Right!
    
    Other examples:
    "hello" ++ " world"
-}

{-| Accessing List Elements 
    * Haskell lists are 0 indexed:
    
    "Steve Buscemi" !! 6
     >> 'B' 
-}

-- head [5, 4, 3, 2, 1]
--  >> 5

-- tail [5, 4, 3, 2, 1]
-- >> [4, 3, 2, 1]

-- last [5, 4, 3, 2, 1]
-- >> 1

-- init [5, 4, 3, 2, 1]
-- >> [[5, 4, 3, 2]

{-| List Comprehensions
    [x*2 | x <- [1..10]] -- drawing values from single list
    
    [x+y | x <- [1, 2, 3], y <- [10. 100, 1000]] -- drawing values from multiple lists
-}
concat :: [[a]] -> [a]
concat xss = [x | xs <- xss, x <- xs]

firsts :: [(a, b)] -> [a]
firsts ps = [x | (x, _) <- ps]

findValue :: Eq a => a -> [(a, b)] -> [b]
findValue k t = [v | (k', v) <- t, k == k']

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and[x <= y | (x, y) <- pairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..n], x == x']
                  where n = length xs - 1

{- | Caesar cipher

    'haskell is fun' encoded as "kdvnhoo lv ixq"

-}
let2int :: Char -> Int
let2int c = ord c - ord 'a'
int2let :: Int -> Char
int2let n = chr(ord 'a' + n)

shift                   :: Int -> Char -> Char
shift n c   | isLower c = int2let((let2int c + n) `mod` 26)
            | otherwise = c

encode :: Int -> String -> String
encode n xs =  [shift n x | x <- xs]

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum[((o - e)^2)/ e | (o, e) <- zip os es]

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct x y = sum[ (*) a b | (a,b) <- zip x y]


foldright :: (a -> b -> b) -> b -> [a] -> b
foldright f v [] = v
foldright f v (x:xs) = f x (foldright f v xs)

{-| foldLeft 
    * Define recursive fucnitons on lists using an operator that is assumed ot associate to the left
    * sum  = sum' 0
             where 
                sum' v[] = v
                sum' v(x: xs) = sum'(v + x) xs
    * Example:  sum [1, 2, 3]
                sum'0 [1, 2, 3]
                sum'(0 + 1)[2, 3]
                sum'((0+1)+2)[3]
                sum'(((0+1)+2)+3)[]
                (((0+1)+2)+3)
                6
    f v []      = v
    f v (x:xs)  = f (v (+) x) xs
   
-}

foldleft            :: (a -> b -> a) -> a -> [b] -> a
foldleft f v []     = v
foldleft f v(x:xs)  = foldleft f (f v x) xs


type Bit = Int

bin2int         :: [Bit] -> Int
bin2int bits    =  sum [ w * b| (w, b) <- zip weights bits]
                    where weights = iterate(*2) 1

bin2inttwo  :: [Bit] -> Int
bin2inttwo  = foldr( \x y -> x + 2 * y) 0



