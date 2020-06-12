module Parsers where
{-# LANGUAGE BlockArguments #-}

import Data.Char
{-|
    Different parsers will likely return different kinds of trees, or more generally, any kind of value.
    This declaration states that a Parser of type a is a function that takes an input string and priduces a list of
     results, ach of which is a pair comprising a result value of type a and an output string.
-}
type Parser a  = String -> [(a, String)]

-- *** Basic Parsers *** --

-- Return
-- Always succeeds with the result without the result v, without consuming any of the input string
yield      ::  a -> Parser a
yield  v    =   \inp -> [(v, inp)]


-- Failure
-- Always fails regardless of the contents of the input string
failure ::  Parser a
failure =   \inp -> []

-- Item
-- Fails if the input string is empty, and succeeds with the first character as the result value otherwise:
item    ::  Parser Char
item    =   \inp -> case inp of
                    [] -> []
                    (x: xs) -> [(x, xs)]

{-|
    Because parsers are functions, they could be applied to a string using normal function application, but we prefer
    to abstract from the representation of parsers by defining our own application function:
-}
parse       ::  Parser a -> String -> [(a, String)]  -- Applicative
parse p inp  =  p inp


-- *** Sequencing *** --
{-|
    Perhaps the simplest way of combining two parsers is to apply one after the other in sequence, with the output string
    returned by the first parser becoming the input string to the second. But how shoudl the result values from the two
    parsers be handled? One approach would be to combine  the two values as a pair, using a sequencing operation for
    parsers with the following type:

        Parser a -> Parser b -> Parser (a, b)

    In practice, however, it turns out to be more convenient to *combine the sequencing of parsers with the processing
    of their result values*, by means of a sequencing operator >>== ("then") defined a follows:

(>>==) :: Parser a -> (a -> Parser b) -> Parser b
p >>== f = \inp -> case parse p inp of
                        [] -> []
                        [(v, out)] -> parse (f v) out

    That is, the parser p >>== f fails if the application of the parse p to the input string fails, and otherwise
    applies the function f to the result value to give a second parser, which is then applied to the ouput string to
    give the final result. In this manner, the result value produced by the first parser is made directly available for
    processing by the second.
    A typical parser build using >>== has the following structure:
        p1 >>= \v1 ->
        p2 >>= \v2 ->
        .
        .
        .
        pn >>= \vn ->
        return (f v1 v2 ... vn)

      That is, apply the parse p1 and call its result value v; then apply the parser p2 and call its result value v2;
      ...; then apply the parse pn and call its result value vn; and finally combine all the results into a single value
      by applying the function f. Haskell provides a special syntax for such parsers, allowing them to be expressed
      in the following, more appealing, form:

        do v1 <- p1
           v2 <- p2
           .
           .
           .
           vn <- pn
           return(f v1 v2 ... vn)
    -}
-- A parser that consumes three characters, discards the second, and returns the first and third as a pair can now be
-- defined as follows:
{-|
    Learning resources:
    1. https://en.m.wikibooks.org/wiki/Haskell/do_notation
    2. https://upload.wikimedia.org/wikipedia/commons/2/26/Haskell.pdf
-}

(>>==) :: Parser a -> (a -> Parser b) -> Parser b
p >>== f = \inp -> case parse p inp of
                        [] -> []
                        [(v, out)] -> parse (f v) out
                        
(>>>) :: Parser a -> Parser b -> Parser b -- executes an action + ignores result, evaluates next action/expression
p >>> f = p >>== \_ -> f


p   ::     Parser (Char, Char)
p   =      item >>== (\ x1 -> item >>== (\ x2 -> item >>== (\ x3 -> yield (x1,x2) )))
--do { x <- item; item; item; o <- item; return o }  --

{-| Choice
    Another way of combining two parsers is to apply the first parser to the input string, and if this fails to apply
    the second instead. Such a choice operator can be defined as follows:
-}
(+++)   ::  Parser a -> Parser a -> Parser a
p +++ q =   \inp -> case parse p inp of
                        [] -> parse q inp
                        [(v, out)] -> [(v, out)]

{-| Derived primitives
    Using the three basic parsers together with sequencing and choice, we can now defined a number of other useful parsing
    primitives. First of all, we defined a parser sat p for single characters that satisfy the predicate p:
-}
sat         :: (Char -> Bool) -> Parser Char
sat p       =   item >>== (\x -> if p x then yield x else failure )

digit       ::  Parser Char
digit       =   sat isDigit

lower       ::  Parser Char
lower       =   sat isLower

upper       ::  Parser Char
upper       =   sat isUpper

letter      ::  Parser Char
letter      =   sat isAlpha

alphanum    ::  Parser Char
alphanum    =   sat isAlphaNum

char        ::  Char -> Parser Char
char x      =   sat(==x)

-- With `char` we can defined a parser `string xs` for the string of characters xs, with the string itself returned as
-- the result value:
-- do notation doesnt work properly
-- the Monad for Parser needs to be inscope

string          ::  String -> Parser String
string []       =   yield[]
string (x:xs)   = char x >>== (\x1 -> string xs >>==( \x2 -> yield(x: xs)))

{-|
    Our next two parsers, many p and many1 p, applu, a parser p as many times as possible until it fails, woith the result
    values from each successful appliaction of p being combined as a list. Diff is aht many1 is lower-bounded by 1 while
    many is 0.
-}

many    :: Parser a -> Parser [a]
many p  =   many1 p +++ yield[]
many1   ::  Parser a -> Parser[a]
many1 p =  p >>== (\v -> many p >>== (\vs -> yield(v:vs)))

{-|
    Using many and many1, we can defined parsers for identifiers comprising a lower-case letter followed
    by zero or more alphanumeric characters, natural numbers comprising one or more digits, and spacing comprising
    zero or more space, tab, or newline characters.
   -}
ident   ::  Parser String
ident   =   lower >>== (\x -> (many alphanum >>== (\xs -> yield (x: xs))))

nat ::  Parser Int
nat =   many1 digit >>== (\xs -> yield( read xs))

space   ::  Parser ()
space   =   many(sat isSpace) >>==(\s -> yield ())

{-|
    Handling spacing

    We define a new primitive that ignores any space before and afte applying a parser
    for a token"
-}
token   ::  Parser a -> Parser a
token p  =   space >>==(\s -> p >>== (\v -> space >>==(\ss -> yield(v))))

-- using token, it si now easy to defined parsers that ignore spacing around
-- identifiers, natural numbers, and special symbols

identifier  ::  Parser String
identifier  =   token ident

natural     ::  Parser Int
natural     =   token nat

symbol      ::  String -> Parser String
symbol xs   =   token (string xs)

{-|
    Such a list begins with an opening square bracket and a nautural number, followed by
    zero or more commas and natural numbers, and concludes with a closing square bracket.
-}
nemptnumset ::  Parser[Int]
nemptnumset =   symbol "[" >>== (\j ->
                    natural >>== (
                    \n  ->
                    many(symbol "," >>== (\k -> natural)) >>== (
                    \ns ->
                    symbol "]" >>> yield(n:ns) -- added the (>>>) instead of (>>), might break things
                    )))

commnum :: Parser [Int]
commnum  =  symbol "[" >>==(\c -> many(natural >>== (\h -> symbol "," >>== (\g -> yield(h)))))

--commnum  =  symbol "[" >>==(\c -> many(natural >>== (\h -> symbol "," >>== (\g -> yield(h)))))
--commnum  =  symbol "[" >>==(\c -> many(digit) >>== (\h -> symbol "," >>== (\g -> yield(h))))

{-|
    Arithmetic Expressions
    
    The syntactic structure of a language can be formalized using the mathematical notion
    of grammar, which is a set of rules that describes how strings of the language can be 
    constructed.
    
    Grammar to have a separate rule for each level of priority, with addition at the lowest level, multiplication at the
    middle, and parentheses and numbers at the highest
    
    expr    ::=     expr + expr | term
    term    ::=     term * term | factor
    factor  ::=     (expr) | nat
    nat     ::=     0 | 1 | 2 | ... 
-}
-- See if you can replace this with teh (>>) operator. This might fix the earlier issues as I guess become
-- Haskell prelude monadic block context could not find the instance for Parser, thus could not appropriately 
-- sequence the operation.

expr    ::  Parser Int
expr    =   term >>== (\t -> (symbol "+" >>== (\s -> expr >>== (\e -> yield( t + e)))) +++ yield(t))

term    ::  Parser Int
term    =   factor >>== (\f -> symbol "*" >>== (\s -> term >>== (\t -> yield(f*t))) +++ yield(f))

factor  ::  Parser Int
factor  =   symbol "(" >>== (\s -> expr >>== (\e -> symbol ")" >>== (\ss -> yield (e)))) +++ natural


eval    ::  String -> Int
eval xs =   case parse expr xs of
                [(n, [])] -> n
                [(_, out)] -> error("unused unput " ++ out)
                [] -> error "invalid input"







