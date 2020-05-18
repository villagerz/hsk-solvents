{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

instance Functor Parser where
  fmap f (Parser g) = Parser $ fmap (first f) . g


--instance Functor Parser where
--  fmap = inParser . fmap . fmap . first

inParser f = Parser . f . runParser


first :: (a -> b) -> (a,c) -> (b,c)
first f l = (f (fst l), snd l)
-- mine
-- instance Applicative Parser where
--   pure a = Parser (\s -> Just(a,s))
--   Parser f <*> Parser y = Parser p3f
--     where p3f xs =
--             case (f xs) of
--               Just (p1f, ss) ->  fmap (first p1f) (y ss)
--               Nothing -> Nothing

-- professors
instance Applicative Parser where
  pure a = Parser (\s -> Just (a, s))
  (Parser fp) <*> xp = Parser $ \s ->
    case fp s of
      Nothing     -> Nothing
      Just (f,s') -> runParser (f <$> xp) s'

abParser :: Parser (Char,Char)
abParser = (,) <$> (char 'a') <*> (char 'b')

abParser_ :: Parser ()
abParser_ = (\_ _ -> ()) <$> (char 'a') <*> (char 'b')

intPair :: Parser [Integer]
intPair =  (\a b ->[a,b]) <$> posInt <* (char ' ') <*> posInt

instance Alternative Parser where
  empty = Parser (const Nothing)
  Parser p1 <|> Parser p2 = Parser p3
    where p3 xs =
            case (p1 xs) of
              left@(Just _) -> left
              Nothing -> p2 xs
              
data IorC = I Integer | C Char


intOrUpper :: Parser ()
intOrUpper = ( nullify posInt <|> nullify (satisfy isUpper))


nullify :: Parser a -> Parser ()
nullify  = (<$>) (\_ -> ())  
