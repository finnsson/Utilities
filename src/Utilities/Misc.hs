-- {-# OPTIONS_GHC -fglasgow-exts -,  
--    
--  #-}
{-# LANGUAGE UndecidableInstances,  OverlappingInstances, FlexibleInstances, TypeSynonymInstances
 #-}
{-|
  Contains useful generic functions not found elsewhere.


-}
module Utilities.Misc where

import Data.Char
import Data.Maybe
import Data.List
import Control.Monad
import Language.Haskell.TH

-- \ Tuple functions

fst3 :: (a,b,c) -> a
fst3 (a,b,c) = a

snd3 :: (a,b,c) -> b
snd3 (a,b,c) = b

trd3 :: (a,b,c) -> c
trd3 (a,b,c) = c
trd (a,b,c) = c


-- \ String functions

-- \ ToString type class as found on stackoverflow (Porges answer at
-- http://stackoverflow.com/questions/968198/haskell-show-screwed-up)
-- Expanded with instance ToString Char.
class ToString a where
    toString :: a -> String

instance ToString String where
    toString = id

instance ToString Char where
    toString c = [c]

instance Show a => ToString a where
    toString = show



-- | Remove all spaces in a string
--
-- > "hello" == removeSpace "  he l lo  "
removeSpace :: String -> String
removeSpace = filter (/= ' ')

-- | fold1 with first argument between.
--
-- > "h.e.l.l.o" == foldr1With "." ["h","e","l","l","o"]
foldr1With :: [a] -> [[a]] -> [a]
foldr1With value = foldr1 (\l r -> l ++ value ++ r)

-- | Remove all line breaks in a string
--
-- > "testtest" == removeBreak "test\n\rtest\r"
removeBreak :: String -> String
removeBreak = filter ((/= '\r') &&* (/= '\n'))

-- | Convert first character in String to lower.
--
-- > lowerFirst "Foo" == "foo"
-- > lowerFirst "BaR" == "baR"
-- > lowerFirst "g0O" == "g0O".'
lowerFirst :: String -> String
lowerFirst = convertFirst toLower

-- | Convert first character in String to upper.
--
-- > upperFirst "foo" == "Foo"
-- > upperFirst "bAr" == "BAr"
-- > upperFirst "G0O" == "G0O".'
upperFirst :: String -> String
upperFirst = convertFirst toUpper

-- | Convert first element in list
--
-- > convertFirst (toUpper) "fO0" == "FO0"
convertFirst :: (a -> a) -> [a] -> [a]
convertFirst _ [] = []
convertFirst f (x:xs) = f x:xs

-- | Convert every space (' ') in a string to a blank ('_') instead. 
--
-- > spaceToBlank " " == "_"
-- > spaceToBlank " foo  " == "_foo__"
-- > spaceToBlank "b a r" == "b_a_r"
spaceToBlank :: String -> String
spaceToBlank "" = ""
spaceToBlank (x:xs) = (if x == ' ' then '_' else x) : spaceToBlank xs

-- | Splits a list @x@ of @a@ into a list of lists of @a@ at every @c@.
--
-- >  "splitBy "foo,bar" "',' == ["foo","bar"] ' 
splitBy :: (Eq a) => a -> [a] -> [[a]]
splitBy _ [] = [[]]
splitBy c x  = if fst p == [] then [snd p] else fst p : splitBy c ( snd p )
               where p = break (== c) x

-- | Trims every element satisfying @c@ from the beginning or end of the list.
--
-- > trim (==' ') "  foo   " == "foo"
trim :: (a -> Bool) -> [a] -> [a]
trim c = reverse . dropWhile c . reverse . dropWhile c

-- | Trims whitespace from the beginning or end.
--
-- > trimWs "  foo  " == "foo"
trimWs :: String -> String
trimWs = trim (==' ')

-- | Lambdifies a function. See '(||*)' and '(&&*)' for uses of 'lambdify'.
-- | Used in order to make operators capable of operating on functions that later on
-- | are supplied some value that all functions operate on.
--
-- > (+*) = lambdify (+)
-- > fourTwo = (*4) +* (*2)
-- > 42 == fourTwo 7
lambdify :: (x -> y -> z) -> (t -> x) -> (t -> y) -> t -> z
lambdify f a b x = f (a x) (b x)

-- | Lambdifies '(||)'.
--
-- > isBlankOrCommaChecker = (==' ') ||* (==',')
-- > isBlankOrComma = isBlankOrCommaChecker 'j'
(||*) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(||*) = lambdify (||)

-- | Lambdifies '(&&)'.
--
-- > isInRangeChecker = (>9) &&* (<30)
-- > isInRange = isInRangeChecker 17
(&&*) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&&*) = lambdify (&&)

-- | 2-point-free operator. Similar to '.', but where
-- | the second function takes two (2) arguments instead of one (1).
--
-- > multAndSquare (^2) .^.. (*)
-- > 36 == multAndSqare 2 3
(^..) :: (c -> d) -> (a -> b -> c) -> a -> b-> d
(f ^.. g) a = f . g a

-- | 3-point-free operator. See '(^..)'.
(^...) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(f ^...g ) a b = f . g a b

-- | Split a 2-tuple 'x' into a 2-stack and pass it to 'f'.
-- | The same as uncurry.
(..%) :: (a -> b -> c) -> (a,b) -> c
(..%) = uncurry 

(..%..) :: (c->d->e) -> (a->b->(c,d)) -> a -> b -> e
(f ..%.. g) a b = f ..% g a b

-- | Split a 3-tuple 'x' into a 3-stack and pass it to 'f'.
(...%) :: (a -> b -> c -> d) -> (a,b,c) -> d
(...%) f x = f (fst3 x) (snd3 x) (trd3 x)


-- | Pipes a monadic return through a non-monadic transformation function.
-- | liftM with arguments flipped.
--
-- > readIO >>* toUpper
(>>*) :: Monad m => m a -> (a -> b) -> m b
(>>*) v f = liftM f v -- v >>= (return . f)

-- > f = ((+) 2, (*) 3)
-- > x = 7
-- > r = f ..@ x
-- 
-- | gives `(9, 21)`, i.e. `(2 + 9, 3 * 7)`.
(..@) :: (a -> b, a -> c) -> a -> (b,c) 
f ..@ x = (fst f x, snd f x)

-- | Same as `..@`, but with a 3-tuple. 
(...@) :: (a -> b, a -> c, a -> d) -> a -> (b,c,d)
f ...@ x = (fst3 f x, snd3 f x, trd3 f x)


-- | Get the variable and the name as a tuple.
-- | Useful whenever you need to print error messages to a log
-- | as well as during testing.
--
-- > x = 10
-- > pair = $(varNamePair "x")
-- > 10 == fst pair
-- > "x" == snd pair
varNamePair :: String -> ExpQ
varNamePair name =
  return $ TupE [ LitE $ StringL name , VarE $ mkName name]
