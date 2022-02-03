import Data.List (group)
-- Problem 1
-- (*) Find the last element of a list.
-- (Note that the Lisp transcription of this problem is incorrect.)
-- Example in Haskell:
-- λ> myLast [1,2,3,4]
-- 4
-- λ> myLast ['x','y','z']
-- 'z'

myLast :: [a] -> a
myLast xs = last xs

-- last is the list function to return the last index of a list
-- it is pattern exhaustive

{----------------------------------------------------------------------------------------------}

-- Problem 2
-- (*) Find the last but one element of a list.
-- (Note that the Lisp transcription of this problem is incorrect.)
-- Example in Haskell:
-- λ> myButLast [1,2,3,4]
-- 3
-- λ> myButLast ['a'..'z']
-- 'y'

myButLast :: [a] -> a
myButLast xs = xs !! (length xs - 2)

-- uses the (!!) operator function to call the length function on the list argument subtracting 2 from the output
-- This uses functions currying taking the output of `length xs` as input into -> `output - 2` as input into -> `xs !! output` as output for -> myButLast xs = output 

{-------------------------------------------------------------------------------------------------}

-- Problem 3
-- (*) Find the K'th element of a list. The first element in the list is number 1.
-- Example:
-- * (element-at '(a b c d e) 3)
-- c
-- Example in Haskell:
-- λ> elementAt [1,2,3] 2
-- 2
-- λ> elementAt "haskell" 5
-- 'e'

elementAt :: [a] -> Int -> a
elementAt xs k = xs !! (k - 1)

-- wow over thought that one for a while. had all sorts of garbage written in here.
-- using the (!!) operator again like problem 2 just grab the index passed in on the list subtracting one since this is index 1 based

-- Problem 4
-- (*) Find the number of elements of a list.
-- Example in Haskell:
-- λ> myLength [123, 456, 789]
-- 3
-- λ> myLength "Hello, world!"
-- 13

myLength :: [a] -> Int 
myLength []     = 0
myLength (_:xs) = 1 + myLength xs

-- so here I am using recursion and pattern matching 
-- basic case is being passed an empty list and since we are calculating length we don't need to worry about multiplying by zero at the end and destroying the product so return 0
-- for the other base case we don't care what the start is since we are just counting and we know it at least exists at this point
-- from here add one and call myLength again with the remainder of the list argument input.
-- continue until you have an empty list and you will sum the product outputting the result

{---------------------------------------------------------------------------------------------------------}

-- Problem 5
-- (*) Reverse a list.
-- Example in Haskell:
-- λ> myReverse "A man, a plan, a canal, panama!"
-- "!amanap ,lanac a ,nalp a ,nam A"
-- λ> myReverse [1,2,3,4]
-- [4,3,2,1]

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- this recursively calls the function passing in the end of the original arg and tacking on the beginning each time 
-- - For Example:
-- > myReverse [1,2,3]
-- > myReverse [2,3] ++ [1]
-- > (myReverse [3] ++ [2]) ++ [1]
-- > ((myReverse [] ++ [3]) ++ [2]) ++ [1]
-- > (([] ++ [3]) ++ [2]) ++ [1]
-- > [3,2,1]

{-----------------------------------------------------------------------------------------------------------}

-- Problem 6
-- (*) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
-- Example in Haskell:
-- λ> isPalindrome [1,2,3]
-- False
-- λ> isPalindrome "madamimadam"
-- True
-- λ> isPalindrome [1,2,4,8,16,8,4,2,1]
-- True

isPalindrome :: (Eq a) => [a] -> Bool 
isPalindrome xs = myReverse xs == xs

-- I am using the myReverse function I wrote above to check if the list argument passed as input is equal to itself and return the output of that function as output of isPalindrome

 {-----------------------------------------------------------------------------------------------------------}

-- Problem 7
-- (**) Flatten a nested list structure.
-- Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).
-- Example:
-- * (my-flatten '(a (b (c d) e)))
-- (A B C D E)
-- Example in Haskell:
-- We have to define a new data type, because lists in Haskell are homogeneous.
-- data NestedList a = Elem a | List [NestedList a]
-- λ> flatten (Elem 5)
-- [5]
-- λ> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
-- [1,2,3,4,5]
-- λ> flatten (List [])
-- []

data NestedList a = Elem a | List [NestedList a]
-- a NestedList is a data type custom defined to be EITHER an Elem of a List of type NestedList

myFlatten :: NestedList a-> [a]
myFlatten (List []) = [] -- if passed an empty Nested List then return an empty list
myFlatten (Elem x) = [x] -- if the NestedList is just an Elem then 'flatten' and 'map' the Elem to a list
myFlatten (List (y:ys)) = myFlatten y ++ myFlatten (List ys) -- otherwise we extrude the first value as an element and the rest as a list and recursively call flatten on each. 

{-----------------------------------------------------------------------------------------------------------}

-- Problem 8
-- (**) Eliminate consecutive duplicates of list elements.
-- If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
-- Example:
-- * (compress '(a a a a b c c a a d e e e e))
-- (A B C A D E)
-- Example in Haskell:
-- λ> compress "aaaabccaadeeee"
-- "abcade"

compress ::(Eq a) => [a] -> [a]
compress = map head . group

-- group looks from equal values and puts them together (using Data.List.group in imports)
-- -- then using (.) |infixr 9| take the output of the grouped values and extract the head off each of the groups.
-- for example:  group "Mississippi"
-- ["M","i","ss","i","ss","i","pp","i"]

-- alternative
find :: (Eq a) => a -> [a] -> Bool
find x []     = True
find x (y:ys) = x == y || find x ys

compress' :: (Eq a) => [a] -> [a]
compress' []     = []
compress' (x:xs) | x `find` xs = compress' xs
           | otherwise = x : compress' xs

{-----------------------------------------------------------------------------------------------------------}

-- Problem 9
-- (**) Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.
-- Example:
-- * (pack '(a a a a b c c a a d e e e e))
-- ((A A A A) (B) (C C) (A A) (D) (E E E E))
-- Example in Haskell:
-- λ> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 
--              'a', 'd', 'e', 'e', 'e', 'e']
-- ["aaaa","b","cc","aa","d","eeee"]

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = let (first, rest) = span (==x) xs -- (==x) conditional for "equal to x"
               in (x:first) : pack rest

{-
span , applied to a predicate p and a list xs , returns a tuple where first element is longest prefix (possibly empty) of xs of elements that satisfy p and second element is the remainder of the list:

span (< 3) [1,2,3,4,1,2,3,4] == ([1,2],[3,4,1,2,3,4])
span (< 9) [1,2,3] == ([1,2,3],[])
span (< 0) [1,2,3] == ([],[1,2,3])
-}

{-----------------------------------------------------------------------------------------------------------}

-- Problem 10
-- (*) Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
-- Example:
-- * (encode '(a a a a b c c a a d e e e e))
-- ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))
-- Example in Haskell:
-- λ> encode "aaaabccaadeeee"
-- [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
encode' :: Eq b => [b] -> [(Int, b)]
encode' [] = []
encode' xs = map (\x -> (length x,head x)) (group xs)
-- group takes the list passed in and groups the similar items together as they come up
 -- then outputs a new list
-- map applies the lambda function to the output of the group function and outputs a new list
 -- in this case we want to take the length of (x) and the head of (x) and build a tuple pair
-- so 
 -- encode' ['a','a','a','b','b','b','c','c','c'] = 
  -- map (\x -> (length x, head x)) (group ['a','a','a','b','b','b','c','c','c'])
   -- map (\"aaa" -> (length "aaa",head "aaa")) (["aaa","bbb","bbb"]) = [(3,'a')]
    -- map (\"bbb" -> (length "bbb",head "bbb")) (["aaa","bbb","bbb"]) = [(3,'a'),(3,'b')]
     -- map (\"ccc" -> (length "ccc",head "ccc")) (["aaa","bbb","bbb"]) = [(3,'a'),(3,'b'),(3,'c')]