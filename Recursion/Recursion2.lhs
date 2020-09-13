> import Data.List
> import System.IO

-- 1. 
Let f x y z = x : ([y] : [z]).  What is the type of f?

> f x y z = x : ([y]:[z])

*Main> :t f
f :: [a] -> a -> [a] -> [[a]]

f is a function that takes list x, element y, and list z, and produces a list that contains the sub-lists x, y, z. 
f adds the list x, element y, and list z to the new list in order x, y, and z.


2. To answer the following questions, use :infoType to see what typeclasses Type is an instance of, then use :infoTypeClass to see what functions / operators the different typeclasses support.
a. The test False<True is allowed because < is provided by a typeclass that Bool is an instance of. What is the typeclass and what is the type of (<) (including the typeclass)?

Prelude> :i <
class Eq a => Ord a where
  ...
  (<) :: a -> a -> Bool
  ...

  Typeclass of (<) is Eq
  Type of (<) is a function that takes/compares two elements and returns boolean type Bool.

b. What are the functions that give the ASCII code for a character and give the ASCII character for an integer (if you use a type annotation ::Char)?  (I.e., fcn1'a' yields 97, fcn297::Char yields 'a'.)  Also, what are their types (including the typeclass)?

The functions are fromEnum and toEnum

Prelude> :t fromEnum
fromEnum :: Enum a => a -> Int
Prelude> :t toEnum
toEnum :: Enum a => Int -> a

The function fromEnum has the type that take an enum element and return an int type element.
The function toEnum has the type that take an int type element and return an enum type element.

Prelude> fromEnum 'b'
98
Prelude> toEnum 45 :: Char
'-'

c. The functions in part (b) are provided by a typeclass that Char is an instance of.  What is the typeclass?

The typeclass of fromEnum is Enum. The typeclass of toEnum is Enum.

3. The function twice list should return true iff some value occurs twice in the list. E.g.,

a. What is the type of twice? (Include the typeclass.)

Prelude> :t filter
filter :: (a -> Bool) -> [a] -> [a]

twice :: Eq a => [a] -> Bool
The type of twice must be a function that takes a list and return Boolean type Bool. The type class of twice is Eq. 

b. Briefly describe the syntactic and semantic bugs in the program below. (For syntactic errors, don't just parrot the Haskell error messages; give a brief human-understandable description.)
 
twice [] = False 								-- if the list is empty, return False
twice [_] = False 							 	-- if the list has only one element, return False
twice [x,x] = True 							 	-- Conflicting definitions for ‘x’. Variable 'x' can only be declared once in a pattern.
Fix: twice [x,y] | x == y = True 
twice ( _ ++ [x] ++ _ ++ [y] ++ _ ) = x == y 	-- '++' function can not be used within pattern.
Fix: twice ( _ : [x] : _ : [y] : _ ) = x == y
twice (h1 : h2 : t) == (h1 == h2 || twice h1 t) -- twice must return a value of Bool. twice is the function that returns a Boolean type Bool (h1).
Fix: twice (h1 : h2 : t) = h1 == h2  || twice (h1 : t)

b. Rewrite twice to make it work.  Keep using definition by cases; feel free to add/change/delete cases as you see fit.

> twice :: (Eq a) => [a] -> Bool
> twice [] = False
> twice [_] = False 
> twice [x,y] | x == y = True 
> twice xs = if (head xs) == (head (tail xs)) then True 
>										   	  else twice ((head xs) : (tail (tail xs)))
>											    || twice (tail xs)

*Main> twice [4,5,3,6,4,7,8,9,11,21,34,45,23,4]
True
*Main> twice [1..15]
False

c. Write a definition by cases for twice that only has two cases (one recursive, one not).

-- recursive

> twice1 :: (Eq a) => [a] -> Bool
> twice1 []  = False
> twice1 [_] = False
> twice1 (x:xs) = x `elem` xs || twice1 xs --twice (x:xs) = if elem x xs then True else twice xs

*Main> twice1 [4,5,3,6,4,7,8,9,11,21,34,45,23,4]
True
*Main> twice1 [1..15]
False

-- non-recursive

> twice1' :: (Eq a) => [a] -> Bool
> twice1' []  = False
> twice1' [_] = False
> twice1' xs = length (nub xs) /= length xs --nub :: Eq a => [a] -> [a]

*Main> twice1' [4,5,3,6,4,7,8,9,11,21,34,45,23,4]
True
*Main> twice1' [1..15]
False

d. Rewrite your definition from part (c) using cases and guards; break up the 3-clause logical or test to use a sequence of guards.  (Don't leave any || in the definition.)
twice x pattern    
		| guard1 -> result1    
		| guard2 -> result2
	(omitted)

> twice2 :: (Eq a) => [a] -> Bool
> twice2 []  = False
> twice2 [_] = False
> twice2 (x:y:xs) | x == y  	= True
>				  | x `elem` xs = True
>				  | otherwise 	= twice2 (y:xs)

*Main> twice2 [4,5,3,6,4,7,8,9,11,21,34,45,23,4]
True
*Main> twice2 [1..15]
False

e. Rewrite your definition from part (c) to be of the form twice x = case x of ....  
You can add guards to a case clause using the syntax
case expr of pattern | guard1 -> result1                  
					 | guard2 -> result2
				(omitted)

> twice3 :: (Eq a) => [a] -> Bool
> twice3 l = case l of 
>	[]      -> False; [_] -> False
>	(x:xs)  -> if x `elem` xs then True 
>						   	  else twice3 xs

*Main> twice3 [4,5,3,6,4,7,8,9,11,21,34,45,23,4]
True
*Main> twice3 [1..15]
False

B. Lecture 4 

Higher-Order Functions

4. Consider the following claim: “A Haskell function is higher order if and only if its type has more than one arrow.”  Is this correct?  Give a brief argument. 

This is strictly correct as Haskell functions are always curried. Since a higher-order function must be able to either takes a function as an argument, or returns a function as its result, no function with a single arrow in its type signature in Haskell can be a higher-order function. Thus, a higher-order function in Haskell must have more than one arrow.

Currying/Uncurrying

5. Let f::(a -> a -> a) -> a -> a ->a 

a. Rewrite f * (2 3) so that it has no syntax errors and yields 6 if f h x y = h x y

Prelude> :t (*)
(*) :: Num a => a -> a -> a

> func :: Num a => (a -> a -> a) -> a -> a -> a
> func f a b = f a b 

*Main> func (*) 2 3
6
*Main> func (+) 4 5
9

b. Write the definition of a function g::((a,a) -> a,(a,a)) -> a so that g is an uncurried version of f.   
Calling your function on *, 2, and 3 should yield 6.

> g :: Num a => ((a, a) -> a, (a, a))-> a 
> g (h, (x, y)) = h (x, y)

Map and Filter

6. Let f1 = filter (\x -> x > 0) and f 2 = filter(\x -> x < 10), and let nbrFilter g x = length(filter g x)

filter :: (a -> Bool) -> [a] -> [a]
map :: (a -> b) -> [a] -> [b]

a. Rewrite f1(f2[-5..15]) so that it uses function composition to apply just one function to the list.

> f1 = filter (\x -> x > 0)
> f2 = filter(\x -> x < 10)
> f3 = filter (\x -> x > 0 && x < 10)
> f4 x = (f1 . f2) x 									   -- f4 :: [Integer] -> [Integer]
> f5 xs = map (\x -> x) (filter(\x -> x > 0 && x < 10) xs) -- f5 :: (Ord b, Num b) => [b] -> [b]

*Main> f1(f2[-5..15])
[1,2,3,4,5,6,7,8,9]
*Main> f3 [-5..15]
[1,2,3,4,5,6,7,8,9]
*Main> f4 [-5..15]
[1,2,3,4,5,6,7,8,9]
*Main> f5 [-5..15]
[1,2,3,4,5,6,7,8,9]

b. Rewrite the nbrFilter function definition to have the form
nbrFilter g = function composition involving length and filter ... and leaving out x.

> nbrFilter g x = length(filter g x)



> nbrFilter g = length . filter g


*Main> :t nbrFilter
nbrFilter :: (a -> Bool) -> [a] -> Int
*Main> nbrFilter (>4) [1..10]
6

> nbrFilter' g = \x -> (length . filter g) x

> nbrfilter' = \g -> \x -> (length . filter g) x


*Main> :t nbrFilter'
nbrFilter' :: (a -> Bool) -> [a] -> Int
*Main> nbrFilter' (>4) [1..10]
6

> nbrFilter'' :: (Ord a, Eq a, Num a) => [a] -> Int
> nbrFilter'' = length . filter (4 <)

*Main> nbrFilter'' [1..10]
6

> nbrFilter''' :: (Ord a, Eq a, Num a) => [a] -> Int
> nbrFilter''' g = length (filter (\z -> z > 4) g)

*Main> nbrFilter''' [1..10]
6

--> nbrFilter''' = map (length . filter ((==) 1)) -- List check
*Main> :t nbrFilter'''
nbrFilter''' :: [[Integer]] -> [Int]

Lambda Functions

7. 

a. Rewrite f g x y = g x (y x) three ways, first f g x = unnamed lambda function, then f g = unnamed lambda function, and finally f = unnamed lambda function.
 
> f7 g x y = g x (y x)

*Main> :t f7
f7 :: (t1 -> t2 -> t3) -> t1 -> (t1 -> t2) -> t3

> f7' g x = (\a -> g x (a x))

*Main> :t f7'
f7' :: (t1 -> t2 -> t3) -> t1 -> (t1 -> t2) -> t3

> f7'' g = \a -> (\b -> g a (b a))

*Main> :t f7''
f7'' :: (t1 -> t2 -> t3) -> t1 -> (t1 -> t2) -> t3

> f7''' = \x -> (\y -> (\z -> x y (z y)))

*Main> :t f7'''
f7''' :: (t1 -> t2 -> t3) -> t1 -> (t1 -> t2) -> t3

b. Briefly, how does var = lambda function relate to first-class functions in Haskell?

The concept of first-class functions is that the functions are no different than any other data used in a program. 
Lambda expressions var = lambda function also can be used both as arguments and well as returned as values from other functions. Lambda expressions share the same idea or derived from the original concept of the first-class functions.
“f g x = \y -> g x (y x)” basically says we are setting “f g x” function to equal to “\y -> g x (y x)” function, using the second one as both argument and returned value.


List Folding

8. Let's re-implement the foldl function in multiple ways. Your foldl only needs to work on lists.

*Main> :t foldl
foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
*Main> sum' xs = foldl (\acc x -> acc + x) 0 xs
*Main> sum' [5..10]
45

a. Write a definition for foldl using conditional expressions: foldl1 f a x = if x == [] then etc.

> foldl'' f a x = if x == [] then a 
>							 else foldl'' f (f a (head x)) (tail x)

*Main> :t foldl''
foldl'' :: Eq a => (t -> a -> t) -> t -> [a] -> t
*Main> sum' xs = foldl'' (\acc x -> acc + x) 0 xs
*Main> sum' [5..10]
45

b. Rewrite the definition using function definition by cases: foldl2 ...

> foldl2 :: (b -> a -> b) -> b -> [a] -> b
> foldl2 _ z [] = z
> foldl2 f z (x:xs) = foldl2 f (f z x) xs

*Main> :t foldl2
foldl2 :: (b -> a -> b) -> b -> [a] -> b
*Main> sum' xs = foldl2 (\acc x -> acc + x) 0 xs
*Main> sum' [5..10]
45

c. Rewrite the definition using a case expression: foldl3 f a x = case x ....

> foldl3 f a x = case x of
>					  [] -> a
>					  (x:xs) -> foldl3 f (f a x) xs

*Main> :t foldl3
foldl3 :: (t1 -> t2 -> t1) -> t1 -> [t2] -> t1
*Main> sum' xs = foldl3 (\acc x -> acc + x) 0 xs
*Main> sum' [5..10]
45
