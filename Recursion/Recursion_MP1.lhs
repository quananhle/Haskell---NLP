1. cycleN :: Int -> [a] -> [a]

Write a function that takes a number n and a list l 
and return a list containing the elements of l repeated n times.

> cycleN :: Int -> [a] -> [a]
> cycleN 0 x = []
> cycleN 1 x = x
> cycleN n x = x ++ cycleN (n-1) x

2. countLessThan :: (Ord a) => a -> [a] -> Int

Write a function that takes a value and a list 
and returns the number of items in the list less than the provided value.

> countLessThan :: (Ord a) => a -> [a] -> Int
> countLessThan _ [] = 0
> countLessThan x (y:ys) = if y < x then (1 + countLessThan x ys) 
>									else (0 + countLessThan x ys)

3. removeAll :: (Eq a) => [a] -> [a] -> [a]

Write a function that takes two lists 
and returns a list containing only those elements in the second list that do not appear in the first.

> removeAll :: Eq a => [a] -> [a] -> [a]
> removeAll _ [] = []
> removeAll toRemove (x:xs)
>   | x `belongs` toRemove = removeAll toRemove xs
>   | otherwise = x:removeAll toRemove xs
>       where belongs _ [] = False
>             belongs a (y:ys)
>                   | a == y = True
>                   | otherwise = belongs a ys

4. join :: a -> [[a]] -> [a]

Write a function that joins lists together using a separator value --- the separator 
should only appear between elements, and not at the end.

> join :: a -> [[a]] -> [a]
> join _ [] = []
> join _ [x] = x
> join s (x:xs) = x ++ [s] ++ (join s xs)

> join' myChar lists = case lists of
>    []      -> []
>    lists   -> attach myChar lists
>    where   attach myChar (x:[]) = x 
>            attach myChar (x:xs) = x ++ (myChar : []) ++ attach myChar xs 

> join'' :: [a] -> [[a]] -> [a]
> join'' s [] = []
> join'' s [x] = x
> join'' s (x:xs) = x ++ s ++ (join'' s xs)

5. unzip' :: [(a,b)] -> ([a], [b])

Write a function that takes a list of two-tuples 
and returns a tuple of two lists -- the first containing the first element of each tuple, 
and the second the second element (the reverse of "zip").

> unzip' :: [(a, b)] -> ([a], [b])
> unzip' [] = ([], [])
> unzip' ((x, y):zs) =
>     let (xs, ys) = unzip' zs 
>      in (x:xs, y:ys)

6. runLengthEncode :: String -> [(Int,Char)]

Run-length encoding is a simple form of data compression 
that replaces characters in a stream with the count of adjacent occurrences 
of that character and just a single instance of the character itself. 
Write a function that takes a string 
and returns a list of tuples reprenting the run-length encoding of that string.

> runLengthEncode :: String -> [(Int,Char)]
> runLengthEncode [] = []
> runLengthEncode (x:xs) = encode 1 x xs where
>	encode n x [] = [(n, x)]
>   	encode n x (y:ys)
> 				| x == y = encode (n + 1) x ys
> 				| otherwise = (n, x) : encode 1 y ys

> runLengthEncode' :: String -> [(Int,Char)]
> runLengthEncode' = foldr encode' []
>      where encode' c []         = [(1,c)]
>            encode' c ((n,x):ts) | c == x    = (n+1,x):ts
>                                 | otherwise = (1,c):(n,x):ts

< runLengthEncode'' :: String -> [(Int, Char)]
< runLengthEncode'' s = map encode'' (group s)
<    where
<    encode'' xs = (length xs, head xs) 

7. runLengthDecode :: [(Int,Char)] -> String

Write a function that takes the output of runLengthEncode 
and returns the original string.

> runLengthDecode :: [(Int,Char)] -> String
> runLengthDecode l = concat (map fromrunLengthEncode l)
>	where
>	fromrunLengthEncode (n, x) = replicate n x

> runLengthDecode' :: [(Int,Char)] -> String
> runLengthDecode' [] = []
> runLengthDecode' ((a,b):xs) = replicate a b ++ (runLengthDecode xs)

8. vigenere :: String -> String -> String

The Vigenere encryption scheme is similar to the Caesar cipher presented in class in that it makes use of shifting, 
but instead of a single numeric key applied uniformly to the entire plain text, 
a string of characters is used as the key. The numeric value of each character (as its position in the alphabet) is used as a shift value, 
and if the key is shorter than the length of the plain text it is simply repeated.

< vigenere :: String -> String -> String
< vigenere x [] = []
< vigenere [] x = x
< vigenere x(y:ys)
<    | (jj < 26) = [ summ] ++ vigenere (tail g) ys
<    | otherwise = [ (['A'..'Z'] !! (jj - 26))] ++ vigenere (tail g) ys
<     where ff = if y `elem` ['.', ',', '?', '!', ':', ';', ' '] then ([' '] ++ x) else x
<           g = cycleN (rem (length ff) (length(y:ys))+1) (ff)
<           ux = toUpper (head g)
<           uy = toUpper y  
<           jj = (ord(ux)-65 +ord(uy)-65)
<           summ = if (jj < 0) || (jj == 3) then chr(ord(uy)) else (['A'..'Z'] !! jj)     mp
