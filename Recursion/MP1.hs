import Data.Char

cycleN :: Int -> [a] -> [a]
cycleN 0 _ = []
cycleN n x = x ++ cycleN (n-1) x

countLessThan :: (Ord a) => a -> [a] -> Int
countLessThan _ [] = 0
countLessThan n (x:xs)
    | n > x = 1 + countLessThan n xs
    | otherwise = countLessThan n xs

removeAll :: (Eq a) => [a] -> [a] -> [a]
removeAll _ [] = []
removeAll check (x:xs)
    | contains check x = x : removeAll check xs
    | otherwise = removeAll check xs
-- removeAll check = filter (contains check)
    where
        contains [] _ = True
        contains (y:ys) l = if y == l then False else (contains ys l)

join :: a -> [[a]] -> [a]
join _ (l:[]) = l ++ []
join s (l:ls) = l ++ (s : join s ls)

unzip' :: [(a,b)] -> ([a], [b])
unzip' l = (left l, right l)
    where
        left [] = []
        left  ((x,_):ls) = x : left  ls
        right [] = []
        right ((_,y):ls) = y : right ls

runLengthEncode :: String -> [(Int, Char)]
runLengthEncode [] = []
runLengthEncode (c:cs) = count 1 c cs
    where
        count n x [] = (n, x) : [] --last ele
        count n x (y:ys)
            | x == y = count (n+1) y ys
            | otherwise = (n, x) : count 1 y ys

runLengthDecode :: [(Int, Char)] -> String
runLengthDecode [] = []
runLengthDecode ((n, c):xs) = (replicate n c) ++ runLengthDecode xs
--runLengthDecode ((0, _):xs) = runLengthDecode xs
--runLengthDecode ((n, c):xs) = c : runLengthDecode ((n-1, c):xs)

vigenere :: String -> String -> String
vigenere key text = vigenere' key text --to remember key
    where
        vigenere' _ [] = []
        vigenere' [] ts = vigenere' key ts
        vigenere' kks@(k:ks) (t:ts)
            | isLetter t = (convert k t) : vigenere' ks ts
            | otherwise = t : vigenere' kks ts
        convert x y = num2char $ char2num x + char2num y
        char2num = \r -> (ord $ toUpper r) - ord 'A'
        num2char = \r -> toUpper $ chr $ (r `rem` 26) + ord 'A'
        --char2num = \r -> (ord (toUpper r)) - 65
        --num2char = \r -> toUpper (chr (r `rem` 26 + 65))

