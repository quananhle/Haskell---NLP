import Data.Char

--helper functions
reverse' :: [a] -> [a]
reverse' = recur []
    where
        recur ys [] = ys
        recur ys (x:xs) = recur (x:ys) xs


--Part 1
any' :: (a -> Bool) -> [a] -> Bool
any' p = foldr ((||) . p) False

all' :: (a -> Bool) -> [a] -> Bool
all' p = foldr ((&&) . p) True

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

cycle' :: [a] -> [a]
cycle' ls = foldr (:) (cycle' ls) ls

scanr' :: (a -> b -> b) -> b -> [a] -> [b]
--scanr' f b ls = foldr (\x y -> (f x $ head y) : y) [b] ls --y accumulates
scanr' f b = foldr (\x y -> (f x $ head y) : y) [b]

scanl' :: (b -> a -> b) -> b -> [a] -> [b]
scanl' f b ls = reverse' $ foldl (\x y -> (f (head x) y) : x) [b] ls

inits :: [a] -> [[a]]
inits ls = foldl (\x _ -> init (head x) : x) [ls] ls
--inits = foldr (\x y -> [] : (map (x:) y)) [[]]

tails :: [a] -> [[a]]
tails ls = reverse' $ foldl (\x _-> (tail $ head x) : x) [ls] ls


--Part 2
minmax :: (Ord a) => [a] -> (a,a)
minmax xxs@(x:_) = (min, max) --assume ele in list
    where
        min = foldr (\a b -> if a < b then a else b) x xxs
        max = foldr (\a b -> if a > b then a else b) x xxs

gap :: (Eq a) => a -> a -> [a] -> Maybe Int
gap s e ls = let r = foldl iter (0, False) ls in
    case r of (c, True) -> Just c
              otherwise -> Nothing
    where
        iter (0, False) v = if s == v then (1, False) else (0, False)
        iter (i, False) v = if e == v then (i, True) else (i+1, False)
        iter r _ = r

evalExpr :: String -> Int
evalExpr ls = fst $ foldl iter (0, True) ls --assume no repeat symbols, default to add
    where
        iter x '+' = (fst x, True)
        iter x '-' = (fst x, False)
        iter x y   = case (snd x) of True  -> ((toNum y) + (fst x), True)
                                     False -> ((fst x) - (toNum y), False)
        toNum c = case c of 
                    '0' -> 0
                    '1' -> 1
                    '2' -> 2
                    '3' -> 3
                    '4' -> 4
                    '5' -> 5
                    '6' -> 6
                    '7' -> 7
                    '8' -> 8
                    '9' -> 9

words' :: String -> [String]
words' ls 
    | cs == [] = cs           --see if we can call head
    | head cs == [] = tail cs --for trailing whitespace
    | otherwise = cs
    where
        (cs, r) = foldr iter b ls 
        iter ' ' y
            | snd y == True = ([] : (fst y), False) --followed letters; new word
            | otherwise     = y                     --leading whitespace
        iter x ([], _) = ([x] : [], True)           --first letter
        iter x ((y:ys), _) = ((x:y) : ys, True)
        b = ([] , False)

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' f ls = reverse' $ fst $ foldl iter ([], True) ls
    where
        iter (x, True) y
            | f y = (x, True)
            | otherwise = ((y : x), False)
        iter (x, _) y = ((y : x), False


--MP1 reimplement
join :: a -> [[a]] -> [a]
join s = foldr iter []
    where
        iter x [] = x ++ []
        iter x y = x ++ (s : y)

unzip' :: [(a, b)] -> ([a], [b])
unzip' = foldr (\x y -> (fst x : fst y, snd x : snd y)) ([], [])
    {-where
        iter x y = (fst x : fst y, snd x : snd y) -}

runLengthEncode :: String -> [(Int, Char)]
runLengthEncode = foldr iter []
    where
        iter x [] = (1, x) : [] --nothing to compare to
        iter x y = if x == snd (head y) then (1 + fst (head y), x) : tail y else (1, x) : y

runLengthDecode :: [(Int, Char)] -> String
runLengthDecode = foldr (\x y -> (replicate' (fst x) (snd x)) ++ y) []
    where
        --iter x y = (replicate' (fst x) (snd x)) ++ y
        replicate' 0 v = []
        replicate' n v = v : replicate' (n-1) v

vigenere :: String -> String -> String
vigenere kks ls = reverse' r
    where
        (r, _) = foldl iter ([], 0) ls --snd is counter into k
        iter x y = if isLetter y then ((convert (ks !! snd x) y) : fst x, 1 + snd x) else (y : fst x, snd x)
        convert k t = num2char $ char2num k + char2num t
        char2num = \r -> (ord $ toUpper r) - ord 'A'
        num2char = \r -> toUpper $ chr $ (r `rem` 26) + ord 'A'
        ks = cycle kks

{-
index :: Eq a => a -> [a] -> Int
index x l = let (_,r) = foldl iter b l
            in case r of Nothing -> error "Not found"
                         Just i -> i
    where 
        iter (i, Nothing) y = if x /= y then (i+1, Nothing) else (i+1, Just i)
        iter r _ = r
        b = (0, Nothing)

index' :: Eq a => a -> [a] -> Int
index' x l = foldr iter (error "Not Found") $ zip l [0..]
    where
        iter (y, i) r = if x == y then i else r --r recursive call

index'' :: Eq a => a -> Int -> [a] -> Int
index'' x n l = let (_, _, r) = foldl iter b l
              in case r of Nothing -> error "Not found"
                           Just i -> i
    where
        iter (i, 1, Nothing) y = if x /= y then (i+1, 1, Nothing)
                                 else (i+1, 1, Just i)
        iter (i, n, Nothing) y = if x /= y then (i+1, n, Nothing)
                                 else (i+1, n-1, Nothing)
        iter r _ = r
        b = (0, n, Nothing)
-}