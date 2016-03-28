perms' :: Eq a => [a] -> [[a]]
perms' [] = [[]]
perms' xs = concatMap (\x -> map (x:) $ perms' $ delete' x xs) xs

-- we don't need (Eq a) context anymore!
perms :: [a] -> [[a]]
perms [] = [[]]
perms xs = map (f xs) (perms' $ indices xs)  where
    indices xs = enumFromTo 0 ((length xs) - 1)
    f es is = map (\i -> es !! i) is

delete' :: Eq a => a -> [a] -> [a]
delete' el xs = let parts = break (\x -> x == el) xs 
                    safetail [] = []
                    safetail xs = tail xs
                in fst parts ++ (safetail . snd) parts
                
                
-- so we're able to permute a list of functions, for example. ain't that nice?
test1 = (length (perms [id, \ x -> 2 * x, const 42])) == 6
test2 = (length (perms ['a', 'b', 'c'])) == 6
                
main = putStrLn $ show $ test1
