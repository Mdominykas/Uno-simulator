module Utils where
import System.Random (StdGen, Random (randomR))

remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove x (h:t) = if x == h then t else h : remove x t

elementById :: [a] -> Int -> Maybe a
elementById [] num = Nothing
elementById (h:t) 0 = Just h
elementById (h:t) n = elementById t (n - 1)

shuffle :: Eq a => [a] -> StdGen -> ([a], StdGen)
shuffle [] rng = ([], rng)
shuffle as rng = (selectedVal : aTail, finalGen)
    where
        (selectedId, newGen) = randomR (0, length as - 1) rng
        selectedVal = case elementById as selectedId of
            Nothing -> head as
            Just val -> val
        (aTail, finalGen) = shuffle (remove selectedVal as) newGen

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs
    | n > length xs = rotate (n `mod` length xs) xs
    | otherwise = take (length xs) (drop n (cycle xs))

incrementByIndex :: (Eq t, Num t, Num a) => [a] -> t -> [a]
incrementByIndex [] _ = []
incrementByIndex (h : t) 0 = h + 1 : t
incrementByIndex (h : t) val = h : incrementByIndex t (val - 1)
