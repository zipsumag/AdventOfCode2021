{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CommonStuff ( separateBy
                   , chunksOf
                   , strSplitAll
                   , break'
                   , getBetween
                   ) where

import qualified Data.List as L

separateBy :: forall a. Eq a => a -> [a] -> [[a]]
separateBy chr = L.unfoldr sep
  where sep :: [a] -> Maybe ([a], [a])
        sep [] = Nothing
        sep x  = Just . fmap (drop 1) . break (== chr) $ x

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = case splitAt n xs of
                     (ys, []) -> [ys]
                     (ys, zs) -> ys:(chunksOf n zs)

break' :: ([a] -> Bool) -> [a] -> ([a], [a])
break' f [] = ([], [])
break' f x@(x0:xs) 
  | f x       = ([], x)
  | otherwise = (x0:y0, y1)
  where (y0, y1) = break' f xs

strSplitAll :: String -> String -> [String]
strSplitAll _ [] = []
strSplitAll sep s
    | null rest = w:[]
    | otherwise = w:(strSplitAll sep . drop (length sep) $ rest)
  where (w, rest) = break' (L.isPrefixOf sep) s
