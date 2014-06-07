module Main where

import qualified Data.Map as Map
import           System.Random

type Tree a = Map.Map (Maybe a) [Maybe a]

oneOf :: RandomGen g => [a] -> g -> (a,g)
oneOf xs g =
  let (n,g') = randomR (0, length xs - 1) g
  in ((!!) xs n, g')

addHeadAndTail :: a -> a -> [a] -> [a]
addHeadAndTail h t xs = [h] ++ xs ++ [t]

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs [_] = []
pairs [x,y] = [(x,y)]
pairs (x:y:xs) = (x,y) : pairs(y:xs)

addPair :: Ord a => Map.Map a [b] -> (a,b) -> Map.Map a [b]
addPair t (x,y) = Map.insert x (y:Map.findWithDefault [] x t) t

addChain :: Ord a => Tree a -> [Maybe a] -> Tree a
addChain t xs = foldl addPair t (pairs xs)

--

readChainStep :: (Ord a, RandomGen g) => Tree a -> (Maybe a, g) -> [a]
readChainStep _ (Nothing, _) = []
readChainStep t (Just w, g) =
  case Map.lookup (Just w) t of
       Nothing -> []
       Just xs -> w:readChainStep t (oneOf xs g)

readChain :: (Ord a, RandomGen g) => Tree a -> g -> [a]
readChain t g =
  readChainStep t (case Map.lookup Nothing t of
                     Nothing -> (Nothing, g)
                     Just ws -> oneOf ws g)

main :: IO ()
main = do
  text <- readFile "corpus.txt"
  let digested = fmap (addHeadAndTail Nothing Nothing . fmap Just . words) (lines text)
      tree = foldl addChain Map.empty digested
  g <- getStdGen
  print $ unwords $ readChain tree g
